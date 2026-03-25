# 0. Load libraries ----

library(tidyverse)
library(stringdist)
library(openxlsx2)

# 1. Define paths ----

drugbiol_path <- "data/AACT/Interventions.xlsx"
other_names_path <- "data/AACT/InterventionOtherNames.xlsx"
drugbank_path <- "data/drugbank_all_full_database/drugbank vocabulary.csv"
output_path <- "results/ClinicalTrials/interventions_drugbank_mapping.xlsx"

required_drugbiol_cols <- c("id", "nct_id", "intervention_type", "name", "description")
required_other_names_cols <- c("other_name_id", "intervention_id", "other_name")

missing_files <- c(drugbiol_path, other_names_path, drugbank_path)[
  !file.exists(c(drugbiol_path, other_names_path, drugbank_path))]

if (length(missing_files) > 0) {
  stop(
    sprintf(
      "Missing required input file(s): %s",
      paste(missing_files, collapse = ", ")
    ),
    call. = FALSE
  )
}

# 2. Load local data ----

Interventions <- wb_to_df(drugbiol_path)
DrugBiol_raw <- Interventions %>% filter(intervention_type %in% c("DRUG", "BIOLOGICAL", "COMBINATION_PRODUCT", "DIETARY_SUPPLEMENT", "OTHER"))
InterventionOtherNames_raw <- wb_to_df(other_names_path)
InterventionOtherNames_raw <- InterventionOtherNames_raw %>% filter(intervention_id %in% DrugBiol_raw$id)

drugbank_vocabulary <- read_csv(drugbank_path, show_col_types = FALSE)

missing_drugbiol_cols <- setdiff(required_drugbiol_cols, names(DrugBiol_raw))
missing_other_names_cols <- setdiff(required_other_names_cols, names(InterventionOtherNames_raw))

if (length(missing_drugbiol_cols) > 0) {
  stop(
    sprintf(
      "DrugBiol.xlsx is missing required column(s): %s",
      paste(missing_drugbiol_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

if (length(missing_other_names_cols) > 0) {
  stop(
    sprintf(
      "InterventionOtherNames.xlsx is missing required column(s): %s",
      paste(missing_other_names_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

DrugBiol_source <- DrugBiol_raw %>%
  select(any_of(c("id", "nct_id", "intervention_type", "name", "description"))) %>%
  distinct()

InterventionOtherNames_source <- InterventionOtherNames_raw %>%
  select(any_of(c("other_name_id", "intervention_id", "other_name"))) %>%
  distinct()

# 3. Dictionaries ----

drug_synonyms <- c(
  "ARAC" = "CYTARABINE",
  "ARA-C" = "CYTARABINE",
  "5-FLUOROURACIL" = "FLUOROURACIL",
  "5-FU" = "FLUOROURACIL",
  "5FU" = "FLUOROURACIL",
  "ADRIAMYCIN" = "DOXORUBICIN",
  "AVASTIN" = "BEVACIZUMAB",
  "IPI" = "IPILIMUMAB",
  "ISATUXIMAB SAR650984" = "ISATUXIMAB",
  "LEUCOVORIN CALCIUM" = "LEUCOVORIN",
  "MEL-" = "MELPHALAN",
  "PEG-L-ASPARAGINASE" = "PEGASPARGASE",
  "PEGYLATED L-ASPARAGINASE" = "PEGASPARGASE",
  "XELODA" = "CAPECITABINE",
  "33A" = "VADASTUXIMAB TALIRINE",
  "UTF" = "TEGAFUR-URACIL"
)

regimen_map <- c(
  "CHOP" = "CYCLOPHOSPHAMIDE,DOXORUBICIN,VINCRISTINE,PREDNISONE",
  "FOLFIRI" = "LEUCOVORIN,FLUOROURACIL,IRINOTECAN",
  "MODIFIED FOLFOX6" = "LEUCOVORIN,FLUOROURACIL,OXALIPLATIN",
  "MFOLFOX6" = "LEUCOVORIN,FLUOROURACIL,OXALIPLATIN",
  "7+3" = "CYTARABINE,DAUNORUBICIN",
  "CFZ-TD-PACE" = "CARFILZOMIB,THALIDOMIDE,DEXAMETHASONE,CISPLATIN,DOXORUBICIN,CYCLOPHOSPHAMIDE,ETOPOSIDE",
  "CFZ-R(T)-D" = "CARFILZOMIB,LENALIDOMIDE,THALIDOMIDE,DEXAMETHASONE",
  "GEMOX" = "GEMCITABINE,OXALIPLATIN",
  "S1" = "TEGAFUR,GIMERACIL,OTERACIL",
  "S-1" = "TEGAFUR,GIMERACIL,OTERACIL",
  "GC" = "GEMCITABINE,CISPLATIN",
  "INQOVI" = "DECITABINE,CEDAZURIDINE",
  "CMB305" = "LV305,G305",
  "CPX-351" = "CYTARABINE,DAUNORUBICIN"
)

descriptor_terms <- c(
  "SINGLE DOSE", "LOW DOSE", "FIXED DOSE", "USUAL CARE",
  "ONCE DAILY", "TWICE DAILY", "DAILY", "BY MOUTH",
  "MONOTHERAPY", "INFUSION", "INFUSIONS", "INJECTION", "INJECTIONS",
  "COMBINE",
  "TABLET", "TABLETS", "CAPSULE", "CAPSULES", "GEL", "SPRAY", "DROPS",
  "SUSPENSION", "SUPPOSITORY", "SUPPOSITORIES",
  "THERAPY", "TREATMENT", "REGIMEN", "COMPARATOR",
  "TOPICAL", "TRANSDERMAL", "TRANSMUCOSAL",
  "SUBCUTANEOUS", "SUBLINGUAL", "SUBMUCOSAL",
  "ORAL", "IV", "PO", "IM", "SC", "INTRAVENOUS", "INTRAMUSCULAR", "RECOMBINANT"
)

salt_terms <- c(
  "HYDROCHLORIDE", "HCL", "ACETATE", "PHOSPHATE", "SULFATE",
  "MESYLATE", "BESYLATE", "TARTRATE", "CITRATE", "MALEATE", "FUMARATE",
  "LIPOSOMAL", "NANOLIPOSOMAL"
)

# 4. Normalization helpers ----

regex_escape <- function(x) {
  str_replace_all(x, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1")
}

descriptor_regex <- regex(
  paste0(
    "\\b(?:",
    paste(regex_escape(sort(descriptor_terms, decreasing = TRUE)), collapse = "|"),
    ")\\b"
  ),
  ignore_case = TRUE
)

salt_regex <- regex(
  paste0(
    "\\b(?:",
    paste(regex_escape(sort(salt_terms, decreasing = TRUE)), collapse = "|"),
    ")\\b"
  ),
  ignore_case = TRUE
)

dose_regex <- regex(
  paste0(
    "\\b\\d+(?:\\.\\d+)?\\s*(?:MG|G|KG|MCG|UG|\\u00B5G|\\u03BCG|ML|L|MMOL|MOL|IU|UNITS?)",
    "(?:\\s*/\\s*(?:KG|M2|M\\^2|DAY|D|WEEK|WK|H|HR|ML))?\\b",
    "|\\b\\d+(?:\\.\\d+)?\\s*%\\b",
    "|\\bM\\^?2\\b"
  ),
  ignore_case = TRUE
)

phase_regex <- regex(
  "^PHASE\\s*[0-9IVX]+[A-Z]?(?:\\s*/\\s*[0-9IVX]+[A-Z]?)?\\s*:\\s*",
  ignore_case = TRUE
)

type_prefix_regex <- regex(
  "^(DRUG|BIOLOGICAL|OTHER|DEVICE|PROCEDURE|RADIATION|COMPARATOR)\\s*:\\s*",
  ignore_case = TRUE
)

combo_word_regex <- regex(
  "\\b(IN COMBINATION WITH|COMBINED WITH|COMBINATION WITH|FOLLOWED BY|PLUS|WITH|AND)\\b",
  ignore_case = TRUE
)

split_pattern <- "\\s*,\\s*|\\s*\\+\\s*|\\s*/\\s*|\\s*;\\s*|\\s*&\\s*"

non_alias_regex <- regex(
  paste0(
    dose_regex,
    "|\\b(?:ORAL|IV|PO|IM|SC|SUBCUTANEOUS|INTRAVENOUS|TABLET|TABLETS|CAPSULE|CAPSULES|SPRAY|DROPS|SUSPENSION|INFUSION|INFUSIONS)\\b"
  ),
  ignore_case = TRUE
)

normalize_unicode <- function(x) {
  x %>%
    str_replace_all("\u03B1", " ALFA ") %>%
    str_replace_all("\u0391", " ALFA ") %>%
    str_replace_all("[\u00AE\u2122\"'•]", " ")
}

extract_candidate_aliases <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return(character(0))
  }

  x <- x %>%
    normalize_unicode() %>%
    str_to_upper()

  main <- x %>%
    str_remove_all("\\([^)]*\\)|\\[[^]]*\\]") %>%
    str_squish()

  aliases <- str_extract_all(
    x,
    "(?<=\\()[^)]*(?=\\))|(?<=\\[)[^]]*(?=\\])"
  )[[1]]

  aliases <- aliases[!is.na(aliases)]
  aliases <- str_squish(aliases)
  aliases <- aliases[aliases != ""]
  aliases <- aliases[!str_detect(aliases, non_alias_regex)]

  unique(c(main, aliases))
}

clean_candidate <- function(x) {
  x %>%
    normalize_unicode() %>%
    str_to_upper() %>%
    str_remove(phase_regex) %>%
    str_remove(type_prefix_regex) %>%
    str_replace_all("\\bCOMPARATOR\\s*:\\s*", " ") %>%
    str_replace_all(combo_word_regex, ",") %>%
    str_replace_all(dose_regex, " ") %>%
    str_replace_all(descriptor_regex, " ") %>%
    str_replace_all("\\s*[,=]+\\s*", ",") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
}

expand_regimen <- function(x) {
  ifelse(x %in% names(regimen_map), regimen_map[x], x)
}

make_core_name <- function(x) {
  x %>%
    str_remove_all(salt_regex) %>%
    str_squish()
}


normalize_interventions <- function(df, input_col, query_col = NULL) {
  has_query_col <- !is.null(query_col) && query_col %in% names(df)
  
  df %>%
    mutate(
      intervention_raw = .data[[input_col]],
      query_name_existing = if (has_query_col) as.character(.data[[query_col]]) else NA_character_
    ) %>%
    mutate(
      candidate_aliases = map(intervention_raw, extract_candidate_aliases),
      existing_aliases = map(
        query_name_existing,
        ~ if (is.na(.x) || !nzchar(.x)) character(0) else as.character(.x)
      ), 
      candidate_aliases = map2(candidate_aliases, existing_aliases, ~ unique(c(.x, .y)))
    ) %>%
    select(-existing_aliases) %>%
    unnest_longer(candidate_aliases, values_to = "candidate_raw", keep_empty = FALSE) %>%
    mutate(
      query_name_clean = clean_candidate(candidate_raw),
      query_name_clean = expand_regimen(query_name_clean),
      query_name_clean = str_split(query_name_clean, split_pattern)
    ) %>%
    unnest(query_name_clean) %>%
    mutate(
      query_name_clean = str_squish(query_name_clean),
      query_name_clean = recode(query_name_clean, !!!drug_synonyms, .default = query_name_clean),
      query_name_core = make_core_name(query_name_clean),
      query_name_clean = na_if(query_name_clean, ""),
      query_name_core = na_if(query_name_core, "")
    ) %>%
    filter(
      !is.na(query_name_clean),
      !query_name_clean %in% c("PLACEBO", "PLACEBOS")
    ) %>%
    distinct()
}


# 5. Normalize source tables ----

DrugBiol <- normalize_interventions(
  df = DrugBiol_source,
  input_col = "name"
)

InterventionOtherNames <- normalize_interventions(
  df = InterventionOtherNames_source,
  input_col = "other_name"
)

# 6. Normalize DrugBank vocabulary ----

drugbank_common <- drugbank_vocabulary %>%
  mutate(
    alias_raw = `Common name`,
    alias_type = "COMMON"
  )

drugbank_synonyms <- drugbank_vocabulary %>%
  mutate(
    alias_raw = Synonyms,
    alias_type = "SYNONYM"
  ) %>%
  separate_longer_delim(alias_raw, delim = "|") %>%
  mutate(alias_raw = str_squish(alias_raw)) %>%
  filter(alias_raw != "", !is.na(alias_raw))

drugbank_aliases <- bind_rows(drugbank_common, drugbank_synonyms) %>%
  mutate(
    alias_clean = clean_candidate(alias_raw),
    alias_core = make_core_name(alias_clean),
    priority = case_when(
      alias_type == "COMMON" ~ 1L,
      alias_type == "SYNONYM" ~ 2L,
      TRUE ~ 99L
    )
  ) %>%
  filter(!is.na(alias_clean), alias_clean != "") %>%
  distinct()

ambiguous_clean <- drugbank_aliases %>%
  distinct(alias_clean, `DrugBank ID`) %>%
  count(alias_clean, name = "n_ids") %>%
  filter(n_ids > 1)

ambiguous_core <- drugbank_aliases %>%
  filter(!is.na(alias_core), alias_core != "") %>%
  distinct(alias_core, `DrugBank ID`) %>%
  count(alias_core, name = "n_ids") %>%
  filter(n_ids > 1)

drugbank_aliases_clean_safe <- drugbank_aliases %>%
  anti_join(ambiguous_clean, by = "alias_clean")

drugbank_aliases_core_safe <- drugbank_aliases %>%
  anti_join(ambiguous_core, by = "alias_core")

# 7. Matching helpers ----

match_drugbank <- function(df, key_col, alias_tbl, alias_col, query_label) {
  df %>%
    left_join(
      alias_tbl,
      by = setNames(alias_col, key_col),
      relationship = "many-to-many"
    ) %>%
    mutate(Query = if_else(!is.na(`DrugBank ID`), query_label, NA_character_))
}

collapse_best_matches <- function(df, id_col, source_label) {
  id_sym <- rlang::sym(id_col)

  df %>%
    mutate(
      match_rank = case_when(
        Query == "CLEAN_ALIAS" ~ 1L,
        Query == "CORE_TO_CLEAN_ALIAS" ~ 2L,
        Query == "CORE_ALIAS" ~ 3L,
        TRUE ~ 99L
      ),
      match_source = source_label
    ) %>%
    arrange(!!id_sym, query_name_clean, match_rank, priority, alias_type) %>%
    group_by(!!id_sym, query_name_clean) %>%
    slice_head(n = 1) %>%
    ungroup()
}

run_match_passes <- function(df, id_col, source_label) {
  original_cols <- names(df)

  pass_1 <- match_drugbank(
    df,
    key_col = "query_name_clean",
    alias_tbl = drugbank_aliases_clean_safe,
    alias_col = "alias_clean",
    query_label = "CLEAN_ALIAS"
  )

  resolved <- pass_1 %>% filter(!is.na(`DrugBank ID`))
  unresolved <- pass_1 %>% filter(is.na(`DrugBank ID`)) %>% select(all_of(original_cols))

  pass_2 <- match_drugbank(
    unresolved,
    key_col = "query_name_core",
    alias_tbl = drugbank_aliases_clean_safe,
    alias_col = "alias_clean",
    query_label = "CORE_TO_CLEAN_ALIAS"
  )

  resolved <- bind_rows(resolved, pass_2 %>% filter(!is.na(`DrugBank ID`)))
  unresolved <- pass_2 %>% filter(is.na(`DrugBank ID`)) %>% select(all_of(original_cols))

  pass_3 <- match_drugbank(
    unresolved,
    key_col = "query_name_core",
    alias_tbl = drugbank_aliases_core_safe,
    alias_col = "alias_core",
    query_label = "CORE_ALIAS"
  )

  resolved <- bind_rows(resolved, pass_3 %>% filter(!is.na(`DrugBank ID`)))
  unresolved <- pass_3 %>% filter(is.na(`DrugBank ID`)) %>% select(all_of(original_cols))

  list(
    mapped = collapse_best_matches(resolved, id_col = id_col, source_label = source_label),
    unresolved = unresolved
  )
}

# 8. Direct DrugBiol matches ----

direct_matches <- run_match_passes(
  df = DrugBiol,
  id_col = "id",
  source_label = "Interventions"
)

# 9. Fallback via InterventionOtherNames ----

other_name_candidates <- InterventionOtherNames %>%
  filter(intervention_id %in% direct_matches$unresolved$id)

other_name_matches <- run_match_passes(
  df = other_name_candidates,
  id_col = "intervention_id",
  source_label = "InterventionOtherNames"
)

other_name_matches$mapped <- other_name_matches$mapped %>%
  left_join(
    DrugBiol_source %>%
      select(id, nct_id, intervention_type, name, description) %>%
      distinct(),
    by = c("intervention_id" = "id"),
    relationship = "many-to-one"
  ) %>%
  distinct() %>%
  mutate(id = intervention_id, .before = other_name_id)

# 10. Final outputs ----

mapped_names <- bind_rows(
  direct_matches$mapped,
  other_name_matches$mapped
) %>%
  mutate(
    matched_candidate = candidate_raw,
    drugbank_id = `DrugBank ID`,
    drugbank_common_name = `Common name`,
    drugbank_alias = alias_raw,
    drugbank_alias_type = alias_type,
    accession_numbers = `Accession Numbers`,
    standard_inchi_key = `Standard InChI Key`
  ) %>%
  transmute(
    id,
    nct_id,
    intervention_type,
    intervention_name = name,
    description,
    match_source,
    match_stage = Query,
    match_rank,
    matched_candidate,
    normalized_name = query_name_clean,
    normalized_name_core = query_name_core,
    other_name_id,
    other_name,
    drugbank_id,
    drugbank_common_name,
    drugbank_alias,
    drugbank_alias_type,
    accession_numbers,
    CAS,
    UNII,
    synonyms = Synonyms,
    standard_inchi_key
) %>%
  distinct() %>%
  arrange(id, normalized_name, match_source, match_rank, drugbank_id)

resolved_ids <- mapped_names %>%
  distinct(id) %>%
  pull(id)

unresolved_interventions <- DrugBiol_source %>%
  filter(!id %in% resolved_ids) %>%
  arrange(id)

unresolved_names <- DrugBiol %>%
  filter(id %in% unresolved_interventions$id) %>%
  transmute(
    id,
    nct_id,
    intervention_type,
    intervention_name = name,
    description,
    matched_candidate = candidate_raw,
    normalized_name = query_name_clean,
    normalized_name_core = query_name_core
  ) %>%
  distinct() %>%
  arrange(id, normalized_name)

## 10.1 Manual review - unresolved names ----

unresolved_search <- unresolved_names %>%
  transmute(
    id,
    nct_id,
    intervention_type,
    intervention_name,
    description,
    matched_candidate,
    query_name_clean = normalized_name,
    query_name_core = normalized_name_core
  ) %>%
  filter(!is.na(query_name_clean), query_name_clean != "") %>%
  distinct()

write_xlsx(unresolved_search, "results/ClinicalTrials/drugbiol_drugbank_mapping_unresolved_names.xlsx")

unresolved_reviewed <- read_xlsx("results/ClinicalTrials/drugbiol_drugbank_mapping_unresolved_names_reviewed.xlsx")

unresolved_reviewed <- unresolved_reviewed %>% left_join(drugbank_vocabulary, by = c("drugbank_id" = "DrugBank ID"))
unresolved_reviewed <- unresolved_reviewed %>% rename(
  drugbank_common_name = `Common name`,
  accession_numbers = `Accession Numbers`,
  synonyms = Synonyms,
  standard_inchi_key = `Standard InChI Key`
) %>% mutate(
  match_source = "Manual",
  match_rank = 1L
)


mapped_names <- bind_rows(
  mapped_names,
  unresolved_reviewed %>% filter(!is.na(drugbank_id))
) %>%
  distinct() %>%
  arrange(id, normalized_name, match_source, match_rank, drugbank_id)


resolved_ids <- mapped_names %>%
  distinct(id) %>%
  pull(id)

unresolved_interventions <- DrugBiol_source %>%
  filter(!id %in% resolved_ids) %>%
  arrange(id)

unresolved_names <- DrugBiol %>%
  filter(id %in% unresolved_interventions$id) %>%
  transmute(
    id,
    nct_id,
    intervention_type,
    intervention_name = name,
    description,
    matched_candidate = candidate_raw,
    normalized_name = query_name_clean,
    normalized_name_core = query_name_core
  ) %>%
  distinct() %>%
  arrange(id, normalized_name)



intervention_map <- mapped_names %>%
  distinct(
    id,
    nct_id,
    intervention_type,
    intervention_name,
    description,
    drugbank_id,
    drugbank_common_name,
    synonyms,
    match_source,
    normalized_name,
    matched_candidate
  ) %>%
  arrange(id, drugbank_id, match_source, normalized_name) %>%
  group_by(id, nct_id, intervention_type, intervention_name, description) %>%
  summarise(
    n_drugbank_ids = n_distinct(drugbank_id),
    drugbank_id = str_c(sort(unique(drugbank_id)), collapse = " | "),
    drugbank_common_names = str_c(sort(unique(drugbank_common_name)), collapse = " | "),
    match_sources = str_c(sort(unique(match_source)), collapse = " | "),
    normalized_names = str_c(sort(unique(normalized_name)), collapse = " | "),
    matched_candidates = str_c(sort(unique(matched_candidate)), collapse = " | "),
    .groups = "drop"
  ) %>%
  arrange(id)

drugs <- mapped_names %>% select(drugbank_id, drugbank_common_name, accession_numbers:standard_inchi_key) %>%
  distinct() %>% arrange(drugbank_id)


summary_tbl <- tibble(
  metric = c(
    "drugbiol_input_rows",
    "drugbiol_interventions",
    "drugbiol_normalized_rows",
    "other_names_input_rows",
    "other_names_interventions",
    "other_names_normalized_rows",
    "drugbank_alias_rows",
    "ambiguous_clean_aliases",
    "ambiguous_core_aliases",
    "mapped_name_rows",
    "mapped_interventions_total",
    "unresolved_interventions"
  ),
  value = c(
    nrow(DrugBiol_raw),
    n_distinct(DrugBiol_raw$id),
    nrow(DrugBiol),
    nrow(InterventionOtherNames_raw),
    n_distinct(InterventionOtherNames_raw$intervention_id),
    nrow(InterventionOtherNames),
    nrow(drugbank_aliases),
    nrow(ambiguous_clean),
    nrow(ambiguous_core),
    nrow(mapped_names),
    length(unique(resolved_ids)),
    nrow(unresolved_interventions)
  )
)


# 11. Save results ----


write_xlsx(drugs, "results/ClinicalTrials/drugs.xlsx")

Interventions <- Interventions %>% left_join(intervention_map %>% select(id, drugbank_id), by = "id")
Interventions <- Interventions %>% separate_longer_delim(drugbank_id, delim = " | ")


intervention_drugs <- Interventions %>%
  select(id, drugbank_id) %>%
  filter(!is.na(drugbank_id)) %>% # remove non-drug / unmapped interventions
  distinct() %>% # security check:one row per intervention-drug relation
  inner_join(
    drugs %>% select(drugbank_id),
    # security check: keep only DrugBank IDs that exist in drugs
    by = "drugbank_id"
  ) %>%
  rename(intervention_id = id) %>%
  mutate(id = paste0("ID0", 1:length(intervention_id)), .before = intervention_id)

write_xlsx(intervention_drugs, "results/ClinicalTrials/intervention_drugs.xlsx")


write_xlsx(
  x = list(
    drugs = drugs,
    intervention_map = intervention_map,
    intervention_drugs = intervention_drugs,
    unresolved_interventions = unresolved_interventions,
    unresolved_names = unresolved_names,
    summary = summary_tbl
  ),
  file = output_path
)

message(sprintf("DrugBank mapping workbook written to: %s", output_path))
