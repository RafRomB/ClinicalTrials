# 0. Load libraries ----

library(tidyverse)
library(stringdist)
library(openxlsx2)

# Before running this, you need the results from 'R/AACT_DrugMapping_llm.R'

# 1. Define paths ----
drugbank_path <- "data/drugbank_all_full_database/drugbank vocabulary.csv"
output_path <- "results/DrugBank/groups_drugbank_mapping.xlsx"
manual_review_path <- "results/DrugBank/drugbiol_drugbank_mapping_unresolved_query_names_reviewed.xlsx"
fuzzy_review_path <- "results/DrugBank/drugbiol_drugbank_mapping_unresolved_review_suggestions.xlsx"
fuzzy_top_n <- 2L

# 2. Load local data ----

## Load LLM results

DesignGroupInterventionDrugs_llm <- read_xlsx("results/AACT/DesignGroupInterventionDrugs_llm.xlsx")

required_llm_cols <- c(
  "group_id", "nct_id", "group_type", "group_title", "group_description",
  "intervention_id", "intervention_name", "intervention_description",
  "query_text", "qwen3_8b", "deepseek_r1_8b", "phi4"
)
llm_model_cols <- c("qwen3_8b", "deepseek_r1_8b", "phi4")

missing_llm_cols <- setdiff(required_llm_cols, names(DesignGroupInterventionDrugs_llm))

if (length(missing_llm_cols) > 0) {
  stop(
    sprintf(
      "DesignGroupInterventionDrugs_llm.xlsx is missing required column(s): %s",
      paste(missing_llm_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

### Clean LLM outputs

clean_llm_drugs <- function(x, empty_value = NA_character_) {
  
  out <- x %>%
    # Normalize line endings / whitespace a bit
    str_replace_all("\\r", "\n") %>%
    
    # Remove hidden thinking blocks like <think> ... </think>
    str_replace_all(regex("<think>.*?</think>", dotall = TRUE, ignore_case = TRUE), " ") %>%
    
    # Remove cases like "user<think>..." if present
    str_replace_all(regex("user\\s*<think>.*?</think>", dotall = TRUE, ignore_case = TRUE), " ") %>%
    
    # Remove markdown reasoning sections that come after the answer
    str_replace(regex("\\n+\\*\\*reasoning:\\*\\*.*$", dotall = TRUE, ignore_case = TRUE), "") %>%
    str_replace(regex("\\n+reasoning:.*$", dotall = TRUE, ignore_case = TRUE), "") %>%
    
    str_replace(regex("\\n+\\*\\*explanation:\\*\\*.*$", dotall = TRUE, ignore_case = TRUE), "") %>%
    str_replace(regex("\\n+explanation:.*$", dotall = TRUE, ignore_case = TRUE), "") %>%
    
    # Remove common "no drugs" responses
    str_replace(regex("^\\s*there are no drug-like interventions administered in this group\\.?\\s*$",
                      ignore_case = TRUE), "") %>%
    str_replace(regex("^\\s*no drug-like interventions\\.?\\s*$",
                      ignore_case = TRUE), "") %>%
    str_replace(regex("^\\s*\\{?\\s*\"?drugs\"?\\s*:\\s*\\[\\s*\\]\\s*\\}?\\s*$",
                      ignore_case = TRUE), "") %>%
    
    # Keep only the part after </think> if some model puts the final answer there
    (\(z) ifelse(str_detect(z, regex("</think>", ignore_case = TRUE)),
                 str_replace(z, regex("^.*?</think>", dotall = TRUE, ignore_case = TRUE), ""),
                 z))() %>%

    # If prose plus a final-answer block leaked through, keep the last final answer.
    (\(z) ifelse(str_detect(z, regex("\\bfinal\\s+answer(?:\\s*\\([^)]*\\))?\\s*:",
                                      ignore_case = TRUE)),
                 str_replace(z, regex("^.*\\bfinal\\s+answer(?:\\s*\\([^)]*\\))?\\s*:\\s*",
                                      dotall = TRUE, ignore_case = TRUE), ""),
                 z))() %>%

    # Remove leading prose labels like "the correct answer is:"
    str_replace(regex(
      "^\\s*(?:the\\s+)?(?:most\\s+accurate\\s+and\\s+general\\s+|correct\\s+|general\\s+)?answer(?:\\s+based\\s+on\\s+[^:]+)?\\s*(?:is|would\\s+be|should\\s+be)?\\s*:\\s*",
      ignore_case = TRUE
    ), "") %>%
    
    # Remove bullets / numbering if they leak into output
    str_replace_all(regex("^\\s*[-*•]\\s*", multiline = TRUE), "") %>%
    str_replace_all(regex("^\\s*\\d+\\.\\s*", multiline = TRUE), "") %>%

    # Remove leaked labels like "**Answer:**" before they become drug candidates
    str_replace_all(regex(
      "^\\s*\\*{0,2}\\s*(?:answer|output|drugs?)\\s*:?\\s*\\*{0,2}\\s*",
      multiline = TRUE,
      ignore_case = TRUE
    ), "") %>%
    
    # Trim
    str_replace_all("[*_`]", " ") %>%
    str_squish()
  
  # Split, trim, remove empties, deduplicate, normalize placebo only
  out <- map_chr(out, function(txt) {
    
    if (is.na(txt) || txt == "") {
      return(empty_value)
    }
    
    drugs <- str_split(txt, "\\s*\\|\\s*|\\s*,\\s*")[[1]]
    drugs <- str_trim(drugs)
    drugs <- str_remove(drugs, regex("^\\s*(?:and|or)\\s+", ignore_case = TRUE))
    drugs <- str_remove_all(drugs, "[*_`]")
    drugs <- str_remove(drugs, "\\s*[.;:]+\\s*$")
    drugs <- str_squish(drugs)
    drugs <- drugs[drugs != ""]
    
    # Drop leftover junk fragments
    drugs <- drugs[!str_detect(drugs, regex(
      "^(reasoning|output|answer|json|none|n/a|na|null|drugs\\s*:)$",
      ignore_case = TRUE
    ))]

    explanatory_fragment <- str_detect(drugs, regex(
      paste(
        "answer|group description|provided information|intervention name",
        "explicitly mentioned|per rule|based on|given the ambiguity",
        "safest extraction|the treatment described|the drugs administered",
        sep = "|"
      ),
      ignore_case = TRUE
    ))
    too_long_for_single_name <- str_count(drugs, "\\S+") > 8
    drugs <- drugs[!(explanatory_fragment | too_long_for_single_name)]
    
    # Normalize placebo only; avoid title-casing everything because it breaks names like mRNA-2416
    drugs <- ifelse(str_to_lower(drugs) %in% c("placebo", "placebos", "matching placebo", "matching placebos"),
                    "Placebo",
                    drugs)
    
    # Deduplicate preserving order
    drugs <- drugs[!duplicated(str_to_lower(drugs))]
    
    if (length(drugs) == 0) empty_value else paste(drugs, collapse = " | ")
  })
  
  out
}

DesignGroupInterventionDrugs_llm <- DesignGroupInterventionDrugs_llm %>% 
  mutate(
    across(
      all_of(llm_model_cols),
      clean_llm_drugs
    )
  )

## Keep one row per LLM-extracted candidate so model vote evidence is retained.

DesignGroupInterventionDrugs_llm_long <- DesignGroupInterventionDrugs_llm %>%
  mutate(across(all_of(llm_model_cols), identity, .names = "{.col}_candidates")) %>%
  pivot_longer(
    cols = all_of(paste0(llm_model_cols, "_candidates")),
    names_to = "source_model",
    values_to = "drugs",
    values_drop_na = TRUE
  ) %>%
  mutate(source_model = str_remove(source_model, "_candidates$")) %>%
  filter(drugs != "") %>%
  separate_longer_delim(drugs, delim = " | ") %>%
  mutate(drugs = str_squish(drugs)) %>%
  filter(drugs != "") %>%
  distinct()


drugbank_vocabulary <- read_csv(drugbank_path, show_col_types = FALSE)

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

dose_pattern <- paste0(
  "\\b\\d+(?:\\.\\d+)?\\s*(?:MG|G|KG|MCG|UG|\\u00B5G|\\u03BCG|ML|L|MMOL|MOL|IU|UNITS?)",
  "(?:\\s*/\\s*(?:KG|M2|M\\^2|DAY|D|WEEK|WK|H|HR|ML))?\\b",
  "|\\b\\d+(?:\\.\\d+)?\\s*%\\b",
  "|\\bM\\^?2\\b"
)

dose_regex <- regex(
  dose_pattern,
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
    dose_pattern,
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

collapse_candidate_votes <- function(df) {
  candidate_key_cols <- c(required_llm_cols, "query_name_clean", "query_name_core")

  df %>%
    group_by(across(all_of(candidate_key_cols))) %>%
    summarise(
      candidate_raw = str_c(sort(unique(na.omit(candidate_raw))), collapse = " | "),
      intervention_raw = str_c(sort(unique(na.omit(intervention_raw))), collapse = " | "),
      n_model_votes = n_distinct(source_model, na.rm = TRUE),
      source_models = str_c(sort(unique(na.omit(source_model))), collapse = " | "),
      llm_vote_confidence = case_when(
        n_model_votes >= 3 ~ "high_3_models",
        n_model_votes == 2 ~ "medium_2_models",
        n_model_votes == 1 ~ "low_1_model",
        TRUE ~ "no_model_vote"
      ),
      .groups = "drop"
    ) %>%
    arrange(group_id, query_name_clean)
}


# 5. Normalize source tables ----

DesignGroupInterventionDrugs <- normalize_interventions(
  df = DesignGroupInterventionDrugs_llm_long,
  input_col = "drugs"
) %>%
  collapse_candidate_votes()


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

make_fuzzy_suggestions <- function(unresolved_df, alias_tbl, top_n = 5L) {
  empty_result <- tibble(
    query_name_clean = character(),
    query_name_core = character(),
    fuzzy_rank = integer(),
    suggested_drugbank_id = character(),
    suggested_common_name = character(),
    suggested_alias = character(),
    suggested_alias_type = character(),
    suggested_alias_clean = character(),
    suggested_match_field = character(),
    suggested_contains_alias = logical(),
    suggested_token_overlap = double(),
    fuzzy_distance = double(),
    fuzzy_similarity = double(),
    fuzzy_review_strength = character()
  )

  unresolved_candidates <- unresolved_df %>%
    filter(!is.na(query_name_clean), query_name_clean != "") %>%
    distinct(query_name_clean, query_name_core)

  alias_candidates <- alias_tbl %>%
    filter(!is.na(alias_clean), alias_clean != "", str_length(alias_clean) >= 3) %>%
    distinct(
      `DrugBank ID`,
      `Common name`,
      alias_raw,
      alias_type,
      alias_clean,
      alias_core,
      priority
    ) %>%
    mutate(
      alias_length = str_length(alias_clean),
      alias_first_char = str_sub(alias_clean, 1, 1),
      alias_tokens = str_split(alias_clean, "[^A-Z0-9]+"),
      alias_first_token = map_chr(alias_tokens, ~ {
        tokens <- .x[.x != ""]
        if (length(tokens) == 0) "" else tokens[1]
      }),
      alias_last_token = map_chr(alias_tokens, ~ {
        tokens <- .x[.x != ""]
        if (length(tokens) == 0) "" else tail(tokens, 1)
      })
    )

  if (nrow(unresolved_candidates) == 0 || nrow(alias_candidates) == 0) {
    return(empty_result)
  }

  map_dfr(seq_len(nrow(unresolved_candidates)), function(i) {
    query_clean <- unresolved_candidates$query_name_clean[[i]]
    query_core <- unresolved_candidates$query_name_core[[i]]

    if (is.na(query_clean) || query_clean == "" || str_length(query_clean) < 3) {
      return(empty_result)
    }

    query_length <- str_length(query_clean)
    query_first_char <- str_sub(query_clean, 1, 1)
    query_tokens <- str_split(query_clean, "[^A-Z0-9]+")[[1]]
    query_tokens <- query_tokens[query_tokens != ""]
    length_window <- max(8L, ceiling(query_length * 0.6))

    candidate_pool <- alias_candidates %>%
      filter(
        (abs(alias_length - query_length) <= length_window & alias_first_char == query_first_char) |
          alias_first_token %in% query_tokens |
          alias_last_token %in% query_tokens
      )

    if (nrow(candidate_pool) < top_n) {
      candidate_pool <- alias_candidates %>%
        filter(abs(alias_length - query_length) <= length_window)
    }

    if (nrow(candidate_pool) == 0) {
      return(empty_result)
    }

    alias_core <- if_else(
      is.na(candidate_pool$alias_core) | candidate_pool$alias_core == "",
      candidate_pool$alias_clean,
      candidate_pool$alias_core
    )

    clean_distance <- stringdist(
      query_clean,
      candidate_pool$alias_clean,
      method = "jw",
      p = 0.1
    )

    core_distance <- rep(Inf, nrow(candidate_pool))

    if (!is.na(query_core) && query_core != "" && str_length(query_core) >= 3) {
      core_distance <- stringdist(
        query_core,
        alias_core,
        method = "jw",
        p = 0.1
      )
    }

    fuzzy_distance <- pmin(clean_distance, core_distance)
    contains_alias <- map_lgl(
      candidate_pool$alias_clean,
      ~ str_detect(query_clean, fixed(.x))
    ) & candidate_pool$alias_length >= 4

    if (!is.na(query_core) && query_core != "") {
      contains_alias <- contains_alias |
        (
          map_lgl(alias_core, ~ str_detect(query_core, fixed(.x))) &
            str_length(alias_core) >= 4
        )
    }

    token_overlap_count <- map_int(
      candidate_pool$alias_tokens,
      ~ sum(.x[.x != ""] %in% query_tokens)
    )
    token_count <- map_int(candidate_pool$alias_tokens, ~ length(.x[.x != ""]))
    token_overlap_ratio <- token_overlap_count / pmax(token_count, 1L)

    candidate_pool %>%
      mutate(
        query_name_clean = query_clean,
        query_name_core = query_core,
        contains_alias = contains_alias,
        token_overlap_ratio = token_overlap_ratio,
        fuzzy_distance = as.numeric(fuzzy_distance),
        fuzzy_similarity = round(1 - fuzzy_distance, 4),
        suggested_match_field = if_else(clean_distance <= core_distance, "clean", "core")
      ) %>%
      filter(is.finite(fuzzy_distance)) %>%
      arrange(desc(contains_alias), desc(token_overlap_ratio), fuzzy_distance, priority, alias_type, alias_clean, `DrugBank ID`) %>%
      group_by(`DrugBank ID`) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      slice_head(n = top_n) %>%
      mutate(
        fuzzy_rank = row_number(),
        fuzzy_review_strength = case_when(
          contains_alias ~ "contained_alias",
          fuzzy_distance <= 0.08 ~ "strong",
          fuzzy_distance <= 0.15 ~ "possible",
          TRUE ~ "weak"
        )
      ) %>%
      transmute(
        query_name_clean,
        query_name_core,
        fuzzy_rank,
        suggested_drugbank_id = `DrugBank ID`,
        suggested_common_name = `Common name`,
        suggested_alias = alias_raw,
        suggested_alias_type = alias_type,
        suggested_alias_clean = alias_clean,
        suggested_match_field,
        suggested_contains_alias = contains_alias,
        suggested_token_overlap = round(token_overlap_ratio, 4),
        fuzzy_distance,
        fuzzy_similarity,
        fuzzy_review_strength
      )
  })
}

make_contained_alias_suggestions <- function(unresolved_df, alias_tbl, top_n = 5L) {
  unresolved_candidates <- unresolved_df %>%
    filter(!is.na(query_name_clean), query_name_clean != "") %>%
    distinct(query_name_clean, query_name_core)

  alias_candidates <- alias_tbl %>%
    filter(!is.na(alias_clean), alias_clean != "", str_length(alias_clean) >= 4) %>%
    distinct(
      `DrugBank ID`,
      `Common name`,
      alias_raw,
      alias_type,
      alias_clean,
      alias_core,
      priority
    ) %>%
    mutate(
      alias_core = if_else(is.na(alias_core) | alias_core == "", alias_clean, alias_core),
      alias_length = str_length(alias_clean),
      alias_first_char = str_sub(alias_clean, 1, 1),
      alias_tokens = str_split(alias_clean, "[^A-Z0-9]+"),
      alias_first_token = map_chr(alias_tokens, ~ {
        tokens <- .x[.x != ""]
        if (length(tokens) == 0) "" else tokens[1]
      }),
      alias_last_token = map_chr(alias_tokens, ~ {
        tokens <- .x[.x != ""]
        if (length(tokens) == 0) "" else tail(tokens, 1)
      }),
      alias_token_count = str_count(alias_clean, "[A-Z0-9]+")
    )

  if (nrow(unresolved_candidates) == 0 || nrow(alias_candidates) == 0) {
    return(make_fuzzy_suggestions(unresolved_df[0, ], alias_tbl, top_n = top_n))
  }

  map_dfr(seq_len(nrow(unresolved_candidates)), function(i) {
    query_clean <- unresolved_candidates$query_name_clean[[i]]
    query_core <- unresolved_candidates$query_name_core[[i]]
    query_tokens <- str_split(query_clean, "[^A-Z0-9]+")[[1]]
    query_tokens <- query_tokens[query_tokens != ""]
    query_first_char <- str_sub(query_clean, 1, 1)

    candidate_pool <- alias_candidates %>%
      filter(
        alias_length <= str_length(query_clean),
        alias_first_char == query_first_char |
          alias_first_token %in% query_tokens |
          alias_last_token %in% query_tokens
      )

    if (nrow(candidate_pool) == 0) {
      return(tibble())
    }

    contains_clean <- map_lgl(candidate_pool$alias_clean, ~ str_detect(query_clean, fixed(.x)))
    contains_core <- rep(FALSE, nrow(candidate_pool))

    if (!is.na(query_core) && query_core != "") {
      contains_core <- map_lgl(candidate_pool$alias_core, ~ str_detect(query_core, fixed(.x)))
    }

    candidate_pool %>%
      mutate(
        contains_clean = contains_clean,
        contains_core = contains_core,
        query_name_clean = query_clean,
        query_name_core = query_core,
        suggested_contains_alias = TRUE,
        suggested_token_overlap = map_dbl(
          str_split(alias_clean, "[^A-Z0-9]+"),
          ~ sum(.x[.x != ""] %in% query_tokens) / max(length(.x[.x != ""]), 1L)
        ),
        fuzzy_distance = 0,
        fuzzy_similarity = 1,
        fuzzy_review_strength = "contained_alias",
        suggested_match_field = if_else(contains_clean, "clean_contains_alias", "core_contains_alias")
      ) %>%
      filter(contains_clean | contains_core) %>%
      arrange(priority, desc(suggested_token_overlap), alias_token_count, alias_length, alias_type, alias_clean) %>%
      group_by(`DrugBank ID`) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      slice_head(n = top_n) %>%
      mutate(fuzzy_rank = row_number()) %>%
      transmute(
        query_name_clean,
        query_name_core,
        fuzzy_rank,
        suggested_drugbank_id = `DrugBank ID`,
        suggested_common_name = `Common name`,
        suggested_alias = alias_raw,
        suggested_alias_type = alias_type,
        suggested_alias_clean = alias_clean,
        suggested_match_field,
        suggested_contains_alias,
        suggested_token_overlap = round(suggested_token_overlap, 4),
        fuzzy_distance,
        fuzzy_similarity,
        fuzzy_review_strength
      )
  })
}

# 8. Direct DesignGroupInterventionDrugs matches ----

direct_matches <- run_match_passes(
  df = DesignGroupInterventionDrugs,
  id_col = "group_id",
  source_label = "DesignGroupInterventionDrugs"
)


# 9. Final outputs ----

mapped_names <- 
  direct_matches$mapped %>%
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
    group_id,
    nct_id,
    group_title,
    group_type,
    group_description,
    intervention_id,
    intervention_name,
    intervention_description,
    query_text,
    qwen3_8b,
    deepseek_r1_8b,
    phi4,
    n_model_votes,
    source_models,
    llm_vote_confidence,
    match_source,
    match_stage = Query,
    match_rank,
    matched_candidate,
    normalized_name = query_name_clean,
    normalized_name_core = query_name_core,
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
  arrange(group_id, normalized_name, match_source, match_rank, drugbank_id)

unresolved <- direct_matches$unresolved %>% distinct()


## 9.1 Manual review - unresolved names ----

unresolved_search <- unresolved %>%
  select(
    all_of(required_llm_cols),
    candidate_raw,
    query_name_clean,
    query_name_core,
    n_model_votes,
    source_models,
    llm_vote_confidence
  ) %>%
  filter(!is.na(query_name_clean), query_name_clean != "") %>%
  distinct()

fuzzy_suggestions <- bind_rows(
  make_contained_alias_suggestions(
    unresolved_df = unresolved,
    alias_tbl = drugbank_aliases,
    top_n = fuzzy_top_n
  ),
  make_fuzzy_suggestions(
    unresolved_df = unresolved,
    alias_tbl = drugbank_aliases,
    top_n = fuzzy_top_n
  )
) %>%
  arrange(
    query_name_clean,
    query_name_core,
    desc(suggested_contains_alias),
    desc(suggested_token_overlap),
    fuzzy_distance,
    fuzzy_rank
  ) %>%
  group_by(query_name_clean, query_name_core, suggested_drugbank_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  group_by(query_name_clean, query_name_core) %>%
  arrange(
    desc(suggested_contains_alias),
    desc(suggested_token_overlap),
    fuzzy_distance,
    fuzzy_rank,
    .by_group = TRUE
  ) %>%
  slice_head(n = fuzzy_top_n) %>%
  mutate(fuzzy_rank = row_number()) %>%
  ungroup()

fuzzy_suggestions_compact <- fuzzy_suggestions %>%
  mutate(
    suggestion = str_c(
      fuzzy_rank,
      ". ",
      suggested_drugbank_id,
      " / ",
      suggested_common_name,
      " [alias: ",
      suggested_alias,
      "; similarity: ",
      fuzzy_similarity,
      "; ",
      fuzzy_review_strength,
      "]"
    )
  ) %>%
  group_by(query_name_clean, query_name_core) %>%
  summarise(
    top_fuzzy_suggestions = str_c(suggestion, collapse = " | "),
    best_fuzzy_drugbank_id = first(suggested_drugbank_id),
    best_fuzzy_common_name = first(suggested_common_name),
    best_fuzzy_alias = first(suggested_alias),
    best_fuzzy_similarity = first(fuzzy_similarity),
    best_fuzzy_review_strength = first(fuzzy_review_strength),
    .groups = "drop"
  )

unresolved_query_review <- unresolved_search %>%
  group_by(query_name_clean, query_name_core) %>%
  summarise(
    drugbank_id = NA_character_,
    n_groups = n_distinct(group_id),
    max_n_model_votes = max(n_model_votes, na.rm = TRUE),
    source_models = str_c(sort(unique(na.omit(unlist(str_split(source_models, "\\s*\\|\\s*"))))), collapse = " | "),
    llm_vote_confidences = str_c(sort(unique(llm_vote_confidence)), collapse = " | "),
    example_group_id = first(group_id),
    example_nct_id = first(nct_id),
    example_group_title = first(group_title),
    example_intervention_name = first(intervention_name),
    example_candidate_raw = first(candidate_raw),
    .groups = "drop"
  ) %>%
  left_join(fuzzy_suggestions_compact, by = c("query_name_clean", "query_name_core")) %>%
  arrange(max_n_model_votes, query_name_clean)

unresolved_review_suggestions <- unresolved_search %>%
  left_join(fuzzy_suggestions, by = c("query_name_clean", "query_name_core"), relationship = "many-to-many") %>%
  mutate(
    review_drugbank_id = NA_character_,
    review_notes = NA_character_,
    .after = query_name_clean
  ) %>%
  arrange(n_model_votes, group_id, query_name_clean, fuzzy_rank)



write_xlsx(x = list(
  unresolved_names = unresolved_search,
  unresolved_query_review = unresolved_query_review,
  unresolved_review_suggestions = unresolved_review_suggestions),
  file = "results/DrugBank/drugbank_mapping_unresolved.xlsx"
  )



manual_mapped_names <- mapped_names[0, ]

if (file.exists(manual_review_path)) {
  unresolved_reviewed <- read_xlsx(manual_review_path)
  missing_review_cols <- setdiff(c("query_name_clean", "drugbank_id"), names(unresolved_reviewed))

  if (length(missing_review_cols) > 0) {
    stop(
      sprintf(
        "Manual review file is missing required column(s): %s",
        paste(missing_review_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  unresolved_reviewed <- unresolved_reviewed %>%
    transmute(
      query_name_clean = str_squish(as.character(query_name_clean)),
      drugbank_id = na_if(str_squish(as.character(drugbank_id)), "")
    ) %>%
    distinct()

  invalid_manual_ids <- unresolved_reviewed %>%
    filter(!is.na(drugbank_id)) %>%
    anti_join(drugbank_vocabulary, by = c("drugbank_id" = "DrugBank ID"))

  if (nrow(invalid_manual_ids) > 0) {
    warning(
      sprintf(
        "Ignoring %s manually reviewed row(s) with DrugBank IDs not present in the vocabulary.",
        nrow(invalid_manual_ids)
      ),
      call. = FALSE
    )
  }

  manual_mapped_names <- unresolved %>%
    left_join(unresolved_reviewed, by = "query_name_clean", relationship = "many-to-many") %>%
    filter(!is.na(drugbank_id)) %>%
    inner_join(drugbank_vocabulary, by = c("drugbank_id" = "DrugBank ID")) %>%
    transmute(
      group_id,
      nct_id,
      group_title,
      group_type,
      group_description,
      intervention_id,
      intervention_name,
      intervention_description,
      query_text,
      qwen3_8b,
      deepseek_r1_8b,
      phi4,
      n_model_votes,
      source_models,
      llm_vote_confidence,
      match_source = "Manual",
      match_stage = "MANUAL_REVIEW",
      match_rank = 1L,
      matched_candidate = candidate_raw,
      normalized_name = query_name_clean,
      normalized_name_core = query_name_core,
      drugbank_id,
      drugbank_common_name = `Common name`,
      drugbank_alias = `Common name`,
      drugbank_alias_type = "MANUAL_ID",
      accession_numbers = `Accession Numbers`,
      CAS,
      UNII,
      synonyms = Synonyms,
      standard_inchi_key = `Standard InChI Key`
    )
} else {
  warning(
    sprintf(
      "Manual review file not found: %s. Continuing with automated matches only.",
      manual_review_path
    ),
    call. = FALSE
  )
}


mapped_names <- bind_rows(
  mapped_names,
  manual_mapped_names
) %>%
  distinct() %>%
  arrange(group_id, normalized_name, match_source, match_rank, drugbank_id)


# Review drugs identified with low confidence (1 LLM vote) and not manually
# curated

lowConf <- mapped_names %>% 
  filter(n_model_votes == 1 & match_source != "Manual")

write_xlsx(lowConf, "results/ClinicalTrials/mapped_drugs_lowConf.xlsx")

# Filter out drugs identified with low confidence (1 LLM vote) and not manually
# curated

mapped_names <-  mapped_names %>% 
  filter(!(n_model_votes == 1 & match_source != "Manual"))

# Add back manually curated drugs identified with low confidence (1 LLM vote) and not manually
# curated

lowConf_reviewed <- read_xlsx("results/ClinicalTrials/mapped_drugs_lowConf_reviewed.xlsx")
lowConf_reviewed <- lowConf_reviewed %>% filter(correct == "yes") %>% select(-correct, -comment)

mapped_names <- bind_rows(mapped_names, lowConf_reviewed)


## 9.2 Final data ----

resolved_ids <- mapped_names %>%
  distinct(group_id) %>%
  pull(group_id)

unresolved_groups <- DesignGroupInterventionDrugs %>%
  filter(!group_id %in% resolved_ids) %>%
  arrange(group_id)



group_map <- mapped_names %>%
  distinct(
    group_id,
    nct_id,
    group_title,
    group_type,
    group_description,
    intervention_id,
    intervention_name,
    intervention_description,
    query_text,
    drugbank_id,
    drugbank_common_name,
    synonyms,
    normalized_name,
    matched_candidate,
    n_model_votes,
    source_models,
    llm_vote_confidence
  ) %>%
  arrange(group_id, drugbank_id, normalized_name) %>%
  group_by(group_id, nct_id, group_type, intervention_name) %>%
  summarise(
    n_drugbank_ids = n_distinct(drugbank_id),
    drugbank_id = str_c(sort(unique(na.omit(drugbank_id))), collapse = " | "),
    drugbank_common_names = str_c(sort(unique(na.omit(drugbank_common_name))), collapse = " | "),
    normalized_names = str_c(sort(unique(na.omit(normalized_name))), collapse = " | "),
    matched_candidates = str_c(sort(unique(na.omit(matched_candidate))), collapse = " | "),
    min_n_model_votes = min(n_model_votes, na.rm = TRUE),
    max_n_model_votes = max(n_model_votes, na.rm = TRUE),
    source_models = str_c(sort(unique(na.omit(unlist(str_split(source_models, "\\s*\\|\\s*"))))), collapse = " | "),
    llm_vote_confidences = str_c(sort(unique(na.omit(llm_vote_confidence))), collapse = " | "),
    one_model_only_normalized_names = str_c(sort(unique(na.omit(normalized_name[n_model_votes == 1]))), collapse = " | "),
    query_texts = str_c(sort(unique(na.omit(query_text))), collapse = " | "),
    .groups = "drop"
  ) %>%
  arrange(group_id)

drugs <- mapped_names %>% select(drugbank_id, drugbank_common_name, accession_numbers:standard_inchi_key) %>%
  distinct() %>% arrange(drugbank_id)


# 10. Save results ----

#write_xlsx(drugs, "results/DrugBank/drugs.xlsx")

DesignGroupInterventionDrugs_mapped <- DesignGroupInterventionDrugs_llm %>% 
  left_join(group_map %>% select(group_id, drugbank_id), by = "group_id")
DesignGroupInterventionDrugs_mapped <- DesignGroupInterventionDrugs_mapped %>% separate_longer_delim(drugbank_id, delim = " | ")


group_drugs <- DesignGroupInterventionDrugs_mapped %>%
  select(group_id, drugbank_id) %>%
  filter(!is.na(drugbank_id)) %>% # remove non-drug / unmapped interventions
  distinct() %>% # security check:one row per intervention-drug relation
  inner_join(
    drugs %>% select(drugbank_id),
    # security check: keep only DrugBank IDs that exist in drugs
    by = "drugbank_id"
  ) %>%
  mutate(id = paste0("ID0", 1:length(group_id)), .before = group_id)

#write_xlsx(group_drugs, "results/DrugBank/group_drugs.xlsx")


write_xlsx(
  x = list(
    drugs = drugs,
    mapped_names = mapped_names,
    group_map = group_map,
    group_drugs = group_drugs,
    unresolved_groups = unresolved_groups
  ),
  file = output_path
)

message(sprintf("DrugBank mapping workbook written to: %s", output_path))
