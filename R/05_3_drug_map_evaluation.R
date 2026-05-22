# 0. Load libraries ----

library(tidyverse)
library(openxlsx2)
library(ggpubr)

# 1. Load data ----
drugs = read_xlsx("results/ClinicalTrials/groups_drugbank_mapping.xlsx", sheet = "drugs")
mapped_names = read_xlsx("results/ClinicalTrials/groups_drugbank_mapping.xlsx", sheet = "mapped_names")
group_map = read_xlsx("results/ClinicalTrials/groups_drugbank_mapping.xlsx", sheet = "group_map")
group_drugs = read_xlsx("results/ClinicalTrials/groups_drugbank_mapping.xlsx", sheet = "group_drugs")
unresolved_groups = read_xlsx("results/ClinicalTrials/groups_drugbank_mapping.xlsx", sheet = "unresolved_groups")
unresolved_names = read_xlsx("results/ClinicalTrials/drugbank_mapping_unresolved.xlsx", sheet = "unresolved_names")
unresolved_query_review = read_xlsx("results/ClinicalTrials/drugbank_mapping_unresolved.xlsx", sheet = "unresolved_query_review")

DesignGroupInterventionDrugs <- read_xlsx("results/ClinicalTrials/DesignGroupInterventionDrugs_llm.xlsx")

# Total intervention arms
n_groups <- length(unique(DesignGroupInterventionDrugs$group_id))

# Arms with at least one drug mapped

n_1drug_mapped <- length(unique(group_map$group_id))

# Unresolved arms

n_unresolved <- sum(!DesignGroupInterventionDrugs$group_id %in% group_map$group_id) 

# Total drugs mapped

n_drugs <- length(unique(drugs$drugbank_id))

unresolved_reviewed <- read_xlsx("results/ClinicalTrials/drugbiol_drugbank_mapping_unresolved_query_names_reviewed.xlsx")
unresolved_names <- unresolved_names %>% left_join(unresolved_reviewed, by = "query_name_clean")
write_xlsx(unresolved_names, "results/ClinicalTrials/unresolved_names_drugbankids.xlsx")

unresolved_names <- unresolved_names %>% filter(is.na(drugbank_id))

# Unresolved names

n_unresolved_names <- length(unique(unresolved_names$query_name_clean))

# Resolved names

n_mapped_names <- nrow(mapped_names)

tibble(
  Variable = c("Total intervention arms",
             "Unique drugs mapped",
             "Arms with at least 1 drug mapped",
             "Unresolved arms",
             "Mapped names",
             "Unmapped names"),
  Count = c(n_groups,
            n_drugs,
            paste0(n_1drug_mapped, "/", n_groups),
            paste0(n_unresolved, "/", n_groups),
            n_mapped_names,
            n_unresolved_names),
  Proportion = c(NA,
                 NA,
                 round(n_1drug_mapped/n_groups,2),
                 round(n_unresolved/n_groups, 2),
                 NA,
                 NA)
) %>% 
  ggtexttable(rows = NULL, theme = ttheme(base_style = "light", base_size = 16))

## Sample for manual evaluation 

DesignGroupInterventionDrugs_mapped <- DesignGroupInterventionDrugs %>%
  left_join(
    group_map %>% select(group_id, drugbank_id, drugbank_common_names, matched_candidates, llm_vote_confidences),
    by = "group_id"
  )


# Calculate manual evaluation sample size, based on https://pmc.ncbi.nlm.nih.gov/articles/PMC4792103/ 
binom_N <- function(Z, p0, E) {
  N <- (Z^2*p0*(1-p0))/E^2
  return(ceiling(N))
}

## Strata: confidence (number of LLM votes)

## Expected accuracy: high_3_models: 0.9; medium_2_models: 0.8;
## (low_1_model were already manually reviewed and filtered out)

## Unresolved arms: evaluate all (n=28)

## Sample sizes target ±0.08 margin of error at 95% CI per stratum
N_high   <- binom_N(Z = 1.96, p0 = 0.9,  E = 0.08)  # ~54
N_medium <- binom_N(Z = 1.96, p0 = 0.8,  E = 0.08)  # ~96

## Assign confidence stratum per arm (worst-case confidence wins)
DesignGroupInterventionDrugs_mapped <- DesignGroupInterventionDrugs_mapped %>%
  mutate(
    eval_stratum = case_when(
      is.na(drugbank_id)                                          ~ "unresolved",
      str_detect(llm_vote_confidences, "medium_2_models")        ~ "medium",
      TRUE                                                        ~ "high"
    )
  )

## Stratified random sample (unresolved arms are evaluated in full)
set.seed(123)

eval_sample <- bind_rows(
  DesignGroupInterventionDrugs_mapped %>% filter(eval_stratum == "high")       %>% slice_sample(n = N_high),
  DesignGroupInterventionDrugs_mapped %>% filter(eval_stratum == "medium")     %>% slice_sample(n = N_medium),
  DesignGroupInterventionDrugs_mapped %>% filter(eval_stratum == "unresolved")
) %>%
  mutate(
    missing_drugs = NA_character_,   # free-text: drug names present in query_text but absent from output
    correct       = NA_character_,   # "yes" | "partial" | "no"
    notes         = NA_character_
  ) %>%
  select(
    eval_stratum,
    group_id, nct_id, group_type, group_title,
    query_text,
    drugbank_common_names,
    matched_candidates,
    missing_drugs, correct, notes
  ) %>%
  arrange(eval_stratum, group_id)

message(sprintf(
  "Evaluation sample: %d arms  (high=%d, medium=%d, unresolved=%d)",
  nrow(eval_sample),
  sum(eval_sample$eval_stratum == "high"),
  sum(eval_sample$eval_stratum == "medium"),
  sum(eval_sample$eval_stratum == "unresolved")
))

write_xlsx(eval_sample, "results/ClinicalTrials/drug_map_eval_sample.xlsx")
