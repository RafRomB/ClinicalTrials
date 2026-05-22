library(tidyverse)
library(openxlsx2)

FDA_llm_curated_combinations <- openxlsx2::read_xlsx("results/FDA/approval_notifications_llm_results_combinations_final_260218.xlsx")
FDA_llm_curated_combinations <- FDA_llm_curated_combinations %>% select(nct, title, description, full_text, url)

Conditions <- read_xlsx("results/AACT/Conditions.xlsx")

successful <- read_xlsx("results/FDA/approval_notifications_llm_results_combinations_final_260218.xlsx")
successful <- successful %>% pull(nct) %>% unique()

non_successful <- read_xlsx("results/ClinicalTrials/protocolSection_WhyStopped_llm_reviewed_safety_efficacy.xlsx")
non_successful <- non_successful %>% pull(nctId) %>% unique()

# Clinical trials dataframe

NCT_df <- bind_rows(
  tibble(
    nct_id = successful,
    FDA_Approved = TRUE
  ),
  tibble(
    nct_id = non_successful,
    FDA_Approved = FALSE
  )
)

# Filter for CLL and AML related conditions
leukemia_conditions <- Conditions %>%
  filter(str_detect(name, 
                    regex("chronic lymphocytic leukemia|CLL|acute myeloid leukemia|AML", 
                          ignore_case = TRUE)))


NCT_leukemia <- leukemia_conditions %>% group_by(nct_id) %>% summarise(name = paste(name, collapse = "; "))
NCT_leukemia <- NCT_leukemia %>% left_join(
  leukemia_conditions %>% select(nct_id, FDA_Approved) %>% distinct(nct_id, FDA_Approved), 
  by = "nct_id")



NCT_leukemia <- NCT_leukemia %>% left_join(FDA_llm_curated_combinations, by = join_by("nct_id" == "nct"))

write.csv(NCT_leukemia, "results/leukemiaApprovalsSuspensions.csv")

