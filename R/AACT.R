# 0. Load libraries ----

library(tidyverse)
library(openxlsx2)
library(RPostgreSQL)
library(webchem)
library(httr2)

# 1. Load data and connect to DB ----

drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="rafrombec", password="f@Q8JysbAxCgTd8")

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

# 2. Data retrieval ----

# Prepare vector with NCT IDs

nct_ids <- NCT_df$nct_id
ids_sql <- paste(sprintf("'%s'", nct_ids), collapse = ",")


## 2.1 MESH ----

query <- sprintf("
                 SELECT id, qualifier, tree_number, description, mesh_term, downcase_mesh_term
                 FROM mesh_terms")

MESH_TERMS <- dbGetQuery(con, query)

MESH_HEADING <- dbGetQuery(conn = con, statement = "SELECT id, qualifier, heading, subcategory FROM mesh_headings")


query <- sprintf("
               SELECT id, nct_id, mesh_term, downcase_mesh_term, mesh_type
               FROM browse_interventions
               WHERE nct_id IN (%s)
               ", ids_sql)
BrowseInterventions <- dbGetQuery(con, query)

MESH_list <- BrowseInterventions %>% filter(mesh_type == "mesh-list")

## 2.2 interventions, intervention_other_names ----

query <- sprintf("
                 SELECT id, nct_id, intervention_type, name, description
                 FROM interventions
                 WHERE nct_id IN (%s)
                 ", ids_sql)


Intervention <- dbGetQuery(con, query)
write_xlsx(Intervention, "data/AACT/Interventions.xlsx")

query <- sprintf("
                 SELECT id AS other_name_id, intervention_id, name AS other_name
                 FROM intervention_other_names
                 WHERE nct_id IN (%s)
                 ", ids_sql)

InterventionOtherNames <- dbGetQuery(con, query)
write_xlsx(InterventionOtherNames, "data/AACT/InterventionOtherNames.xlsx")


### 2.1.1 Map interventions to identifiers ----

# Run script 'ACCT_DrugMapping.R'

## 2.2 studies ----

query <- sprintf("SELECT
                 nct_id,
                 study_first_submitted_date, 
                 results_first_submitted_date, 
                 last_update_submitted_date, 
                 last_update_posted_date, 
                 last_update_posted_date_type, 
                 start_month_year, 
                 start_date_type, 
                 start_date, 
                 completion_month_year, 
                 completion_date_type, 
                 completion_date, 
                 target_duration, 
                 study_type, 
                 acronym, 
                 baseline_population, 
                 brief_title, 
                 official_title, 
                 overall_status, 
                 last_known_status, 
                 phase, enrollment, 
                 enrollment_type, 
                 source, 
                 limitations_and_caveats, 
                 number_of_arms, 
                 number_of_groups, 
                 why_stopped, 
                 has_expanded_access, 
                 expanded_access_type_individual, 
                 expanded_access_type_intermediate, 
                 expanded_access_type_treatment, 
                 has_dmc, is_fda_regulated_drug, 
                 is_fda_regulated_device, 
                 is_unapproved_device, 
                 is_us_export, 
                 biospec_retention, 
                 biospec_description, 
                 created_at, 
                 updated_at, 
                 source_class, 
                 expanded_access_nctid, 
                 expanded_access_status_for_nctid, 
                 fdaaa801_violation, 
                 baseline_type_units_analyzed, 
                 patient_registry 
                 FROM studies
                 WHERE nct_id IN (%s)
                 ", ids_sql)


studies <- dbGetQuery(con, query)

## 2.3 design_groups, design_group_interventions ----

query <- sprintf("
                 SELECT id, nct_id, group_type, title, description
                 FROM design_groups
                 WHERE nct_id IN (%s)
                 ", ids_sql)


DesignGroups <- dbGetQuery(con, query)


query <- sprintf("
                 SELECT id, nct_id, design_group_id, intervention_id
                 FROM design_group_interventions
                 WHERE nct_id IN (%s)
                 ", ids_sql)


DesignGroupInterventions <- dbGetQuery(con, query)
