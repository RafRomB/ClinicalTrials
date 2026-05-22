# 0. Load libraries ----

library(tidyverse)
library(openxlsx2)
library(RPostgreSQL)
library(httr2)

# 1. Load data and connect to DB ----

drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org", port=5432, user="your_user", password="your_password")

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
               SELECT *
               FROM browse_interventions
               WHERE nct_id IN (%s)
               ", ids_sql)
BrowseInterventions <- dbGetQuery(con, query)
BrowseInterventions <- BrowseInterventions %>% left_join(NCT_df, by = "nct_id")
write_xlsx(BrowseInterventions, "results/AACT/BrowseInterventions.xlsx")

query <- sprintf("
               SELECT *
               FROM browse_conditions
               WHERE nct_id IN (%s)
               ", ids_sql)
BrowseConditions <- dbGetQuery(con, query)
BrowseConditions <- BrowseConditions %>% left_join(NCT_df, by = "nct_id")
write_xlsx(BrowseConditions, "results/AACT/BrowseConditions.xlsx")


## 2.2 interventions, intervention_other_names ----

query <- sprintf("
                 SELECT id, nct_id, intervention_type, name, description
                 FROM interventions
                 WHERE nct_id IN (%s)
                 ", ids_sql)


Interventions <- dbGetQuery(con, query)
write_xlsx(Interventions, "data/AACT/Interventions.xlsx")

query <- sprintf("
                 SELECT id AS other_name_id, intervention_id, name AS other_name
                 FROM intervention_other_names
                 WHERE nct_id IN (%s)
                 ", ids_sql)

InterventionOtherNames <- dbGetQuery(con, query)
write_xlsx(InterventionOtherNames, "data/AACT/InterventionOtherNames.xlsx")

## 2.3 design_groups, design_group_interventions ----

query <- sprintf("
                 SELECT id, nct_id, group_type, title, description
                 FROM design_groups
                 WHERE nct_id IN (%s)
                 ", ids_sql)


DesignGroups <- dbGetQuery(con, query)
write_xlsx(DesignGroups, "data/AACT/DesignGroups.xlsx")

query <- sprintf("
                 SELECT id, nct_id, design_group_id, intervention_id
                 FROM design_group_interventions
                 WHERE nct_id IN (%s)
                 ", ids_sql)


DesignGroupInterventions <- dbGetQuery(con, query)
write_xlsx(DesignGroupInterventions, "data/AACT/DesignGroupInterventions.xlsx")


## 2.4 Map drugs to groups ----

# Run script 'R/ACCT_DesignGroupInterventionDrugs_Mapping.R' ----

# The resulting data is in 'results/groups_drugbank_mapping.xlsx', which includes
# several tabs with the retrieved drugs and the mapping

## 2.5 studies ----

query <- sprintf("SELECT *
                 FROM studies
                 WHERE nct_id IN (%s)
                 ", ids_sql)


Studies <- dbGetQuery(con, query)

Studies <- Studies %>% left_join(NCT_df, by = "nct_id")
write_xlsx(Studies, "results/AACT/Studies.xlsx")

## 2.6 Conditions ----

query <- sprintf("
                 SELECT * FROM conditions WHERE nct_id IN (%s)
                 ", ids_sql)

Conditions <- dbGetQuery(con, query)
Conditions <- Conditions %>% left_join(NCT_df, by = "nct_id")
write_xlsx(Conditions, "results/AACT/Conditions.xlsx")

## 2.7 ResultGroup ----

query <- sprintf("
                 SELECT * FROM result_groups WHERE nct_id IN (%s)
                 ", ids_sql)

ResultGroups <- dbGetQuery(con, query)



