# FDA-Approved vs. Non-Approved Drug Combination Clinical Trials: Comparative Analysis of ClinicalTrials.gov Records

## Project Structure

```
.
├── ClinicalTrials.Rproj
├── data
│   ├── AACT
│   │   ├── DesignGroupInterventions.xlsx
│   │   ├── DesignGroups.xlsx
│   │   ├── InterventionOtherNames.xlsx
│   │   └── Interventions.xlsx
│   └── drugbank_all_full_database
│       ├── drugbank vocabulary.csv
├── figures
│   └── README.md
├── R
│   ├── 01_FDA.R
│   ├── 02_FDA_tailscale.R
│   ├── 03_0_clinicaltrials.R
│   ├── 03_1_llm_clinicaltrials_classification_tailscale.R
│   ├── 03_2_llm_clinicaltrials_WhyStopped_tailscale.R
│   ├── 04_clinicaltrialsAPI_info.R
│   ├── 05_0_AACT.R
│   ├── 05_1_AACT_DrugMapping_llm.R
│   ├── 05_2_AACT_DesignGroupInterventionDrugs_Mapping.R
│   ├── 05_3_drug_map_evaluation.R
│   ├── 06_AACT_Analysis.R
│   ├── figures.R
│   └── leukemia_JieZhu.R
├── README.md
└── results
    ├── AACT
    │   ├── BrowseConditions.xlsx
    │   ├── BrowseInterventions.xlsx
    │   ├── Conditions.xlsx
    │   ├── DesignGroupInterventionDrugs_llm.xlsx
    │   └── Studies.xlsx
    ├── ClinicalTrials
    │   ├── clintrials_comb_eval_metrics.xlsx
    │   ├── clintrials.Rdata
    │   ├── mapped_drugs_lowConf_reviewed.xlsx
    │   ├── protocolSection_251230_llm_ensemble.xlsx
    │   ├── protocolSection_251230_llm_test_manual_eval_260216.xlsx
    │   ├── protocolSection_251230_llm.xlsx
    │   ├── protocolSection_251230.xlsx
    │   ├── protocolSection_WhyStopped_llm_eval_260217.xlsx
    │   ├── protocolSection_WhyStopped_llm_reviewed_safety_efficacy.xlsx
    │   ├── protocolSection_WhyStopped_llm_reviewed.xlsx
    │   ├── protocolSection_WhyStopped_llm.xlsx
    │   ├── protocolSection_WhyStopped.xlsx
    │   ├── Studies_API.xlsx
    │   └── WhyStopped_eval_metrics.xlsx
    ├── DrugBank
    │   ├── drugbank_mapping_unresolved.xlsx
    │   ├── drugbiol_drugbank_mapping_unresolved_query_names_reviewed.xlsx
    │   ├── drug_map_eval_sample.xlsx
    │   ├── groups_drugbank_mapping.xlsx
    │   └── mapped_drugs_lowConf_reviewed.xlsx
    ├── FDA
    │   ├── AA_verified_benefit.csv
    │   ├── AA_withdrawal.csv
    │   ├── approval_notifications_2006_2016.csv
    │   ├── approval_notifications_2017_2020.csv
    │   ├── approval_notifications_2020_2025.csv
    │   ├── approval_notifications_llm_results_combinations_final_260218.xlsx
    │   ├── approval_notifications_llm_results_combinations_final.xlsx
    │   ├── approval_notifications_llm_results_ensembled_260212_combinations.xlsx
    │   ├── approval_notifications_llm_results_ensembled_260212.xlsx
    │   ├── approval_notifications_merged_table_llm_results.xlsx
    │   ├── approval_notifications_merged_table.xlsx
    │   ├── approval_notifications_test.xlsx
    │   ├── approvals_without_NCT_curated.xlsx
    │   ├── FDA_eval_metrics.xlsx
    │   └── multi_nct_rows_curated.csv
    └── leukemiaApprovalsSuspensions.csv

12 directories, 69 files

```

## Data Sources

- [FDA Approval Notifications](https://www.fda.gov/drugs/drug-approvals-and-databases/resources-information-approved-drugs)
- [ClinicalTrials.gov API](https://clinicaltrials.gov/data-api/api)
- [Aggregated Analysis of ClinicalTrials.gov database (AACT)](https://aact.ctti-clinicaltrials.org/connect)
- [DrugBank](https://go.drugbank.com/)

Each page contains information and resources explaining the contained data. 
FDA approvals information was retrieved using web scrapping.
ClinicalTrials.gov API and AACT database data were retrieved programatically in R.
DrugBank dictionary was downloaded from the webpage (user account needed).

## R scripts

Description of the R scripts in the `R/` folder:

- `01_FDA.R`: Information retrieval and processing of the FDA approval notifications.
  - `02_FDA_tailscale.R`: LLM classification of FDA approval notifications.
- `03_0_clinicaltrials.R`: ClinicalTrials.gov API data retrieval and processing.
  - `03_01_llm_clinicaltrials_classification_tailscale.R`: LLM classification of oncology trials into drug combination or single.
  - `03_2_llm_clinicaltrials_WhyStopped_tailscale.R`: LLM classification of TERMINATED/SUSPENDED/WITHDRAWN clinical trials into efficacy/safety/nonclinical reasons for failure.
- `04_clinicaltrialsAPI_info.R`: Studies information retrieval directly from the ClinicalTrials.gov API.
- `05_0_AACT.R`: Studies information retrieval using AACT database.
  - `05_1_AACT_DrugMapping_llmR`: Drug names extraction for clinical trials groups.
  - `05_2_AACT_DesignGroupInterventionsDrugs_Mapping.R`: Normalization, processing, curation, and mapping of drug names from study arm groups to DrugBank IDs.
  - `05_3_drug_map_evaluation.R`: Generation of sample of studies for the evaluation of the drug name retrieval and DrugBank ID mapping.
- `06_AACT_Analysis.R`: Comparative analysis of approved vs. non-approved drug combination clinical trials.
- `figures.R`: Generation of figures summarizing the LLM performance metrics.
- `leukemia_JieZhu.R`: Retrieval of studies information for chronic lymphocytic leukemia (CLL) and  acute myeloid leukemia (AML) [(10.64898/2026.05.18.725869)](https://www.biorxiv.org/content/10.64898/2026.05.18.725869v1).

The complete execution of `01_FDA.R`, `03_0_clinicaltrials.R`, and `05_0_AACT.R` scripts depend on data generated in the nested scripts. 
It is indicated in the code when those files should be run to generate the data (which is provided in the folder in any case).

## Data and Files

The `FileList.xlsx` contains a description of the different files contained in `/data/` and `/results/` folders. 

These additional files are also provided:

- `data/drugbank_all_full_database/drugbank vocabulary.csv`: DrugBank vocabulary with drug names and associated information, including DrugBank IDs.
- `results/leukemiaApprovalsSuspensions.csv`: Studies information for CLL and AML.


A Google Docs document with an initial draft of the "Methods" can be accessed [here](https://docs.google.com/document/d/1mWLI3vYvP-fpJYjBvCxxRgg-uYzoRQ5xLtzP7L2URYY/edit?usp=sharing).
And also a summary presentation with the initial results can be found [here](https://docs.google.com/presentation/d/14KhR4ZtK8wW7aoagB7CsEYwR98XE7PSWPP9Ke0eghME/edit?slide=id.p1#slide=id.p1).

