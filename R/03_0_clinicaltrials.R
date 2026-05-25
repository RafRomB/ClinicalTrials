# 0. Load libraries ----
library(httr2)
library(tidyverse)
library(jsonlite)
library(rollama)
library(cowplot)
library(openxlsx2)

options(rollama_seed = 42)

curl_translate("curl -X GET https://clinicaltrials.gov/search?cond=cancer%20OR%20neoplasm%20OR%20tumor&term=combination%20OR%20combined%20OR%20drug%20combination%20OR%20combination%20therapy%20OR%20multi-agent&intr=Drug&aggFilters=phase:2%203%204,results:with,status:act%20com%20ter")


# 1. Extract Data from clinicaltrials.com API ----
clin_trials_url <- "https://clinicaltrials.gov/api/v2/studies"

# Retrieve studies

clin_trials_response <- function(url,
                                 cond = NULL,
                                 titles = NULL,
                                 term = NULL,
                                 intr = NULL,
                                 advFilter = NULL,
                                 aggFilters = NULL,
                                 overallStatus = NULL,
                                 pageSize = 1000) {
  studies <- list()
  pageToken <- NULL
  
  repeat {
    resp <- request(url) |>
      req_method("GET") |>
      req_url_query(
        query.cond = cond,
        query.titles = titles,
        query.term = term,
        query.intr = intr,
        filter.advanced = advFilter,
        aggFilters = aggFilters,
        filter.overallStatus = overallStatus,
        pageSize = pageSize,
        pageToken = pageToken
      ) |>
      req_perform()
    
    body <- resp_body_json(resp)
    studies <- c(studies, body[["studies"]] %||% list())
    
    pageToken <- body[["nextPageToken"]]
    if (is.null(pageToken))
      break
  }
  
  return(studies)
}

## 1.1 Initial search ----

 
combo_titles <- paste(
  '(',
  '"in combination" OR combination OR combined OR "combination therapy" OR',
  '"multi-agent" OR multiagent OR multidrug OR "multi drug" OR regimen OR doublet OR triplet OR',
  '"add-on" OR addon OR adjunct OR "co-administered" OR coadministered OR',
  'plus OR with',
  ')',
  # capture titles like "... with placebo/chemo/immunotherapy"
  'OR (with AND (placebo OR chemotherapy OR immunotherapy))',
  # capture titles like NCT01168973: "X and Y versus X alone"
  'OR ((versus OR vs OR "compared with") AND (alone OR placebo) AND (chemotherapy OR immunotherapy OR antibody OR inhibitor OR targeted))'
)



## **1.3 ACTIVE_NOT_RECRUITING,SUSPENDED,COMPLETED,TERMINATED,WITHDRAWN,UNKNOWN** ----

# Retrieved: 251230 17:22

clintrials <- clin_trials_response(url = clin_trials_url, 
                                   cond = "(cancer OR tumor OR tumour OR malignant OR carcinoma 
                                   OR sarcoma OR lymphoma OR leukemia OR leukaemia OR myeloma 
                                   OR blastoma OR neuroblastoma OR myelodysplastic)", 
                                   titles = combo_titles, 
                                   advFilter = NULL,
                                   aggFilters = "phase:2 3 4",
                                   overallStatus = "ACTIVE_NOT_RECRUITING,SUSPENDED,COMPLETED,TERMINATED,WITHDRAWN,UNKNOWN",
                                   pageSize = 1000)


# Save studies NIH Clinical Trials IDs
studies_id <- clintrials %>%
  map_dfr(\(x) {
    tibble(
      study_id = x %>% pluck("protocolSection", "identificationModule", "nctId")
    )
  })


# Name the studies with the IDs
names(clintrials) <- studies_id$study_id
#save(clintrials, file = "results/ClinicalTrials/clintrials.Rdata")
load("results/ClinicalTrials/clintrials.Rdata")



# Save studies NIH Clinical Trials IDs
studies_id <- clintrials %>%
  map_dfr(\(x) {
    tibble(
      study_id = x %>% pluck("protocolSection", "identificationModule", "nctId")
    )
  })

# Filter approved studies for drug combinations based on NCT
combination_approvals <- openxlsx2::read_xlsx("results/FDA/approval_notifications_llm_results_combinations_final.xlsx")

combination_approvals$clinicaltrialgov <- combination_approvals$nct %in% studies_id$study_id

write_xlsx(combination_approvals, "results/FDA/approval_notifications_llm_results_combinations_final_ClinTrials.xlsx")

table(combination_approvals$clinicaltrialgov)

no_study <- combination_approvals %>% filter(clinicaltrialgov == FALSE)

combination_approvals <- combination_approvals %>% filter(clinicaltrialgov == TRUE)
length(unique(combination_approvals$nct))
length(unique(combination_approvals$ID))


## 1.5 LLM Classification ----


# Function to collapse fields

collapse_field <- function(x,
                           field,
                           inner_sep = "|",  # between multiple values inside one element
                           outer_sep = ";") { # between different elements of x
  
  # If the whole thing is missing/empty
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  
  vals <- vapply(
    x,
    function(el) {
      if (is.null(el)) return(NA_character_)
      
      val <- el[[field]]
      if (is.null(val)) return(NA_character_)
      
      # Flatten possible nested lists like interventionNames
      if (is.list(val)) {
        val <- unlist(val, use.names = FALSE)
      }
      
      val <- as.character(val)
      # Collapse multiple values within *one* element (e.g., multiple interventionNames)
      paste(val, collapse = inner_sep)
    },
    character(1)
  )
  
  # Remove all-NA / empty cases
  vals <- vals[!is.na(vals) & vals != ""]
  if (!length(vals)) return(NA_character_)
  
  # Collapse across elements (e.g., across armGroups)
  paste(vals, collapse = outer_sep)
}

# Protocol Section

protocolSection <- function(study) {

  tibble(
    # IdentificationModule
    nctId = study$protocolSection$identificationModule$nctId,
    BriefTitle = study$protocolSection$identificationModule$briefTitle,
    OfficialTitle = study$protocolSection$identificationModule$officialTitle,
    Acronym = study$protocolSection$identificationModule$acronym,
    OrgFullName = study$protocolSection$identificationModule$organization$fullName,
    OrgClass = study$protocolSection$identificationModule$organization$class,
    
    # StatusModule
    StatusVerifiedDate = study$protocolSection$statusModule$statusVerifiedDate,
    OverallStatus = study$protocolSection$statusModule$overallStatus,
    WhyStopped = study$protocolSection$statusModule$whyStopped,
    HasExpandedAccess = study$rotocolSection$statusModule$expandedAccessInfo$hasExpandedAccess,
    ExpandedAccessStatusForNCTId = study$protocolSection$statusModule$expandedAccessInfo$statusForNctId,
    StartDate = study$protocolSection$statusModule$startDateStruct$date,
    StartDateType = study$protocolSection$statusModule$startDateStruct$type,
    PrimaryCompletionDate = study$protocolSection$statusModule$primaryCompletionDateStruct$date,
    PrimaryCompletionDateType = study$protocolSection$statusModule$primaryCompletionDateStruct$type,
    CompletionDate = study$protocolSection$statusModule$completionDateStruct$date,
    CompletionDateType = study$protocolSection$statusModule$completionDateStruct$type,
    StudyFirstPostDate = study$protocolSection$statusModule$studyFirstPostDateStruct$date,
    StudyFirstPostDateType = study$protocolSection$statusModule$studyFirstPostDateStruct$type,
    ResultsFirstPostDate = study$protocolSection$statusModule$resultsFirstPostDateStruct$date,
    DispFirstPostDate = study$protocolSection$statusModule$dispFirstPostDateStruct$date,
    LastUpdatePostDate = study$protocolSection$statusModule$lastUpdatePostDateStruct$date,
    LastUpdatePostDateType = study$protocolSection$statusModule$lastUpdatePostDateStruct$type,
    
    # SponsorCollaboratorsModule
    ResponsiblePartyType = study$protocolSection$sponsorCollaboratorsModule$responsibleParty$type,
    LeadSponsorName = study$protocolSection$sponsorCollaboratorsModule$leadSponsor$name,
    LeadSponsorClass = study$protocolSection$sponsorCollaboratorsModule$leadSponsor$class,
    CollaboratorName = study$protocolSection$sponsorCollaboratorsModule$collaborators$name,
    CollaboratorClass = study$protocolSection$sponsorCollaboratorsModule$collaborators$class,
    NumCollaborators = study$protocolSection$sponsorCollaboratorsModule$numCollaborators,
    NumCollaboratorsPlusLead = study$protocolSection$sponsorCollaboratorsModule$numCollaboratorsPlusLead,
    
    # OversightModule
    OversightHasDMC = study$protocolSection$oversightModule$oversightHasDmc,
    IsFDARegulatedDrug = study$protocolSection$oversightModule$isFdaRegulatedDrug,
    IsFDARegulatedDevice = study$protocolSection$oversightModule$isFdaRegulatedDevice,
    IsUnapprovedDevice = study$protocolSection$oversightModule$isUnapprovedDevice,
    IsPPSD = study$protocolSection$oversightModule$isPpsd,
    IsUSExport = study$protocolSection$oversightModule$isUsExport,
    FDAAA801Violation = study$protocolSection$oversightModule$fdaaa801Violation,
    
    # DescriptionModule
    
    BriefSummary = study$protocolSection$descriptionModule$briefSummary,
    DetailedDescription = study$protocolSection$descriptionModule$detailedDescription,
    
    # ConditionsModule
    
    Condition = paste0(study$protocolSection$conditionsModule$conditions, collapse = ";"),
    NumConditions = length(study$protocolSection$conditionsModule$conditions),
    Keyword = paste0(study$protocolSection$conditionsModule$keywords, collapse = ";"),
    
    # DesignModule
    
    StudyType = study$protocolSection$designModule$studyType,
    NPtrsToThisExpAccNCTId = study$protocolSection$designModule$nPtrsToThisExpAccNctId,
    ExpAccTypeIndividual = study$protocolSection$designModule$expandedAccessTypes$individual,
    ExpAccTypeIntermediate = study$protocolSection$designModule$expandedAccessTypes$intermediate,
    ExpAccTypeTreatment = study$protocolSection$designModule$expandedAccessTypes$treatment,
    PatientRegistry = study$protocolSection$designModule$patientRegistry,
    TargetDuration = study$protocolSection$designModule$targetDuration,
    Phase = paste0(study$protocolSection$designModule$phases, collapse = ";"),
    NumPhases = length(study$protocolSection$designModule$phases),
    DesignAllocation = study$protocolSection$designModule$designInfo$allocation,
    DesignInterventionModel = study$protocolSection$designModule$designInfo$interventionModel,
    DesignInterventionModelDescription = study$protocolSection$designModule$designInfo$interventionModelDescription,
    DesignPrimaryPurpose = study$protocolSection$designModule$designInfo$primaryPurpose,
    DesignObservationalModel = study$protocolSection$designModule$designInfo$observationalModel,
    DesignTimePerspective = study$protocolSection$designModule$designInfo$timePerspective,
    DesignMasking = study$protocolSection$designModule$designInfo$maskingInfo$masking,
    DesignMaskingDescription = study$protocolSection$designModule$designInfo$maskingInfo$maskingDescription,
    DesignWhoMasked = paste0(study$protocolSection$designModule$designInfo$maskingInfo$whoMasked, collapse = ";"),
    BioSpecRetention = study$protocolSection$designModule$bioSpec$retention,
    BioSpecDescription = study$protocolSection$designModule$bioSpec$retention,
    EnrollmentCount = study$protocolSection$designModule$enrollmentInfo$count,
    EnrollmentType = study$protocolSection$designModule$enrollmentInfo$type,
    ArmGroupLabel = collapse_field(study$protocolSection$armsInterventionsModule$armGroups,"label"),
    ArmGroupType = collapse_field(study$protocolSection$armsInterventionsModule$armGroups,"type"),
    ArmGroupDescription = collapse_field(study$protocolSection$armsInterventionsModule$armGroups, "description"),
    ArmGroupInterventionName = collapse_field(study$protocolSection$armsInterventionsModule$armGroups, "interventionNames"),
    #NumArmGroupInterventionNames = study$protocolSection$armsInterventionsModule$armGroups$numArmGroupInterventionNames,
    NumArmGroups = length(study$protocolSection$armsInterventionsModule$armGroups),
    InterventionType = collapse_field(study$protocolSection$armsInterventionsModule$interventions,"type"),
    InterventionName = collapse_field(study$protocolSection$armsInterventionsModule$interventions, "name"),
    InterventionDescription = collapse_field(study$protocolSection$armsInterventionsModule$interventions, "description"),
    InterventionArmGroupLabel = collapse_field(study$protocolSection$armsInterventionsModule$interventions, "armGroupLabels"),
    #NumInterventionArmGroupLabels = study$protocolSection$armsInterventionsModule$interventions$numInterventionArmGroupLabels,
    InterventionOtherName = collapse_field(study$protocolSection$armsInterventionsModule$interventions, "otherNames"),
    #NumInterventionOtherNames = study$protocolSection$armsInterventionsModule$interventions$numInterventionOtherNames, 
    NumInterventions = length(study$protocolSection$armsInterventionsModule$interventions),
    
    # OutcomesModule
    
    PrimaryOutcomeMeasure = study$protocolSection$outcomesModule$primaryOutcomes$measure,
    PrimaryOutcomeDescription = study$protocolSection$outcomesModule$primaryOutcomes$description,
    PrimaryOutcomeTimeFrame = study$protocolSection$outcomesModule$primaryOutcomes$timeFrame,
    #NumPrimaryOutcomes = study$protocolSection$outcomesModule$numPrimaryOutcomes,
    SecondaryOutcomeMeasure = study$protocolSection$outcomesModule$secondaryOutcomes$measure,
    SecondaryOutcomeDescription = study$protocolSection$outcomesModule$secondaryOutcomes$description,
    SecondaryOutcomeTimeFrame = study$protocolSection$outcomesModule$secondaryOutcomes$timeFrame,
    #NumSecondaryOutcomes = study$protocolSection$outcomesModule$numSecondaryOutcomes,
    OtherOutcomeMeasure = study$protocolSection$outcomesModule$otherOutcomes$measure,
    OtherOutcomeDescription = study$protocolSection$outcomesModule$otherOutcomes$description,
    OtherOutcomeTimeFrame = study$protocolSection$outcomesModule$otherOutcomes$timeFrame,
    #NumOtherOutcomes = study$protocolSection$outcomesModule$numOtherOutcomes,
    #NumOutcomes = study$protocolSection$outcomesModule$numOutcomes,
    
    # EligibilityModule
    
    EligibilityCriteria = study$protocolSection$eligibilityModule$eligibilityCriteria,
    HealthyVolunteers = study$protocolSection$eligibilityModule$healthyVolunteers,
    Sex = study$protocolSection$eligibilityModule$sex,
    GenderBased = study$protocolSection$eligibilityModule$genderBased,
    GenderDescription = study$protocolSection$eligibilityModule$genderDescription,
    MinimumAge = study$protocolSection$eligibilityModule$minimumAge,
    MaximumAge = study$protocolSection$eligibilityModule$maximumAge,
    StdAge = paste0(study$protocolSection$eligibilityModule$stdAges, collapse = ";"),
    NumStdAges = length(study$protocolSection$eligibilityModule$stdAges),
    StudyPopulation = study$protocolSection$eligibilityModule$studyPopulation,
    SamplingMethod = study$protocolSection$eligibilityModule$samplingMethod,
    
    # ContactsLocationsModule
    
    NumCentralContacts = study$protocolSection$contactsLocationsModule$numCentralContacts,
    NumOverallOfficials = study$protocolSection$contactsLocationsModule$numOverallOfficials,
    LocationCountry = study$protocolSection$contactsLocationsModule$locations$country,
    NumLocationContacts = study$protocolSection$contactsLocationsModule$locations$numLocationContacts,
    LocationCountryCode = study$protocolSection$contactsLocationsModule$locations$countryCode,
    LocationGeoPoint = study$protocolSection$contactsLocationsModule$locations$geoPoint,
    NumLocations = study$protocolSection$contactsLocationsModule$numLocations,
    NumUniqueLocationCountries = study$protocolSection$contactsLocationsModule$numUniqueLocationCountries,
    
    # ReferencesModule
    
    ReferencePMID = study$protocolSection$referencesModule$references$pmid,
    RetractionPMID = study$protocolSection$referencesModule$references$retractions$pmid,
    RetractionSource = study$protocolSection$referencesModule$references$retractions$source,
    #NumRetractionsForRef = study$protocolSection$referencesModule$references$numRetractionsForRef,
    #NumReferences = study$protocolSection$referencesModule$numReferences,
    #NumRetractionsAllRefs = study$protocolSection$referencesModule$numRetractionsAllRefs,
    AvailIPDType = study$protocolSection$referencesModule$availIpds$type,
    
    # IPDSharingStatementModule
    
    IPDSharing = study$protocolSection$ipdSharingStatementModule$ipdSharing,
    #IPDSharingInfoType = study$protocolSection$ipdSharingStatementModule$infoTypes,
    #NumIPDSharingInfoTypes = study$protocolSection.ipdSharingStatementModule.numIpdSharingInfoTypes,
    
  )
}

# protocolSection_251230 <- clintrials %>%
#   map(protocolSection) %>%
#   bind_rows()
# protocolSection_251230 <- protocolSection_251230 %>% select(nctId, BriefTitle, BriefSummary, ArmGroupDescription)
#openxlsx2::write_xlsx(protocolSection_251230, "results/ClinicalTrials/protocolSection_251230.xlsx")

## Run '/R/03_1_llm_clinicaltrials_classification_tailscale.R' script in GPU machine.


### 1.5.1 LLM Results ----

load("results/ClinicalTrials/clintrials.Rdata")

protocolSection_llm <- openxlsx2::read_xlsx("results/ClinicalTrials/protocolSection_251230_llm.xlsx")

# Clean llm outputs to contain only 'single' or 'combination'

clean_output <- function(x) {
  x <- tolower(x)
  # extract the FIRST match of either word
  m <- stringr::str_extract(x, "\\b(single|combination)\\b")
  ifelse(is.na(m), "single", m)
}

protocolSection_llm <- protocolSection_llm %>% mutate(
  across(c(qwen3_8b, deepseek_r1_8b, phi4), clean_output)
)

llm_results <- protocolSection_llm %>% select(qwen3_8b, deepseek_r1_8b, phi4)

# Majority of votes

protocolSection_llm$ensemble <- apply(llm_results, 1, function(x) {
  prop <- table(x)
  names(prop)[which.max(prop)]
})

#### Review disagreement results ----

protocolSection_llm$disagreement <- apply(llm_results, 1, function(x) length(unique(x)) > 1)

#write_xlsx(protocolSection_llm, "results/ClinicalTrials/protocolSection_251230_llm_ensemble.xlsx")

protocolSection_llm <- read_xlsx("results/ClinicalTrials/protocolSection_251230_llm_ensemble.xlsx")

barplot_from_table <- function(data, X = NULL, Y, X_name = "X", Y_name = "Count", is.na = FALSE) {
  # Deal with NAs
  
  if (is.null(X)) {
    X <- names(table(data[[Y]], useNA = "ifany"))
  }
  
  if (is.na) {}
  
  data[[Y]][data[[Y]] == "NA"] <- NA
  
  df <- tibble(X = factor(X), Y = as.numeric(table(data[[Y]], useNA = "ifany")))
  colnames(df) <- c(X_name, Y_name)
  df %>% ggplot(aes(x = .data[[X_name]], y = .data[[Y_name]])) + 
    geom_bar(aes(fill = .data[[X_name]], color = .data[[X_name]]), alpha = 0.3, stat = "identity") +
    geom_text(aes(label = paste(100*round(.data[[Y_name]]/sum(.data[[Y_name]]), 3), "%", sep = "")), vjust = -0.6, fontface = "bold") +
    geom_text(aes(label = .data[[Y_name]]), vjust = 1.6, color = "gray50") +
    cowplot::theme_cowplot()
}

#plotly::ggplotly(barplot_from_table(data = protocolSection_llm, X = c("no", "yes"), Y = "disagreement"))

(p1 <- barplot_from_table(data = protocolSection_llm, Y = "disagreement", 
                   X_name = "Disagreement", Y_name = "Count") + labs(title = "Disagreement in LLMs Classification")
)

p2 <- barplot_from_table(data = protocolSection_llm,
                   Y = "ensemble", X_name = "Class", Y_name = "Count") + 
  labs(title = "'ensemble' Classification Results",
       subtitle = "Results of the majority voting of the three LLMs")


protocolSection_llm_filtered <- protocolSection_llm %>% filter(ensemble == "combination") 

p3 <- barplot_from_table(protocolSection_llm_filtered,
                         Y = "disagreement", X_name = "Disagreement", Y_name = "Count") + 
  labs(title = "Disagreement in 'ensemble' Results")


plot_grid(p1, p2, p3, nrow = 1)

### 1.5.2 LLM Performance Evaluation ----

# Calculate manual evaluation sample size, based on https://pmc.ncbi.nlm.nih.gov/articles/PMC4792103/ and assuming an expected accuracy
# of 0.5 and 95% +- 5% confidence interval

binom_N <- function(Z, p0, E) {
  N <- (Z^2*p0*(1-p0))/E^2
  return(ceiling(N))
}

N <- binom_N(Z = 1.96, p0 = 0.5, E = 0.05)

prop.table(table((protocolSection_llm$ensemble)))

set.seed(123)
protocolSection_llm_test <- rsample::initial_split(protocolSection_llm, prop = N/nrow(protocolSection_llm), strata = ensemble)
protocolSection_llm_test <- rsample::training(protocolSection_llm_test)

prop.table(table((protocolSection_llm_test$ensemble)))


# protocolSection_llm_test %>% arrange(nctId) %>% # to shuffle order of "single" / "combination"
#   select(-c("qwen3_8b", "deepseek_r1_8b", "phi4","ensemble", "disagreement")) %>% 
#   openxlsx2::write_xlsx("results/ClinicalTrials/protocolSection_251230_llm_test_manual_eval.xlsx")

# Manual evaluation of classification of test sample
# Load results

protocolSection_llm_test_manual_eval <- read_xlsx("results/ClinicalTrials/protocolSection_251230_llm_test_manual_eval_260216.xlsx")

protocolSection_llm_test_manual_eval <- protocolSection_llm_test_manual_eval %>% left_join(protocolSection_llm %>% select(nctId, qwen3_8b, deepseek_r1_8b, phi4, ensemble), by = "nctId")

protocolSection_llm_test_manual_eval <- protocolSection_llm_test_manual_eval %>% mutate(manual_eval = as.factor(manual_eval),
                                                                                        qwen3_8b = as.factor(qwen3_8b),
                                                                                        deepseek_r1_8b = as.factor(deepseek_r1_8b),
                                                                                        phi4 = as.factor(phi4),
                                                                                        ensemble = as.factor(ensemble))

retrieve_metrics <- function(data, reference, models) {
  
  metrics <- tibble()
  
  for (m in models){
    
    model <- m
    
    df <- tibble(model = m, 
                 accuracy = yardstick::accuracy(data, reference, m)$.estimate, 
                 sensitivity = yardstick::sens(data, reference, m)$.estimate,
                 specificity = yardstick::spec(data, reference, m)$.estimate,
                 precision = yardstick::precision(data, reference, m)$.estimate,
                 #recall = yardstick::recall(data, reference, m)$.estimate,
                 f1 = yardstick::f_meas(data, reference, m)$.estimate,
                 mcc = yardstick::mcc(data, reference, m)$.estimate)
    
    metrics <- bind_rows(metrics, df)
    
  }
  
  return(metrics)
}



(clintrials_comb_eval_metrics <- retrieve_metrics(
  data = protocolSection_llm_test_manual_eval,
  reference = "manual_eval",
  models = c("qwen3_8b", "deepseek_r1_8b", "phi4", "ensemble")
  )
)

write_xlsx(clintrials_comb_eval_metrics, "results/ClinicalTrials/clintrials_comb_eval_metrics.xlsx")

conf_matrix <- yardstick::conf_mat(protocolSection_llm_test_manual_eval, truth = "manual_eval", estimate = "ensemble") %>% autoplot(type = "heatmap") + # Use tiles for the heatmap cells
  scale_fill_gradient(low = "white", high = "slateblue") + labs(title = "Evaluation of LLM performance for single-drug/combination of clinical trials")


### 1.5.3 Aproval notifications in filtered studies ----

## Run code in point 1.3 to obtain the combination_approvals df

combination_approvals <- openxlsx2::read_xlsx("results/FDA/approval_notifications_llm_results_combinations_final_ClinTrials.xlsx")
length(unique(combination_approvals$ID)) # 203

protocolSection_llm_filtered <- protocolSection_llm %>% filter(ensemble == "combination") 

combination_approvals <- combination_approvals %>% filter(clinicaltrialgov == TRUE)

table(combination_approvals$nct %in% protocolSection_llm_filtered$nctId)

not_found_approvals <- combination_approvals %>% filter(!nct %in% protocolSection_llm_filtered$nctId)

not_found_studies <- protocolSection_llm %>% filter(nctId %in% not_found_approvals$nct)

combination_approvals <- combination_approvals %>% filter(nct %in% protocolSection_llm_filtered$nctId)
length(unique(combination_approvals$ID)) # 199
length(unique(combination_approvals$nct)) # 168



#2. Final filtering of combination clinical trials ----

protocolSection_llm <- protocolSection_llm %>% filter(ensemble == "combination") 
rm(protocolSection_llm_filtered)

clintrials <- clintrials[names(clintrials) %in% protocolSection_llm$nctId]

protocolSection_combination <- clintrials %>%
  map(protocolSection) %>%
  bind_rows()

# Filter to include only studies with TREATMENT purpose, and that have a non-empty WhyStopped field

protocolSection_WhyStopped <- protocolSection_combination %>% filter(DesignPrimaryPurpose == "TREATMENT", !is.na(WhyStopped))
#openxlsx2::write_xlsx(protocolSection_WhyStopped, "results/ClinicalTrials/protocolSection_WhyStopped.xlsx")

#### WhyStopped - LLM classification ----

# Run 'llm_clinicaltrials_WhyStopped_tailscale.R' script

protocolSection_WhyStopped_llm <- openxlsx2::read_xlsx("results/ClinicalTrials/protocolSection_WhyStopped_llm.xlsx")

clean_output <- function(x) {
  x <- tolower(x)
  # extract the FIRST match of either word
  m <- stringr::str_extract(x, "\\b(safety|efficacy|nonclinical)\\b")
  ifelse(is.na(m), "nonclinical", m)
}

protocolSection_WhyStopped_llm <- protocolSection_WhyStopped_llm %>% mutate(
  across(c(qwen3_14b, deepseek_r1_8b, phi4), clean_output)
)

llm_results <- protocolSection_WhyStopped_llm %>% select(qwen3_14b, deepseek_r1_8b, phi4)

protocolSection_WhyStopped_llm$ensemble <- apply(llm_results, 1, function(x) { # Majority of votes
  prop <- table(x)
  names(prop)[which.max(prop)]
})

protocolSection_WhyStopped_llm$needs_review <- apply(llm_results, 1, function(x) { # Majority of votes
  agreement = max(table(x))/length(x)
  (agreement < 2/3)
})

#write_xlsx(protocolSection_WhyStopped_llm, "results/ClinicalTrials/protocolSection_WhyStopped_llm_reviewed.xlsx")

protocolSection_WhyStopped_llm <- read_xlsx("results/ClinicalTrials/protocolSection_WhyStopped_llm_reviewed.xlsx")


protocolSection_WhyStopped_llm <- read_xlsx("results/ClinicalTrials/protocolSection_WhyStopped_llm_reviewed.xlsx")
protocolSection_WhyStopped_llm_safety_efficacy <- protocolSection_WhyStopped_llm %>% filter(ensemble %in% c("efficacy", "safety"))
write_xlsx(protocolSection_WhyStopped_llm_safety_efficacy, "results/ClinicalTrials/protocolSection_WhyStopped_llm_reviewed_safety_efficacy.xlsx")



#### WhyStopped - LLM classification evaluation  ----

# all records predicted as safety were manually annotated (n = 131)

safety <- protocolSection_WhyStopped_llm %>% filter(ensemble == "safety")

# enriched sample of records predicted as efficacy (n = 120) and a random sample of records predicted as nonclinical (n = 200)

efficacy <- protocolSection_WhyStopped_llm %>% filter(ensemble == "efficacy")
set.seed(123)
efficacy <- efficacy[sample.int(n = nrow(efficacy), size = 120, replace = FALSE),]

nonclinical <- protocolSection_WhyStopped_llm %>% filter(ensemble == "nonclinical")
set.seed(123)
nonclinical <- nonclinical[sample.int(n = nrow(nonclinical), size = 200, replace = FALSE),]

protocolSection_WhyStopped_lmm_eval <- bind_rows(safety, efficacy, nonclinical) %>% arrange(nctId)
protocolSection_WhyStopped_lmm_eval <- protocolSection_WhyStopped_lmm_eval %>% select(nctId, WhyStopped)
#write_xlsx(protocolSection_WhyStopped_lmm_eval, "results/ClinicalTrials/protocolSection_WhyStopped_llm_eval.xlsx")

protocolSection_WhyStopped_lmm_eval <- read_xlsx("results/ClinicalTrials/protocolSection_WhyStopped_llm_eval_260217.xlsx")

protocolSection_WhyStopped_lmm_eval <- protocolSection_WhyStopped_lmm_eval %>% left_join(protocolSection_WhyStopped_llm %>% select(nctId, qwen3_14b, deepseek_r1_8b, phi4, ensemble), by = "nctId")

protocolSection_WhyStopped_lmm_eval <- protocolSection_WhyStopped_lmm_eval %>% mutate(manual_eval = as.factor(manual_eval),
                                                                                        qwen3_14b = as.factor(qwen3_14b),
                                                                                        deepseek_r1_8b = as.factor(deepseek_r1_8b),
                                                                                        phi4 = as.factor(phi4),
                                                                                        ensemble = as.factor(ensemble))

WhyStopped_eval_metrics <- retrieve_metrics(
  data = protocolSection_WhyStopped_lmm_eval,
  reference = "manual_eval",
  models = c("qwen3_14b", "deepseek_r1_8b", "phi4", "ensemble")
)
write_xlsx(WhyStopped_eval_metrics, "results/ClinicalTrials/WhyStopped_eval_metrics.xlsx")

WhyStopped_conf_matrix <- yardstick::conf_mat(protocolSection_WhyStopped_lmm_eval, truth = "manual_eval", estimate = "ensemble") %>% autoplot(type = "heatmap") + # Use tiles for the heatmap cells
  scale_fill_gradient(low = "white", high = "slateblue") + labs(title = "Evaluation of LLM performance for WhyStopped labels")
WhyStopped_conf_matrix
save(WhyStopped_conf_matrix, file = "results/ClinicalTrials/WhyStopped_conf_matrix.RData")


# 3. Create file with final list of studies ----

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

write_xlsx(NCT_df, "results/Approved_NonApproved_FDA_studies.xlsx")







