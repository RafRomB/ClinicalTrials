library(httr2)
library(tidyverse)
library(jsonlite)
library(rollama)


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

# oncology_cond <- paste(
#   c(
#     "cancer","neoplasm","tumor","carcinoma","malignancy","\"solid tumor\"",
#     "leukemia","lymphoma","myeloma", "neuroblastoma"
#   ),
#   collapse = " OR "
# )
# 
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

clintrials <- clin_trials_response(url = clin_trials_url, 
                                   cond = "(cancer OR tumor OR tumour OR malignant OR carcinoma 
                                   OR sarcoma OR lymphoma OR leukemia OR leukaemia OR myeloma 
                                   OR blastoma OR neuroblastoma OR myelodysplastic)", 
                                   titles = combo_titles, 
                                   advFilter = NULL,
                                   aggFilters = "phase:1 2 3 4",
                                   overallStatus = "NOT_YET_RECRUITING,RECRUITING,ENROLLING_BY_INVITATION,ACTIVE_NOT_RECRUITING,SUSPENDED,COMPLETED,TERMINATED,WITHDRAWN,UNKNOWN",
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

combination_approvals <- openxlsx2::read_xlsx("results/FDA/approval_notifications_combinations_final.xlsx")
(length(unique(combination_approvals$nct)))

# Filter approved studies for drug combinations based on NCT

combination_approvals$clinicaltrialgov <- combination_approvals$nct %in% studies_id$study_id

table(combination_approvals$clinicaltrialgov)

no_study <- combination_approvals %>% filter(clinicaltrialgov == FALSE)

combination_approvals <- combination_approvals %>% filter(clinicaltrialgov == TRUE)

length(unique(combination_approvals$nct))

## 1.2 Phase 2, 3, 4 ----


clintrials <- clin_trials_response(url = clin_trials_url, 
                                   cond = "(cancer OR tumor OR tumour OR malignant OR carcinoma 
                                   OR sarcoma OR lymphoma OR leukemia OR leukaemia OR myeloma 
                                   OR blastoma OR neuroblastoma OR myelodysplastic)", 
                                   titles = combo_titles, 
                                   advFilter = NULL,
                                   aggFilters = "phase:2 3 4",
                                   overallStatus = "NOT_YET_RECRUITING,RECRUITING,ENROLLING_BY_INVITATION,ACTIVE_NOT_RECRUITING,SUSPENDED,COMPLETED,TERMINATED,WITHDRAWN,UNKNOWN",
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


# Filter approved studies for drug combinations based on NCT

combination_approvals$clinicaltrialgov <- combination_approvals$nct %in% studies_id$study_id

table(combination_approvals$clinicaltrialgov)

no_study <- combination_approvals %>% filter(clinicaltrialgov == FALSE)

combination_approvals <- combination_approvals %>% filter(clinicaltrialgov == TRUE)
length(unique(combination_approvals$nct))


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
save(clintrials, file = "results/ClinicalTrials/clintrials.Rdata")
load("results/ClinicalTrials/clintrials.Rdata")

# Filter approved studies for drug combinations based on NCT
combination_approvals <- openxlsx2::read_xlsx("results/FDA/approval_notifications_combinations_final.xlsx")

combination_approvals$clinicaltrialgov <- combination_approvals$nct %in% studies_id$study_id

table(combination_approvals$clinicaltrialgov)

no_study <- combination_approvals %>% filter(clinicaltrialgov == FALSE)

combination_approvals <- combination_approvals %>% filter(clinicaltrialgov == TRUE)
length(unique(combination_approvals$nct))

## 1.4 WITH RESULTS ----
# 
# clintrials <- clin_trials_response(url = clin_trials_url, 
#                                    cond = "(cancer OR tumor OR tumour OR malignant OR carcinoma 
#                                    OR sarcoma OR lymphoma OR leukemia OR leukaemia OR myeloma 
#                                    OR blastoma OR neuroblastoma OR myelodysplastic)", 
#                                    titles = combo_titles, 
#                                    advFilter = NULL,
#                                    aggFilters = "phase:2 3 4,results:with",
#                                    overallStatus = "ACTIVE_NOT_RECRUITING,SUSPENDED,COMPLETED,TERMINATED,WITHDRAWN,UNKNOWN",
#                                    pageSize = 1000)
# 
# 
# # Save studies NIH Clinical Trials IDs
# studies_id <- clintrials %>%
#   map_dfr(\(x) {
#     tibble(
#       study_id = x %>% pluck("protocolSection", "identificationModule", "nctId")
#     )
#   })
# 
# 
# # Name the studies with the IDs
# names(clintrials) <- studies_id$study_id
# 
# 
# # Filter approved studies for drug combinations based on NCT
# 
# combination_approvals$clinicaltrialgov <- combination_approvals$nct %in% studies_id$study_id
# 
# table(combination_approvals$clinicaltrialgov)
# 
# no_study <- combination_approvals %>% filter(clinicaltrialgov == FALSE)
# 
# combination_approvals <- combination_approvals %>% filter(clinicaltrialgov == TRUE)
# length(unique(combination_approvals$nct))


## 1.5 LLM Classification ----


ping_ollama()


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

set.seed(123)
nct_sample <- studies_id$study_id[runif(n = 100, min = 1, max = length(studies_id$study_id))]


sample_protocolSection <- clintrials[nct_sample] %>%
  map(protocolSection) %>%
  bind_rows()
sample_protocolSection <- sample_protocolSection %>% select(nctId, BriefTitle, BriefSummary, ArmGroupDescription)
#openxlsx2::write_xlsx(sample_protocolSection, "results/ClinicalTrials/sample_protocolSection.xlsx")
sample_protocolSection <- openxlsx2::read_xlsx("results/ClinicalTrials/sample_protocolSection.xlsx")



## Ollama LLM Filtering ----

sample_protocolSection$query_text <- str_squish(
  paste0(
    "BRIEF TITLE:\n",
    sample_protocolSection$BriefTitle,
    "\n\n",
    "BRIEF SUMMARY:\n",
    sample_protocolSection$BriefSummary,
    "\n ARM GROUP DESCRIPTION:\n",
    sample_protocolSection$ArmGroupDescription,
    "\n"
  ))


examples_fs <- tibble::tribble(
  ~text, ~answer,
  paste0("BRIEF TITLE: Study of Drug A Plus Drug B in Advanced NSCLC
BRIEF SUMMARY: Evaluate safety and efficacy of Drug A in combination with Drug B versus Drug B alone.
ARM GROUP DESCRIPTION: Drug A + Drug B;Drug B alone"),
  "combination",
  paste0("BRIEF TITLE: Study of Drug C Versus Placebo in Metastatic Cancer
BRIEF SUMMARY: Randomized placebo-controlled trial evaluating Drug C. Some participants receive placebo.
ARM GROUP DESCRIPTION: Drug C;Placebo"),
  "single",
  paste0("BRIEF TITLE: Drug D With FOLFOX in Colorectal Cancer
BRIEF SUMMARY: Drug D combined with standard chemotherapy regimen FOLFOX.
ARM GROUP DESCRIPTION: Drug D + FOLFOX"), 
  "combination",
  paste0("BRIEF TITLE: B-CAP Versus Single-Agent Brentuximab Vedotin in Hodgkin Lymphoma
BRIEF SUMMARY: Compare multi-agent chemotherapy regimen with single-agent therapy.
ARM GROUP DESCRIPTION: B-CAP (brentuximab vedotin, cyclophosphamide, doxorubicin, prednisolone);Brentuximab vedotin alone"),
  "combination",
  paste0("BRIEF TITLE: Dose Escalation of Drug E in Solid Tumors
BRIEF SUMMARY: Phase 1 dose escalation of Drug E at multiple dose levels and schedules.
ARM GROUP DESCRIPTION: Drug E low dose;Drug E medium dose;Drug E high dose"),
  "single",
  paste0("BRIEF TITLE: Drug F With Radiotherapy in Head and Neck Cancer
BRIEF SUMMARY: Evaluate Drug F administered with radiotherapy.
ARM GROUP DESCRIPTION: Drug F + radiotherapy;Radiotherapy alone"),
  "single",
  paste0("BRIEF TITLE: Immunotherapy Plus Chemotherapy in Breast Cancer
BRIEF SUMMARY: Evaluate Drug G (anti-PD-1) plus paclitaxel.
ARM GROUP DESCRIPTION: Drug G + paclitaxel;Paclitaxel"),
  "combination",
  paste0("BRIEF TITLE: Study of Drug H (Subcutaneous Formulation) in Lymphoma
BRIEF SUMMARY: Evaluate Drug H subcutaneous vs intravenous formulations.
ARM GROUP DESCRIPTION: Drug H SC;Drug H IV"),
  "single"
)


classify_single_model_robust <- function(text_vec,
                                         model,
                                         batch_size = 32,
                                         examples_fs = NULL) {
  n <- length(text_vec)
  idx_list <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
  
  # Pre-allocate with NA so we can see failed/unfinished rows
  labels <- rep(NA_character_, n)
  
  for (idx in idx_list) {
    cat("Model", model, "- processing indices", min(idx), "-", max(idx), "...\n")
    
    q <- make_query(
      text     = text_vec[idx],
      template = "{text}\n{prompt}",
      prompt   = paste(
        "Classify this study as 'combination' or 'single' using the rules.",
        "Return exactly one word: combination or single.",
        sep = "\n"
      ),
      system = paste(
        "You are a biomedical clinical trial text classifier.",
        "Task: Given a clinicaltrials.gov study text composed of BRIEF TITLE, BRIEF SUMMARY, and ARM GROUP DESCRIPTION,",
        "classify the study as either:",
        "- 'combination' if ANY study arm includes two or more distinct systemic anticancer drugs as treatment, or a named multi-drug regimen.",
        "- 'single' if ALL study arms use only ONE systemic anticancer drug (even if compared to placebo, best supportive care, surgery, or radiotherapy).",
        "",
        "How to decide:",
        "1) Look primarily at ARM GROUP DESCRIPTION. If missing/NA, use title + summary.",
        "2) Identify systemic anticancer agents in each arm (e.g., chemo drugs, immunotherapy, targeted therapy, ADCs).",
        "3) If any arm contains >=2 distinct anticancer agents, output 'combination'.",
        "",
        "Counts as COMBINATION:",
        "- Explicit multi-drug arms using connectors like '+', 'plus', 'with', 'in combination with'.",
        "- Named multi-agent regimens (e.g., FOLFOX, FOLFIRI, FLOT, CHOP, R-CHOP, ABVD, ICE, BEACOPP).",
        "- Arms where a regimen name is followed by multiple drugs in parentheses (comma-separated).",
        "- 'Drug A + chemo' or 'Drug A + Drug B' (even if the comparator is single-agent).",
        "",
        "Counts as SINGLE (do NOT upgrade to combination):",
        "- Drug vs placebo / best supportive care.",
        "- Multiple DOSES, schedules, formulations, or routes of the same drug.",
        "- Mention of biomarkers, diagnostic tests, imaging, companion diagnostics.",
        "- Surgery and/or radiotherapy added WITHOUT a second systemic anticancer drug.",
        "- Non-anticancer supportive meds (antiemetics, analgesics, antibiotics, anticoagulants, growth factors).",
        "",
        "Edge rule:",
        "- If one arm is combination and another is single-agent, label the STUDY as 'combination'.",
        "",
        "Output format:",
        "- Output exactly ONE word, lowercase: combination OR single.",
        "Do not output any explanations.",
        sep = "\n"
      ),
      examples = examples_fs
    )
    
    res <- tryCatch(
      {
        query(q, model = model, screen = FALSE, output = "text")
      },
      error = function(e) {
        message("HTTP/rollama error for model ", model,
                " indices ", min(idx), "-", max(idx), ": ", e$message)
        NULL
      }
    )
    
    if (!is.null(res)) {
      clean <- res %>%
        tolower() %>%
        trimws() #%>%
        # ensure we keep only the keyword if model adds text
        # str_extract("\\b(single|combination)\\b")
      
      labels[idx] <- ifelse(is.na(clean), "unknown", clean)
    } else {
      # leave labels[idx] as NA so you can detect failures
      labels[idx] <- NA_character_
    }
  }
  
  labels
}

models <- c("qwen3:8b", "deepseek-r1:8b", "phi4")

## LLM Performance ----

llm_results <- lapply(models, function(m) {
  classify_single_model_robust(
    text_vec    = sample_protocolSection$query_text,
    model       = m,
    batch_size  = 32,       
    examples_fs = examples_fs
  )
})

names(llm_results) <- models

# Bind back to the data frame
for (m in models) {
  sample_protocolSection[[paste0(gsub("[:\\-]", "_", m))]] <- llm_results[[m]]
}

llm_results <- sample_protocolSection %>% select(qwen3_8b, deepseek_r1_8b, phi4)

# Majority of votes

sample_protocolSection$ensemble <- apply(llm_results, 1, function(x) {
  prop <- table(x)
  names(prop)[which.max(prop)]
})

#openxlsx2::write_xlsx(sample_protocolSection, "results/ClinicalTrials/sample_protocolSection_llm.xlsx")
sample_protocolSection_lmm <- openxlsx2::read_xlsx("results/ClinicalTrials/sample_protocolSection_llm.xlsx")

sample_protocolSection_curated <- openxlsx2::read_xlsx("results/ClinicalTrials/sample_protocolSection_manual_curated.xlsx") %>% select(manual_eval)

sample_protocolSection_lmm$manual_eval <- sample_protocolSection_curated$manual_eval


sample_protocolSection_lmm[c("qwen3_8b",
                              "deepseek_r1_8b",
                              "phi4",
                              "ensemble",
                              "manual_eval")] <- lapply(sample_protocolSection_lmm[c("qwen3_8b",
                                                                                      "deepseek_r1_8b",
                                                                                      "phi4",
                                                                                      "ensemble",
                                                                                      "manual_eval")], function(x) {
                                                                                        x <- as.character(x)
                                                                                        x[!x %in% c("single", "combination")] <- "single" # Replace NAs or NULL with single
                                                                                        factor(x, levels = c("single", "combination"))
                                                                                      })


retrieve_metrics <- function(data, reference, models) {
  
  metrics <- tibble()
  
  for (m in models){
    
    model <- m
    
    df <- tibble(model = m, 
                 accuracy = yardstick::accuracy(data, reference, m)$.estimate, 
                 sensitivity = yardstick::sens(data, reference, m)$.estimate,
                 specificity = yardstick::spec(data, reference, m)$.estimate,
                 precision = yardstick::precision(data, reference, m)$.estimate,
                 recall = yardstick::recall(data, reference, m)$.estimate,
                 f1 = yardstick::f_meas(data, reference, m)$.estimate,
                 mcc = yardstick::mcc(data, reference, m)$.estimate)
    
    metrics <- bind_rows(metrics, df)
    
  }
  
  return(metrics)
}


retrieve_metrics(data = sample_protocolSection_lmm, reference = "manual_eval", models = c("qwen3_8b", "deepseek_r1_8b", "phi4", "ensemble"))

## LLM Classification of Studies ----

protocolSection_251230 <- clintrials %>%
  map(protocolSection) %>%
  bind_rows()
protocolSection_251230 <- protocolSection_251230 %>% select(nctId, BriefTitle, BriefSummary, ArmGroupDescription)
#openxlsx2::write_xlsx(protocolSection_251230, "results/ClinicalTrials/protocolSection_251230.xlsx")
protocolSection_251230 <- openxlsx2::read_xlsx("results/ClinicalTrials/protocolSection_251230.xlsx")

protocolSection_251230$query_text <- str_squish(
  paste0(
    "BRIEF TITLE:\n",
    protocolSection_251230$BriefTitle,
    "\n\n",
    "BRIEF SUMMARY:\n",
    protocolSection_251230$BriefSummary,
    "\n ARM GROUP DESCRIPTION:\n",
    protocolSection_251230$ArmGroupDescription,
    "\n"
  ))

models <- c("qwen3:8b", "deepseek-r1:8b", "phi4")

llm_results <- lapply(models, function(m) {
  classify_single_model_robust(
    text_vec    = protocolSection_251230$query_text,
    model       = m,
    batch_size  = 50,       
    examples_fs = examples_fs
  )
})

names(llm_results) <- models

# Bind back to the data frame
for (m in models) {
  protocolSection_251230[[paste0(gsub("[:\\-]", "_", m))]] <- llm_results[[m]]
}

openxlsx2::write_xlsx(protocolSection_251230, "results/ClinicalTrials/protocolSection_251230_llm.xlsx")

llm_results <- sample_protocolSection %>% select(qwen3_8b, deepseek_r1_8b, phi4)

# Majority of votes

sample_protocolSection$ensemble <- apply(llm_results, 1, function(x) {
  prop <- table(x)
  names(prop)[which.max(prop)]
})




# ChEMBL ----


## Function to extract drug information

extract_drugs_from_study <- function(study) {
  nct_id <- study$protocolSection$identificationModule$nctId %||% NA_character_
  
  interventions <- study$protocolSection$armsInterventionsModule$interventions
  
  # If no interventions, skip this study
  if (is.null(interventions)) return(NULL)
  
  # Build a tibble with one row per DRUG intervention
  map_dfr(interventions, function(intv) {
    
    tibble(
      nct_id = nct_id,
      type   = intv$type %||% NA_character_,
      name   = intv$name %||% NA_character_,
      otherNames = if (!is.null(intv$otherNames)) {
        paste(unlist(intv$otherNames), collapse = ", ")
      } else {
        NA_character_
      },
      armGroupLabels = if (!is.null(intv$armGroupLabels)) {
        paste(unlist(intv$armGroupLabels), collapse = ", ")
      } else {
        NA_character_
      }
    )
  })
}

drug_table <- map_dfr(clintrials_comb_app, extract_drugs_from_study)

## Retrieve drugs InChI Keys using ChEMBL ----

chembl_molecule_url <- "https://www.ebi.ac.uk/chembl/api/data/molecule.json"

#small helper to query ChEMBL for a single drug name

chembl_query_molecule <- function(drug_name, limit = 20) {
  req <- request(chembl_molecule_url) |>
    req_url_query(
      pref_name__iexact = drug_name,  # case-insensitive exact match on preferred name
      limit = limit
    )
  
  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = TRUE)
}


# Extract InChI key from the response

extract_inchikeys_from_result <- function(drug_name, result) {
  mols <- result$molecules
  
  ## Checking for response
  
  if (is.null(mols) || NROW(mols) == 0) {
    return(tibble::tibble(
      query              = drug_name,
      molecule_chembl_id = NA_character_,
      pref_name          = NA_character_,
      standard_inchi_key = NA_character_
    ))
  }
  
  mols_tbl <- tibble::as_tibble(mols)
  
  if(length(mols_tbl$molecule_structures) == 1 && is.na(mols_tbl$molecule_structures)) {
    mols_tbl %>%
      dplyr::mutate(
        query = drug_name,
        # molecule_structures is itself a data.frame; just grab its column
        standard_inchi_key = NA_character_
      ) %>%
      dplyr::select(query, molecule_chembl_id, pref_name, standard_inchi_key)
  } else {
    mols_tbl %>%
      dplyr::mutate(
        query = drug_name,
        # molecule_structures is itself a data.frame; just grab its column
        standard_inchi_key = molecule_structures$standard_inchi_key
      ) %>%
      dplyr::select(query, molecule_chembl_id, pref_name, standard_inchi_key) 
  }
}


chembl_inchikey_from_name <- function(drug_name,
                                      limit = 20,
                                      first_hit_only = TRUE) {
  res  <- chembl_query_molecule(drug_name, limit = limit)
  df   <- extract_inchikeys_from_result(drug_name, res)
  
  if (first_hit_only) {
    df[1, , drop = FALSE]
  } else {
    df
  }
}

chembl_inchikey_from_name("imatinib")


inchikey_df <- map(unique(drug_table$name), chembl_inchikey_from_name, first_hit_only = FALSE) %>% list_rbind()

## Left joint with drug table

drug_table <- left_join(drug_table, inchikey_df, by = c("name" = "query"))

## Left joint with clinical trials table

combination_approvals_filtered <- left_join(combination_approvals_filtered, drug_table, by = c("nct" = "nct_id"))

#openxlsx2::write_xlsx(combination_approvals_filtered, "results/FDA/approval_notifications_combinations_1stdraft_InChIKeys.xlsx")


## Clean table with only entries with InChI Keys

combination_approvals_filtered <- openxlsx2::read_xlsx("results/FDA/approval_notifications_combinations_1stdraft_InChIKeys.xlsx")


combination_approvals_filtered <- combination_approvals_filtered %>% filter(!is.na(standard_inchi_key))

#openxlsx2::write_xlsx(combination_approvals_filtered, "results/FDA/approval_notifications_combinations_1stdraft_InChIKeys_filtered.xlsx")



