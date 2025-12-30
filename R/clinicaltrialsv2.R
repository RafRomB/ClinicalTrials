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


## 1.3 ACTIVE_NOT_RECRUITING,SUSPENDED,COMPLETED,TERMINATED,WITHDRAWN,UNKNOWN ----


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


# Filter approved studies for drug combinations based on NCT

combination_approvals$clinicaltrialgov <- combination_approvals$nct %in% studies_id$study_id

table(combination_approvals$clinicaltrialgov)

no_study <- combination_approvals %>% filter(clinicaltrialgov == FALSE)

combination_approvals <- combination_approvals %>% filter(clinicaltrialgov == TRUE)
length(unique(combination_approvals$nct))

## 1.4 WITH RESULTS ----

clintrials <- clin_trials_response(url = clin_trials_url, 
                                   cond = "(cancer OR tumor OR tumour OR malignant OR carcinoma 
                                   OR sarcoma OR lymphoma OR leukemia OR leukaemia OR myeloma 
                                   OR blastoma OR neuroblastoma OR myelodysplastic)", 
                                   titles = combo_titles, 
                                   advFilter = NULL,
                                   aggFilters = "phase:2 3 4,results:with",
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


# Filter approved studies for drug combinations based on NCT

combination_approvals$clinicaltrialgov <- combination_approvals$nct %in% studies_id$study_id

table(combination_approvals$clinicaltrialgov)

no_study <- combination_approvals %>% filter(clinicaltrialgov == FALSE)

combination_approvals <- combination_approvals %>% filter(clinicaltrialgov == TRUE)
length(unique(combination_approvals$nct))











combination_approvals_filtered[duplicated(combination_approvals_filtered$nct), ]

table(combination_approvals_filtered[!duplicated(combination_approvals_filtered$nct),]$clinicaltrialgov)

clintrials_comb_app <- clintrials[names(clintrials) %in% combination_approvals_filtered$nct]

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


protSection <- clintrials[nct_sample] %>%
  map(protocolSection) %>%
  bind_rows()
#openxlsx2::write_xlsx(protSection, "results/ClinicalTrials/protocolSection_sample.xlsx")
protSection <- openxlsx2::read_xlsx("results/ClinicalTrials/protocolSection_sample.xlsx")

## Ollama LLM Filtering ----

llm_protSection <- tibble(classification_text = str_squish(
  paste0(
    "SUMMARY:\n",
    protSection$BriefSummary,
    "\n\n",
    "ARM GROUP LABLE:\n",
    protSection$ArmGroupLabel,
    "ARM GROUP DESCRIPTION:\n",
    protSection$ArmGroupDescription,
    "\n"
  )
))


examples_fs <- tibble::tribble(
  ~text, ~answer,
  paste0(
    "SUMMARY:\n",
    "The study proposes to evaluate the safety and efficacy of the combination of trastuzumab emtansine (T-DM1) and vinorelbine in HER2+ metastatic breast cancer patients.\n\n",
    "ARM GROUP LABEL:\n",
    "Phase 1: T-DM1 + Vinorelbine;Phase 2: T-DM1 + RP2D Vinorelbine\n\n",
    "ARM GROUP DESCRIPTION:\n",
    "One cycle of trastuzumab emtansine (T-DM1)/vinorelbine combination treatment is defined as 21 days. Vinorelbine and T-DM1 are given together as cancer treatment.\n"
  ),
  "combination",
  paste0(
    "SUMMARY:\n",
    "This study evaluates the safety and efficacy of PepCan versus placebo in subjects with head and neck cancers in remission.\n\n",
    "ARM GROUP LABEL:\n",
    "PepCan;Placebo\n\n",
    "ARM GROUP DESCRIPTION:\n",
    "Four injections of PepCan for a total of 7 injections, or four injections of placebo for a total of 7 injections.\n"
  ),
  "single"
)


qwen3_class <- make_query(
  text     = llm_protSection$classification_text,
  template = "TRIAL INFORMATION:\n{text}\n\nTASK:\n{prompt}",
  prompt   = "Categories: single, combination",
  system = paste(
    "You are a biomedical trial classifier.",
    "",
    "Your task:",
    "Decide whether each clinical study involves AT LEAST ONE ARM in which",
    "TWO OR MORE DRUGS are given together as part of the cancer treatment regimen.",
    "",
    "Definitions:",
    "- 'combination' = at least one arm uses TWO OR MORE DRUGS together",
    "  as active treatment for the disease.",
    "- 'single' = all arms use at most ONE DRUG at a time for active treatment,",
    "  even if there are multiple arms (e.g. drug vs placebo, or drug A vs drug B).",
    "",
    "Important rules:",
    "- Only count pharmacologically active DRUGS used to treat the disease.",
    "- Do NOT count:",
    "  - placebo, saline, vehicle, or sham treatments;",
    "  - non-drug interventions (exercise, behavioral, dietary supplement only, devices);",
    "- If ANY arm clearly treats patients with two or more drugs TOGETHER",
    "  (e.g. 'T-DM1 + Vinorelbine', 'cisplatin + etoposide'), classify as 'combination'.",
    "- If all arms are single-drug or non-drug, classify as 'single'.",
    "",
    "Output format:",
    "- Answer with EXACTLY ONE WORD: 'single' or 'combination'.",
    "- Do NOT explain your answer."
  ),
  examples = examples_fs
) %>%
  query(model = "qwen3:8b",
        screen = FALSE,
        output = "text") |>
  tolower() %>%
  trimws()


deepseek_class <- make_query(
  text     = llm_protSection$classification_text,
  template = "TRIAL INFORMATION:\n{text}\n\nTASK:\n{prompt}",
  prompt   = "Categories: single, combination",
  system = paste(
    "You are a biomedical trial classifier.",
    "",
    "Your task:",
    "Decide whether each clinical study involves AT LEAST ONE ARM in which",
    "TWO OR MORE DRUGS are given together as part of the cancer treatment regimen.",
    "",
    "Definitions:",
    "- 'combination' = at least one arm uses TWO OR MORE DRUGS together",
    "  as active treatment for the disease.",
    "- 'single' = all arms use at most ONE DRUG at a time for active treatment,",
    "  even if there are multiple arms (e.g. drug vs placebo, or drug A vs drug B).",
    "",
    "Important rules:",
    "- Only count pharmacologically active DRUGS used to treat the disease.",
    "- Do NOT count:",
    "  - placebo, saline, vehicle, or sham treatments;",
    "  - non-drug interventions (exercise, behavioral, dietary supplement only, devices);",
    "- If ANY arm clearly treats patients with two or more drugs TOGETHER",
    "  (e.g. 'T-DM1 + Vinorelbine', 'cisplatin + etoposide'), classify as 'combination'.",
    "- If all arms are single-drug or non-drug, classify as 'single'.",
    "",
    "Output format:",
    "- Answer with EXACTLY ONE WORD: 'single' or 'combination'.",
    "- Do NOT explain your answer."
  ),
  examples = examples_fs
) %>%
  query(model = "deepseek-r1:8b",
        screen = FALSE,
        output = "text") |>
  tolower() %>%
  trimws()



llm_clintrials <- protSection %>% select(nctId, BriefTitle, BriefSummary, DetailedDescription, ArmGroupLabel, ArmGroupDescription)
llm_clintrials$classification_text <- llm_protSection$classification_text
llm_clintrials$qwen3 <- qwen3_class
llm_clintrials$deepseek <- deepseek_class

openxlsx2::write_xlsx(llm_clintrials, "results/ClinicalTrials/protocolSection_llm.xlsx")

llm_clintrials_curated <- openxlsx2::read_xlsx("results/ClinicalTrials/protocolSection_llm_curated.xlsx", skip_empty_rows = T, skip_empty_cols = T)

prop.table(table(llm_clintrials_curated$qwen3 == llm_clintrials_curated$manual))
prop.table(table(llm_clintrials_curated$deepseek == llm_clintrials_curated$manual))

