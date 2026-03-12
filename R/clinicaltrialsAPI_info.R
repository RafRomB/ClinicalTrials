# 0. Load libraries ----

library(tidyverse)
library(openxlsx2)

# 1. Load data ----

load("results/ClinicalTrials/clintrials_260219.Rdata")

successful <- read_xlsx("results/FDA/approval_notifications_llm_results_combinations_final_260218.xlsx")
successful <- successful %>% pull(nct) %>% unique()

non_successful <- read_xlsx("results/ClinicalTrials/protocolSection_WhyStopped_llm_reviewed_safety_efficacy.xlsx")
non_successful <- non_successful %>% pull(nctId) %>% unique()

# Clinical trials dataframe

NCT_df <- bind_rows(
  tibble(
    NCT = successful,
    FDA_Approved = TRUE
  ),
  tibble(
    NCT = non_successful,
    FDA_Approved = FALSE
  )
)

# 2. Load functions ----

# Delimiters (collision-safe)

# Use non-printing ASCII control chars (very unlikely to appear in CT.gov text).
DELIMS <- list(
  # Generic levels
  L1 = "\u001E", # Record Separator  (outermost list: measures / groups / denoms)
  L2 = "\u001D", # Group Separator   (classes)
  L3 = "\u001F", # Unit Separator    (categories)
  L4 = "\u001A", # Substitute        (measurements / counts)
  
  # Specific lists
  DENOM = "\u001C",  # File Separator (denoms)
  NA_TOKEN = "<NA>"  # explicit placeholder when keep_na_levels = TRUE
)

#`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

is_collection <- function(x) {
  is.list(x) && (is.null(names(x)) || all(names(x) == ""))  # list-of-records
}

scalar_to_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.atomic(x) && length(x) == 1) return(as.character(x))
  if (is.atomic(x) && length(x) > 1)  return(paste(as.character(x), collapse = ","))
  NA_character_
}

collapse_chr <- function(x, sep, keep_na = FALSE, na_token = DELIMS$NA_TOKEN) {
  x <- as.character(x)
  if (!keep_na) x <- x[!is.na(x) & nzchar(x)]
  if (keep_na)  x[is.na(x) | !nzchar(x)] <- na_token
  if (length(x) == 0) NA_character_ else paste(x, collapse = sep)
}

# collapse_path(): general hierarchical collapse for nested list-of-records

# x     : list (often a collection, e.g., measures)
# path  : c("classes","categories","measurements","value")
# seps  : separators for each collection level encountered in order.
#         For measures->classes->categories->measurements: seps = c(L1,L2,L3,L4)
# keep_na_levels: if TRUE, keep explicit NA placeholders to preserve alignment.
collapse_path <- function(x, path, seps, keep_na_levels = TRUE, na_token = DELIMS$NA_TOKEN) {
  rec <- function(obj, path, sep_idx) {
    # If current node is a collection (list-of-records), collapse each element.
    if (is_collection(obj)) {
      if (length(obj) == 0) return(if (keep_na_levels) na_token else NA_character_)
      sep <- seps[[sep_idx]] %||% ";"
      parts <- map_chr(obj, ~ rec(.x, path, sep_idx + 1))
      return(collapse_chr(parts, sep = sep, keep_na = keep_na_levels, na_token = na_token))
    }
    
    # If no more path, return scalar representation.
    if (length(path) == 0) return(scalar_to_chr(obj))
    
    # If not a list or missing key, return NA/placeholder.
    if (!is.list(obj)) return(if (keep_na_levels) na_token else NA_character_)
    key <- path[[1]]
    child <- obj[[key]] %||% NULL
    if (is.null(child)) return(if (keep_na_levels) na_token else NA_character_)
    
    rec(child, path[-1], sep_idx)
  }
  
  rec(x, path, sep_idx = 1)
}

# # Convenience helpers (optional)
collapse_field <- function(x, field, sep, keep_na_levels = TRUE) {
  collapse_path(x, c(field), seps = list(sep), keep_na_levels = keep_na_levels)
}




# Function to collapse fields

# collapse_field <- function(x,
#                            field,
#                            inner_sep = "|",  # between multiple values inside one element
#                            outer_sep = ";") { # between different elements of x
# 
#   # If the whole thing is missing/empty
#   if (is.null(x) || length(x) == 0) {
#     return(NA_character_)
#   }
# 
#   vals <- vapply(
#     x,
#     function(el) {
#       if (is.null(el)) return(NA_character_)
# 
#       val <- el[[field]]
#       if (is.null(val)) return(NA_character_)
# 
#       # Flatten possible nested lists like interventionNames
#       if (is.list(val)) {
#         val <- unlist(val, use.names = FALSE)
#       }
# 
#       val <- as.character(val)
#       # Collapse multiple values within *one* element (e.g., multiple interventionNames)
#       paste(val, collapse = inner_sep)
#     },
#     character(1)
#   )
# 
#   # Remove all-NA / empty cases
#   vals <- vals[!is.na(vals) & vals != ""]
#   if (!length(vals)) return(NA_character_)
# 
#   # Collapse across elements (e.g., across armGroups)
#   paste(vals, collapse = outer_sep)
# }

# #`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
# 
# collapse_nested <- function(x,
#                             outer_field,   # e.g. "achievements"
#                             inner_field,   # e.g. "groupId"
#                             inner_sep = "|",
#                             outer_sep = ";") {
#   if (is.null(x) || length(x) == 0) return(NA_character_)
#   
#   vals <- map_chr(x, function(el) {
#     inner_list <- el[[outer_field]]
#     if (is.null(inner_list) || length(inner_list) == 0) return(NA_character_)
#     
#     inner_vals <- map_chr(inner_list, function(z) {
#       v <- z[[inner_field]]
#       if (is.null(v)) return(NA_character_)
#       if (is.list(v)) v <- unlist(v, use.names = FALSE)
#       as.character(v)
#     })
#     
#     inner_vals <- inner_vals[!is.na(inner_vals) & inner_vals != ""]
#     if (!length(inner_vals)) return(NA_character_)
#     paste(inner_vals, collapse = inner_sep)
#   })
#   
#   vals <- vals[!is.na(vals) & vals != ""]
#   if (!length(vals)) return(NA_character_)
#   paste(vals, collapse = outer_sep)
# }
# 
# 
# collapse_path <- function(x,
#                           path,                      # character vector, e.g. c("classes","categories","title")
#                           seps = c(";", "||", "|", ","), # sep per list depth: measures;classes;categories;measurements
#                           leaf_sep = tail(seps, 1),  # used if a leaf is a vector
#                           keep_na_levels = 1,        # keep NA placeholders at these collapse levels (e.g. measures)
#                           na_label = "NA",
#                           unique = FALSE) {
#   
#   stopifnot(is.character(path), length(path) >= 1)
#   
#   # 1) Walk down `path`, broadcasting over list-of-items when needed
#   walk <- function(obj, i = 1) {
#     if (i > length(path)) return(obj)
#     if (is.null(obj) || !is.list(obj)) return(NULL)
#     
#     key <- path[i]
#     has_names <- !is.null(names(obj)) && any(nzchar(names(obj)))
#     
#     if (has_names && key %in% names(obj)) {
#       return(walk(obj[[key]], i + 1))
#     }
#     
#     # not a named element here => treat as list-of-items and broadcast
#     lapply(obj, walk, i = i)
#   }
#   
#   # 2) Collapse the resulting tree with separators by depth
#   collapse_tree <- function(obj, level = 1) {
#     if (is.null(obj)) return(NA_character_)
#     
#     if (!is.list(obj)) {
#       v <- obj
#       if (is.list(v)) v <- unlist(v, use.names = FALSE)
#       v <- as.character(v)
#       v <- v[!is.na(v) & v != ""]
#       if (!length(v)) return(NA_character_)
#       return(paste(v, collapse = leaf_sep))
#     }
#     
#     vals <- vapply(obj, collapse_tree, character(1), level = level + 1)
#     if (unique) vals <- unique(vals)
#     
#     if (level %in% keep_na_levels) {
#       if (all(is.na(vals) | vals == "")) return(NA_character_)
#       vals[is.na(vals) | vals == ""] <- na_label
#     } else {
#       vals <- vals[!is.na(vals) & vals != ""]
#       if (!length(vals)) return(NA_character_)
#     }
#     
#     sep <- seps[min(level, length(seps))]
#     paste(vals, collapse = sep)
#   }
#   
#   collapse_tree(walk(x, 1), level = 1)
# }
# 
# 
# # collapse_vec <- function(x, sep = ";") {
# #   x <- x %||% character(0)
# #   if (!length(x)) return(NA_character_)
# #   paste(as.character(x), collapse = sep)
# # }

# Helper to convert dates

safe_date <- function(x) {
  x <- x %||% NA_character_
  if (is.na(x) || x == "") return(as.Date(NA))
  as.Date(lubridate::parse_date_time(x, orders = c("Y-m-d", "Y-m")))
}

# Helper to count elements

count_delimited_elements <- function(x, outer_sep, inner_sep) {
  # Split by outer separator
  groups <- str_split(x, outer_sep)[[1]]
  
  # Count inner separators + 1 for each group
  counts <- sapply(groups, function(g) {
    str_count(g, inner_sep) + 1
  })
  
  # Rejoin with outer separator
  result <- paste(counts, collapse = outer_sep)
  return(result)
}

# Protocol Section

protocolSection <- function(study, keep_na_levels = TRUE, delims = DELIMS) {
  
  # Retrieve information
  df <- tibble(
    # IdentificationModule ----
    NCT = study$protocolSection$identificationModule$nctId,
    NCTIdAlias = paste0(study$protocolSection$identificationModule$nctIdAliases, collapse = delims$L1),
    OrgStudyId = study$protocolSection$identificationModule$orgStudyIdInfo$id,
    OrgStudyIdType = study$protocolSection$identificationModule$orgStudyIdInfo$type,
    BriefTitle = study$protocolSection$identificationModule$briefTitle,
    OfficialTitle = study$protocolSection$identificationModule$officialTitle,
    Acronym = study$protocolSection$identificationModule$acronym,
    OrgFullName = study$protocolSection$identificationModule$organization$fullName,
    OrgClass = study$protocolSection$identificationModule$organization$class,
    
    # StatusModule ----
    StatusVerifiedDate = lubridate::ym(study$protocolSection$statusModule$statusVerifiedDate),
    OverallStatus = study$protocolSection$statusModule$overallStatus,
    WhyStopped = study$protocolSection$statusModule$whyStopped,
    HasExpandedAccess = study$protocolSection$statusModule$expandedAccessInfo$hasExpandedAccess,
    ExpandedAccessStatusForNCTId = study$protocolSection$statusModule$expandedAccessInfo$statusForNctId,
    StartDate = safe_date(study$protocolSection$statusModule$startDateStruct$date),
    StartDateType = study$protocolSection$statusModule$startDateStruct$type,
    PrimaryCompletionDate = safe_date(study$protocolSection$statusModule$primaryCompletionDateStruct$date),
    PrimaryCompletionDateType = study$protocolSection$statusModule$primaryCompletionDateStruct$type,
    CompletionDate = safe_date(study$protocolSection$statusModule$completionDateStruct$date),
    CompletionDateType = study$protocolSection$statusModule$completionDateStruct$type,
    StudyFirstPostDate = safe_date(study$protocolSection$statusModule$studyFirstPostDateStruct$date),
    StudyFirstPostDateType = study$protocolSection$statusModule$studyFirstPostDateStruct$type,
    ResultsWaived = study$protocolSection$statusModule$resultsWaived,
    ResultsFirstPostDate = safe_date(study$protocolSection$statusModule$resultsFirstPostDateStruct$date),
    DispFirstPostDate = safe_date(study$protocolSection$statusModule$dispFirstPostDateStruct$date),
    DispFirstPostDateType = study$protocolSection$statusModule$dispFirstPostDateStruct$type,
    LastUpdatePostDate = safe_date(study$protocolSection$statusModule$lastUpdatePostDateStruct$date),
    LastUpdatePostDateType = study$protocolSection$statusModule$lastUpdatePostDateStruct$type,
    
    # SponsorCollaboratorsModule ----
    ResponsiblePartyType = study$protocolSection$sponsorCollaboratorsModule$responsibleParty$type,
    LeadSponsorName = study$protocolSection$sponsorCollaboratorsModule$leadSponsor$name,
    LeadSponsorClass = study$protocolSection$sponsorCollaboratorsModule$leadSponsor$class,
    CollaboratorName = collapse_field(
      study$protocolSection$sponsorCollaboratorsModule$collaborators,
      "name", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    CollaboratorClass = collapse_field(
      study$protocolSection$sponsorCollaboratorsModule$collaborators,
      "class", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    NumCollaborators = length(
      study$protocolSection$sponsorCollaboratorsModule$collaborators
    ),
    #NumCollaboratorsPlusLead = study$protocolSection$sponsorCollaboratorsModule$numCollaboratorsPlusLead,
    
    # OversightModule ----
    OversightHasDMC = study$protocolSection$oversightModule$oversightHasDmc,
    IsFDARegulatedDrug = study$protocolSection$oversightModule$isFdaRegulatedDrug,
    IsFDARegulatedDevice = study$protocolSection$oversightModule$isFdaRegulatedDevice,
    IsUnapprovedDevice = study$protocolSection$oversightModule$isUnapprovedDevice,
    #IsPPSD = study$protocolSection$oversightModule$isPpsd,
    IsUSExport = study$protocolSection$oversightModule$isUsExport,
    FDAAA801Violation = study$protocolSection$oversightModule$fdaaa801Violation,
    
    # DescriptionModule ----
    
    BriefSummary = study$protocolSection$descriptionModule$briefSummary,
    DetailedDescription = study$protocolSection$descriptionModule$detailedDescription,
    
    # ConditionsModule ----
    
    Condition = paste0(
      study$protocolSection$conditionsModule$conditions,
      collapse = delims$L1
    ),
    NumConditions = length(study$protocolSection$conditionsModule$conditions),
    Keyword = paste0(
      study$protocolSection$conditionsModule$keywords,
      collapse = delims$L1
    ),
    
    # DesignModule ----
    StudyType = study$protocolSection$designModule$studyType,
    NPtrsToThisExpAccNCTId = study$protocolSection$designModule$nPtrsToThisExpAccNctId,
    ExpAccTypeIndividual = study$protocolSection$designModule$expandedAccessTypes$individual,
    ExpAccTypeIntermediate = study$protocolSection$designModule$expandedAccessTypes$intermediate,
    ExpAccTypeTreatment = study$protocolSection$designModule$expandedAccessTypes$treatment,
    PatientRegistry = study$protocolSection$designModule$patientRegistry,
    TargetDuration = study$protocolSection$designModule$targetDuration,
    Phase = paste0(study$protocolSection$designModule$phases, collapse = delims$L1),
    
    NumPhases = length(study$protocolSection$designModule$phases),
    
    DesignAllocation = study$protocolSection$designModule$designInfo$allocation,
    DesignInterventionModel = study$protocolSection$designModule$designInfo$interventionModel,
    DesignInterventionModelDescription = study$protocolSection$designModule$designInfo$interventionModelDescription,
    DesignPrimaryPurpose = study$protocolSection$designModule$designInfo$primaryPurpose,
    DesignObservationalModel = study$protocolSection$designModule$designInfo$observationalModel,
    DesignTimePerspective = study$protocolSection$designModule$designInfo$timePerspective,
    DesignMasking = study$protocolSection$designModule$designInfo$maskingInfo$masking,
    DesignMaskingDescription = study$protocolSection$designModule$designInfo$maskingInfo$maskingDescription,
    DesignWhoMasked = paste0(
      study$protocolSection$designModule$designInfo$maskingInfo$whoMasked,
      collapse = delims$L1
    ),
    BioSpecRetention = study$protocolSection$designModule$bioSpec$retention,
    BioSpecDescription = study$protocolSection$designModule$bioSpec$description,
    EnrollmentCount = study$protocolSection$designModule$enrollmentInfo$count,
    EnrollmentType = study$protocolSection$designModule$enrollmentInfo$type,
    ArmGroupLabel = collapse_field(
      study$protocolSection$armsInterventionsModule$armGroups,
      "label", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    ArmGroupType = collapse_field(
      study$protocolSection$armsInterventionsModule$armGroups,
      "type", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    ArmGroupDescription = collapse_field(
      study$protocolSection$armsInterventionsModule$armGroups,
      "description", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    ArmGroupInterventionName = collapse_path(
      study$protocolSection$armsInterventionsModule$armGroups,
      "interventionNames", seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels
    ),
    
    #NumArmGroupInterventionNames = study$protocolSection$armsInterventionsModule$armGroups$numArmGroupInterventionNames,
    NumArmGroups = length(study$protocolSection$armsInterventionsModule$armGroups),
    
    InterventionType = collapse_field(
      study$protocolSection$armsInterventionsModule$interventions,
      "type", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    InterventionName = collapse_field(
      study$protocolSection$armsInterventionsModule$interventions,
      "name", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    InterventionDescription = collapse_field(
      study$protocolSection$armsInterventionsModule$interventions,
      "description", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    InterventionArmGroupLabel = collapse_path(
      study$protocolSection$armsInterventionsModule$interventions,
      "armGroupLabels", seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels
    ),
    
    #NumInterventionArmGroupLabels = study$protocolSection$armsInterventionsModule$interventions$numInterventionArmGroupLabels,
    
    InterventionOtherName = collapse_path(
      study$protocolSection$armsInterventionsModule$interventions,
      "otherNames", sep = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels
    ),
    
    #NumInterventionOtherNames = study$protocolSection$armsInterventionsModule$interventions$numInterventionOtherNames,
    NumInterventions = length(
      study$protocolSection$armsInterventionsModule$interventions
    ),
    
    # OutcomesModule ----
    
    PrimaryOutcomeMeasure = collapse_field(
      study$protocolSection$outcomesModule$primaryOutcomes,
      "measure", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    PrimaryOutcomeDescription = collapse_field(
      study$protocolSection$outcomesModule$primaryOutcomes,
      "description", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    PrimaryOutcomeTimeFrame = collapse_field(
      study$protocolSection$outcomesModule$primaryOutcomes,
      "timeFrame", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    
    NumPrimaryOutcomes = length(study$protocolSection$outcomesModule$primaryOutcomes),
    
    SecondaryOutcomeMeasure = collapse_field(
      study$protocolSection$outcomesModule$secondaryOutcomes,
      "measure", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    SecondaryOutcomeDescription = collapse_field(
      study$protocolSection$outcomesModule$secondaryOutcomes,
      "description", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    SecondaryOutcomeTimeFrame = collapse_field(
      study$protocolSection$outcomesModule$secondaryOutcomes,
      "timeFrame", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    
    NumSecondaryOutcomes = length(study$protocolSection$outcomesModule$secondaryOutcomes),
    
    OtherOutcomeMeasure = collapse_field(
      study$protocolSection$outcomesModule$otherOutcomes,
      "measure", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    OtherOutcomeDescription = collapse_field(
      study$protocolSection$outcomesModule$otherOutcomes,
      "description", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    OtherOutcomeTimeFrame = collapse_field(
      study$protocolSection$outcomesModule$otherOutcomes,
      "timeFrame", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    
    NumOtherOutcomes = length(study$protocolSection$outcomesModule$otherOutcomes),
    #NumOutcomes = study$protocolSection$outcomesModule$numOutcomes,
    
    # EligibilityModule ----
    EligibilityCriteria = study$protocolSection$eligibilityModule$eligibilityCriteria,
    HealthyVolunteers = study$protocolSection$eligibilityModule$healthyVolunteers,
    Sex = study$protocolSection$eligibilityModule$sex,
    GenderBased = study$protocolSection$eligibilityModule$genderBased,
    GenderDescription = study$protocolSection$eligibilityModule$genderDescription,
    MinimumAge = study$protocolSection$eligibilityModule$minimumAge,
    MaximumAge = study$protocolSection$eligibilityModule$maximumAge,
    StdAge = paste0(
      study$protocolSection$eligibilityModule$stdAges,
      collapse = delims$L1
    ),
    
    NumStdAges = length(study$protocolSection$eligibilityModule$stdAges),
    
    StudyPopulation = study$protocolSection$eligibilityModule$studyPopulation,
    SamplingMethod = study$protocolSection$eligibilityModule$samplingMethod,
    
    # ContactsLocationsModule ----
    NumCentralContacts = length(
      study$protocolSection$contactsLocationsModule$centralContacts
    ),
    NumOverallOfficials = length(
      study$protocolSection$contactsLocationsModule$overallOfficials
    ),
    
    LocationCountry = collapse_field(
      study$protocolSection$contactsLocationsModule$locations,
      "country", sep = delims$L1, keep_na_levels = keep_na_levels
    ),
    
    #NumLocationContacts = study$protocolSection$contactsLocationsModule$locations$numLocationContacts,
    
    #LocationCountryCode = study$protocolSection$contactsLocationsModule$locations$countryCode,
    #LocationGeoPoint = study$protocolSection$contactsLocationsModule$locations$geoPoint,
    
    NumLocations = length(study$protocolSection$contactsLocationsModule$locations),
    #NumUniqueLocationCountries = study$protocolSection$contactsLocationsModule$numUniqueLocationCountries,
    
    # ReferencesModule ----
    ReferencePMID = collapse_field(study$protocolSection$referencesModule$references, "pmid", 
                                   sep = delims$L1, keep_na_levels = keep_na_levels),
    ReferenceType = collapse_field(study$protocolSection$referencesModule$references, "type", 
                                   sep = delims$L1, keep_na_levels = keep_na_levels),
    #RetractionPMID = collapse_field(study$protocolSection$referencesModule$references$retractions, "pmid"),
    #RetractionSource = collapse_field(study$protocolSection$referencesModule$references$retractions, "source"),
    
    #NumRetractionsForRef = study$protocolSection$referencesModule$references$numRetractionsForRef,
    #NumReferences = study$protocolSection$referencesModule$numReferences,
    #NumRetractionsAllRefs = study$protocolSection$referencesModule$numRetractionsAllRefs,
    
    #AvailIPDId = collapse_field(study$protocolSection$referencesModule$availIpds, "id"),
    AvailIPDType = collapse_field(study$protocolSection$referencesModule$availIpds, "type",
                                  sep = delims$L1, keep_na_levels = keep_na_levels),
    
    #NumAvailIPDs = study$protocolSection$referencesModule$numAvailIpDs
    
    # IPDSharingStatementModule ----
    IPDSharing = study$protocolSection$ipdSharingStatementModule$ipdSharing,
    IPDSharingDescription = study$protocolSection$ipdSharingStatementModule$description,
    IPDSharingInfoType = paste0(study$protocolSection$ipdSharingStatementModule$infoTypes, collapse = delims$L1),
    NumIPDSharingInfoTypes = length(study$protocolSection$ipdSharingStatementModule$infoTypes),
    
    # Optional: keep delimiter spec (so your later unnesting knows how to split)
    Delims = paste(names(delims), unlist(delims), sep = "=", collapse = ";")
  )
  
  # Add additional variables
  
  df <- df %>% mutate(NumOutcomes = sum(df$NumPrimaryOutcomes + 
                                          df$NumSecondaryOutcomes + 
                                          df$NumOtherOutcomes), .after = NumOtherOutcomes)
  
  df <- df %>% mutate(NumUniqueLocationCountries = lapply(df$LocationCountry %>% str_split(pattern = delims$L1), function(x) length(unique(x))) %>% unlist(),
                      .after = NumLocations)
  df$NumUniqueLocationCountries[is.na(df$LocationCountry)] <- NA
  df$NumLocations[is.na(df$LocationCountry)] <- NA
  
  return(df)
}


resultSection <- function(study, keep_na_levels = TRUE, delims = DELIMS) {
  
  nct <- purrr::pluck(study, "protocolSection", "identificationModule", "nctId",
                      .default = NA_character_)
  
  # pfm <- purrr::pluck(study, "resultsSection", "participantFlowModule",
  #                    .default = NA_character_)
  
  # bcm <- purrr::pluck(study, "resultsSection", "baselineCharacteristicsModule",
  #                     .default = NULL)
  # 
  # 
  # pfmGroups <- pfm$groups %||% list()
  # pfmPeriods <- pfm$periods %||% list()
  # 
  df <- tibble(
    # IdentificationModule ----
    NCT = nct,
    
    # ParticipantFlowModule ----
    FlowPreAssignmentDetails = study$resultsSection$participantFlowModule$preAssignmentDetails,
    FlowRecruitmentDetails = study$resultsSection$participantFlowModule$recruitmentDetails,
    FlowTypeUnitsAnalyzed = study$resultsSection$participantFlowModule$typeUnitsAnalyzed,
    FlowGroupId = collapse_field(study$resultsSection$participantFlowModule$groups, "id", sep = delims$L1, keep_na_levels = keep_na_levels),
    FlowGroupTitle = collapse_field(study$resultsSection$participantFlowModule$groups, "title", sep = delims$L1, keep_na_levels = keep_na_levels),
    FlowGroupDescription = collapse_field(study$resultsSection$participantFlowModule$groups, "description", sep = delims$L1, keep_na_levels = keep_na_levels),
    
    NumFlowGroups = length(study$resultsSection$participantFlowModule$groups),
    
    FlowPeriodTitle = collapse_field(study$resultsSection$participantFlowModule$periods, "title", sep = delims$L1, keep_na_levels = keep_na_levels),
    FlowMilestoneType = collapse_path(study$resultsSection$participantFlowModule$periods, path = c("milestones", "type"), 
                                      seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    FlowMilestoneComment = collapse_path(study$resultsSection$participantFlowModule$periods, path = c("milestones","comment"), 
                                         seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    FlowAchievementGroupId = collapse_path(study$resultsSection$participantFlowModule$periods, path = c("milestones", "achievements", "groupId"),
                                           seps = list(delims$L1, delims$L2, delims$L3), keep_na_levels = keep_na_levels),
    FlowAchievementComment = collapse_path(study$resultsSection$participantFlowModule$periods, path = c("milestones", "achievements", "comment"),
                                           seps = list(delims$L1, delims$L2, delims$DENOM), keep_na_levels = keep_na_levels),
    FlowAchievementNumSubjects = collapse_path(study$resultsSection$participantFlowModule$periods, c("milestones", "achievements", "numSubjects"),
                                               seps = list(delims$L1, delims$L2, delims$DENOM), keep_na_levels = keep_na_levels),
    FlowAchievementNumUnits = collapse_path(study$resultsSection$participantFlowModule$periods, path = c("milestones", "achievements", "numUnits"),
                                            seps = list(delims$L1, delims$L2, delims$DENOM), keep_na_levels = keep_na_levels),
    
    NumFlowMilestones = length(study$resultsSection$participantFlowModule$periods %>% pluck(1, "milestones")),
    
    FlowDropWithdrawType = collapse_path(study$resultsSection$participantFlowModule$periods, path =  c("dropWithdraws", "type"), 
                                         seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels), 
    FlowDropWithdrawComment = collapse_path(study$resultsSection$participantFlowModule$periods, path = c("dropWithdraws", "comment"),
                                            seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels), 
    FlowReasonGroupId = collapse_path(study$resultsSection$participantFlowModule$periods, path = c("dropWithdraws", "reasons", "groupId"),
                                      seps = list(delims$L1, delims$L2, delims$DENOM), keep_na_levels = keep_na_levels), 
    FlowReasonComment = collapse_path(study$resultsSection$participantFlowModule$periods, path = c("dropWithdraws", "reasons", "comment"),
                                      seps = list(delims$L1, delims$L2, delims$DENOM), keep_na_levels = keep_na_levels),
    FlowReasonNumSubjects = collapse_path(study$resultsSection$participantFlowModule$periods, path = c("dropWithdraws", "reasons", "numSubjects"),
                                          seps = list(delims$L1, delims$L2, delims$DENOM), keep_na_levels = keep_na_levels),
    
    NumFlowDropWithdraws = length(study$resultsSection$participantFlowModule$periods %>% pluck(1, "dropWithdraws")),
    NumFlowPeriods = length(study$resultsSection$participantFlowModule$periods),
    
    # BaselineCharacteristicsModule ----
    BaselinePopulationDescription = study$resultsSection$baselineCharacteristicsModule$populationDescription,
    BaselineTypeUnitsAnalyzed = study$resultsSection$baselineCharacteristicsModule$typeUnitsAnalyzed,
    BaselineGroupId = collapse_field(study$resultsSection$baselineCharacteristicsModule$groups, "id",
                                     sep = delims$L1, keep_na_levels = keep_na_levels),
    BaselineGroupTitle = collapse_field(study$resultsSection$baselineCharacteristicsModule$groups, "title",
                                        sep = delims$L1, keep_na_levels = keep_na_levels),
    BaselineGroupDescription = collapse_field(study$resultsSection$baselineCharacteristicsModule$groups, "description",
                                              sep = delims$L1, keep_na_levels = keep_na_levels),
    
    NumBaselineGroups = length(study$resultsSection$baselineCharacteristicsModule$groups),
    
    BaselineDenomUnits = collapse_field(study$resultsSection$baselineCharacteristicsModule$denoms, "units",
                                        sep = delims$DENOM, keep_na_levels = keep_na_levels),
    BaselineDenomCountGroupId = collapse_path(study$resultsSection$baselineCharacteristicsModule$denoms, path = c("counts", "groupId"), 
                                              seps = list(delims$DENOM, delims$L4), keep_na_levels = keep_na_levels),
    BaselineDenomCountValue = collapse_path(study$resultsSection$baselineCharacteristicsModule$denoms, path = c("counts", "value"),
                                            seps = list(delims$DENOM, delims$L4), keep_na_levels = keep_na_levels),
    
    NumBaselineDenoms = length(study$resultsSection$baselineCharacteristicsModule$denoms),
    
    BaselineMeasureTitle = collapse_field(study$resultsSection$baselineCharacteristicsModule$measures, "title",
                                          sep = delims$L1, keep_na_levels = keep_na_levels),
    BaselineMeasureDescription = collapse_field(study$resultsSection$baselineCharacteristicsModule$measures, "description",
                                                sep = delims$L1, keep_na_levels = keep_na_levels),
    BaselineMeasurePopulationDescription = collapse_field(study$resultsSection$baselineCharacteristicsModule$measures, "populationDescription",
                                                          sep = delims$L1, keep_na_levels = keep_na_levels),
    BaselineMeasureParamType = collapse_field(study$resultsSection$baselineCharacteristicsModule$measures, "paramType",
                                              sep = delims$L1, keep_na_levels = keep_na_levels),
    BaselineMeasureDispersionType = collapse_field(study$resultsSection$baselineCharacteristicsModule$measures, "dispersionType",
                                                   sep = delims$L1, keep_na_levels = keep_na_levels),
    BaselineMeasureUnitOfMeasure = collapse_field(study$resultsSection$baselineCharacteristicsModule$measures, "unitOfMeasure",
                                                  sep = delims$L1, keep_na_levels = keep_na_levels),
    BaselineMeasureCalculatePct = collapse_field(study$resultsSection$baselineCharacteristicsModule$measures, "calculatePct",
                                                 sep = delims$L1, keep_na_levels = keep_na_levels),
    BaselineMeasureDenomUnitsSelected = collapse_field(study$resultsSection$baselineCharacteristicsModule$measures, "denomUnitsSelected",
                                                       sep = delims$L1, keep_na_levels = keep_na_levels),
    BaselineMeasureDenomUnits = collapse_field(study$resultsSection$baselineCharacteristicsModule$measures$denoms, "units",
                                               sep = delims$L1, keep_na_levels = keep_na_levels),
    BaselineMeasureDenomCountGroupId = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures$denoms, path = c("counts", "groupId"),
                                                     seps = list(delims$DENOM, delims$L4), keep_na_levels = keep_na_levels),
    BaselineMeasureDenomCountValue = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures$denoms, path = c("counts", "value"),
                                                   seps = list(delims$DENOM, delims$L4), keep_na_levels = keep_na_levels),
    
    NumBaselineMeasureDenoms = length(study$resultsSection$baselineCharacteristicsModule$measures$denoms), ##
    
    BaselineClassTitle = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures, path = c("classes", "title"), 
                                       seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    BaselineClassDenomUnits = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures, path = c("classes", "denoms", "units"),
                                            seps = list(delims$L1, delims$L2, delims$DENOM)),
    BaselineClassDenomCountGroupId = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures, c("classes", "denoms", "counts", "groupId"),
                                                   seps = list(delims$L1, delims$L2, delims$DENOM, delims$L4), keep_na_levels = keep_na_levels),
    BaselineClassDenomCountValue = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures, path = c("classes", "denoms", "counts", "value"),
                                                  seps = list(delims$L1, delims$L2, delims$DENOM, delims$L4), keep_na_levels = keep_na_levels),
    BaselineCategoryTitle = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures, path = c("classes", "categories", "title"), 
                                          seps = list(delims$L1, delims$L2, delims$L3), keep_na_levels = keep_na_levels),
    BaselineMeasurementGroupId = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures, path = c("classes", "categories", "measurements","groupId"), 
                                               seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    BaselineMeasurementValue = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures, path = c("classes", "categories", "measurements", "value"), 
                                             seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    BaselineMeasurementSpread = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures, path = c("classes", "categories", "measurements", "spread"),
                                              seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    BaselineMeasurementLowerLimit = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures, path = c("classes", "categories", "measurements", "lowerLimit"),
                                                  seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    BaselineMeasurementUpperLimit = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures, path = c("classes","categories","measurements", "upperLimit"),
                                                  seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    BaselineMeasurementComment = collapse_path(study$resultsSection$baselineCharacteristicsModule$measures, path = c("classes","categories","measurements","comment"),
                                               seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    
    # OutcomeMeasuresModule ----
    OutcomeMeasureType = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "type",
                                        sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasureTitle = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "title",
                                         sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasureDescription = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "description",
                                               sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasurePopulationDescription = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "populationDescription",
                                                         sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasureReportingStatus = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "reportingStatus",
                                                   sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasureAnticipatedPostingDate = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "anticipatedPostingDate",
                                                          sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasureParamType = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "paramType",
                                             sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasureDispersionType = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "dispersionType",
                                                  sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasureUnitOfMeasure = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "unitOfMeasure",
                                                 sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasureCalculatePct = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "calculatePct",
                                                sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasureTimeFrame = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "timeFrame",
                                             sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasureTypeUnitsAnalyzed = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "typeUnitsAnalyzed",
                                                     sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeMeasureDenomUnitsSelected = collapse_field(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, "denomUnitsSelected",
                                                      sep = delims$L1, keep_na_levels = keep_na_levels),
    OutcomeGroupId = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("groups", "id"),
                                   seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeGroupTitle = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("groups", "title"),
                                      seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeGroupDescription = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("groups", "description"),
                                            seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    
    NumOutcomeGroups = length(study$resultsSection$outcomeMeasuresModule$outcomeMeasures), ##
    
    OutcomeDenomUnits = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("denoms", "units"),
                                      seps = list(delims$L1, delims$DENOM), keep_na_levels = keep_na_levels),
    OutcomeDenomCountGroupId = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, c("denoms", "counts", "groupId"),
                                             seps = list(delims$L1, delims$DENOM, delims$L4), keep_na_levels = keep_na_levels),
    OutcomeDenomCountValue = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("denoms", "counts", "value"),
                                           seps = list(delims$L1, delims$DENOM, delims$L4), keep_na_levels = keep_na_levels),
    
    OutcomeClassTitle = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("classes", "title"),
                                      seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeClassDenomUnits = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("classes", "denoms", "units"),
                                           seps = list(delims$L1, delims$L2, delims$DENOM), keep_na_levels = keep_na_levels),
    OutcomeClassDenomCountGroupId = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("classes", "denoms", "counts", "groupId"),
                                                  seps = list(delims$L1, delims$L2, delims$DENOM, delims$L4), keep_na_levels = keep_na_levels),
    OutcomeClassDenomCountValue = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("classes","denoms", "counts","value"),
                                                seps = list(delims$L1, delims$L2, delims$DENOM, delims$L4), keep_na_levels = keep_na_levels),
    OutcomeCategoryTitle = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("classes", "categories", "title"),
                                         seps = list(delims$L1, delims$L2, delims$L3), keep_na_levels = keep_na_levels),
    OutcomeMeasurementGroupId = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("classes", "categories", "measurements", "groupId"),
                                              seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    OutcomeMeasurementValue = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("classes", "categories", "measurements", "value"),
                                            seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    OutcomeMeasurementSpread = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("classes", "categories", "measurements", "spread"),
                                             seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    OutcomeMeasurementLowerLimit = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("classes","categories","measurements","lowerLimit"),
                                                 seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    OutcomeMeasurementUpperLimit = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("classes", "categories", "measurements", "upperLimit"),
                                                 seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    OutcomeMeasurementComment = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("classes","categories","measurements","comment"),
                                              seps = list(delims$L1, delims$L2, delims$L3, delims$L4), keep_na_levels = keep_na_levels),
    
    OutcomeAnalysisParamType = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "paramType"),
                                             seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisParamValue = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "paramValue"),
                                              seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisDispersionType = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path =c("analyses", "dispersionType"),
                                                  seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisDispersionValue = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "dispersionValue"),
                                                   seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisStatisticalMethod = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "statisticalMethod"),
                                                     seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisStatisticalComment = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses","statisticalComment"),
                                                      seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisPValue = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "pValue"),
                                          seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisPValueComment = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "pValueComment"),
                                                 seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisCINumSides = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "ciNumSides"),
                                              seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisCIPctValue = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, c("analyses", "ciPctValue"),
                                              seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisCILowerLimit = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, c("analyses", "ciLowerLimit"),
                                                seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisCIUpperLimit = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "ciUpperLimit"),
                                                seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisCILowerLimitComment = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, c("analyses", "ciLowerLimitComment"),
                                                       seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisCIUpperLimitComment = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, c("analyses", "ciUpperLimitComment"),
                                                       seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisEstimateComment = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, c("analyses", "estimateComment"),
                                                   seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisTestedNonInferiority = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, c("analyses", "testedNonInferiority"),
                                                        seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisNonInferiorityType = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "nonInferiorityType"),
                                                      seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisNonInferiorityComment = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "nonInferiorityComment"),
                                                         seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisOtherAnalysisDescription = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "otherAnalysisDescription"),
                                                            seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisGroupDescription = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "groupDescription"),
                                                    seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OutcomeAnalysisGroupId = collapse_path(study$resultsSection$outcomeMeasuresModule$outcomeMeasures, path = c("analyses", "groupIds"),
                                           seps = list(delims$L1, delims$L2, delims$L3), keep_na_levels = keep_na_levels),
    
    # AdverseEventsModule ----
    EventsFrequencyThreshold = study$resultsSection$adverseEventsModule$frequencyThreshold,
    EventsTimeFrame = study$resultsSection$adverseEventsModule$timeFrame,
    EventsDescription = study$resultsSection$adverseEventsModule$description,
    EventsAllCauseMortalityComment = study$resultsSection$adverseEventsModule$allCauseMortalityComment,
    EventGroupId = collapse_field(study$resultsSection$adverseEventsModule$eventGroups, "id",
                                  sep = delims$L1, keep_na_levels = keep_na_levels),
    EventGroupTitle = collapse_field(study$resultsSection$adverseEventsModule$eventGroups, "title",
                                     sep = delims$L1, keep_na_levels = keep_na_levels),
    EventGroupDescription = collapse_field(study$resultsSection$adverseEventsModule$eventGroups, "description",
                                           sep = delims$L1, keep_na_levels = keep_na_levels),
    EventGroupDeathsNumAffected = collapse_field(study$resultsSection$adverseEventsModule$eventGroups, "deathsNumAffected",
                                                 sep = delims$L1, keep_na_levels = keep_na_levels),
    EventGroupDeathsNumAtRisk = collapse_field(study$resultsSection$adverseEventsModule$eventGroups, "deathsNumAtRisk",
                                               sep = delims$L1, keep_na_levels = keep_na_levels),
    EventGroupSeriousNumAffected = collapse_field(study$resultsSection$adverseEventsModule$eventGroups, "seriousNumAffected",
                                                  sep = delims$L1, keep_na_levels = keep_na_levels),
    EventGroupSeriousNumAtRisk = collapse_field(study$resultsSection$adverseEventsModule$eventGroups, "seriousNumAtRisk",
                                                sep = delims$L1, keep_na_levels = keep_na_levels),
    EventGroupOtherNumAffected = collapse_field(study$resultsSection$adverseEventsModule$eventGroups, "otherNumAffected",
                                                sep = delims$L1, keep_na_levels = keep_na_levels),
    EventGroupOtherNumAtRisk = collapse_field(study$resultsSection$adverseEventsModule$eventGroups, "otherNumAtRisk",
                                              sep = delims$L1, keep_na_levels = keep_na_levels),
    
    SeriousEventTerm = collapse_field(study$resultsSection$adverseEventsModule$seriousEvents, "term",
                                      sep = delims$L1, keep_na_levels = keep_na_levels),
    SeriousEventOrganSystem = collapse_field(study$resultsSection$adverseEventsModule$seriousEvents, "organSystem",
                                             sep = delims$L1, keep_na_levels = keep_na_levels),
    SeriousEventSourceVocabulary = collapse_field(study$resultsSection$adverseEventsModule$seriousEvents, "sourceVocabulary",
                                                  sep = delims$L1, keep_na_levels = keep_na_levels),
    SeriousEventAssessmentType = collapse_field(study$resultsSection$adverseEventsModule$seriousEvents, "assessmentType",
                                                sep = delims$L1, keep_na_levels = keep_na_levels),
    SeriousEventNotes = collapse_field(study$resultsSection$adverseEventsModule$seriousEvents, "notes",
                                       sep = delims$L1, keep_na_levels = keep_na_levels),
    SeriousEventStatsGroupId = collapse_path(study$resultsSection$adverseEventsModule$seriousEvents, path = c("stats", "groupId"),
                                             seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    SeriousEventStatsNumEvents = collapse_path(study$resultsSection$adverseEventsModule$seriousEvents, path = c("stats", "numEvents"),
                                               seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    SeriousEventStatsNumAffected = collapse_path(study$resultsSection$adverseEventsModule$seriousEvents, path = c("stats", "numAffected"),
                                                 seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    SeriousEventStatsNumAtRisk = collapse_path(study$resultsSection$adverseEventsModule$seriousEvents, path = c("stats", "numAtRisk"),
                                               seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    
    OtherEventTerm = collapse_field(study$resultsSection$adverseEventsModule$otherEvents, "term",
                                    sep = delims$L1, keep_na_levels = keep_na_levels),
    OtherEventOrganSystem = collapse_field(study$resultsSection$adverseEventsModule$otherEvents, "organSystem",
                                           sep = delims$L1, keep_na_levels = keep_na_levels),
    OtherEventSourceVocabulary = collapse_field(study$resultsSection$adverseEventsModule$otherEvents, "sourceVocabulary",
                                                sep = delims$L1, keep_na_levels = keep_na_levels),
    OtherEventAssessmentType = collapse_field(study$resultsSection$adverseEventsModule$otherEvents, "assessmentType",
                                              sep = delims$L1, keep_na_levels = keep_na_levels),
    OtherEventNotes = collapse_field(study$resultsSection$adverseEventsModule$otherEvents, "notes",
                                     sep = delims$L1, keep_na_levels = keep_na_levels),
    OtherEventStatsGroupId = collapse_path(study$resultsSection$adverseEventsModule$otherEvents, path = c("stats", "groupId"),
                                           seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OtherEventStatsNumEvents = collapse_path(study$resultsSection$adverseEventsModule$otherEvents, path = c("stats", "numEvents"),
                                             seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OtherEventStatsNumAffected = collapse_path(study$resultsSection$adverseEventsModule$otherEvents, path = c("stats", "numAffected"),
                                               seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    OtherEventStatsNumAtRisk = collapse_path(study$resultsSection$adverseEventsModule$otherEvents, path = c("stats", "numAtRisk"),
                                             seps = list(delims$L1, delims$L2), keep_na_levels = keep_na_levels),
    
    # MoreInfoModule ----
    LimitationsAndCaveatsDescription = study$resultsSection$moreInfoModule$limitationsAndCaveats$description,
    AgreementPISponsorEmployee = study$resultsSection$moreInfoModule$certainAgreement$piSponsorEmployee,
    AgreementRestrictionType = study$resultsSection$moreInfoModule$certainAgreement$restrictionType,
    AgreementRestrictiveAgreement = study$resultsSection$moreInfoModule$certainAgreement$restrictiveAgreement,
    AgreementOtherDetails = study$resultsSection$moreInfoModule$certainAgreement$otherDetails,
    
    # Optional: keep delimiter spec (so your later unnesting knows how to split)
    Delims = paste(names(delims), unlist(delims), sep = "=", collapse = ";")
  )
  
  # Fix Num columns columns ----
  
  df <- df %>%
    mutate(across(starts_with("Num"), ~ na_if(., 0)))
  
  return(df)
}

hasResults <- function(study) {
  df <- tibble(
    NCT = study$protocolSection$identificationModule$nctId,
    hasResults = study$hasResults
  )
  return(df)
}


## Additional columns
# #NumFlowAchievements: Number of Arms/Groups (for each milestone)
# df <- df %>% dplyr::mutate(
#   NumFlowAchievements = count_delimited_elements(
#     df$FlowAchievementGroupId,
#     outer_sep = delims$L2,
#     inner_sep = delims$L3
#   ), .before = NumFlowMilestones
# )
# 
# #NumFlowReasons: number of arm/group in reason not completed
# df <- df %>% dplyr::mutate(
#   NumFlowReasons = count_delimited_elements(
#     df$FlowReasonGroupId,
#     outer_sep = delims$L2,
#     inner_sep = delims$DENOM
#   ), .after = FlowReasonNumSubjects
# )



# 3. Retrieve data ----

## 3.1 protocolSection ----

protocolSection_df <- clintrials %>%
  map(protocolSection) %>%
  bind_rows()

protocolColumns <- c("NCT
  NCTIdAlias
  OrgStudyId
  OrgStudyIdType
  BriefTitle
  OfficialTitle
  Acronym
  OrgFullName
  OrgClass
  StatusVerifiedDate
  OverallStatus
  WhyStopped
  HasExpandedAccess
  ExpandedAccessStatusForNCTId
  StartDate
  StartDateType
  PrimaryCompletionDate
  PrimaryCompletionDateType
  CompletionDate
  CompletionDateType
  StudyFirstPostDate
  StudyFirstPostDateType
  ResultsWaived
  ResultsFirstPostDate
  DispFirstPostDate
  DispFirstPostDateType
  LastUpdatePostDate
  LastUpdatePostDateType
  ResponsiblePartyType
  LeadSponsorName
  LeadSponsorClass
  CollaboratorName
  CollaboratorClass
  NumCollaborators
  OversightHasDMC
  IsFDARegulatedDrug
  IsFDARegulatedDevice
  IsUnapprovedDevice
  IsUSExport
  FDAAA801Violation
  BriefSummary
  DetailedDescription
  Condition
  NumConditions
  Keyword
  StudyType
  NPtrsToThisExpAccNCTId
  ExpAccTypeIndividual
  ExpAccTypeIntermediate
  ExpAccTypeTreatment
  PatientRegistry
  TargetDuration
  Phase
  NumPhases
  DesignAllocation
  DesignInterventionModel
  DesignInterventionModelDescription
  DesignPrimaryPurpose
  DesignObservationalModel
  DesignTimePerspective
  DesignMasking
  DesignMaskingDescription
  DesignWhoMasked
  BioSpecRetention
  BioSpecDescription
  EnrollmentCount
  EnrollmentType
  ArmGroupLabel
  ArmGroupType
  ArmGroupDescription
  ArmGroupInterventionName
  NumArmGroups
  InterventionType
  InterventionName
  InterventionDescription
  InterventionArmGroupLabel
  InterventionOtherName
  NumInterventions
  PrimaryOutcomeMeasure
  PrimaryOutcomeDescription
  PrimaryOutcomeTimeFrame
  NumPrimaryOutcomes
  SecondaryOutcomeMeasure
  SecondaryOutcomeDescription
  SecondaryOutcomeTimeFrame
  NumSecondaryOutcomes
  OtherOutcomeMeasure
  OtherOutcomeDescription
  OtherOutcomeTimeFrame
  NumOtherOutcomes
  EligibilityCriteria
  HealthyVolunteers
  Sex
  GenderBased
  GenderDescription
  MinimumAge
  MaximumAge
  StdAge
  NumStdAges
  StudyPopulation
  SamplingMethod
  NumCentralContacts
  NumOverallOfficials
  LocationCountry
  NumLocations
  NumUniqueLocationCountries
  ReferencePMID
  ReferenceType
  AvailIPDType
  IPDSharing
  IPDSharingDescription
  IPDSharingInfoType
  NumIPDSharingInfoTypes"
)


protocolColumns <- protocolColumns %>% str_split(pattern = "\n", simplify = T) %>% as.vector() %>% str_replace("  ", replacement = "")

(protocolColumnsNotInDf <- protocolColumns[!protocolColumns %in% colnames(protocolSection_df)])

(allNAs <- colnames(protocolSection_df[, sapply(protocolSection_df, function(x) all(is.na(x)))]))

ArmGroups <- protocolSection_df %>% select(NCT, ArmGroupLabel, ArmGroupType, ArmGroupDescription, ArmGroupInterventionName)

ArmGroups <- ArmGroups %>% separate_rows(ArmGroupLabel:ArmGroupInterventionName, sep = DELIMS$L1) %>% 
  separate_rows(ArmGroupInterventionName, sep = DELIMS$L2)
Interventions <- protocolSection_df %>% select(NCT, InterventionName, InterventionType, InterventionDescription, 
                                               InterventionArmGroupLabel, InterventionOtherName)

Interventions <- Interventions %>% separate_rows(InterventionName:InterventionOtherName, sep = DELIMS$L1) %>%
  separate_rows(InterventionArmGroupLabel, sep = DELIMS$L2)



## 3.3 hasResults ----

hasResults_df <- clintrials %>%
  map(hasResults) %>%
  bind_rows()

## 3.2 resultSection ----

resultSection_df <- clintrials %>%
  map(resultSection) %>%
  bind_rows()

# 4. Unnest columns ----

tmp <- resultSection_df %>% select(NCT, OtherEventTerm, OtherEventStatsGroupId, OtherEventStatsNumEvents)

# Unnest multiple columns with the same separator
unnestmp <- tmp %>%
  separate_rows(OtherEventTerm, OtherEventStatsGroupId, OtherEventStatsNumEvents, sep = delims$L1)
