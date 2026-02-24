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

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

collapse_vec <- function(x, sep = ";") {
  x <- x %||% character(0)
  if (!length(x)) return(NA_character_)
  paste(as.character(x), collapse = sep)
}

safe_date <- function(x) {
  x <- x %||% NA_character_
  if (is.na(x) || x == "") return(as.Date(NA))
  as.Date(lubridate::parse_date_time(x, orders = c("Y-m-d", "Y-m")))
}

# Protocol Section

protocolSection <- function(study) {
  # Retrieve information
  df <- tibble(
    # IdentificationModule ----
    NCT = study$protocolSection$identificationModule$nctId,
    NCTIdAlias = paste0(study$protocolSection$identificationModule$nctIdAliases, collapse = ";"),
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
      "name"
    ),
    CollaboratorClass = collapse_field(
      study$protocolSection$sponsorCollaboratorsModule$collaborators,
      "class"
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
      collapse = ";"
    ),
    NumConditions = length(study$protocolSection$conditionsModule$conditions),
    Keyword = paste0(
      study$protocolSection$conditionsModule$keywords,
      collapse = ";"
    ),
    
    # DesignModule ----
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
    DesignWhoMasked = paste0(
      study$protocolSection$designModule$designInfo$maskingInfo$whoMasked,
      collapse = ";"
    ),
    BioSpecRetention = study$protocolSection$designModule$bioSpec$retention,
    BioSpecDescription = study$protocolSection$designModule$bioSpec$description,
    EnrollmentCount = study$protocolSection$designModule$enrollmentInfo$count,
    EnrollmentType = study$protocolSection$designModule$enrollmentInfo$type,
    ArmGroupLabel = collapse_field(
      study$protocolSection$armsInterventionsModule$armGroups,
      "label"
    ),
    ArmGroupType = collapse_field(
      study$protocolSection$armsInterventionsModule$armGroups,
      "type"
    ),
    ArmGroupDescription = collapse_field(
      study$protocolSection$armsInterventionsModule$armGroups,
      "description"
    ),
    ArmGroupInterventionName = collapse_field(
      study$protocolSection$armsInterventionsModule$armGroups,
      "interventionNames"
    ),
    
    #NumArmGroupInterventionNames = study$protocolSection$armsInterventionsModule$armGroups$numArmGroupInterventionNames,
    NumArmGroups = length(study$protocolSection$armsInterventionsModule$armGroups),
    
    InterventionType = collapse_field(
      study$protocolSection$armsInterventionsModule$interventions,
      "type"
    ),
    InterventionName = collapse_field(
      study$protocolSection$armsInterventionsModule$interventions,
      "name"
    ),
    InterventionDescription = collapse_field(
      study$protocolSection$armsInterventionsModule$interventions,
      "description"
    ),
    InterventionArmGroupLabel = collapse_field(
      study$protocolSection$armsInterventionsModule$interventions,
      "armGroupLabels"
    ),
    
    #NumInterventionArmGroupLabels = study$protocolSection$armsInterventionsModule$interventions$numInterventionArmGroupLabels,
    
    InterventionOtherName = collapse_field(
      study$protocolSection$armsInterventionsModule$interventions,
      "otherNames"
    ),
    
    #NumInterventionOtherNames = study$protocolSection$armsInterventionsModule$interventions$numInterventionOtherNames,
    NumInterventions = length(
      study$protocolSection$armsInterventionsModule$interventions
    ),
    
    # OutcomesModule ----
    
    PrimaryOutcomeMeasure = collapse_field(
      study$protocolSection$outcomesModule$primaryOutcomes,
      "measure"
    ),
    PrimaryOutcomeDescription = collapse_field(
      study$protocolSection$outcomesModule$primaryOutcomes,
      "description"
    ),
    PrimaryOutcomeTimeFrame = collapse_field(
      study$protocolSection$outcomesModule$primaryOutcomes,
      "timeFrame"
    ),
    
    NumPrimaryOutcomes = length(study$protocolSection$outcomesModule$primaryOutcomes),
    
    SecondaryOutcomeMeasure = collapse_field(
      study$protocolSection$outcomesModule$secondaryOutcomes,
      "measure"
    ),
    SecondaryOutcomeDescription = collapse_field(
      study$protocolSection$outcomesModule$secondaryOutcomes,
      "description"
    ),
    SecondaryOutcomeTimeFrame = collapse_field(
      study$protocolSection$outcomesModule$secondaryOutcomes,
      "timeFrame"
    ),
    
    NumSecondaryOutcomes = length(study$protocolSection$outcomesModule$secondaryOutcomes),
    
    OtherOutcomeMeasure = collapse_field(
      study$protocolSection$outcomesModule$otherOutcomes,
      "measure"
    ),
    OtherOutcomeDescription = collapse_field(
      study$protocolSection$outcomesModule$otherOutcomes,
      "description"
    ),
    OtherOutcomeTimeFrame = collapse_field(
      study$protocolSection$outcomesModule$otherOutcomes,
      "timeFrame"
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
      collapse = ";"
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
      "country"
    ),
    
    #NumLocationContacts = study$protocolSection$contactsLocationsModule$locations$numLocationContacts,
    
    #LocationCountryCode = study$protocolSection$contactsLocationsModule$locations$countryCode,
    #LocationGeoPoint = study$protocolSection$contactsLocationsModule$locations$geoPoint,
    
    NumLocations = length(study$protocolSection$contactsLocationsModule$locations),
    #NumUniqueLocationCountries = study$protocolSection$contactsLocationsModule$numUniqueLocationCountries,
    
    # ReferencesModule ----
    ReferencePMID = collapse_field(study$protocolSection$referencesModule$references, "pmid"),
    ReferenceType = collapse_field(study$protocolSection$referencesModule$references, "type"),
    #RetractionPMID = collapse_field(study$protocolSection$referencesModule$references$retractions, "pmid"),
    #RetractionSource = collapse_field(study$protocolSection$referencesModule$references$retractions, "source"),
    
    #NumRetractionsForRef = study$protocolSection$referencesModule$references$numRetractionsForRef,
    #NumReferences = study$protocolSection$referencesModule$numReferences,
    #NumRetractionsAllRefs = study$protocolSection$referencesModule$numRetractionsAllRefs,
    
    #AvailIPDId = collapse_field(study$protocolSection$referencesModule$availIpds, "id"),
    AvailIPDType = collapse_field(study$protocolSection$referencesModule$availIpds, "type"),
    
    #NumAvailIPDs = study$protocolSection$referencesModule$numAvailIpDs
    
    # IPDSharingStatementModule ----
    IPDSharing = study$protocolSection$ipdSharingStatementModule$ipdSharing,
    IPDSharingDescription = study$protocolSection$ipdSharingStatementModule$description,
    IPDSharingInfoType = paste0(study$protocolSection$ipdSharingStatementModule$infoTypes, collapse = ";"),
    NumIPDSharingInfoTypes = length(study$protocolSection$ipdSharingStatementModule$infoTypes)
  )
  
  # Add additional variables
  
  df <- df %>% mutate(NumOutcomes = sum(df$NumPrimaryOutcomes + 
                                          df$NumSecondaryOutcomes + 
                                          df$NumOtherOutcomes), .after = NumOtherOutcomes)
  
  df <- df %>% mutate(NumUniqueLocationCountries = lapply(df$LocationCountry %>% str_split(pattern = ";"), function(x) length(unique(x))) %>% unlist(),
                      .after = NumLocations)
  df$NumUniqueLocationCountries[is.na(df$LocationCountry)] <- NA
  df$NumLocations[is.na(df$LocationCountry)] <- NA
  
  return(df)
}

resultSection <- function(study) {
  tibble(
    # IdentificationModule
    NCT = study$protocolSection$identificationModule$nctId,
    
    # ParticipantFlowModule
    FlowPreAssignmentDetails = study$resultsSection$participantFlowModule$preAssignmentDetails,
    FlowRecruitmentDetails = study$resultsSection$participantFlowModule$recruitmentDetails,
    FlowTypeUnitsAnalyzed = study$resultsSection$participantFlowModule$typeUnitsAnalyzed,
      FlowGroupId = collapse_field(study$resultsSection$participantFlowModule$groups, "id"),
      FlowGroupTitle = collapse_field(study$resultsSection$participantFlowModule$groups, "title"),
      FlowGroupDescription = collapse_field(study$resultsSection$participantFlowModule$groups, "description"),
    
    NumFlowGroups = length(study$resultsSection$participantFlowModule$numFlowGroups), ##
    
      FlowPeriodTitle = study$resultsSection$participantFlowModule$periods$title,
      FlowMilestoneType = study$resultsSection$participantFlowModule$periods$milestones$type,
      FlowMilestoneComment = study$resultsSection$participantFlowModule$periods$milestones$comment,
        FlowAchievementGroupId = collapse_field(study$resultsSection$participantFlowModule$periods$milestones$achievements, "groupId"),
        FlowAchievementComment = collapse_field(study$resultsSection$participantFlowModule$periods$milestones$achievements, "comment"),
        FlowAchievementNumSubjects = collapse_field(study$resultsSection$participantFlowModule$periods$milestones$achievements, "numSubjects"),
        FlowAchievementNumUnits = collapse_field(study$resultsSection$participantFlowModule$periods$milestones$achievements, "numUnits"),
    
    NumFlowAchievements = length(study$resultsSection$participantFlowModule$periods$milestones$numFlowAchievements), ##
    NumFlowMilestones = length(study$resultsSection$participantFlowModule$periods$numFlowMilestones), ##
    
      FlowDropWithdrawType = study$resultsSection$participantFlowModule$periods$dropWithdraws$type,
      FlowDropWithdrawComment = study$resultsSection$participantFlowModule$periods$dropWithdraws$comment,
        FlowReasonGroupId = study$resultsSection$participantFlowModule$periods$dropWithdraws$reasons$groupId,
        FlowReasonComment = study$resultsSection$participantFlowModule$periods$dropWithdraws$reasons$comment,
        FlowReasonNumSubjects = study$resultsSection$participantFlowModule$periods$dropWithdraws$reasons$numSubjects,
    
    NumFlowReasons = length(study$resultsSection$participantFlowModule$periods$dropWithdraws$numFlowReasons), ##
    NumFlowDropWithdraws = length(study$resultsSection$participantFlowModule$periods$numFlowDropWithdraws), ##
    NumFlowPeriods = study$resultsSection$participantFlowModule$numFlowPeriods, ##
    
    # BaselineCharacteristicsModule
    BaselinePopulationDescription = study$resultsSection$baselineCharacteristicsModule$populationDescription,
    BaselineTypeUnitsAnalyzed = study$resultsSection$baselineCharacteristicsModule$typeUnitsAnalyzed,
    BaselineGroupId = study$resultsSection$baselineCharacteristicsModule$groups$id, 
    BaselineGroupTitle = study$resultsSection$baselineCharacteristicsModule$groups$title,
    BaselineGroupDescription = study$resultsSection$baselineCharacteristicsModule$groups$description,
    
    NumBaselineGroups = study$resultsSection$baselineCharacteristicsModule$numBaselineGroups, ##
    
    BaselineDenomUnits = study$resultsSection$baselineCharacteristicsModule$denoms$units,
    BaselineDenomCountGroupId = study$resultsSection$baselineCharacteristicsModule$denoms$counts$groupId,
    BaselineDenomCountValue = study$resultsSection$baselineCharacteristicsModule$denoms$counts$value,
    
    NumBaselineDenoms = study$resultsSection$baselineCharacteristicsModule$numBaselineDenoms, ##
    
    BaselineMeasureTitle = study$resultsSection$baselineCharacteristicsModule$measures$title,
    BaselineMeasureDescription = study$resultsSection$baselineCharacteristicsModule$measures$description,
    BaselineMeasurePopulationDescription = study$resultsSection$baselineCharacteristicsModule$measures$populationDescription,
    BaselineMeasureParamType = study$resultsSection$baselineCharacteristicsModule$measures$paramType,
    BaselineMeasureDispersionType = study$resultsSection$baselineCharacteristicsModule$measures$dispersionType,
    BaselineMeasureUnitOfMeasure = study$resultsSection$baselineCharacteristicsModule$measures$unitOfMeasure,
    BaselineMeasureCalculatePct = study$resultsSection$baselineCharacteristicsModule$measures$calculatePct,
    BaselineMeasureDenomUnitsSelected = study$resultsSection$baselineCharacteristicsModule$measures$denomUnitsSelected,
    BaselineMeasureDenomCountGroupId = study$resultsSection$baselineCharacteristicsModule$measures$denoms$counts$groupId,
    BaselineMeasureDenomCountValue = study$resultsSection$baselineCharacteristicsModule$measures$denoms$counts$value,
    
    NumBaselineMeasureDenoms = study$resultsSection$baselineCharacteristicsModule$measures$numBaselineMeasureDenoms, ##
    
    BaselineClassTitle = study$resultsSection$baselineCharacteristicsModule$measures$classes$title,
    BaselineClassDenomUnits = study$resultsSection$baselineCharacteristicsModule$measures$classes$denoms$units,
    BaselineClassDenomCountGroupId = study$resultsSection$baselineCharacteristicsModule$measures$classes$denoms$counts$groupId,
    BaselineClassDenomCountValuen = study$resultsSection$baselineCharacteristicsModule$measures$classes$denoms$counts$value,
    BaselineCategoryTitle = study$resultsSection$baselineCharacteristicsModule$measures$classes$categories$title, 
    BaselineMeasurementGroupId = study$resultsSection$baselineCharacteristicsModule$measures$classes$categories$measurements$groupId,
    BaselineMeasurementValue = study$resultsSection$baselineCharacteristicsModule$measures$classes$categories$measurements$value,
    BaselineMeasurementSpread = study$resultsSection$baselineCharacteristicsModule$measures$classes$categories$measurements$spread,
    BaselineMeasurementLowerLimit = study$resultsSection$baselineCharacteristicsModule$measures$classes$categories$measurements$lowerLimit,
    BaselineMeasurementUpperLimit = study$resultsSection$baselineCharacteristicsModule$measures$classes$categories$measurements$upperLimit,
    BaselineMeasurementComment = study$resultsSection$baselineCharacteristicsModule$measures$classes$categories$measurements$comment, 
    
    NumBaselineMeasurements = study$resultsSection$baselineCharacteristicsModule$measures$classes$categories$numBaselineMeasurements, ##
    NumBaselineCategories = study$resultsSection$baselineCharacteristicsModule$measures$classes$numBaselineCategories, ##
    NumBaselineClasses = study$resultsSection$baselineCharacteristicsModule$measures$numBaselineClasses, ##
    NumBaselineMeasures = study$resultsSection$baselineCharacteristicsModule$numBaselineMeasures, ##
    
    # OutcomeMeasuresModule
    OutcomeMeasureType = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$type,
    OutcomeMeasureTitle = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$title,
    OutcomeMeasureDescription = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$description,
    OutcomeMeasurePopulationDescription = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$populationDescription,
    OutcomeMeasureReportingStatus = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$reportingStatus, 
    OutcomeMeasureAnticipatedPostingDate = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$anticipatedPostingDate, 
    OutcomeMeasureParamType = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$paramType,
    OutcomeMeasureDispersionType = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$dispersionType,
    OutcomeMeasureUnitOfMeasure = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$unitOfMeasure,
    OutcomeMeasureCalculatePct = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$calculatePct,
    OutcomeMeasureTimeFrame = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$timeFrame,
    OutcomeMeasureTypeUnitsAnalyzed = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$typeUnitsAnalyzed,
    OutcomeMeasureDenomUnitsSelected = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$denomUnitsSelected,
    OutcomeGroupId = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$groups$id,
    OutcomeGroupTitle = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$groups$title,
    OutcomeGroupDescription = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$groups$description,
    
    NumOutcomeGroups = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$numOutcomeGroups, ##
    
    OutcomeDenomUnits = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$denoms$units,
    OutcomeDenomCountGroupId = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$denoms$counts$groupId, 
    OutcomeDenomCountValue = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$denoms$counts$value,
    
    NumOutcomeDenoms = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$numOutcomeDenoms, ##
    
    OutcomeClassTitle = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$title,
    OutcomeClassDenomUnits = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$denoms$units,
    OutcomeClassDenomCountGroupId = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$denoms$counts$groupId,
    OutcomeClassDenomCountValue = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$denoms$counts$value,
    OutcomeCategoryTitle = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$categories$title,
    OutcomeMeasurementGroupId = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$categories$measurements$groupId,
    OutcomeMeasurementValue = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$categories$measurements$value,
    OutcomeMeasurementSpread = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$categories$measurements$spread,
    OutcomeMeasurementLowerLimit = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$categories$measurements$lowerLimit,
    OutcomeMeasurementUpperLimit = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$categories$measurements$upperLimit,
    OutcomeMeasurementComment = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$categories$measurements$comment,
    
    NumOutcomeMeasurements = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$categories$numOutcomeMeasurements, ##
    NumOutcomeCategories = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$classes$numOutcomeCategories, ##
    NumOutcomeClasses = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$numOutcomeClasses, ##
    
    OutcomeAnalysisParamType = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$paramType, 
    OutcomeAnalysisParamValue = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$paramValue,
    OutcomeAnalysisDispersionType = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$dispersionType,
    OutcomeAnalysisDispersionValue = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$dispersionValue,
    OutcomeAnalysisStatisticalMethod = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$statisticalMethod,
    OutcomeAnalysisStatisticalComment = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$statisticalComment,
    OutcomeAnalysisPValue = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$pValue,
    OutcomeAnalysisPValueComment = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$pValueComment,
    OutcomeAnalysisCINumSides = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$ciNumSides,
    OutcomeAnalysisCIPctValue = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$ciPctValue,
    OutcomeAnalysisCILowerLimit = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$ciLowerLimit,
    OutcomeAnalysisCIUpperLimit = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$ciUpperLimit,
    OutcomeAnalysisCILowerLimitComment = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$ciLowerLimitComment,
    OutcomeAnalysisCIUpperLimitComment = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$ciUpperLimitComment,
    OutcomeAnalysisEstimateComment = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$estimateComment,
    OutcomeAnalysisTestedNonInferiority = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$testedNonInferiority,
    OutcomeAnalysisNonInferiorityType = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$nonInferiorityType,
    OutcomeAnalysisNonInferiorityComment = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$nonInferiorityComment,
    OutcomeAnalysisOtherAnalysisDescription = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$otherAnalysisDescription,
    OutcomeAnalysisGroupDescription = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$groupDescription,
    OutcomeAnalysisGroupId = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$groupIds,
    
    NumOutcomeAnalysisGroupIds = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$analyses$numOutcomeAnalysisGroupIds, ##
    NumOutcomeAnalyses = study$resultsSection$outcomeMeasuresModule$outcomeMeasures$numOutcomeAnalyses, ##
    NumOutcomeMeasures = study$resultsSection$outcomeMeasuresModule$numOutcomeMeasures, ##
    
    # AdverseEventsModule
    EventsFrequencyThreshold = study$resultsSection$adverseEventsModule$frequencyThreshold,
    EventsTimeFrame = study$resultsSection$adverseEventsModule$timeFrame,
    EventsDescription = study$resultsSection$adverseEventsModule$description,
    EventsAllCauseMortalityComment = study$resultsSection$adverseEventsModule$allCauseMortalityComment,
    EventGroupId = study$resultsSection$adverseEventsModule$eventGroups$id,
    EventGroupTitle = study$resultsSection$adverseEventsModule$eventGroups$title,
    EventGroupDescription = study$resultsSection$adverseEventsModule$eventGroups$description,
    EventGroupDeathsNumAffected = study$resultsSection$adverseEventsModule$eventGroups$deathsNumAffected,
    EventGroupDeathsNumAtRisk = study$resultsSection$adverseEventsModule$eventGroups$deathsNumAtRisk,
    EventGroupSeriousNumAffected = study$resultsSection$adverseEventsModule$eventGroups$seriousNumAffected,
    EventGroupSeriousNumAtRisk = study$resultsSection$adverseEventsModule$eventGroups$seriousNumAtRisk,
    EventGroupOtherNumAffected = study$resultsSection$adverseEventsModule$eventGroups$otherNumAffected,
    EventGroupOtherNumAtRisk = study$resultsSection$adverseEventsModule$eventGroups$otherNumAtRisk,
    
    NumEventGroups = study$resultsSection$adverseEventsModule$numEventGroups, ##
    
    SeriousEventTerm = study$resultsSection$adverseEventsModule$seriousEvents$term,
    SeriousEventOrganSystem = study$resultsSection$adverseEventsModule$seriousEvents$organSystem,
    SeriousEventSourceVocabulary = study$resultsSection$adverseEventsModule$seriousEvents$sourceVocabulary,
    SeriousEventAssessmentType = study$resultsSection$adverseEventsModule$seriousEvents$assessmentType,
    SeriousEventNotes = study$resultsSection$adverseEventsModule$seriousEvents$notes,
    SeriousEventStatsGroupId = study$resultsSection$adverseEventsModule$seriousEvents$stats$groupId,
    SeriousEventStatsNumEvents = study$resultsSection$adverseEventsModule$seriousEvents$stats$numEvents,
    SeriousEventStatsNumAffected = study$resultsSection$adverseEventsModule$seriousEvents$stats$numAffected,
    SeriousEventStatsNumAtRisk = study$resultsSection$adverseEventsModule$seriousEvents$stats$numAtRisk,
    
    NumSeriousEventStatss = study$resultsSection$adverseEventsModule$seriousEvents$numSeriousEventStatss, ##
    NumSeriousEvents = study$resultsSection$adverseEventsModule$numSeriousEvents, ##
    
    OtherEventTerm = study$resultsSection$adverseEventsModule$otherEvents$term,
    OtherEventOrganSystem = study$resultsSection$adverseEventsModule$otherEvents$organSystem,
    OtherEventSourceVocabulary = study$resultsSection$adverseEventsModule$otherEvents$sourceVocabulary,
    OtherEventAssessmentType = study$resultsSection$adverseEventsModule$otherEvents$assessmentType,
    OtherEventNotes = study$resultsSection$adverseEventsModule$otherEvents$notes,
    OtherEventStatsGroupId = study$resultsSection$adverseEventsModule$otherEvents$stats$groupId,
    OtherEventStatsNumEvents = study$resultsSection$adverseEventsModule$otherEvents$stats$numEvents,
    OtherEventStatsNumAffected = study$resultsSection$adverseEventsModule$otherEvents$stats$numAffected,
    OtherEventStatsNumAtRisk = study$resultsSection$adverseEventsModule$otherEvents$stats$numAtRisk,
    
    NumOtherEventStatss = study$resultsSection$adverseEventsModule$otherEvents$numOtherEventStatss, ##
    NumOtherEvents = study$resultsSection$adverseEventsModule$numOtherEvents, ##
    NumEvents = study$resultsSection$adverseEventsModule$numEvents, ##
    
    # MoreInfoModule
    LimitationsAndCaveatsDescription = study$resultsSection$moreInfoModule$limitationsAndCaveats$description,
    AgreementPISponsorEmployee = study$resultsSection$moreInfoModule$certainAgreement$piSponsorEmployee,
    AgreementRestrictionType = study$resultsSection$moreInfoModule$certainAgreement$restrictionType,
    AgreementRestrictiveAgreement = study$resultsSection$moreInfoModule$certainAgreement$restrictiveAgreement,
    AgreementOtherDetails = study$resultsSection$moreInfoModule$certainAgreement$otherDetails,
  )
}

# 3. Retrieve data ----

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


study <- clintrials[[4]]

resultSection_df <- clintrials %>%
  map(resultSection) %>%
  bind_rows()


