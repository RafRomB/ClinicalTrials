# 0. Load libraries ----

library(httr2)
library(openxlsx2)
library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)
library(tibble)
library(jsonlite)
library(rollama)

# 1. Load data ----

Interventions <- read_xlsx("data/AACT/Interventions.xlsx")
DesignGroups <- read_xlsx("data/AACT/DesignGroups.xlsx")
DesignGroupInterventions <- read_xlsx("data/AACT/DesignGroupInterventions.xlsx")


# 2. Join data ----

DesignGroupInterventionDrugs <- DesignGroups %>% left_join(
  DesignGroupInterventions %>% select(design_group_id, intervention_id),
  by = c("id" = "design_group_id")
  ) %>% left_join(
    Interventions %>% rename(
      intervention_id = id,
      intervention_name = name,
      intervention_description = description
    ) %>% select(-nct_id),
    by = "intervention_id"
  ) %>% rename(
    group_id = id,
    group_title = title,
    group_description = description
  ) %>% group_by(
    across(group_id:group_description)
    ) %>% summarise(
      intervention_id = str_c(sort(unique(intervention_id)), collapse = " | "),
      intervention_name = str_c(sort(unique(intervention_name)), collapse = " | "),
      intervention_description = str_c(sort(unique(intervention_description)), collapse = " | "),
      .groups = "drop"
    )

## Prepare query text
DesignGroupInterventionDrugs$query_text <- stringr::str_squish(
  paste0(
    "GROUP TITLE:\n",
    DesignGroupInterventionDrugs$group_title,
    ".\n\n",
    "GROUP DESCRIPTION:\n",
    DesignGroupInterventionDrugs$group_description,
    ".\n INTERVENTION NAME:\n",
    DesignGroupInterventionDrugs$intervention_name,
    ".\n\n",
    "INTERVENTION DESCRIPTION:\n",
    DesignGroupInterventionDrugs$intervention_description
  ))


# LLM processing ----

# Set seed for LLMs
options(rollama_seed = 42)

# Write examples
examples_fs <- tibble::tribble(
  ~text, ~answer,
  paste0("GROUP TITLE: Placebo (Saline, 0.9% Sodium Chloride). 
  GROUP DESCRIPTION: Placebo in combination with mitoxantrone, etoposide and cytarabine (MEC) or fludarabine, cytarabine and idarubicin (FAI).
  INTERVENTION NAME:Placebo.
  INTERVENTION DESCRIPTION: Saline, 0.9% Sodium Chloride"),
  "Placebo | Mitoxantrone | Etoposide | Cytarabine | Fludarabine | Idarubicin",
  paste0("GROUP TITLE: Placebo.
  GROUP DESCRIPTION: Subjects will take an equal number of placebo 
  tablets as the group receiving tamibarotene divided as twice daily orally, 
  starting 1 week before chemotherapy and continuing through all 6 cycles and 
  through the duration of the study. Paclitaxel (IV; 200 mg/m2) and carboplatin 
  (IV; AUC=6) will be administered once every 3 weeks for up to 6 cycles.
  INTERVENTION NAME: Placebo.
  INTERVENTION DESCRIPTION: Tablets, orally, daily"),
  "Placebo | Paclitaxel | Carboplatin",
  paste0("GROUP TITLE: Dabrafenib and trametinib placebos.
  GROUP DESCRIPTION: Subjects received matching placebos orally for 12 months. 
  INTERVENTION NAME: Placebos.
  INTERVENTION DESCRIPTION: The placebo capsules and tablets contained the 
  same inactive ingredients and film coatings as the dabrafenib and trametinib study treatment"), 
  "Placebo",
  paste0("GROUP TITLE: Arm A: mRNA-2416 Alone. 
  GROUP DESCRIPTION: Participants will be administered mRNA-2416 through an intratumoral injection at the applicable dose on Days 1 and 15 for six 28-day cycles.
  INTERVENTION NAME: mRNA-2416. 
  INTERVENTION DESCRIPTION: mRNA encoding human OX40L"),
  "mRNA-2416",
  paste0("GROUP TITLE: Placebo Arm. 
  GROUP DESCRIPTION: Placebo + Gemcitabine + Cisplatin. 
  INTERVENTION NAME: Placebo. 
  INTERVENTION DESCRIPTION: IV infusion every 3 weeks with gemcitabine plus cisplatin 
  up to 8 cycles followed by monotherapy every 4 weeks until disease progression or other discontinuation criteria."),
  "Placebo | Gemcitabine | Cisplatin",
  paste0("GROUP TITLE: Vaccine Alone.
  GROUP DESCRIPTION: Subjects received vaccine immunization injected intra-dermally 
  or subcutaneously on day 1. The vaccine was an emulsification consisting of 250 mcg 
  each of the following peptides: Melan-A, gp100, MAGE-3, and NA17 as well as GM-CSF 125 mcg 
  and Montanide. A second and third vaccination was given at 2 weeks and 4 weeks after the first. 
  If there was no evidence of cancer progression, additional courses of three vaccinations 
  administered at 2 week intervals were administered until disease progression.
  INTERVENTION NAME: 4-peptide melanoma vaccine. 
  INTERVENTION DESCRIPTION: Experimental cancer vaccine given as a shot under the skin once every two weeks"),
  "4-peptide melanoma vaccine",
  paste0("GROUP TITLE: High-dose IL-2. 
  GROUP DESCRIPTION: NA. 
  INTERVENTION NAME: Irradiated donor lymphocyte infusion. 
  INTERVENTION DESCRIPTION: NA"),
  "IL-2",
  paste0("GROUP TITLE: Placebo Oral Tablet.
  GROUP DESCRIPTION: Induction chemotherapy followed by chemoradiotherapy without aspirin Placebo daily during the chemoradiotherapy.
  INTERVENTION NAME: Placebo Oral Tablet.
  INTERVENTION DESCRIPTION: chemoradiotherapy with capecitabine and placebo Placebo daily during chemoradiotherapy"),
  "Placebo | Capecitabine",
  paste0("GROUP TITLE: Single Agent Dose Escalation.
  GROUP DESCRIPTION: Participants with BRCA 1/2 mutant or HRD+ solid tumors will receive escalating doses of TNG348 to estimate the MTD. 
  INTERVENTION NAME: TNG348. 
  INTERVENTION DESCRIPTION: Ubiquitin Specific Peptidase 1 (USP1) inhibitor"),
  "TNG348",
  paste0("GROUP TITLE: Arm A: Relatlimab + Nivolumab
  GROUP DESCRIPTION: Combination
  INTERVENTION NAME: Relatlimab | Nivolumab
  INTERVENTION DESCRIPTION: Specified dose on specified day | Specified dose on specified days"),
  "Relatlimab | Nivolumab",
)

# Function to extract drug names from one query
extract_drugs_robust <- function(text_vec,
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
        "You extract administered drug names from clinical trial group text using the rules.",
        "Task: From the text, return the drug-like interventions actually administered in this study group.",
        "Output results in this format: 'Drug 1 | Drug 2 | Drug 3'. If no drug-like intervention is present, return: ''",
        sep = "\n"
      ),
      system = paste(
        "Extract administered drug names from clinical trial group text.",
        'Include:',
        '- small molecules',
        '- biologics / antibodies',
        '- peptides / cytokines',
        '- named investigational drugs or code names',
        '- named vaccines or named composite drug products',
        '- Placebo as a special intervention label when the group is a placebo group',

        'Exclude:',
        '- radiotherapy, surgery, transplant, infusion procedures, donor lymphocyte infusion, cell therapy, imaging, devices',
        '- drug targets or encoded proteins when they are not the administered product',
        '- excipients or placebo composition details (for example saline, sodium chloride, inactive ingredients)',
        '- dose, schedule, route, formulation details',
        '- regimen acronyms alone unless the component drugs are explicitly written in the text',
        '- supportive wording that is not a drug name',

        'Important rules:',
        '1. Use all fields jointly: GROUP TITLE, GROUP DESCRIPTION, INTERVENTION NAME, INTERVENTION DESCRIPTION.',
        '2. Extract only agents actually given in this arm/group.',
        '3. If the text says placebo or matching placebo(s), return "Placebo" only for the placebo product. Do not return the active drugs whose placebo versions were used unless the active drugs were actually administered in this group.',
        '4. If a named intervention is a vaccine or composite product, return the named intervention itself, not every listed ingredient/component, unless the text clearly says the components were administered as separate drugs.',
        '5. If a group combines placebo with active drugs, include Placebo and the active drugs.',
        '6. If GROUP TITLE identifies a drug-like treatment but INTERVENTION NAME is a non-drug procedure, keep the drug-like treatment and exclude the procedure.',
        '7. Normalize obvious case/plural variants:',
          '- "placebo", "Placebos", "matching placebos" -> "Placebo"',
        '8. Return unique names only, in order of first appearance.',
        '9. Do not explain your reasoning.',
        '10. Output results in this format: "Drug 1 | Drug 2 | Drug 3". If no drug-like intervention is present, return: ""',
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


llm_results <- lapply(models, function(m) {
  extract_drugs_robust(
    text_vec    = DesignGroupInterventionDrugs$query_text,
    model       = m,
    batch_size  = 50,       
    examples_fs = examples_fs
  )
})

names(llm_results) <- models

# Bind back to the data frame
for (m in models) {
  DesignGroupInterventionDrugs[[paste0(gsub("[:\\-]", "_", m))]] <- llm_results[[m]]
}

write_xlsx(DesignGroupInterventionDrugs, "results/AACT/DesignGroupInterventionDrugs_llm.xlsx")

