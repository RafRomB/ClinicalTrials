library(httr2)
library(tidyverse)
library(jsonlite)
library(rollama)


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
