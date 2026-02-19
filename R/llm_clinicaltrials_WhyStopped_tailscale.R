library(httr2)
#library(tidyverse)
library(magrittr)
library(stringr)
library(tibble)
library(jsonlite)
library(rollama)

options(rollama_seed = 42)

protocolSection_WhyStopped <- openxlsx2::read_xlsx("results/protocolSection_WhyStopped.xlsx")


examples_fs <- tibble::tribble(
  ~text, ~answer,
  
  # Negation / business
  "Business decision, not driven by safety concerns; no new safety signals have been observed in the ociperlimab program.",
  "nonclinical",
  "Company decision to discontinue the AVE1642 development program, not due to any safety or efficacy concerns",
  "nonclinical",
  "The Sponsor decided to discontinue this study due to a corporate restructuring intended to prioritize clinical development of select programs. No patients enrolled in this study and no patients received investigational product.",
  "nonclinical",
  
  # Vague / should be nonclinical by your design
  "Stopping rules met",
  "nonclinical",
  "Interim Analysis",
  "nonclinical",
  "withdrawn",
  "nonclinical",
  
  # Accrual / enrollment (nonclinical)
  "slow enrollment",
  "nonclinical",
  "Due to poor clinical trial accrual, the study was terminated.",
  "nonclinical",
  "Study was terminated due to slow subject accrual",
  "nonclinical",

  # Admin/regulatory/logistics (nonclinical)
  "Study was never submitted to the IRB & never opened. PI is leaving institution.",
  "nonclinical",
  "Drug supply issues",
  "nonclinical",
  "Required re-formulation of DFMO from IV to capsule to maintain safety",
  "nonclinical",
  
  # Efficacy / futility (explicit)
  "Lack of efficacy",
  "efficacy",
  "DSMB futility analysis",
  "efficacy",
  "Study terminated for meeting protocol specified futility criteria.",
  "efficacy",
  "Phase 2 of recruitment was contingent on 5 of 15 patients responding, which did not occur.",
  "efficacy",
  "The study was stopped after interim analysis indicated that overall result was not sufficient to satisfy per-protocol criteria to move forward in metastatic, castration-resistant prostate cancer (mCRPC) and non-small cell lung cancer (NSCLC) cohorts.",
  "efficacy",
  "It did not show a significant benefit to justify completing the full target accrual.",
  "efficacy",
  
  # Safety / toxicity (explicit)
  "Withdrawn due to toxicity",
  "safety",
  "Two patients in the first dose level be counted as reaching DLT. DSMB recommend terminated early this trial.",
  "safety",
  "Due to safety; specifically a higher rate of deaths, including fatal infections, in the SGN33A arm versus the control arm",
  "safety"
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
        "Classify the study stop reason.",
        "Return exactly one label: efficacy, safety, or nonclinical",
        sep = "\n"
      ),
      system = paste(
        "You are a biomedical clinical trial discontinuation classifier.",
        "Input: one free-text reason for why a clinical trial was terminated/suspended/withdrawn (ClinicalTrials.gov).",
        "Task: Return EXACTLY ONE label from this set:",
        "- efficacy",
        "- safety",
        "- nonclinical",
        "",
        "Definitions:",
        "- 'efficacy': the text explicitly states futility or insufficient efficacy/benefit (e.g., 'futility', 'lack of efficacy', 'failed to meet endpoint', 'did not meet endpoint', 'insufficient activity', 'interim analysis indicated results not sufficient to continue', 'protocol-specified futility criteria met', 'insufficient responders / response threshold not met').",
        "- 'safety': the text explicitly states safety/toxicity as the cause (e.g., 'toxicity', 'unacceptable toxicity', 'safety concerns', 'safety signal', 'higher rate of deaths', 'fatal infections', 'DLT', 'DSMB recommended termination due to safety').",
        "- 'nonclinical': all other reasons (accrual/recruitment/enrollment, funding, sponsor/company/business decision, reprioritization, administrative/regulatory/IRB/IND issues, site not activated, competing studies, protocol redesign, drug supply/manufacturing, feasibility/dropouts).",
        "Critical rules (precision-first):",
        "1) Negations override keywords: if the text says 'no safety concerns/signals' or 'not due to safety/efficacy', then output 'nonclinical' unless there is an explicit efficacy or safety failure stated elsewhere.",
        "2) Be conservative: choose efficacy or safety ONLY when the reason is explicit. If vague (e.g., 'Stopping rules met', 'Interim analysis'), output 'nonclinical'.",
        "3) If multiple reasons are given, choose the primary causal reason. If a nonclinical reason is stated clearly and the efficacy/safety language is weak or indirect (e.g., 'not encouraging'), output 'nonclinical.'",
        "",
        "Output format:",
        "Output must be exactly ONE label: efficacy OR safety OR nonclinical, lowercase, and nothing else (no punctuation, no explanations).",
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

models <- c("qwen3:14b", "deepseek-r1:8b", "phi4")


llm_results <- lapply(models, function(m) {
  classify_single_model_robust(
    text_vec    = protocolSection_WhyStopped$WhyStopped,
    model       = m,
    batch_size  = 50,       
    examples_fs = examples_fs
  )
})

names(llm_results) <- models

# Bind back to the data frame
for (m in models) {
  protocolSection_WhyStopped[[paste0(gsub("[:\\-]", "_", m))]] <- llm_results[[m]]
}

openxlsx2::write_xlsx(protocolSection_WhyStopped, "results/protocolSection_WhyStopped_llm.xlsx")


