library(httr2)
#library(tidyverse)
library(magrittr)
library(stringr)
library(tibble)
library(jsonlite)
library(rollama)

merged_table <- openxlsx2::read_xlsx("results/approval_notifications_merged_table.xlsx")

models <- c("qwen3:14b", "deepseek-r1:8b","phi4")

examples_fs <- tibble::tribble(
  ~text, ~answer,
  
  # COMBINATION 1 – immunotherapy + chemo regimen
  "The Food and Drug Administration approved the PD-1 inhibitor lumabrex
   in combination with platinum-based chemotherapy (cisplatin and pemetrexed)
   for the first-line treatment of adults with metastatic non-small cell lung cancer.",
  "combination",
  
  # COMBINATION 2 – two targeted agents together
  "The Food and Drug Administration approved the combination of the kinase inhibitor
   trexanib and the anti-VEGF antibody varelizumab for adults with unresectable
   hepatocellular carcinoma who have not received prior systemic therapy.",
  "combination",
  
  # COMBINATION 3 – acronym regimen explicitly defined as multiple drugs
  "The Food and Drug Administration approved zefitolimab in combination with
   Z-CHOP chemotherapy, consisting of zefitolimab, cyclophosphamide, doxorubicin,
   vincristine, and prednisone, for adults with previously untreated diffuse
   large B-cell lymphoma.",
  "combination",
  
  # SINGLE 1 – one drug, prior therapy mentioned but not co-administered
  "The Food and Drug Administration granted accelerated approval to the kinase inhibitor
   sevotrinib for adults with metastatic melanoma whose disease has progressed following
   prior immunotherapy and BRAF inhibitor treatment.",
  "single",
  
  # SINGLE 2 – one drug + diagnostic test
  "The Food and Drug Administration approved the monoclonal antibody carlumab
   for adults with locally advanced or metastatic gastric cancer whose tumors
   overexpress CLX1, as detected by an FDA-approved companion diagnostic test.",
  "single",
  
  # SINGLE 3 – one drug in multiple phases, plus supportive care
  "The Food and Drug Administration approved the oral agent neraxetine as adjuvant
   and maintenance monotherapy for adults with resected stage III colorectal cancer.
   Patients may receive antiemetics and other supportive medications as needed.",
  "single"
)


classify_FDA_robust <- function(text_vec,
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
        "You are a biomedical text classifier.",
        "Task: Given the text of an FDA cancer drug approval notification, decide whether the approval is for:",
        "- a combination regime treatment that includes more than one distinct anticancer drug ('combination'), or",
        "- a single anticancer ('single') drug treatment.",
        "",
        "Classification rules:",
        "- 'single' = The approved treatment uses one active anticancer drug.",
        "  - Multiple names for the same drug (generic name, brand names, or aliases) still count as one drug.",
        "  - The only addition of hyaluronidase formulations to one active anticancer drug does not count as a combination.",
        "  - Mention of diagnostic tests, biomarkers, or companion diagnostics does NOT change this to a combination.",
        "- 'combination' = The approved treatment regimen includes two or more distinct anticancer drugs that are part of the approved regimen, given together or sequentially.",
        "  - A named chemotherapy regimen composed of multiple drugs (e.g., FOLFOX, FLOT, CHOP) counts as a combination.",
        "  - Immunotherapy + chemotherapy, targeted therapy + chemotherapy, or any combination of multiple systemic anticancer drugs is 'combination'.",
        "  - Ignore diagnostic tests when deciding.",
        "",
        "Output format:",
        "- Answer with exactly one word: 'combination' or 'single'.",
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

llm_results <- lapply(models, function(m) {
  classify_FDA_robust(
    text_vec    = merged_table$query_text,
    model       = m,
    batch_size  = 50,       
    examples_fs = examples_fs
  )
})


names(llm_results) <- models

# Bind back to the data frame
for (m in models) {
  merged_table[[paste0(gsub("[:\\-]", "_", m))]] <- llm_results[[m]]
}

openxlsx2::write_xlsx(merged_table, "results/approval_notifications_merged_table_llm_results.xlsx")
