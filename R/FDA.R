# 0. Load Libraries ----

library(tidyverse)
library(rvest)
#library(polite)
library(httr2)
library(jsonlite)
library(xml2)
library(lubridate)
library(DT)
library(rollama)
library(yardstick)


# 1. Retrieve Approval Notifications ----

## 1.1 Oncology (Cancer)/Hematologic Malignancies Approval Notifications ----

### 2020 - 2025 ----

fdaurl <- "https://www.fda.gov/drugs/resources-information-approved-drugs/oncology-cancerhematologic-malignancies-approval-notifications"

# Fetch page with robust HTTPS handling
resp <- request(fdaurl) |>
  req_user_agent("rafrombec (R httr2)") |>
  req_retry(max_tries = 3) |>
  req_perform()

pg <- read_html(resp_body_string(resp))


# Each entry is presented as a paragraph with a link (title), followed by description text and a trailing date.
# We'll target paragraphs that contain a link to an FDA drug page, then parse out parts.

rows <- html_elements(pg, css = "td")

href <- rows %>%
  html_element("a") %>%
  html_attr("href") %>%
  # Make relative links absolute
  map_chr(~ url_absolute(.x, fdaurl))

full_txt <- rows %>% html_text2()  # preserves spaces better than html_text(trim=TRUE)

title <- full_txt[seq(1,length(full_txt),3)]
description <- full_txt[seq(2,length(full_txt),3)]
app_date <- full_txt[seq(3,length(full_txt),3)]
urls <- href[seq(1,length(href),3)]


fda_approvals <- tibble(
  title = title,
  description = description,
  url = urls,
  date = app_date
  
)

datatable(
  fda_approvals,
  options = list(
    pageLength = 20,
    scrollX = TRUE
  )
)

# Write table
write.csv(fda_approvals, file = "results/FDA/approval_notifications_2020_2025.csv")

### 2017-2020 ----

fdaurl <- "https://wayback.archive-it.org/7993/20201219232235/https://www.fda.gov/drugs/resources-information-approved-drugs/hematologyoncology-cancer-approvals-safety-notifications"

# Fetch page with robust HTTPS handling
resp <- request(fdaurl) |>
  req_user_agent("rafrombec (R httr2)") |>
  req_retry(max_tries = 3) |>
  req_perform()

pg <- read_html(resp_body_string(resp))


# Grab list items that contain a 'More info' link (case-insensitive)

ls <- html_elements(
  pg,
  xpath = "//main//li[.//a[contains(translate(normalize-space(.), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'more info')]]"
)

# Extract the text for each list item, trimming off the trailing "More information" bit
item_text <- ls %>%
  html_text2()

# Extract the href of the "More information" link
archived_href <- ls %>%
  html_element("a") %>%
  html_attr("href") %>%
  # Make absolute relative to the archive page
  xml2::url_absolute(base = fdaurl)

fda_approvals_2017_2020 <- tibble(
  description = item_text,
  url = archived_href
)

datatable(
  fda_approvals_2017_2020,
  options = list(
    pageLength = 20,
    scrollX = TRUE
  )
)

write.csv(fda_approvals_2017_2020, file = "results/FDA/approval_notifications_2017_2020.csv")


##### Retrieve dates from descriptions ----

# Extract the text for each list item, trimming off the trailing "More information" bit
item_text <- ls %>%
  html_text2()

extract_date_after_more_info <- function(x) {
  # 1) Split on "More info" or "More information" (case-insensitive)
  split_pat <- regex("\\bMore\\s+info(?:rmation)?\\b\\s*[:\\.\\-–—]*\\s*", ignore_case = TRUE)
  parts <- str_split_fixed(x, split_pat, 2)
  after <- parts[, 2]                          # text after "More info..."
  
  # 2) Extract a Month-name date from the 'after' part
  date_pat <- "(?i)\\b(?:jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sep(?:t(?:ember)?)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?)\\.?\\s+\\d{1,2},\\s+\\d{4}\\b"
  date_str <- str_extract(after, date_pat)
  
  # 3) Parse to Date (handles both "Nov" and "November")
  date_val <- suppressWarnings(mdy(date_str))
  
  tibble(
    text = x,
    after_more_info = na_if(str_trim(after), ""),
    date_str = date_str,
    date = date_val
  )
}

dates <- extract_date_after_more_info(item_text)


### 2006-2016 ----

#### 2013 - 2016 ----
fdaurl <- "https://wayback.archive-it.org/7993/20170111064250/http://www.fda.gov/Drugs/InformationOnDrugs/ApprovedDrugs/ucm279174.htm"

# Fetch page with robust HTTPS handling
resp <- request(fdaurl) |>
  req_user_agent("rafrombec (R httr2)") |>
  req_retry(max_tries = 3) |>
  req_perform()

pg <- read_html(resp_body_string(resp))


# Grab list items that contain a 'More info' link (case-insensitive)

ls <- html_elements(
  pg,
  xpath = "//main//li[.//a[contains(translate(normalize-space(.), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'more info')]]"
)

# Extract the text for each list item, trimming off the trailing "More information" bit
item_text <- ls %>%
  html_text2()

# Extract the href of the "More information" link
archived_href <- ls %>%
  html_element("a") %>%
  html_attr("href") %>%
  # Make absolute relative to the archive page
  xml2::url_absolute(base = fdaurl)

fda_approvals_2013_2016 <- tibble(
  description = item_text,
  url = archived_href
)

datatable(
  fda_approvals_2013_2016,
  options = list(
    pageLength = 20,
    scrollX = TRUE
  )
)


#### 2012 ----

fdaurl <- "https://wayback.archive-it.org/7993/20170111231726/http://www.fda.gov/Drugs/InformationOnDrugs/ApprovedDrugs/ucm381452.htm"

# Fetch page with robust HTTPS handling
resp <- request(fdaurl) |>
  req_user_agent("rafrombec (R httr2)") |>
  req_retry(max_tries = 3) |>
  req_perform()

pg <- read_html(resp_body_string(resp))


# Grab list items that contain a 'More info' link (case-insensitive)

ls <- html_elements(
  pg,
  xpath = "//main//li[.//a[contains(translate(normalize-space(.), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'more info')]]"
)

# Extract the text for each list item, trimming off the trailing "More information" bit
item_text <- ls %>%
  html_text2()

# Extract the href of the "More information" link
archived_href <- ls %>%
  html_element("a") %>%
  html_attr("href") %>%
  # Make absolute relative to the archive page
  xml2::url_absolute(base = fdaurl)

fda_approvals_2012 <- tibble(
  description = item_text,
  url = archived_href
)

datatable(
  fda_approvals_2012,
  options = list(
    pageLength = 20,
    scrollX = TRUE
  )
)

#### 2011 ----

fdaurl <- "https://wayback.archive-it.org/7993/20170111231727/http://www.fda.gov/Drugs/InformationOnDrugs/ApprovedDrugs/ucm381453.htm"

# Fetch page with robust HTTPS handling
resp <- request(fdaurl) |>
  req_user_agent("rafrombec (R httr2)") |>
  req_retry(max_tries = 3) |>
  req_perform()

pg <- read_html(resp_body_string(resp))

# Grab list items that contain a 'More info' link (case-insensitive)

ls <- html_elements(
  pg,
  xpath = "//main//li[.//a[contains(translate(normalize-space(.), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'more info')]]"
)

# Extract the text for each list item, trimming off the trailing "More information" bit
item_text <- ls %>%
  html_text2()

# Extract the href of the "More information" link
archived_href <- ls %>%
  html_element("a") %>%
  html_attr("href") %>%
  # Make absolute relative to the archive page
  xml2::url_absolute(base = fdaurl)

fda_approvals_2011 <- tibble(
  description = item_text,
  url = archived_href
)

datatable(
  fda_approvals_2011,
  options = list(
    pageLength = 20,
    scrollX = TRUE
  )
)

#### 2010 ----

fdaurl <- "https://wayback.archive-it.org/7993/20170111231728/http://www.fda.gov/Drugs/InformationOnDrugs/ApprovedDrugs/ucm381454.htm"

# Fetch page with robust HTTPS handling
resp <- request(fdaurl) |>
  req_user_agent("rafrombec (R httr2)") |>
  req_retry(max_tries = 3) |>
  req_perform()

pg <- read_html(resp_body_string(resp))


# Grab list items that contain a 'More info' link (case-insensitive)

ls <- html_elements(
  pg,
  xpath = "//main//li[.//a[contains(translate(normalize-space(.), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'more info')]]"
)

# Extract the text for each list item, trimming off the trailing "More information" bit
item_text <- ls %>%
  html_text2()

# Extract the href of the "More information" link
archived_href <- ls %>%
  html_element("a") %>%
  html_attr("href") %>%
  # Make absolute relative to the archive page
  xml2::url_absolute(base = fdaurl)

fda_approvals_2010 <- tibble(
  description = item_text,
  url = archived_href
)

datatable(
  fda_approvals_2010,
  options = list(
    pageLength = 20,
    scrollX = TRUE
  )
)

#### 2006 - 2009 ----

fdaurl <- "https://wayback.archive-it.org/7993/20170111231729/http://www.fda.gov/Drugs/InformationOnDrugs/ApprovedDrugs/ucm279177.htm"

# Fetch page with robust HTTPS handling
resp <- request(fdaurl) |>
  req_user_agent("rafrombec (R httr2)") |>
  req_retry(max_tries = 3) |>
  req_perform()

pg <- read_html(resp_body_string(resp))

# Grab list items that contain a 'More info' link (case-insensitive)

ls <- html_elements(
  pg,
  xpath = "//main//li[.//a[contains(translate(normalize-space(.), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'more info')]]"
)

# Extract the text for each list item, trimming off the trailing "More information" bit
item_text <- ls %>%
  html_text2()

# Extract the href of the "More information" link
archived_href <- ls %>%
  html_element("a") %>%
  html_attr("href") %>%
  # Make absolute relative to the archive page
  xml2::url_absolute(base = fdaurl)

fda_approvals_2006_2009 <- tibble(
  description = item_text,
  url = archived_href
)

datatable(
  fda_approvals_2006_2009,
  options = list(
    pageLength = 20,
    scrollX = TRUE
  )
)

fda_approvals_2006_2016 <- rbind(fda_approvals_2013_2016, fda_approvals_2012, 
                                 fda_approvals_2011, fda_approvals_2010, 
                                 fda_approvals_2006_2009)
write.csv(fda_approvals_2006_2016, file = "results/FDA/approval_notifications_2006_2016.csv")

approval_notifications <- read.csv("results/FDA/approval_notifications_2020_2025.csv", row.names = 1)
old_approvals <- read.csv("results/FDA/approval_notifications_2017_2020.csv", row.names = 1)
old_approvals <- rbind(old_approvals, read.csv("results/FDA/approval_notifications_2006_2016.csv", row.names = 1))

approval_notifications <- bind_rows(approval_notifications, old_approvals)
approval_notifications$type <- "ApprovalNotifications"


## 1.2 Verified Clinical Benefit | Cancer Accelerated Approvals ----

fdaurl <- "https://www.fda.gov/drugs/resources-information-approved-drugs/verified-clinical-benefit-cancer-accelerated-approvals"

# Fetch page with robust HTTPS handling
resp <- request(fdaurl) |>
  req_user_agent("rafrombec (R httr2)") |>
  req_retry(max_tries = 3) |>
  req_perform()

pg <- read_html(resp_body_string(resp))

# 1) target the table rows (safer than grabbing all td's at once)
tbl <- pg |>
  html_element("article") |>
  html_element("table")

trs <- tbl |> html_elements("tbody tr")

# 2) parse each row
fda_approvals <- map_dfr(trs, function(tr) {
  tds <- tr |> html_elements("td")
  if (length(tds) < 4)
    return(NULL)
  
  drug_name <- tds[[1]] |> html_text2()
  AA_indication <- tds[[2]] |> html_text2()
  AA_date <- tds[[3]] |> html_text2()
  TA_date <- tds[[4]] |> html_text2()
  
  # links are in column 2; some rows have multiple <a> (e.g., bullets/footnotes)
  links <- tds[[2]] |> html_elements("a")
  link_text <- links |> html_text2()
  link_href <- links |> html_attr("href") |> url_absolute(fdaurl)
  
  # drop in-page anchors like "#footnote" (optional, but usually desired)
  keep <- !is.na(link_href) &
    !str_starts(link_href, paste0(fdaurl, "#"))
  link_text <- link_text[keep]
  link_href <- link_href[keep]
  
  tibble(
    drug_name = drug_name,
    AA_indication = AA_indication,
    url = dplyr::first(link_href),
    # single “main” URL per row
    AA_date = AA_date,
    TA_date = TA_date#,
    #all_urls = paste(unique(link_href), collapse = "; "),
    # optional: keep all
    #all_url_text = paste(unique(link_text), collapse = "; ")# optional: keep all
  )
})


datatable(
  fda_approvals,
  options = list(
    pageLength = 20,
    scrollX = TRUE
  )
)

fda_approvals$type <- "AA_VerifiedBenefit"

write.csv(fda_approvals, file = "results/FDA/AA_verified_benefit.csv")


## 1.3 Withdrawn | Cancer Accelerated Approvals ----

fdaurl <- "https://www.fda.gov/drugs/resources-information-approved-drugs/withdrawn-cancer-accelerated-approvals"

# Fetch page with robust HTTPS handling
resp <- request(fdaurl) |>
  req_user_agent("rafrombec (R httr2)") |>
  req_retry(max_tries = 3) |>
  req_perform()

pg <- read_html(resp_body_string(resp))

# 1) target the table rows (safer than grabbing all td's at once)
tbl <- pg |>
  html_element("article") |>
  html_element("table")

trs <- tbl |> html_elements("tbody tr")

# 2) parse each row
fda_approvals <- map_dfr(trs, function(tr) {
  tds <- tr |> html_elements("td")
  if (length(tds) < 4)
    return(NULL)
  
  drug_name <- tds[[1]] |> html_text2()
  AA_indication <- tds[[2]] |> html_text2()
  AA_date <- tds[[3]] |> html_text2()
  withdrawal_date <- tds[[4]] |> html_text2()
  
  # links are in column 2; some rows have multiple <a> (e.g., bullets/footnotes)
  links <- tds[[2]] |> html_elements("a")
  link_text <- links |> html_text2()
  link_href <- links |> html_attr("href") |> url_absolute(fdaurl)
  
  # drop in-page anchors like "#footnote" (optional, but usually desired)
  keep <- !is.na(link_href) &
    !str_starts(link_href, paste0(fdaurl, "#"))
  link_text <- link_text[keep]
  link_href <- link_href[keep]
  
  tibble(
    drug_name = drug_name,
    AA_indication = AA_indication,
    url = dplyr::first(link_href),
    # single “main” URL per row
    AA_date = AA_date,
    withdrawal_date = withdrawal_date#,
    #all_urls = paste(unique(link_href), collapse = "; "),
    # optional: keep all
    #all_url_text = paste(unique(link_text), collapse = "; ")# optional: keep all
  )
})

datatable(
  fda_approvals,
  options = list(
    pageLength = 20,
    scrollX = TRUE
  )
)

fda_approvals$type <- "AA_Withdrawal"

write.csv(fda_approvals, file = "results/FDA/AA_withdrawal.csv")


# 2. Filter Drug Combination Approvals ----

## 2.1 Load tables ----

approval_notifications <- read.csv("results/FDA/approval_notifications_2020_2025.csv", row.names = 1)
old_approvals <- read.csv("results/FDA/approval_notifications_2017_2020.csv", row.names = 1)
old_approvals <- rbind(old_approvals, read.csv("results/FDA/approval_notifications_2006_2016.csv", row.names = 1))

approval_notifications <- bind_rows(approval_notifications, old_approvals)
approval_notifications$type <- "ApprovalNotifications"

AA_benefit <- read.csv("results/FDA/AA_verified_benefit.csv", row.names = 1)

AA_withdrawal <- read.csv("results/FDA/AA_withdrawal.csv", row.names = 1)

approval_notifications$query_text <- approval_notifications$description

AA_benefit$query_text <- paste(
  AA_benefit$drug_name,
  AA_benefit$AA_indication,
  sep = " "
)

AA_withdrawal$query_text <- paste(
  AA_withdrawal$drug_name,
  AA_withdrawal$AA_indication,
  sep = " "
)


merged_table <- bind_rows(approval_notifications, AA_benefit, AA_withdrawal)




## 2.2 Ollama LLM Filtering ----

ping_ollama()

models <- c("qwen3:8b", "deepseek-r1:8b","phi4")

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

merged_table$type <- as.factor(merged_table$type)

prop.table(table(merged_table$type))


### 2.2.1 Test set ----

set.seed(123)
merged_table_test <- rsample::initial_split(merged_table, prop = 0.15, strata = type)
merged_table_test <- rsample::training(merged_table_test)

prop.table(table(merged_table_test$type))

#openxlsx2::write_xlsx(merged_table_test, "results/FDA/approval_notifications_test.xlsx")
merged_table_test <- openxlsx2::read_xlsx("results/FDA/approval_notifications_test.xlsx")


llm_results <- lapply(models, function(m) {
  make_query(
    text     = merged_table_test$query_text,
    template = "{text}\n{prompt}",
    prompt   = "Categories: combination, single",
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
  ) %>%
    query(model = m, screen = FALSE, output = "text") |>
    tolower() %>%
    trimws()
})

names(llm_results) <- models

# Bind back to the data frame
for (m in models) {
  merged_table_test[[paste0(gsub("[:\\-]", "_", m))]] <- llm_results[[m]]
}

llm_results <- merged_table_test %>% select(qwen3_8b, deepseek_r1_8b, phi4)

# Majority of votes

merged_table_test$ensemble <- apply(llm_results, 1, function(x) {
  prop <- table(x)
  names(prop)[which.max(prop)]
})


#openxlsx2::write_xlsx(merged_table_test, "results/FDA/approval_notifications_test_ensembled.xlsx")
approval_notifications_test <- openxlsx2::read_xlsx("results/FDA/approval_notifications_test_ensembled.xlsx")

approval_notifications_test_curated <- openxlsx2::read_xlsx("results/FDA/approval_notifications_test.xlsx") %>% select(manual_eval)

approval_notifications_test$manual_eval <- approval_notifications_test_curated$manual_eval

#approval_notifications_test <- approval_notifications_test %>% mutate(across(.cols = c(qwen3_8b, deepseek_r1_8b, phi4, ensemble, manual_eval), .fns = as.factor))

approval_notifications_test[c("qwen3_8b",
                              "deepseek_r1_8b",
                              "phi4",
                              "ensemble",
                              "manual_eval")] <- lapply(approval_notifications_test[c("qwen3_8b",
                                                                                      "deepseek_r1_8b",
                                                                                      "phi4",
                                                                                      "ensemble",
                                                                                      "manual_eval")], function(x) {
                                                                                        x <- as.character(x)
                                                                                        x[!x %in% c("single", "combination")] <- "single"
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


retrieve_metrics(data = approval_notifications_test, reference = "manual_eval", models = c("qwen3_8b", "deepseek_r1_8b", "phi4", "ensemble"))

yardstick::conf_mat(approval_notifications_test, truth = "manual_eval", estimate = "ensemble") %>% autoplot(type = "heatmap") + # Use tiles for the heatmap cells
  scale_fill_gradient(low = "white", high = "slateblue")


df <- tibble()


tmp <- approval_notifications_test[approval_notifications_test$ensemble != approval_notifications_test$manual_eval,]

prop.table(table(approval_notifications_test$manual_eval))


## Metrics for combinations studies only
combination_test <- approval_notifications_test %>% filter(manual_eval == "combination")

retrieve_metrics(data = combination_test, reference = "manual_eval", models = c("qwen3_8b", "deepseek_r1_8b", "phi4", "ensemble"))



### 2.2.2 Complete data ----

models <- c("qwen3:8b", "deepseek-r1:8b", "phi4")

library(tidyverse)
library(rollama)

llm_results <- lapply(models, function(m) {
  make_query(
    text     = merged_table$query_text,
    template = "{text}\n{prompt}",
    prompt   = "Categories: combination, single",
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
  ) %>%
    query(model = m, screen = FALSE, output = "text") |>
    tolower() %>%
    trimws()
})

names(llm_results) <- models

# Bind back to the data frame
for (m in models) {
  merged_table[[paste0(gsub("[:\\-]", "_", m))]] <- llm_results[[m]]
}


# Save table
#openxlsx2::write_xlsx(merged_table, "results/FDA/approval_notifications_llm_results.xlsx")

merged_table <- openxlsx2::read_xlsx("results/FDA/approval_notifications_llm_results.xlsx")
llm_results <- merged_table %>% select(qwen3_8b, deepseek_r1_8b, phi4)


# Clean llm outputs to contain only 'single' or 'combination'

clean_output <- function(x) {
  x <- tolower(x)
  # extract the FIRST match of either word
  m <- stringr::str_extract(x, "\\b(single|combination)\\b")
  ifelse(is.na(m), "unknown", m)
}

merged_table <- merged_table %>% mutate(
  across(c(qwen3_8b, deepseek_r1_8b, phi4), clean_output)
)

llm_results <- llm_results %>% mutate(
  across(c(qwen3_8b, deepseek_r1_8b, phi4), clean_output)
)

# Majority of votes

merged_table$ensemble <- apply(llm_results, 1, function(x) {
  prop <- table(x)
  names(prop)[which.max(prop)]
})

#### Review disagreement results ----

merged_table$disagreement <- apply(llm_results, 1, function(x) length(unique(x)) > 1)

#openxlsx2::write_xlsx(merged_table, "results/FDA/approval_notifications_llm_results_ensembled.xlsx")
approval_notifications_ensembled <- openxlsx2::read_xlsx("results/FDA/approval_notifications_llm_results_ensembled.xlsx")

# Column 'ensemble_corrected' indicates those studies for which the 
# ensemble results were corrected (e.g., if 'yes', the study was for a combination
# but the majority of votes assigned it to single, so the value was changed to 'combination')

approval_notifications_ens_curated <- openxlsx2::read_xlsx("results/FDA/approval_notifications_llm_results_ens_curated.xlsx")

approval_notifications_combinations <- approval_notifications_ens_curated %>% filter(ensemble == "combination")

## 2.3 Retrieve full text for combination approval notifications ----

url_inspect <- function(url) {
  
  central_text <- NA_character_
  
  # 0. Request
  tryCatch({
    resp <- request(url) |>
      req_user_agent("rafrombec (R httr2)") |>
      req_retry(max_tries = 5) |>
      req_perform()
    pg <- read_html(resp_body_string(resp))
    
    # 1. Grab the central article (the middle column)
    article <- pg %>%
      html_element("article")
    
    # 2. Extract the text
    central_vec <- article %>%
      html_text2()
    
    # 3. Collapse to a single string
    central_text <- str_c(central_vec, collapse = "\n\n")
  },
  error = function(e) {
    message("Error for URL: ", url, " -> ", e$message)
    NA_character_
  })
  return(central_text)
}

approval_notifications_combinations$full_text <- map_chr(approval_notifications_combinations$url, .progress = T, ~ {
  Sys.sleep(1)
  url_inspect(.x)
})

table(is.na(approval_notifications_combinations$full_text))

tmp <- approval_notifications_combinations %>% filter(is.na(full_text))

#openxlsx2::write_xlsx(approval_notifications_combinations, "results/FDA/approval_notifications_llm_results_combinations.xlsx")

approval_notifications_combinations <- openxlsx2::read_xlsx("results/FDA/approval_notifications_llm_results_combinations.xlsx")

# Add internal row ID for later

approval_notifications_combinations$row_ID <- paste("study_", 1:nrow(approval_notifications_combinations))


length(unique(approval_notifications_combinations$row_ID))

# 3. Extract NCT ID and drug names ----

## 3.1 Extract NCT ID ----

# Extract all raw matches
approval_notifications_combinations <- approval_notifications_combinations %>%
  mutate(
    # 1) extract all matches (list-column)
    nct_raw = str_extract_all(full_text, "(?i)\\bNCT\\s*\\d{8}\\b"),
    
    # 2) normalize: remove spaces and force uppercase
    nct_ids = map(nct_raw, ~ .x %>%
                    str_replace_all("\\s+", "") %>%
                    str_to_upper()),
    
    # 3) collapse to a single string, or NA if empty
    nct = map_chr(
      nct_ids,
      ~ if (length(.x) == 0) {
        NA_character_
      } else {
        paste(unique(.x), collapse = ";")
      }
    )
  ) %>%
  select(-nct_raw, -nct_ids)

# Further analyze studies for which no NCT was found: 67 studies for which no NCT was disclosed

no_nct <- approval_notifications_combinations %>% filter(is.na(nct))

models <- c("llama3.1:8b-instruct-q4_K_M", "qwen3:4b-instruct-2507-q8_0")

sys_trials <- paste(
  "You are an information extraction engine.",
  "Task: From the provided FDA drug approval notification text, extract the names/acronyms of clinical studies or clinical trials that the approval is based on.",
  "",
  "Rules:",
  "- Return ONLY trial/study names or acronyms that appear verbatim in the text (e.g., OAK, POPLAR).",
  "- Do NOT return endpoints, biomarkers, diseases, stats, agencies, or drug names (OS, HR, PD-L1, NSCLC, FDA are NOT trials).",
  "- If none are present, return exactly: NONE",
  "",
  "Output format (STRICT):",
  "- Output only the trial names separated by semicolons, no extra words.",
  "- Example: OAK;POPLAR",
  "- No spaces. No punctuation besides semicolons.",
  sep = "\n"
)

prompt_trials <- "Extract the trial/study names now."

llm_results <- lapply(models, function(m) {
  out <- make_query(
    text     = no_nct$full_text,
    template = "{text}\n\n{prompt}",
    prompt   = prompt_trials,
    system   = sys_trials
  ) %>%
    query(
      model        = m,
      screen       = FALSE,
      output       = "text",
      model_params = list(temperature = 0)
    ) %>%
    toupper() %>%
    trimws()
  
  tibble(
    row_id = seq_along(out),
    model  = m,
    raw    = out
  )
})


names(llm_results) <- models


# Bind back to the data frame
for (m in models) {
  no_nct[[paste0(gsub("[:\\-]", "_", m))]] <- llm_results[[m]]$raw
}

#openxlsx2::write_xlsx(no_nct, "results/FDA/approvals_without_NCT.xlsx")

# Manually evaluate the studies acronyms in 'approval_notifications_without_NCT.xlsx' to retrieve
# NCT

no_nct <- openxlsx2::read_xlsx("results/FDA/approvals_without_NCT_curated.xlsx")

table(is.na(no_nct$nct))
# FALSE  TRUE 
# 26    41 

approval_notifications_combinations <- approval_notifications_combinations %>%
  left_join(no_nct %>% select(row_ID, nct), by = "row_ID", suffix = c("", ".new")) %>%
  mutate(nct = coalesce(nct, nct.new)) %>%
  select(-nct.new)

table(is.na(approval_notifications_combinations$nct))
# FALSE  TRUE 
# 218    41 


approval_notifications_combinations %>%
  mutate(nct = str_split(nct, ";")) %>% # split
  unnest(nct) %>%                       # to long vector
  mutate(nct = str_trim(nct)) %>%       # remove spaces
  filter(!is.na(nct), nct != "", nct != "NA") %>% # drop NA/"NA"/""
  pull(nct) %>% unique() %>% length()   # count unique elements

# 184


multi_nct_rows <- approval_notifications_combinations %>%
  filter(str_detect(nct, ";"))
#write.csv(multi_nct_rows, "results/FDA/multi_nct_rows.csv")


multi_nct_rows <- read.csv("results/FDA/multi_nct_rows_curated.csv")

multi_nct_rows <- multi_nct_rows %>% mutate(
  nct = str_split(nct, ";"),
  nct_claim = str_split(nct_claim, ";")) %>% 
  unnest(cols = c(nct, nct_claim))


approval_notifications_combinations <- approval_notifications_combinations %>% mutate(
  nct = str_split(nct, ";")
) %>% unnest(nct)

approval_notifications_combinations %>% group_by(nct) %>% summarise(count = n()) %>% filter(count > 1, nct != "NA")

single_efficacy <- multi_nct_rows %>% filter(nct_claim == "single_efficacy")

approval_notifications_combinations <- approval_notifications_combinations %>% 
  filter(!nct %in% single_efficacy$nct, !is.na(nct), nct != "NA")

length(unique(approval_notifications_combinations$row_ID))
length(unique(approval_notifications_combinations$nct))


openxlsx2::write_xlsx(approval_notifications_combinations, "results/FDA/approval_notifications_combinations_final.xlsx")

