# 0. Load Libraries ----

library(tidyverse)
library(rvest)
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
  date = as.Date(app_date, tryFormats = c("%m/%d/%Y", "%m-%d-%Y"))
)

datatable(
  fda_approvals,
  options = list(
    pageLength = 20,
    scrollX = TRUE
  )
)

# Filter to limit results to 2025
fda_approvals <- fda_approvals %>% filter(date < as.Date("2026-01-01", format = "%Y-%m-%d"))

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
#openxlsx2::write_xlsx(merged_table, "results/FDA/approval_notifications_merged_table.xlsx")

## 2.2 Ollama LLM Filtering ----

### 2.2.1 LLM Results ----

## Run script `\R\FDA_tailscale.R`

FDA_llm <- openxlsx2::read_xlsx("results/FDA/approval_notifications_merged_table_llm_results.xlsx")

# Clean llm outputs to contain only 'single' or 'combination'

clean_output <- function(x) {
  x <- tolower(x)
  # extract the FIRST match of either word
  m <- stringr::str_extract(x, "\\b(single|combination)\\b")
  ifelse(is.na(m), "single", m)
}

FDA_llm <- FDA_llm %>% mutate(
  across(c(qwen3_14b, deepseek_r1_8b, phi4), clean_output)
)

FDA_llm <- FDA_llm %>% add_column(ID = paste("ID", 1:nrow(FDA_llm), sep = "_"),.before = "title")

merged_table <- FDA_llm %>% select(-c("qwen3_14b", "deepseek_r1_8b", "phi4"))

llm_results <- FDA_llm %>% select(qwen3_14b, deepseek_r1_8b, phi4)

llm_results <- llm_results %>% mutate(
  across(c(qwen3_14b, deepseek_r1_8b, phi4), clean_output)
)

# Majority of votes

FDA_llm$ensemble <- apply(llm_results, 1, function(x) {
  prop <- table(x)
  names(prop)[which.max(prop)]
})

#### Review disagreement results ----

FDA_llm$disagreement <- apply(llm_results, 1, function(x) length(unique(x)) > 1)

#openxlsx2::write_xlsx(FDA_llm, "results/FDA/approval_notifications_llm_results_ensembled.xlsx")

# Column 'ensemble_corrected' indicates those studies for which the 
# ensemble results were corrected (e.g., if 'yes', the study was for a combination
# but the majority of votes assigned it to single, so the value was changed to 'combination')

FDA_llm_curated <- openxlsx2::read_xlsx("results/FDA/approval_notifications_llm_results_ensembled_260212.xlsx")

FDA_llm_curated_combinations <- FDA_llm_curated %>% filter(ensemble == "combination")

### 2.2.2 LLM Performance Evaluation ----

# Calculate manual evaluation sample size, based on https://pmc.ncbi.nlm.nih.gov/articles/PMC4792103/ and assuming an expected accuracy
# of 0.5 and 95% +- 5% confidence interval

binom_N <- function(Z, p0, E) {
  N <- (Z^2*p0*(1-p0))/E^2
  return(ceiling(N))
}

# infinite-population
n0 <- binom_N(Z = 1.96, p0 = 0.5, E = 0.05)

# finite-population correction

n <- n0/(1+(n0-1)/nrow(merged_table))

set.seed(123)
merged_table_test <- rsample::initial_split(merged_table, prop = n/nrow(merged_table), strata = type)
merged_table_test <- rsample::training(merged_table_test)

prop.table(table(merged_table$type))
prop.table(table(merged_table_test$type))

#openxlsx2::write_xlsx(merged_table_test, "results/FDA/approval_notifications_test.xlsx")

# Load manually curated table
merged_table_test <- openxlsx2::read_xlsx("results/FDA/approval_notifications_test.xlsx")

merged_table_test <- merged_table_test %>% left_join(FDA_llm_curated %>% select(ID, qwen3_14b, deepseek_r1_8b, phi4, ensemble), by = "ID")

merged_table_test <- merged_table_test %>% mutate(manual_eval = as.factor(manual_eval),
                                                  qwen3_14b = as.factor(qwen3_14b),
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



(FDA_eval_metrics <- retrieve_metrics(
    data = merged_table_test,
    reference = "manual_eval",
    models = c("qwen3_14b", "deepseek_r1_8b", "phi4", "ensemble")
  )
)
openxlsx2::write_xlsx(FDA_eval_metrics, "results/FDA/FDA_eval_metrics.xlsx")


conf_matrix <- yardstick::conf_mat(merged_table_test, truth = "manual_eval", estimate = "ensemble") %>% autoplot(type = "heatmap") + # Use tiles for the heatmap cells
  scale_fill_gradient(low = "white", high = "slateblue") +
  labs(title = "Evaluation of LLM performance for FDA approval notifications")

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

FDA_llm_curated_combinations$full_text <- map_chr(FDA_llm_curated_combinations$url, .progress = T, ~ {
  Sys.sleep(1)
  url_inspect(.x)
})

table(is.na(FDA_llm_curated_combinations$full_text))

tmp <- FDA_llm_curated_combinations %>% filter(is.na(full_text))

#openxlsx2::write_xlsx(FDA_llm_curated_combinations, "results/FDA/approval_notifications_llm_results_ensembled_260212_combinations.xlsx")

FDA_llm_curated_combinations <- openxlsx2::read_xlsx("results/FDA/approval_notifications_llm_results_ensembled_260212_combinations.xlsx")


# 3. Extract NCT ID and drug names ----

## 3.1 Extract NCT ID ----

# Extract all raw matches
FDA_llm_curated_combinations <- FDA_llm_curated_combinations %>%
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

no_nct <- FDA_llm_curated_combinations %>% filter(is.na(nct))

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

no_nct_curated <- openxlsx2::read_xlsx("results/FDA/approvals_without_NCT_curated.xlsx")

table(is.na(no_nct_curated$nct))
# FALSE  TRUE 
# 27    42 

FDA_llm_curated_combinations <- FDA_llm_curated_combinations %>%
  left_join(no_nct_curated %>% select(ID, nct), by = "ID", suffix = c("", ".new")) %>%
  mutate(nct = coalesce(nct, nct.new)) %>%
  select(-nct.new)

table(is.na(FDA_llm_curated_combinations$nct))
# FALSE  TRUE 
# 220    42


FDA_llm_curated_combinations %>%
  mutate(nct = str_split(nct, ";")) %>% # split
  unnest(nct) %>%                       # to long vector
  mutate(nct = str_trim(nct)) %>%       # remove spaces
  filter(!is.na(nct), nct != "", nct != "NA") %>% # drop NA/"NA"/""
  pull(nct) %>% unique() %>% length()   # count unique elements

# 185

# Evaluate studies with multiple NCT to remove those studies that are only for single drugs

multi_nct_rows <- FDA_llm_curated_combinations %>%
  filter(str_detect(nct, ";"))
#write.csv(multi_nct_rows, "results/FDA/multi_nct_rows.csv")


multi_nct_rows <- read.csv("results/FDA/multi_nct_rows_curated.csv", row.names = 1)

multi_nct_rows <- multi_nct_rows %>% mutate(
  nct = str_split(nct, ";"),
  nct_claim = str_split(nct_claim, ";")) %>% 
  unnest(cols = c(nct, nct_claim))


FDA_llm_curated_combinations <- FDA_llm_curated_combinations %>% mutate(
  nct = str_split(nct, ";")
) %>% unnest(nct)

nrow(FDA_llm_curated_combinations %>% group_by(nct) %>% summarise(count = n()) %>% filter(count > 1, nct != "NA"))
# 40

FDA_llm_curated_combinations %>% group_by(nct) %>% summarise(count = n()) %>% filter(count > 1, nct != "NA")

single_efficacy <- multi_nct_rows %>% filter(nct_claim == "single_efficacy")
length(unique(single_efficacy$nct))
# 8

FDA_llm_curated_combinations <- FDA_llm_curated_combinations %>% 
  filter(!nct %in% single_efficacy$nct, !is.na(nct), nct != "NA")

length(unique(FDA_llm_curated_combinations$ID)) # 206
length(unique(FDA_llm_curated_combinations$nct)) # 177

#openxlsx2::write_xlsx(FDA_llm_curated_combinations, "results/FDA/approval_notifications_llm_results_combinations_final.xlsx")

# 4. Alignment with ClinicalTrials.gov query ----

FDA_llm_curated_combinations <- openxlsx2::read_xlsx("results/FDA/approval_notifications_llm_results_combinations_final.xlsx")
length(unique(FDA_llm_curated_combinations$ID)) # 206
length(unique(FDA_llm_curated_combinations$nct)) # 177

protocolSection_llm <- openxlsx2::read_xlsx("results/ClinicalTrials/protocolSection_251230_llm_ensemble.xlsx")
protocolSection_llm_comb <- protocolSection_llm %>% filter(ensemble == "combination")

FDA_llm_curated_combinations$clinicaltrialgov <- FDA_llm_curated_combinations$nct %in% protocolSection_llm_comb$nctId

not_found_approvals <- FDA_llm_curated_combinations %>% filter(clinicaltrialgov == FALSE)

not_found_approvals$exclusion <- c(
  "amyloidosis",
  "only single-drug arms",
  "only single-drug arms",
  "only single-drug arms",
  "amyloidosis",
  "still recruiting",
  "phase 1",
  "still recruiting",
  "phase 1",
  "observational study",
  "amyloidosis",
  "phase 1",
  "wrong link provided"
)

query_align <- not_found_approvals %>% filter(exclusion %in% c("still recruiting", "phase 1", "observational study")) %>% pull(nct)
tmp_df <- FDA_llm_curated_combinations %>% filter(!nct %in% query_align)
length(unique(tmp_df$ID)) # 206
length(unique(tmp_df$nct)) # 173

FDA_llm_curated_combinations <- FDA_llm_curated_combinations %>% filter(clinicaltrialgov == TRUE)

length(unique(FDA_llm_curated_combinations$ID)) # 199
length(unique(FDA_llm_curated_combinations$nct)) # 168

openxlsx2::write_xlsx(FDA_llm_curated_combinations, "results/FDA/approval_notifications_llm_results_combinations_final_260218.xlsx")
