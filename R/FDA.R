# 0. Load Libraries ----

library(tidyverse)
library(rvest)
library(polite)
library(httr2)
library(xml2)
library(lubridate)
library(DT)

# 1. Approval Notifications ----

## 2020 - 2025 ----

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

combinations <- fda_approvals %>% 
  filter(str_detect(
  title,
  regex("combination", ignore_case = TRUE)) |
    str_detect(description, regex("combination", ignore_case = TRUE))
)

# Write table
write.csv(fda_approvals, file = "results/FDA/approval_notifications_2020_2025.csv")

## 2017-2020 ----

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


## 2006-2016 ----

### 2013 - 2016 ----
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


### 2012 ----

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

### 2011 ----

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

### 2010 ----

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

### 2006 - 2009 ----

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


