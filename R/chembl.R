library(httr2)
library(tidyverse)


curl_translate("curl https://www.ebi.ac.uk/chembl/api/data/molecule?molecule_chembl_id__in=CHEMBL25,CHEMBL941,CHEMBL1000")


## Helper function to access ChEMBL API

chembl_url <- "https://www.ebi.ac.uk/chembl/api/data/activity.json"

chembl.response <- function(chembl_url, chembl_id, limit, offset) {
  chembl_response <- request(chembl_url) %>%
    req_url_query(
      limit = limit,
      offset = offset,
      assay_type = "A",
      molecule_chembl_id = chembl_id,
    ) %>%
    req_perform() %>% resp_body_json()
}


## Function to retrieve PK data

chembl_PK <- function(chembl_url, chembl_id = "CHEMBL185", limit = 100, offset = 0) {
  
  # Use helper function to obtain the response from the ChEMBL API
  chembl_response <- chembl.response(chembl_url = chembl_url, chembl_id = chembl_id, limit = limit, offset = offset)
  
  # Extract activities and pagination information
  chembl_data <- chembl_response$activities
  pagination <- chembl_response$page_meta
  
  # Iterate over the pages to obtain the complete data
  while(pagination$offset < pagination$total_count) {
    chembl_response_next <- chembl.response(chembl_url = chembl_url, chembl_id = chembl_id, limit = limit, offset = (pagination$offset+pagination$limit))
    pagination <- chembl_response_next$page_meta
    chembl_data <- append(chembl_data, chembl_response_next$activities)
  }
  
  # Select only those assays performed in vivo
  matches <- sapply(chembl_data, function(m) {
    organism_assay <- m$bao_label == "organism-based format"
    return(organism_assay)
  })
  
  # If there are matches, select them
  
  if (length(matches > 0)) {
    organism_assay <- which(matches)
    organism_assay <- chembl_data[organism_assay]
  } else {
    organism_assay <- data.frame()
  }
  
  # Create data frame with the results
  
  PD_df <- organism_assay %>% map_dfr(\(x) {
    tibble(
      molecule_pref_name = x %>% pluck("molecule_pref_name"),
      molecule_chembl_id = x %>% pluck("molecule_chembl_id"),
      assay_description = x %>% pluck("assay_description"),
      target_organism = x %>% pluck("target_organism"),
      standard_type = x %>% pluck("standard_type"),
      standard_units = x %>% pluck("standard_units"),
      standard_relation = x %>% pluck("standard_relation"),
      standard_upper_value = x %>% pluck("standard_upper_value"),
      standard_value = x %>% pluck("standard_value"),
      type = x %>% pluck("type"),
      units = x %>% pluck("units"),
      upper_value = x %>% pluck("upper_value"),
      value = x %>% pluck("value")
    )
  })
  
  # return the data frame
  
  return(PD_df)
}




chembl_url <- "https://www.ebi.ac.uk/chembl/api/data/activity.json"


test <- chembl_PK(chembl_url = chembl_url)


# Assume you have a vector of ChEMBL IDs
chembl_ids <- c("CHEMBL185", "CHEMBL1431", "CHEMBL521")  # Example list

# Wrap your function safely to catch errors
safe_chembl_PK <- safely(chembl_PK, otherwise = NULL)

# Apply to all ChEMBL IDs
pk_results <- map_dfr(chembl_ids, function(id) {
  res <- safe_chembl_PK(chembl_url = chembl_url, chembl_id = id)
  if (!is.null(res$result)) {
    return(res$result)
  } else {
    message(paste("Failed to retrieve data for", id))
    return(NULL)
  }
})







