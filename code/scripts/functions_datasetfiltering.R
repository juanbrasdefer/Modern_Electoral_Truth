# Script Setup ----------------------------------------------------------------------
### libraries, directory ------------------------------------------------------------
library(tidyverse)


filter_ANES_dset <- function(dataset, var_to_find, year_of_dset, ref_df = var_names_ref){
  variable_coded <- var_names_ref %>% 
    filter(variable_common == var_to_find) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  correct_value <- var_names_ref %>%
    filter(variable == variable_coded) %>%
    pull(filter_value)
  
  filtered_dataset <- dataset %>%
    filter(!!sym(variable_coded) == correct_value)
  return(filtered_dataset)
}



filter_ANES_attributes <- function(dataset, var_to_find_1, var_to_find_2, year_of_dset, ref_df = var_names_ref){
  variable_coded_1 <- var_names_ref %>% 
    filter(variable_common == var_to_find_1) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  variable_coded_2 <- var_names_ref %>% 
    filter(variable_common == var_to_find_2) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  correct_value_1 <- var_names_ref %>%
    filter(variable == variable_coded_1) %>%
    pull(filter_value)
  
  correct_value_2 <- var_names_ref %>%
    filter(variable == variable_coded_2) %>%
    pull(filter_value)
  
  for_comparison_1 <- strsplit(correct_value_1, ",")[[1]]
  for_comparison_2 <- strsplit(correct_value_2, ",")[[1]]
  
  filtered_dataset <- dataset %>%
    filter(as.character(!!sym(variable_coded_1)) %in% for_comparison_1 | as.character(!!sym(variable_coded_2)) %in% for_comparison_2)
  
  return(filtered_dataset)
}



summarize_ANES_votes <- function(dataset, var_to_find, year_of_dset, ref_df = var_names_ref){
  variable_coded <- var_names_ref %>% 
    filter(variable_common == var_to_find) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  correct_value <- var_names_ref %>%
    filter(variable == variable_coded) %>%
    pull(filter_value)
  
  num_rows <- nrow(dataset)
  
  summarized_dataset <- dataset %>%
    group_by(!!sym(variable_coded)) %>%
    summarize(vote_counts = n()) 
  
  summarized_dataset <- summarized_dataset %>%
    mutate(vote_proportion = round(100*(vote_counts/num_rows),2))
  
  return(summarized_dataset)
}


