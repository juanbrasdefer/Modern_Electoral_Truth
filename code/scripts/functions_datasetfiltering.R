# Script Setup ----------------------------------------------------------------------
### libraries, directory ------------------------------------------------------------
library(tidyverse)

filter_ANES_vote <- function(dataset, var_to_find, year_of_dset, ref_df = validation_ref){
  variable_coded <- ref_df %>% 
    filter(variable_common == var_to_find) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  correct_value <- ref_df %>%
    filter(variable == variable_coded) %>%
    pull(filter_value)
  
  filtered_dataset <- dataset %>%
    filter(!!sym(variable_coded) == correct_value)
  return(filtered_dataset)
}

clean_ANES_for_scoring <- function(dataset, var_to_find, year_of_dset, ref_df = validation_ref){
  
  variable_coded <- ref_df %>% 
    filter(variable_common == var_to_find) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  correct_values <- ref_df %>%
    filter(variable == variable_coded) %>%
    pull(cleaning_values)
  
  for_comparison <- strsplit(correct_values, ",")[[1]]
  
  filtered_dataset <- dataset %>%
    filter(as.character(!!sym(variable_coded)) %in% for_comparison)
  
  return(filtered_dataset)
}



score_leftright <- function(dataset, var_to_find, year_of_dset, ref_df = validation_ref){
  
  variable_coded <- ref_df %>% 
    filter(variable_common == var_to_find) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  if(year_of_dset == 2000){
    
    dataset_scored <- dataset %>%
      mutate(score_leftright = (0.25*(4 - abs(4-(!!sym(variable_coded))))))
    
    return(dataset_scored)
    
  }else{
    
    dataset_scored <- dataset %>%
      mutate(score_leftright = (0.2*(5 - abs(5-(!!sym(variable_coded))))))
    
    return(dataset_scored)
  }
}


score_economic <- function(dataset, var_to_find, year_of_dset, ref_df = validation_ref){
  
  variable_coded <- ref_df %>% 
    filter(variable_common == var_to_find) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  if(year_of_dset %in% c(2008, 2012, 2016, 2020)){
    dataset_scored <- dataset %>%
      mutate(score_economic = ifelse(!!sym(variable_coded) == 1,1,
                                     ifelse(!!sym(variable_coded) == 2, 0.75, 
                                            ifelse(!!sym(variable_coded) == 3, 0.2,0))))
    # mapping scale of 1-5 where 3, 4, and 5 mean they saw no change or negative change
    
    return(dataset_scored)
    
  } else if(year_of_dset == 2004) {
    dataset_scored <- dataset %>%
      mutate(score_economic = ifelse(!!sym(variable_coded) == 1,1,
                                     ifelse(!!sym(variable_coded) == 5, 0.2, 0)))
    # 1 means larger, 5 about the same, 3 means smaller
    
    return(dataset_scored)

  }else if(year_of_dset == 2000){
    dataset_scored <- dataset %>%
      mutate(score_economic = ifelse(!!sym(variable_coded) == 5,1,
                                     ifelse(!!sym(variable_coded) == 4, 0.75, 
                                            ifelse(!!sym(variable_coded) == 3, 0.2,0))))
    # mapping scale of 1-5 where 3, 4, and 5 mean they saw no change or negative change
    return(dataset_scored)
    }
}

score_informed <- function(dataset, var_to_find, year_of_dset, ref_df = validation_ref){
  
  variable_coded <- ref_df %>% 
    filter(variable_common == var_to_find) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  if(year_of_dset == 2008){
    dataset_scored <- dataset %>%
      mutate(score_informed = (0.2*(5 - !!sym(variable_coded))))
    
    # results in scale of 0 - 0.8
    # which has the added benefit of underweighting 2008
    # which is nice because it's a strange question
    
    return(dataset_scored)
    
    
  }else{
    dataset_scored <- dataset %>%
      mutate(score_informed = ifelse((!!sym(variable_coded)) == 1, 1, 0))
    
    return(dataset_scored)
  }
}



score_changevoter <- function(dataset, score_economic = "score_economic", 
                              score_leftright = "score_leftright", 
                              score_informed = "score_informed"){
  
  dataset_scored <- dataset %>%
  mutate(changevoter_score = ((0.5* !!sym(score_economic)) + (0.3 * !!sym(score_leftright)) + (0.2 * !!sym(score_informed))))
  
         return(dataset_scored)
}


filter_ANES_attribute_single <- function(dataset, var_to_find, year_of_dset, ref_df = validation_ref){
  
  variable_coded <- ref_df %>% 
    filter(variable_common == var_to_find) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  correct_value <- ref_df %>%
    filter(variable == variable_coded) %>%
    pull(filter_value)
  
  for_comparison <- strsplit(correct_value, ",")[[1]]

  filtered_dataset <- dataset %>%
    filter(as.character(!!sym(variable_coded)) %in% for_comparison)
  
  return(filtered_dataset)
}


filter_ANES_attributes_OR <- function(dataset, var_to_find_1, var_to_find_2, year_of_dset, ref_df = var_names_ref_old){
  variable_coded_1 <- ref_df %>% 
    filter(variable_common == var_to_find_1) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  variable_coded_2 <- ref_df %>% 
    filter(variable_common == var_to_find_2) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  correct_value_1 <- ref_df %>%
    filter(variable == variable_coded_1) %>%
    pull(filter_value)
  
  correct_value_2 <- ref_df %>%
    filter(variable == variable_coded_2) %>%
    pull(filter_value)
  
  for_comparison_1 <- strsplit(correct_value_1, ",")[[1]]
  for_comparison_2 <- strsplit(correct_value_2, ",")[[1]]
  
  filtered_dataset <- dataset %>%
    filter(as.character(!!sym(variable_coded_1)) %in% for_comparison_1 | as.character(!!sym(variable_coded_2)) %in% for_comparison_2)
  
  return(filtered_dataset)
}



summarize_ANES_preschoice <- function(dataset, var_to_find, year_of_dset, ref_df = validation_ref){
  variable_coded <- ref_df %>% 
    filter(variable_common == var_to_find) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  correct_value <- ref_df %>%
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


encode_preschoice <- function(dataset, var_to_find, year_of_dset, ref_df = validation_ref){
  variable_coded <- ref_df %>% 
    filter(variable_common == var_to_find) %>%
    filter(ANES_year == year_of_dset) %>%
    pull(variable)
  
  correct_values <- ref_df %>%
    filter(variable == variable_coded) %>%
    pull(cleaning_values)
  
  for_comparison <- strsplit(correct_values, ",")[[1]]
  
  encoded_joinable_df <- dataset %>%
    filter(as.character(!!sym(variable_coded)) %in% for_comparison) %>%
    mutate(year_preschoice_coded = paste0(year_of_dset,"_",as.character(!!sym(variable_coded))))
  
  return(encoded_joinable_df)
}



