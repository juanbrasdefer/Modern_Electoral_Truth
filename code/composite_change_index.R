
# Script Setup ----------------------------------------------------------------------
### libraries, directory ------------------------------------------------------------
library(tidyverse)
library(here)



# load data -------------------------------------------------------------------------

# ref dataset for CCI
ref_cci <- read_csv(here("data/ref/change_index_nominees.csv"))

crosstab_campaigndocs_totals <- read_csv(here("data/results/crosstab_campaigndocs_totals.csv"))






# join ----------------------------------------------------------------------------
crosstab_campaigndocs_totals_slct <- crosstab_campaigndocs_totals %>%
  select(speaker_year_id,
         percent_changegrams,
         total_changegrams,
         nchars,
         nominee_yn)


cci_attributes <- ref_cci %>% 
  left_join(crosstab_campaigndocs_totals_slct, by = "speaker_year_id") #%>%
  # mutate(relative_toall_changegrams = (percent_changegrams - min(percent_changegrams, na.rm = TRUE)) / (max(percent_changegrams, na.rm = TRUE) - min(percent_changegrams, na.rm = TRUE))) %>%
  # group_by(year) %>%
  # mutate(max_year_changegram = max(percent_changegrams, na.rm = TRUE)) %>%
  # mutate(relative_inyear_changegrams = percent_changegrams / max_year_changegram) %>%
  # ungroup() %>%
  # group_by(nominee_yn) %>%
  # mutate(relative_nomineeyn_changegrams = (percent_changegrams - min(percent_changegrams, na.rm = TRUE)) / 
  #          (max(percent_changegrams, na.rm = TRUE) - min(percent_changegrams, na.rm = TRUE))) %>%
  # ungroup()

#cci_attributes_temp <- cci_attributes 


cci_attributes_clean <- cci_attributes_temp %>%
  select(speaker_year_id,
         speaker,
         year,
         relative_inyear_changegrams,
         visible_minority,
         gender,
         highest_govt_office,
         surname_political_dynasty,
         affiliation_relativeto_incumbent,
         pol_party) 


weight_changegrams <- 0.5
weight_minority <- 0.1
weight_gender <- 0.1
weight_govtoffice <- 0.1
weight_dynasty <- 0.1
weight_affiliation <- 0.1
  

cci_attributes_weighted <- cci_attributes_clean %>%
  mutate(change_index_score = ((relative_inyear_changegrams*weight_changegrams)+
                           (weight_minority*visible_minority)+
                           (weight_gender*gender)+
                           (weight_govtoffice*highest_govt_office)+
                           (weight_dynasty*surname_political_dynasty)+
                           (weight_affiliation*affiliation_relativeto_incumbent))) %>%
  select(speaker_year_id,
         speaker,
         year,
         change_index_score,
         everything()) 

cci_attributes_weighted %>%
  write_csv(here("data/results/candidate_change_index_results.csv"))


