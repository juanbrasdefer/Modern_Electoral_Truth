
# Script Setup ----------------------------------------------------------------------
### libraries, directory ------------------------------------------------------------
library(tidyverse)
library(here)


here::i_am("code/composite_change_index.R")
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
  left_join(crosstab_campaigndocs_totals_slct, by = "speaker_year_id") %>%
  # original excel formula for age scoring # =1-LOG((AGEATELECTION-35)/100)-1
  mutate(age_score = -log10((age_sept_election_year - 35) / 100)) %>% # is great, but if we get values below 45 it breaks down (ie: spits out a num above 1)
  #mutate(age_score1 = (1-((age_sept_election_year-35)/100))) %>% # was good, but didnt penalize oldies enough
  #mutate(age_score1 = (1 / (1 + exp(0.15 * (age_sept_election_year - 45))))+0.12) %>% penalizes the 45 and older too hard... 47 is 0.54
  # key measure of changegrams: 
  # key: within year relative changegrams
  group_by(year) %>%
  mutate(max_year_changegram = max(percent_changegrams, na.rm = TRUE)) %>%
  mutate(relative_inyear_changegrams = percent_changegrams / max_year_changegram) %>%
  # for fun: relative to all candidates changegrams
  mutate(relative_toall_changegrams = (percent_changegrams - min(percent_changegrams, na.rm = TRUE)) / (max(percent_changegrams, na.rm = TRUE) - min(percent_changegrams, na.rm = TRUE))) %>%
  # for fun: relative to all nomieens changegrams
  group_by(nominee_yn) %>%
  mutate(relative_nomineeyn_changegrams = (percent_changegrams - min(percent_changegrams, na.rm = TRUE)) / 
           (max(percent_changegrams, na.rm = TRUE) - min(percent_changegrams, na.rm = TRUE))) %>%
  ungroup()
  





cci_attributes_clean <- cci_attributes %>%
  select(speaker_year_id,
         speaker,
         year,
         relative_inyear_changegrams,
         visible_minority,
         gender,
         age_score,
         highest_govt_office,
         surname_political_dynasty,
         affiliation_relativeto_incumbent,
         pol_party,
         everything()) 


weight_changegrams <- 0.5
weight_minority <- 0.083
weight_gender <- 0.083
weight_age <- 0.083
weight_govtoffice <- 0.083
weight_dynasty <- 0.083
weight_incumbent <- 0.083

# weight_changegrams <- 0.4
# weight_minority <- 0.1
# weight_gender <- 0.1
# weight_age <- 0.1
# weight_govtoffice <- 0.1
# weight_dynasty <- 0.1
# weight_incumbent <- 0.1

cci_attributes_weighted <- cci_attributes_clean %>%
  mutate(change_index_score = ((relative_inyear_changegrams*weight_changegrams)+
                           (weight_minority*visible_minority)+
                           (weight_gender*gender)+
                           (weight_govtoffice*highest_govt_office)+
                           (weight_dynasty*surname_political_dynasty)+
                           (weight_age*age_score)+
                           (weight_incumbent*affiliation_relativeto_incumbent))) %>%
  select(speaker_year_id,
         speaker,
         year,
         change_index_score,
         everything()) 

cci_attributes_weighted %>%
  write_csv(here("data/results/CCI_results.csv"))


