
# STUDY ONE, Candidate Change Posturing ----------------------------------------------------------------------

# looking at DEBATES
# using dictionary of 'change' unigrams,
# frequency counts analysis of candidate rhetoric in debates

# libraries, directory --------------------------------------------------------

library(tidyverse)
library(here)
library(tictoc)
library(ggrepel)

here::i_am("code/old/old_exploring_dictionaries_debates.R")

# load our custom dictionary of change
change_dict <- read_csv(here("data/dictionaries/change_dictionary.csv"))

# unigram stems from dictionary 
unigrams_as_list <- as.list(change_dict$stem)

# load ref dataframe for categorizing candidates as 'nominees yn', 'presidents yn'
ref_nominee_yn <- read_csv(here("data/ref/campaigndocs_nominee_yn_ref.csv"))


# DEBATES ====================================================================

# load debate data --------------------------------------------------------------------
debates_jm_raw <- read.csv(here("data/APP_UCSB/josiah_mcmillan/pres_debates.csv"))


# filter-out non-candidates (like moderators) 
# filter-out debates preceding 21st century 
debates_contemporary = debates_jm_raw %>% 
  filter(year > 1999)  %>% 
  filter(candidate == 1) %>% 
  mutate(year = as.factor(year)) %>%
  mutate(speaker_year_id = paste0(speaker, "_", year)) 


# group data by debate and speaker
# so that each row becomes everything said by one candidate at a specific debate
# aka concatenate the text for each candidate at each debate
debates_contemporary_bydebate <- debates_contemporary %>%
  group_by(date, speaker, year, type, speaker_year_id) %>%
  summarise(text = paste(text, collapse = " "), .groups = "drop") %>%
  mutate(nchars = nchar(text))

# then further group together by election year
# so that all biden debates are in either the row biden_2012 or biden_2020 
debates_contemporary_byyear <- debates_contemporary_bydebate %>%
  group_by(speaker, year, speaker_year_id) %>%
  summarise(text = paste(text, collapse = " "), .groups = "drop") %>%
  mutate(nchars = nchar(text))

# need to check out: 
# there is a 11/28/2007 entry for Clinton
# that says she appeared at the republican debate
# this means that a lot in the dataset could be wrong

# similarly there's a 
# 10/4/2016 entry for trump 
# that is clearly for the vice presidential debate and anti-trump





# debate dictionary counts ----------------------------------------------------------------

# create a new dataframe with only speaker_year IDs
# which will become the base for a df of counts
# rows will be speaker_year
# columns will be unigram stem
# ergo: crosstab of frequencies
crosstab_debates <- debates_contemporary_byyear %>%
  group_by(speaker_year_id, speaker, year) %>%
  summarise(across(everything(), ~ first(.), .names = "meta_{.col}")) %>% # Preserve metadata
  select(speaker_year_id,
         speaker,
         year)  # Keep only a few columns for now


# count occurrences of each word
# and add as column to crosstab df

tictoc::tic()
for (word in unigrams_as_list) {
  crosstab_debates[[word]] <- debates_contemporary_byyear %>%
    group_by(speaker_year_id) %>%
    summarise(count = sum(str_count(tolower(text), 
                                    tolower(word))), .groups = "drop") %>%
    pull(count)
}
tictoc::toc()
# about 20 seconds for 90 debates and 56 dict terms
# though length of debate text varies massively


# then add two things to crosstab
# total number of change terms for each row (sum frequency of each)
  # (ie: we don't differentiate between 'importance' of each term)
# text length (from other df, hence a left_join)
crosstab_debates_totals <- crosstab_debates %>%
  mutate(total_changegrams = rowSums(across(where(is.numeric)), 
                                     na.rm = TRUE)) %>%
  left_join(debates_contemporary_byyear%>%select(speaker_year_id,
                                                 nchars),    
            by = "speaker_year_id") %>%
  select(speaker_year_id,
         total_changegrams,
         speaker,
         year,
         nchars)

# then add a percentage column
# that divites total freq change terms/ num chars in text
# to account for the fact that some speak way more than others
crosstab_debates_totals <- crosstab_debates_totals %>%
  mutate(percent_changegrams = (total_changegrams/(nchars/6))) %>% # div 6 for avg word length in chars
                                                                          # chars (4.5) plus one space character
  arrange(desc(percent_changegrams)) 

crosstab_debates_totals %>%
  write_csv(here("data/results/crosstab_debates_totals.csv"))
















