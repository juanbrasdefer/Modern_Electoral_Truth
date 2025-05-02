# STUDY ONE, Candidate Change Posturing ----------------------------------------------------------------------

# using dictionary of 'change' unigrams,
# frequency counts analysis of candidate rhetoric in campaign documents

# libraries, directory --------------------------------------------------------

library(tidyverse)
library(here)
library(tictoc)
library(ggrepel)

here::i_am("code/1_2_dictionary_analysis.R")
source(here("code/scripts/graph_colours.R")) # for graphing

# load our custom dictionary of change
change_dict <- read_csv(here("data/dictionaries/change_dictionary.csv"))

# unigram stems from dictionary 
unigrams_as_list <- as.list(change_dict$stem)

# load ref dataframe for matching candidates (speakers) to their political parties
ref_affiliations <- read_csv(here("data/ref/campaigndocs_polparty_ref.csv"))

# load ref dataframe for categorizing candidates as 'nominees yn', 'presidents yn'
ref_nominee_yn <- read_csv(here("data/ref/campaigndocs_nominee_yn_ref.csv"))



# CAMPAIGN DOCS ====================================================================

# 1. Load and Clean Campaign Data --------------------------------------------------------------------
campaigndocs_raw <- readRDS(here("data/APP_UCSB/scraped_campaigndocs.rds"))
  
  
# then group together by election year
# so that all clinton speeches are in either the row clinton_2008 or clinton_2016
campaigndocs_byyear <- campaigndocs_raw %>%
  select(speaker,
         date,
         title,
         body,
         nchars) %>%
  mutate(year = str_extract(date, trimws("(?<=,\\s)\\d{4}"))) %>% # extract years for mutating
  mutate(year = ifelse(year == "2023", "2024",
                       ifelse(year == "2019", "2020",
                              ifelse(year == "2015", "2016",
                                     ifelse(year == "2011", "2012",
                                            ifelse(year == "2007", "2008",
                                                   ifelse(year == "2003", "2004",
                                                          ifelse(year == "1999", "2000", year
                                                                 )))))))) %>%
  filter(as.integer(year) > 1999) %>% # keep only observations beginning in 2000
  filter(!(year %in% c("2022", "2021", "2018", "2017", "2006", "2002"))) %>% # get rid of docs not occuring during electoral cycle
  mutate(speaker = str_replace_all(speaker,
                                   "Donald J. Trump \\(1st Term\\)",
                                   "Donald J. Trump")) %>% # quick name fix
  mutate(speaker_year_id = paste0(speaker, "_", year)) %>% # create new unique columns for speaker/year
  group_by(speaker, year, speaker_year_id) %>%
  summarise(text = paste(body, collapse = " "), .groups = "drop") %>%
  mutate(nchars = nchar(text)) # for each document, determine length of doc

unique(campaigndocs_byyear$year) # just for fun

# 2. Frequency Counts, Cross-Tabulate Campaign Docs  ----------------------------------------------------------------

# aka dictionary counts 
# create a new dataframe with only speaker_year IDs
# which will become the base for a df of counts
# rows will be speaker_year
# columns will be unigram stem
# ergo: crosstab of frequencies
crosstab_campaigndocs <- campaigndocs_byyear %>%
  group_by(speaker_year_id, speaker, year) %>%
  summarise(across(everything(), ~ first(.), .names = "meta_{.col}")) %>% # Preserve metadata
  select(speaker_year_id,
         speaker,
         year)  # keep only a few columns for now



# count occurrences of each word
# and add as column to crosstab df
tictoc::tic()
for (word in unigrams_as_list) {
  crosstab_campaigndocs[[word]] <- campaigndocs_byyear %>%
    group_by(speaker_year_id) %>%
    summarise(count = sum(str_count(tolower(text), 
                                    tolower(word))), .groups = "drop") %>%
    pull(count)
}
tictoc::toc()
# about 18 mins 
# though length of debate text varies massively


# then add two things to crosstab
# total number of change terms for each row (sum frequency of each)
# (ie: we don't differentiate between 'importance' of each term)
# text length (from other df, hence a left_join)

# then add a percentage column
# that divides total freq change terms/ num chars in text
# to account for the fact that some speak way more than others
crosstab_campaigndocs_totals <- crosstab_campaigndocs %>%
  mutate(total_changegrams = rowSums(across(where(is.numeric)), 
                                     na.rm = TRUE)) %>%
  left_join(campaigndocs_byyear%>%select(speaker_year_id,
                                                 nchars),    
            by = "speaker_year_id") %>%
  select(speaker_year_id,
         speaker,
         year,
         total_changegrams,
         nchars) %>%
  left_join(ref_affiliations, by = "speaker") %>% # adding in our other candidate metadata
  left_join(ref_nominee_yn, by = "speaker_year_id") %>%
  # adding all our percentage measures
  # (which become more relevant once we do the overall change index)
  mutate(percent_changegrams = (total_changegrams/(nchars/6))) %>% # div 6 for avg word length in chars
  arrange(desc(percent_changegrams))    # chars (4.5) plus one space character
 
# save results
crosstab_campaigndocs_totals %>%
  write_csv(here("data/results/candidates/crosstab_campaigndocs_totals.csv"))



