
# Script Setup ----------------------------------------------------------------------
### libraries, directory ------------------------------------------------------------
library(tidyverse)
library(here)
library(patchwork)

here::i_am("code/analysis_ANES.R")
options(scipen = 999)


### load data -----------------------------------------------------------------------
ANES_2020_raw <- read_csv(here("data/ANES/2020/anes_timeseries_2020_csv_20220210/anes_timeseries_2020_csv_20220210.csv"))



# DATASET Cleaning and Filtering ----------------------------------------------------------------------

### ANES_2020_clean - Standardizing alt codings of NAs  ------------------------------------------------------------------
ANES_2020_clean <- ANES_2020_raw %>%
  mutate(across(everything(), ~ ifelse(. < 0, # re-coding the ANES missing values as NA
                                       NA, .))) %>% # ANES uses -9, -8, -7 etc to mean different NA reasons
  mutate(across(everything(), ~ ifelse(. > 997, # also, in feeling-thermometer questions, a value of 998 or 999 means they didnt
                                       NA, .))) # know of the subject being discussed (like: do you like feminism? i dont recognize that word)


### ANES_2020_clean_slct - Reducing Dataset: Select Columns  -----------------------------------------------------

ANES_2020_clean_slct <- ANES_2020_clean %>%
  # some quick 'mutates' to simplify the variables that are split across two variabls
  # such as nancy pelosi name recognition, which is divided into two variables because
  # they used AB testing for different wordings of the question
  # so we combine each pair into one variable
  mutate(V201101_V201102_summary = ifelse(V201101 %in% c(1,2), # V201101 and V201102, PRE: Did R vote for President in 2016 
                                          V201101, ifelse(V201102 %in% c(1,2),  # Coded in ANES as: 1. YES, 2. NO
                                                          V201102, NA))) %>%    # combine into new column, NA if neither 1 nor 2
  mutate(V202139y1_V202139y2_summary = ifelse(V202139y1 %in% c(0,1), # V202139y1 and V202139y2, POST: Office recall: Speaker of the House ‐ Nancy Pelosi
                                              V202139y1, ifelse(V202139y2 %in% c(0,1),  # V202139y1 Coded in ANES as: 0. Incorrect, 1. Correct
                                                                V202139y2, ifelse(V202139y2 %in% c(2), # V202139y2 Coded in ANES as: 0. Incorrect, 1. Partially correct, 2. Correct
                                                                                  1, NA)))) %>%  # re-coding 'partially correct' from experimental phrasing to 1        
  mutate(V202140y1_V202140y2_summary = ifelse(V202140y1 %in% c(0,1), # V202140y1 and V202139y2, #V202140y2 POST: Office recall: German Chancellor ‐ Angela Merkel 
                                              V202140y1, ifelse(V202140y2 %in% c(0,1),  # Both Coded in ANES as: 0. Incorrect, 1. Correct
                                                                V202140y2, NA))) %>%          
  mutate(V202142y1_V202142y2_summary = ifelse(V202142y1 %in% c(0,1), # V202142y1 and V202142y2, POST: Office recall: SCOTUS Chief Justice ‐ John Roberts
                                              V202142y1, ifelse(V202142y1 %in% c(2),
                                                                1, ifelse(V202142y2 %in% c(0,1),  # V202142y1 Coded in ANES as: 0. Incorrect, 1. Partially correct, 2. Correct
                                                                          V202142y2, NA)))) %>%          # V202142y2 Coded in ANES as: 0. Incorrect, 1. Correct
  select(2, # Case ID
      # Voter election history variables
      V202072, # V202072 - POST: Did R vote for President in this election
      V202073, # V202073 - POST: For whom did R vote for President
      V201101_V201102_summary, # Joint V201101 and V201102 - PRE: Did R vote for President in 2016 
      V201103, # V201103 - PRE: Recall of last (2016) Presidential vote choice
      V201104, # V201104 - PRE: Did R vote for president in 2012 election
      V201105, # V201105 - PRE: recall of 2012 presidential vote choice
      # Political Involvement/ View of Politics
      V202014, #V202014 POST: R go to any political meetings, rallies, speeches, dinners, # 1 yes, 2 no
      V202025, #V202025 POST: Has R in past 12 months: joined a protest march, rally, or demonstration, # 1 yes, 2 no
      V202406, #V202406 POST: CSES5‐Q01: How interested in politics is R, #1. Very interested, 2. Somewhat interested, 3. Not very interested, 4. Not at all interested
      V202214, #V202214 POST: [REV] Politics/government too complicated to understand, #1. Always, 2. Most of the time, 3. About half the time, 4. Some of the time, 5. Never
      V202439, #V202439 POST: CSES5‐Q18: Left‐right‐self, #0. Left, 10. Right
      V202216, #V202216 POST: Important differences in what major parties stand for, #1. Yes, differences, 2. No, no differences
      V202431, #V202431 POST: CSES5‐Q14a: 5pt scale: Does it make a difference who is in power, #5-point scale, 1. It doesn’t make any difference 5. It makes a big difference l
      # Information Level
      V202138y, #V202138y POST: Office recall: Vice‐President ‐ Mike Pence [coded], #0. Incorrect, 1. Correct
      V202139y1_V202139y2_summary, # Joint V202139y1 and V202139y2 POST: Office recall: Speaker of the House ‐ Nancy Pelosi,  #0. Incorrect, 1. Correct
      V202140y1_V202140y2_summary, # Joint V202140y1 and V202140y2 POST: Office recall: German Chancellor ‐ Angela Merkel, #0. Incorrect, 1. Correct
      V202142y1_V202142y2_summary, # Joint V202142y1 and V202139y2, POST: Office recall: SCOTUS Chief Justice ‐ John Roberts,  #0. Incorrect, 1. Correct
      # Beliefs
      V202158, #V202158 POST: Feeling thermometer: Dr. Anthony Fauci, 0-100 scale,#998. Don’t know, 999. Don’t recognize
      V202160, #V202160 POST: Feeling thermometer: feminists
      V202159,#V202159 POST: Feeling thermometer: Christian fundamentalists
      V202162, #V202162 POST: Feeling thermometer: labor unions
      V202265, #V202265 POST: Fewer problems if there was more emphasis on traditional family values, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
      V202224, #V202224 POST: How important that more women get elected to political office, #1. Extremely important, 2. Very important, 3. Moderately important, 4. A little important, 5. Not at all important
      # Review of State of Union/ Government's Job
      V202427, #V202427 POST: CSES5‐Q09: How good/bad a job has government done in last 4 years, #1. Very good job, 2. Good job, 3. Bad job, 4. Very bad job
      V202430, #V202430 POST: CSES5‐Q11: State of economy better or worse over past 12 months, #1. Gotten much better, 2. Gotten somewhat better, 3. Stayed about the same, 4. Gotten somewhat worse, 5. Gotten much worse
      V202317, #V202317 POST: How much opportunity in America for average person to get ahead, #1. A great deal, 2. A lot, 3. A moderate amount, 4. A little, 5. None
      V202271, #V202271 POST: Is the US better or worse than most other countries, #1. Better, 2. Worse, 3. The same
      V202212, #V202212 POST: [STD] Public officials don't care what people think, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
      V202411, #V202411 POST: CSES5‐Q04c: Attitudes about elites: most politicians are trustworthy, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
      V202304, #V202304 POST: Our political system only works for insiders with money and power, #how well does the statement describe your views #1. Not at all well, 2. Not very well, 3. Somewhat well, 4. Very well, 5. Extremely well
      # Demographic
      V202355, #V202355 POST: Does R currently live in a rural or urban area, #1. Rural area, 2. Small town, 3. Suburb, 4. City
      V202468x, #V202468x PRE‐POST: SUMMARY: Total (family) income
                    #1. Under $9,999, 2. $10,000-14,999, 3. $15,000-19,999, 4. $20,000-24,999
                    #5. $25,000-29,999, 6. $30,000-34,999, 7. $35,000-39,999, 8. $40,000-44,999
                    #9. $45,000-49,999, 10. $50,000-59,999, 11. $60,000-64,999, 12. $65,000-69,999
                    #13. $70,000-74,999, 14. $75,000-79,999, 15. $80,000-89,999, 16. $90,000-99,999
                    #17. $100,000-109,999, 18. $110,000-124,999, 19. $125,000-149,999, 20. $150,000-174,999
                    #21. $175,000-249,999, 22. $250,000 or more
      # New Variables
        V202173, #V202173 POST: Feeling thermometer: scientists
        V202213, #V202213 POST: [STD] Have no say about what goverment does #1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
        V202253, #V202253 POST: Less government better OR more that government should be doing, #1. The less government the better, 2. More things government should be doing
        V202259x,#V202259x POST: SUMMARY: Favor/oppose government trying to reduce income inequality 1. Favor a great deal 2. Favor a moderate amount 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose a moderate amount 7. Oppose a great deal
        V202260, #V202260 POST: Society should make sure everyone has equal opportunity #1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
        V202264, #V202264 POST: The world is changing & we should adjust view of moral behavior 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
        V202290x,#V202290x POST: SUMMARY: Better/worse if man works and woman takes care of home, #1. Much better 2. Somewhat better 3. Slightly better 4. Makes no difference 5. Slightly worse 6. Somewhat worse 7. Much worse
        V202305, #V202305 POST: Because of rich and powerful it's difficult for the rest to get ahead-- describes your view 1. Not at all well 2. Not very well 3. Somewhat well 4. Very well 5. Extremely well
        V202308x,#V202308x POST: SUMMARY: Trust ordinary people/experts for public policy, #1. Trust ordinary people much more 2. Trust ordinary people somewhat more 3. Trust both the same 4. Trust experts somwhat more 5. Trust experts much more
        V202309, #V202309 POST: How much do people need help from experts to understand science #1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
        V202320x,#V202320x POST: SUMMARY: Economic mobility, 1. A great deal easier 2. A moderate amount easier 3. A little easier 4. The same 5. A litte harder 6. A moderate amount harder 7. A great deal harder
        V202332, #V202332 POST: How much is climate change affecting severe weather/temperatures in US, 1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
        V202333, #V202333 POST: How important is issue of climate change to R, 1. Not at all important 2. A little important 3. Moderately important 4. Very important 5. Extremely important
        V202361x,#V202361x POST: SUMMARY: Favor/oppose free trade agreement, 1. Favor a great deal 2. Favor moderately 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose moderately 7. Oppose a great deal
        V202400, #V202400 POST: How much is China a threat to the United States, 1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
        V202407, #V202407 POST: CSES5‐Q02: How closely does R follow politics in media 1. Very closely 2. Fairly closely 3. Not very closely 4. Not at all
        V202410, #V202410 POST: CSES5‐Q04b: Attitudes about elites: politicians do not care about people 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
        V202412, #V202412 POST: CSES5‐Q04d: Attitudes about elites: politicians are main problem in US 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
        V202413, #V202413 POST: CSES5‐Q04e: Attitudes about elites: strong leader in government is good 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
        V202414, #V202414 POST: CSES5‐Q04f: Attitudes about elites: people should make policy decisions 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
        V202424, #V202424 POST: CSES5‐Q06d: National identity: how important to follow America's customs 1. Very important 2. Fairly important 3. Not very important 4. Not important at all
        V202440  #V202440 POST: CSES5‐Q21: Satisfaction with democratic process 1. Very satisfied 2. Fairly satisfied 4. Not very satisfied 5. Not at all satisfied
      )



colSums(is.na(ANES_2020_clean_slct)) 
# of our voter belief/ attribute columns, max missing is ~1060
# average is about 850 missing values, so about 10%
# aka no hugely worrisome columns




### voted_change_20_allelecs_slct - Create Subset: Voted for Change in 2020, 16, 12 -------------------------
change_cand_20_20 <- 1 # Change candidate in 2020 in the 2020 ANES dataset
# Joe Biden is coded as 1 in this dataset
change_cand_16_20 <- 2 # Change candidate in 2016 in the 2020 ANES dataset
# Donald Trump is code as 2 in this dataset
change_cand_12_20 <- 1 # Change candidate in 2012 in the 2020 ANES dataset
# Barack Obama is code as 1 in this dataset

# df: find voters that voted Obama -> Trump -> Biden
voted_change_20_allelecs <- ANES_2020_clean_slct %>%
  filter(!is.na(V202072)) %>% # V202072 - POST: Did R vote for President in this election
  # Filters out: refused q, didnt participate in POST...
  # N: 8280->6029
  filter(V202072 == 1) %>%    # V202072 - POST: Did R vote for President in this election
  # Filters: answered YES in voting for president 
  # N: 6029->5952
  filter(V202073 == change_cand_20_20) %>%  # V202073 - POST: For whom did R vote for President
  # Filter: Voted for change candidate (Biden) 
  # N: 5952->3267 (means 55% voted Biden)
  # note: this was a clean variable, no coded -9, -8 etc
  filter(!is.na(V201101_V201102_summary)) %>%  # PRE: Did R vote for President in 2016 
  # Filters out: refused q, didnt do post vote survey...
  # N: 3267->3259
  filter(V201101_V201102_summary == 1) %>%    # V201101_V201102_summary - PRE: Did R vote for President in 2016 
  # Filters: answered YES in voting for president 
  # N: 3259->2820
  filter(!is.na(V201103)) %>%                 # V201103 - PRE: Recall of last (2016) Presidential vote choice
  # Filters out: refused q, didnt do post vote survey..
  # N: 2820->2809
  filter(V201103 == change_cand_16_20) %>%    # V201103 - PRE: Recall of last (2016) Presidential vote choice
  # Filter: VOTED for Donald Trump
  # N: 2809->189 (7% voted for Trump)
  filter(!is.na(V201104)) %>%                 # V201104 - PRE: Did R vote for president in 2012 election
  # Filters out: refused q, didnt participate in POST...
  # N: 189->187
  filter(!is.na(V201105)) %>%           # V201105 - PRE: recall of 2012 presidential vote choice
  # Filters out: refused q, didnt do post vote survey..
  # N: 187->156
  filter(V201105 == change_cand_12_20)      # V201105 - PRE: recall of 2012 presidential vote choice
  # Filter: VOTED for Barack Obama
  # N: 156->89 (57% voted Obama)
# meaning that we have 89 people in this 
# dataset that voted Obama -> Trump -> Biden  
# let's see who they are!!


# we also get rid of columns that are not useful for summary stats
# columns that helped us filter earlier
droplist <- c("V202072", # V202072 - POST: Did R vote for President in this election
              "V202073", # V202073 - POST: For whom did R vote for President
              "V201101_V201102_summary", # Joint V201101 and V201102 - PRE: Did R vote for President in 2016 
              "V201103", # V201103 - PRE: Recall of last (2016) Presidential vote choice
              "V201104", # V201104 - PRE: Did R vote for president in 2012 election
              "V201105") # V201105 - PRE: recall of 2012 presidential vote choice
# note: this droplist is used here and later on fulldset as well

# select (drop)
voted_change_20_allelecs_slct <- voted_change_20_allelecs %>%
  select(-all_of(droplist))


# COMPARISON - Summary Stats -------------------------------------------------------------------------

### function - compute summary stats for visual browsing ------------------------------------------------
# checks variable type to decide application
get_summary <- function(vec) {
  vec <- na.omit(vec)
  n_unique <- length(unique(vec))
  # Treat as ordinal/categorical
  return(as.list(prop.table(table(vec))))
  # Continuous
  #return(list(mean = mean(vec), sd = sd(vec), median = median(vec)))
}

### function - mapping utility for summary stats  ------------------------------------------------
# wrapper to apply across dataset
summarize_dataset <- function(df) {
  map(df, get_summary)
}


### comparison_sumstats - sub vs full dsets summary stats ----------------------------------------------------------------
# drop observations that didn't vote in 2020
voted_ANES_2020_clean_slct <- ANES_2020_clean_slct %>%
  # since we are going to be looking for indicators that tell us why people vote the way they do
  # it makes sense to drop all observations that didnt vote in 2020
  # since we don't have the counterfactual for these people of "who would they have voted for"
  filter(V202072 == 1) %>%
  # we also get rid of columns that are not useful for summary stats
  # columns that helped us filter earlier
  select(-all_of(droplist))
  
  


  
# for both datasets,
fulldset <- voted_ANES_2020_clean_slct[ , -1]   # drop the ID column
fulldset_summary <- summarize_dataset(fulldset)   # and apply custom function (wrapper and get_summary())

subset <- voted_change_20_allelecs_slct[ , -1] # repeat for sub
subset_summary <- summarize_dataset(subset) # same wrapper and summary function


# combine into tibble (similar to list) for storage
# variable names as rows, nested structure for values
combined_summary <- tibble(
  variable = names(fulldset_summary),
  fulldset = fulldset_summary,
  subset = subset_summary)

# then turn into long format
comparison_sumstats <- combined_summary %>%
  unnest_wider(fulldset, names_sep = "_") %>% # new naming scheme for variables
  unnest_wider(subset, names_sep = "_")

# for ease of reordering columns:
# rename so number in var name come first
colnames(comparison_sumstats) <- ifelse(
  colnames(comparison_sumstats) == "variable", "variable",  # ifelse() is used to skip over our first column, called 'variable'
  paste0(str_extract(colnames(comparison_sumstats), "\\d+"), "_", str_extract(colnames(comparison_sumstats), "fulldset|subset"))
  # for all other columns, rename to bin_var naming convention
  # where bin is ordinal, binary, semi-continuous bin
  )

# fetch names of all columns except the 'variable' column
all_cols <- colnames(comparison_sumstats)
data_cols <- setdiff(all_cols, "variable") # drop col named "variable"

# extract the number and type (fulldset/subset) to sort properly
col_order <- tibble(name = data_cols,
  varnum = as.numeric(str_extract(data_cols, "^\\d+")),
  type = str_extract(data_cols, "fulldset|subset"))

# arrange columns by bins and dataset, meaning we get: 
  # 'variable', '1_fulldset', '1_subset', '2_fulldset', '2_subset'...
  # where variable has all our variables as rows
  # and the '1_fulldset' column holds values for all variables in fulldset that have '1' in their scale
sorted_names <- col_order %>%
  arrange(varnum, type) %>%
  pull(name)

# finally, reorder the original dataframe
comparison_sumstats <- comparison_sumstats %>%
  select(variable, all_of(sorted_names))


# COMPARISON - Graph Overlays -------------------------------------------------------------------------

# 2020 Select Variables - Graphing Comparison between full dataset and subset (change voters)

### annotate variable types (binary, ordinal, cont.) --------------------------------------------------

### var_types_slct_2020 - manual annotation --------------------------------------------------------
# manually defining types
var_types_slct_2020 <- tibble::tibble(
  variable = c(
    # Political Involvement/ View of Politics
    "V202014", #V202014 POST: R go to any political meetings, rallies, speeches, dinners, # 1 yes, 2 no
    "V202025", #V202025 POST: Has R in past 12 months: joined a protest march, rally, or demonstration, # 1 yes, 2 no
    "V202406", #V202406 POST: CSES5‐Q01: How interested in politics is R, #1. Very interested, 2. Somewhat interested, 3. Not very interested, 4. Not at all interested
    "V202214", #V202214 POST: [REV] Politics/government too complicated to understand, #1. Always, 2. Most of the time, 3. About half the time, 4. Some of the time, 5. Never
    "V202439", #V202439 POST: CSES5‐Q18: Left‐right‐self, #0. Left, 10. Right
    "V202216", #V202216 POST: Important differences in what major parties stand for, #1. Yes, differences, 2. No, no differences
    "V202431", #V202431 POST: CSES5‐Q14a: 5pt scale: Does it make a difference who is in power, #5-point scale, 1. It doesn’t make any difference 5. It makes a big difference l
    # Information Level
    "V202138y", #V202138y POST: Office recall: Vice‐President ‐ Mike Pence [coded], #0. Incorrect, 1. Correct
    "V202139y1_V202139y2_summary", # Joint V202139y1 and V202139y2 POST: Office recall: Speaker of the House ‐ Nancy Pelosi,  #0. Incorrect, 1. Correct
    "V202140y1_V202140y2_summary", # Joint V202140y1 and V202140y2 POST: Office recall: German Chancellor ‐ Angela Merkel, #0. Incorrect, 1. Correct
    "V202142y1_V202142y2_summary", # Joint V202142y1 and V202139y2, POST: Office recall: SCOTUS Chief Justice ‐ John Roberts,  #0. Incorrect, 1. Correct
    # Beliefs
    "V202158", #V202158 POST: Feeling thermometer: Dr. Anthony Fauci, 0-100 scale,#998. Don’t know, 999. Don’t recognize
    "V202160", #V202160 POST: Feeling thermometer: feminists
    "V202159",#V202159 POST: Feeling thermometer: Christian fundamentalists
    "V202162", #V202162 POST: Feeling thermometer: labor unions
    "V202265", #V202265 POST: Fewer problems if there was more emphasis on traditional family values, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "V202224", #V202224 POST: How important that more women get elected to political office, #1. Extremely important, 2. Very important, 3. Moderately important, 4. A little important, 5. Not at all important
    # Review of State of Union/ Government's Job
    "V202427", #V202427 POST: CSES5‐Q09: How good/bad a job has government done in last 4 years, #1. Very good job, 2. Good job, 3. Bad job, 4. Very bad job
    "V202430", #V202430 POST: CSES5‐Q11: State of economy better or worse over past 12 months, #1. Gotten much better, 2. Gotten somewhat better, 3. Stayed about the same, 4. Gotten somewhat worse, 5. Gotten much worse
    "V202317", #V202317 POST: How much opportunity in America for average person to get ahead, #1. A great deal, 2. A lot, 3. A moderate amount, 4. A little, 5. None
    "V202271", #V202271 POST: Is the US better or worse than most other countries, #1. Better, 2. Worse, 3. The same
    "V202212", #V202212 POST: [STD] Public officials don't care what people think, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "V202411", #V202411 POST: CSES5‐Q04c: Attitudes about elites: most politicians are trustworthy, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "V202304", #V202304 POST: Our political system only works for insiders with money and power, #how well does the statement describe your views #1. Not at all well, 2. Not very well, 3. Somewhat well, 4. Very well, 5. Extremely well
    # Demographic
    "V202355", #V202355 POST: Does R currently live in a rural or urban area, #1. Rural area, 2. Small town, 3. Suburb, 4. City
    "V202468x",#V202468x PRE‐POST: SUMMARY: Total (family) income
                        #1. Under $9,999, 2. $10,000-14,999, 3. $15,000-19,999, 4. $20,000-24,999
                        #5. $25,000-29,999, 6. $30,000-34,999, 7. $35,000-39,999, 8. $40,000-44,999
                        #9. $45,000-49,999, 10. $50,000-59,999, 11. $60,000-64,999, 12. $65,000-69,999
                        #13. $70,000-74,999, 14. $75,000-79,999, 15. $80,000-89,999, 16. $90,000-99,999
                        #17. $100,000-109,999, 18. $110,000-124,999, 19. $125,000-149,999, 20. $150,000-174,999
                        #21. $175,000-249,999, 22. $250,000 or more
    # New Variables
    "V202173", #V202173 POST: Feeling thermometer: scientists
    "V202213", #V202213 POST: [STD] Have no say about what goverment does #1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "V202253", #V202253 POST: Less government better OR more that government should be doing, #1. The less government the better, 2. More things government should be doing
    "V202259x",#V202259x POST: SUMMARY: Favor/oppose government trying to reduce income inequality 1. Favor a great deal 2. Favor a moderate amount 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose a moderate amount 7. Oppose a great deal
    "V202260", #V202260 POST: Society should make sure everyone has equal opportunity #1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "V202264", ##V202264 POST: The world is changing & we should adjust view of moral behavior 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "V202290x",#V202290x POST: SUMMARY: Better/worse if man works and woman takes care of home, #1. Much better 2. Somewhat better 3. Slightly better 4. Makes no difference 5. Slightly worse 6. Somewhat worse 7. Much worse
    "V202305", #V202305 POST: Because of rich and powerful it's difficult for the rest to get ahead-- describes your view 1. Not at all well 2. Not very well 3. Somewhat well 4. Very well 5. Extremely well
    "V202308x",#V202308x POST: SUMMARY: Trust ordinary people/experts for public policy, #1. Trust ordinary people much more 2. Trust ordinary people somewhat more 3. Trust both the same 4. Trust experts somwhat more 5. Trust experts much more
    "V202309", #V202309 POST: How much do people need help from experts to understand science #1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "V202320x",#V202320x POST: SUMMARY: Economic mobility, 1. A great deal easier 2. A moderate amount easier 3. A little easier 4. The same 5. A litte harder 6. A moderate amount harder 7. A great deal harder
    "V202332", #V202332 POST: How much is climate change affecting severe weather/temperatures in US, 1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "V202333", #V202333 POST: How important is issue of climate change to R, 1. Not at all important 2. A little important 3. Moderately important 4. Very important 5. Extremely important
    "V202361x",#V202361x POST: SUMMARY: Favor/oppose free trade agreement, 1. Favor a great deal 2. Favor moderately 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose moderately 7. Oppose a great deal
    "V202400", #V202400 POST: How much is China a threat to the United States, 1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "V202407", #V202407 POST: CSES5‐Q02: How closely does R follow politics in media 1. Very closely 2. Fairly closely 3. Not very closely 4. Not at all
    "V202410", #V202410 POST: CSES5‐Q04b: Attitudes about elites: politicians do not care about people 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "V202412", #V202412 POST: CSES5‐Q04d: Attitudes about elites: politicians are main problem in US 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "V202413", #V202413 POST: CSES5‐Q04e: Attitudes about elites: strong leader in government is good 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "V202414", #V202414 POST: CSES5‐Q04f: Attitudes about elites: people should make policy decisions 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "V202424", #V202424 POST: CSES5‐Q06d: National identity: how important to follow America's customs 1. Very important 2. Fairly important 3. Not very important 4. Not important at all
    "V202440" #V202440 POST: CSES5‐Q21: Satisfaction with democratic process 1. Very satisfied 2. Fairly satisfied 4. Not very satisfied 5. Not at all satisfied
  ),
  type = c(
    # Political Involvement/ View of Politics
    "binary", #V202014 POST: R go to any political meetings, rallies, speeches, dinners, # 1 yes, 2 no
    "binary", #V202025 POST: Has R in past 12 months: joined a protest march, rally, or demonstration, # 1 yes, 2 no
    "ordinal", #V202406 POST: CSES5‐Q01: How interested in politics is R, #1. Very interested, 2. Somewhat interested, 3. Not very interested, 4. Not at all interested
    "ordinal", #V202214 POST: [REV] Politics/government too complicated to understand, #1. Always, 2. Most of the time, 3. About half the time, 4. Some of the time, 5. Never
    "ordinal", #V202439 POST: CSES5‐Q18: Left‐right‐self, #0. Left <-> 10. Right
    "binary", #V202216 POST: Important differences in what major parties stand for, #1. Yes, differences, 2. No, no differences
    "ordinal", #V202431 POST: CSES5‐Q14a: 5pt scale: Does it make a difference who is in power, #5-point scale, 1. It doesn’t make any difference 5. It makes a big difference l
    # Information Level
    "binary", #V202138y POST: Office recall: Vice‐President ‐ Mike Pence [coded], #0. Incorrect, 1. Correct
    "binary", # Joint V202139y1 and V202139y2 POST: Office recall: Speaker of the House ‐ Nancy Pelosi,  #0. Incorrect, 1. Correct
    "binary", # Joint V202140y1 and V202140y2 POST: Office recall: German Chancellor ‐ Angela Merkel, #0. Incorrect, 1. Correct
    "binary", # Joint V202142y1 and V202139y2, POST: Office recall: SCOTUS Chief Justice ‐ John Roberts,  #0. Incorrect, 1. Correct
    # Beliefs
    "continuous", #V202158 POST: Feeling thermometer: Dr. Anthony Fauci, 0-100 scale,#998. Don’t know, 999. Don’t recognize
    "continuous", #V202160 POST: Feeling thermometer: feminists
    "continuous",#V202159 POST: Feeling thermometer: Christian fundamentalists
    "continuous", #V202162 POST: Feeling thermometer: labor unions
    "ordinal", #V202265 POST: Fewer problems if there was more emphasis on traditional family values, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "ordinal", #V202224 POST: How important that more women get elected to political office, #1. Extremely important, 2. Very important, 3. Moderately important, 4. A little important, 5. Not at all important
    # Review of State of Union/ Government's Job
    "ordinal", #V202427 POST: CSES5‐Q09: How good/bad a job has government done in last 4 years, #1. Very good job, 2. Good job, 3. Bad job, 4. Very bad job
    "ordinal", #V202430 POST: CSES5‐Q11: State of economy better or worse over past 12 months, #1. Gotten much better, 2. Gotten somewhat better, 3. Stayed about the same, 4. Gotten somewhat worse, 5. Gotten much worse
    "ordinal", #V202317 POST: How much opportunity in America for average person to get ahead, #1. A great deal, 2. A lot, 3. A moderate amount, 4. A little, 5. None
    "ordinal", #V202271 POST: Is the US better or worse than most other countries, #1. Better, 2. Worse, 3. The same
    "ordinal", #V202212 POST: [STD] Public officials don't care what people think, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "ordinal", #V202411 POST: CSES5‐Q04c: Attitudes about elites: most politicians are trustworthy, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "ordinal", #V202304 POST: Our political system only works for insiders with money and power, #how well does the statement describe your views #1. Not at all well, 2. Not very well, 3. Somewhat well, 4. Very well, 5. Extremely well
    # Demographic
    "ordinal", #V202355 POST: Does R currently live in a rural or urban area, #1. Rural area, 2. Small town, 3. Suburb, 4. City
    "continuous", #V202468x PRE‐POST: SUMMARY: Total (family) income, #1. <-> 21.
    # New Variables
    "continuous", #V202173 POST: Feeling thermometer: scientists
    "ordinal", #V202213 POST: [STD] Have no say about what goverment does #1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "binary", #V202253 POST: Less government better OR more that government should be doing, #1. The less government the better, 2. More things government should be doing
    "ordinal",#V202259x POST: SUMMARY: Favor/oppose government trying to reduce income inequality 1. Favor a great deal 2. Favor a moderate amount 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose a moderate amount 7. Oppose a great deal
    "ordinal", #V202260 POST: Society should make sure everyone has equal opportunity #1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "ordinal", #V202264 POST: The world is changing & we should adjust view of moral behavior 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "ordinal",#V202290x POST: SUMMARY: Better/worse if man works and woman takes care of home, #1. Much better 2. Somewhat better 3. Slightly better 4. Makes no difference 5. Slightly worse 6. Somewhat worse 7. Much worse
    "ordinal", #V202305 POST: Because of rich and powerful it's difficult for the rest to get ahead-- describes your view 1. Not at all well 2. Not very well 3. Somewhat well 4. Very well 5. Extremely well
    "ordinal",#V202308x POST: SUMMARY: Trust ordinary people/experts for public policy, #1. Trust ordinary people much more 2. Trust ordinary people somewhat more 3. Trust both the same 4. Trust experts somwhat more 5. Trust experts much more
    "ordinal", #V202309 POST: How much do people need help from experts to understand science #1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "ordinal",#V202320x POST: SUMMARY: Economic mobility, 1. A great deal easier 2. A moderate amount easier 3. A little easier 4. The same 5. A litte harder 6. A moderate amount harder 7. A great deal harder
    "ordinal", #V202332 POST: How much is climate change affecting severe weather/temperatures in US, 1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "ordinal", #V202333 POST: How important is issue of climate change to R, 1. Not at all important 2. A little important 3. Moderately important 4. Very important 5. Extremely important
    "ordinal",#V202361x POST: SUMMARY: Favor/oppose free trade agreement, 1. Favor a great deal 2. Favor moderately 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose moderately 7. Oppose a great deal
    "ordinal", #V202400 POST: How much is China a threat to the United States, 1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "ordinal", #V202407 POST: CSES5‐Q02: How closely does R follow politics in media 1. Very closely 2. Fairly closely 3. Not very closely 4. Not at all
    "ordinal", #V202410 POST: CSES5‐Q04b: Attitudes about elites: politicians do not care about people 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "ordinal", #V202412 POST: CSES5‐Q04d: Attitudes about elites: politicians are main problem in US 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "ordinal", #V202413 POST: CSES5‐Q04e: Attitudes about elites: strong leader in government is good 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "ordinal", #V202414 POST: CSES5‐Q04f: Attitudes about elites: people should make policy decisions 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "ordinal", #V202424 POST: CSES5‐Q06d: National identity: how important to follow America's customs 1. Very important 2. Fairly important 3. Not very important 4. Not important at all
    "ordinal" #V202440 POST: CSES5‐Q21: Satisfaction with democratic process 1. Very satisfied 2. Fairly satisfied 4. Not very satisfied 5. Not at all satisfied
  ))


### joint_long_df - format comparison data for further use ----------------------------------------------------------------------
# add variable names as a column first
fulldset_long <- fulldset %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(dataset = "fulldset")
# repeat for subset
subset_long <- subset %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(dataset = "subset")

# combine both datasets
joint_long_df <- bind_rows(fulldset_long, subset_long)

# merge with annotated (manual) 'variable type' info
joint_long_df <- joint_long_df %>%
  left_join(var_types_slct_2020, by = "variable")



### function - plot ordinal data (side-by-side bar chart) ---------------------------------------
plot_bar_comparison <- function(var_name) {
  joint_long_df %>%
    filter(variable == var_name) %>%
    ggplot(aes(x = as.factor(value), fill = dataset)) +
    geom_bar(
      aes(y = after_stat(prop), group = dataset),
      stat = "count",           # count the number of responses
      position = "dodge"        # put bars side-by-side instead of stacking
    ) +
    scale_fill_manual(values = c("fulldset" = "#3c5cf9", 
                                 "subset" = "#7ed994")) +
    labs(title = var_name, x = "Response", y = "Proportion") +
    theme_minimal()
}

### function - plot continuous data (overlayed density plots) ---------------------------------------
plot_density_comparison <- function(var_name) {
  joint_long_df %>%
    filter(variable == var_name, !is.na(value)) %>%
    ggplot(aes(x = as.numeric(value), fill = dataset, color = dataset)) +
    geom_density(alpha = 0.4) +
    scale_fill_manual(values = c("fulldset" = "#3a6ad1", 
                                 "subset" = "#36914c")) +
    scale_color_manual(values = c("fulldset" = "#3a6ad1", 
                                  "subset" = "#36914c")) +
    labs(title = var_name, x = "Score", y = "Density") +
    theme_minimal()
}


### function - triage variable into graph type -----------------------------------------------------
plot_by_type <- function(var_name, var_types = var_types_slct_2020) {
  var_type <- var_types %>% filter(variable == var_name) %>% pull(type)
  
  if (var_type %in% c("binary", "ordinal")) {
    plot_bar_comparison(var_name)
  } else if (var_type == "continuous") {
    plot_density_comparison(var_name)
  } else {
    message("Unknown variable type")
  }
}






### plot - big patchwork plot (all vars) ------------------------------------------------------------------
# Get all variable names
var_list <- var_types_slct_2020$variable

# Generate the list of plots
plot_list <- lapply(var_list, plot_by_type)

# Combine all plots into 8 rows x 7 columns
ggsave(here("outputs/ANES_comparison_grid_all.png"), wrap_plots(plot_list, nrow = 8, ncol = 7), width = 30, height = 40)






# COMPARISON - Statistical Independence -------------------------------------------------------------------------

### Past - T-Test on one variable -----------------------------------------------------------------
# Run t-test for variable V202430 comparing fulldset vs subset (econ better or worse)
t_test_result <- joint_long_df %>%
  filter(variable == "V202430") %>%       # focus on just this variable
  t.test(value ~ dataset, data = .)       # run the t-test comparing the two groups

# View the results
t_test_result

#V202430


# t = -2.4573
#This is the t-statistic, which measures the difference in group means relative to 
#the variability in the data. A more extreme t (positive or negative) implies a stronger 
#difference between groups.

#df = 89.282
#degrees of freedom, adjusted for unequal variances (because this is Welch’s t-test).

#p-value = 0.01593
#This is the probability of observing a difference as extreme as the one in your sample if 
#there were no true difference between groups.
#Since p < 0.05, we reject the null hypothesis — you’ve found a statistically significant difference between the two groups.

#95 percent confidence interval:
#-0.48108648 -0.05091893
#This is the confidence interval for the difference in means (fulldset - subset), 
#and since the whole interval is less than 0, this reinforces that subset tends to score higher on this item.

#fulldset mean = 3.664
#subset mean = 3.930



### Past - T-Test on all variables -----------------------------------------------------------------
#A t-test makes strong assumptions:
#It assumes interval-level measurement (equal distances between points).
#It assumes normal distribution (bell curve shape).
#It assumes mean is a meaningful summary.

# Step 1: Get a list of variables you'd like to run t-tests on
# (assuming your variable column is called "variable" and dataset is long-form)
variables_to_test <- joint_long_df %>%
  filter(type %in% c("ordinal","continuous", "binary")) %>% # doesn't filter any out
  distinct(variable) %>%
  pull(variable)

# Step 2: Run t-tests in a loop, collect results
comparison_ttest <- map_dfr(variables_to_test, function(var) {
  
  test_data <- joint_long_df %>%
    filter(variable == var)
  
  # If fewer than 2 groups, skip (helps prevent errors)
  if (length(unique(test_data$dataset)) < 2) return(NULL)
  
  # Run Welch's t-test
  t_test <- tryCatch({
    t.test(value ~ dataset, data = test_data)
  }, error = function(e) return(NULL))
  
  if (is.null(t_test)) return(NULL)
  
  # Extract stats using broom::tidy
  tidy_result <- broom::tidy(t_test)
  
  tibble(
    variable = var,
    t_statistic = tidy_result$statistic,
    df = tidy_result$parameter,
    p_value = tidy_result$p.value,
    mean_fulldset = mean(test_data$value[test_data$dataset == "fulldset"], na.rm = TRUE),
    mean_subset = mean(test_data$value[test_data$dataset == "subset"], na.rm = TRUE),
    conf_low = tidy_result$conf.low,
    conf_high = tidy_result$conf.high
  )
})



### Binary - Fisher's Exact  -----------------------------------------------------------
# step 1: filter variables to only binary variables 
binary_vars <- joint_long_df %>%
  group_by(variable) %>%
  filter(!is.na(value)) %>% # key!!! otherwise NA would be counted as a 3rd level
  summarise(n_levels = n_distinct(value)) %>% 
  filter(n_levels == 2) %>% # (only variables with 2 levels, aka 1 and 0)
  pull(variable)

# step 2: Function to run Fisher's Test and extract results
### function - apply Fisher's test to binary variables -----------------------------------
run_fisher_test <- function(var_name) {
  tab <- joint_long_df %>%
    filter(variable == var_name) %>%
    filter(!is.na(value)) %>%
    count(dataset, value) %>%
    pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>%
    column_to_rownames("value") %>%
    as.matrix()
  
  test_result <- fisher.test(tab)
  
  tibble(
    variable = var_name,
    p_value = test_result$p.value,
    odds_ratio = test_result$estimate,
    conf_low = test_result$conf.int[1],
    conf_high = test_result$conf.int[2]
  )
}

### comparison_fisher - results of fisher test --------------------------------------------------
# Step 3: Apply across all binary variables
comparison_fisher <- map_dfr(binary_vars, run_fisher_test)


# Fisher’s test calculates the exact probability of obtaining the 
# observed table (and more extreme tables) under the null hypothesis of independence.
# The probability of observing a specific table under the null:

# The p-value is the sum of probabilities of all tables as extreme or more extreme than the observed one.
# Uses the hypergeometric distribution.
# No chi-square approximation or large sample assumption — that's why it's “exact.”







### Ordinal - Wilcoxon Rank-Sum Test --------------------------------------------------

# Wilcox is a Non-Parametric Alternative
# aka doesnt assume normality of distribution (parametric) and 'honours' ordinality more strictly
# compares ranks between two groups — perfect for your scenario.

# Gives you a p-value indicating group differences
# It tests whether values from one group tend to be higher or lower than 
# the other — without assuming equal distances between categories.

# Wilcoxon algorithm:
  # Combines both groups into a single pool.
  # Ranks all values from lowest to highest (regardless of group).
  # Sums the ranks for each group.
  # Checks whether the sum of ranks differs more than we’d expect by chance.
  # So instead of comparing means, it's comparing distributional shifts. 
# It asks:
# “Is one group generally giving higher or lower responses than the other?”

# wilcoxon effect on important aspects of the numbers:
#Ordinal: Since it uses ranks, it respects the order of values without assuming equal spacing.
#Skewed distributions: No need for normality — skew, kurtosis, or multi-modal shapes don’t matter.
#Outliers: Not as sensitive as t-tests, because outliers only affect ranks slightly.
#Multi-modality: Still effective, because it's not estimating a single center like a mean, but rather comparing the overall "tendency to rank higher/lower."

#A t-test could mislead you by overemphasizing the “mean” which may sit at 4 (neutral) even if there's bimodality.
#A Wilcoxon test will detect whether one group tends to choose more favorable values than 
# another — even if both groups are bi-modal or skewed.
#So for this variable, the Wilcoxon test will tell you whether the distribution of opinions is 
# meaningfully shifted between fullset and subset — without assuming the data looks like 
# a normal curve or that “Favor a great deal” is exactly 6 steps away from “Oppose a great deal”.

# Caveat:
# It doesn’t tell you how the distributions differ — just that one is generally “higher” or “lower.”
# You might still want to plot distributions or calculate medians to add context.


# Step 1: Filter to ordinal variables (e.g., 3–10 levels, all integers)
ordinal_vars <- joint_long_df %>%
  group_by(variable) %>%
  filter(!is.na(value)) %>%
  summarise(n_levels = n_distinct(value)) %>%
  filter(n_levels >= 3, n_levels <= 100) %>%  # adjust this range as needed
  pull(variable)

### function - apply Wilcoxon's test to ordinal variables -----------------------------------
# Step 2: Function to run Wilcoxon Test and extract results
run_wilcox_test <- function(var_name) {
  data <- joint_long_df %>%
    filter(variable == var_name, !is.na(value))
  
  test_result <- wilcox.test(value ~ dataset, data = data)
  
  medians <- data %>% # add mean as well
    group_by(dataset) %>%
    summarise(mean = mean(value)) %>%
    pivot_wider(names_from = dataset, values_from = mean)
  
  tibble(
    variable = var_name,
    p_value = test_result$p.value,
    mean_fulldset = mean$fulldset,
    mean_subset = mean$subset
  )
}


### comparison_wilcox - results of wilcox test --------------------------------------------------
# Step 3: Apply across all ordinal variables
comparison_wilcox <- map_dfr(ordinal_vars, run_wilcox_test)

#p_value: Whether the distributions differ significantly between groups.
#statistic: Wilcoxon W value (used internally in calculating the p-value).

#You can tweak the level threshold in filter(n_levels >= 3, n_levels <= 10) depending on how wide your ordinal scales are.
#This test assumes that while ordinal, your values have a ranking (e.g., 1 = strongly disagree, 5 = strongly agree).
#It does not assume equal intervals between the ranks — unlike a t-test.



### graphing wilcox ------------------------------------------------------------------------
plot_shift_violin <- function(var_name, df = joint_long_df) {
  df %>%
    filter(variable == var_name, !is.na(value)) %>%
    ggplot(aes(x = dataset, y = value, fill = dataset)) +
    # Violin for distribution
    geom_violin(alpha = 0.4, color = NA, trim = FALSE) +
    # Stripchart of individual points
    geom_jitter(aes(color = dataset), width = 0.15, alpha = 0.6, size = 0.5) +
    # Median lines
    stat_summary(fun = median, geom = "crossbar", width = 0.5,
                 color = "black", fatten = 0, linetype = "dashed") +
    labs(title = var_name,
         y = "Response (Ordinal Scale)",
         x = NULL) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("fulldset" = "#3a6ad1", "subset" = "#36914c")) +
    scale_color_manual(values = c("fulldset" = "#3a6ad1", "subset" = "#36914c"))
}

plot_shift_violin("V202259x")

# "#1f77b4"
# "#3a6ad1", 
