
# Script Setup ----------------------------------------------------------------------
### libraries, directory ------------------------------------------------------------
library(tidyverse)
library(here)
library(patchwork)


here::i_am("code/generalization_experiment_ANES.R")

# load data ------------------------------------------------------------------

# load reference dataframe that will help us deal with the fact that
# each of the surveys use different naming scheme for variables
var_names_ref <- read_csv(here("data/ref/ANES_generalization_exp_reftable.csv"))



# load years of ANES surveys
ANES_2016_raw <- read.delim(here("data/ANES/2016/anes_timeseries_2016/anes_timeseries_2016_rawdata.txt"),
                            sep = "|", header = TRUE)  #a tab-separated
ANES_2012_raw <- read.delim(here("data/ANES/2012/anes_timeseries_2012/anes_timeseries_2012_rawdata.txt"), 
                            sep = "|", header = TRUE)  # tab-separated
ANES_2008_raw <- read.delim(here("data/ANES/2008/anes_timeseries_2008/anes_timeseries_2008_rawdata.txt"), 
                            sep = "|", header = TRUE)  # tab-separated
ANES_2004_raw <- read.delim(here("data/ANES/2004/anes2004/nes04dat.txt"), 
                            sep = ",", header = TRUE)  # comma separated
ANES_2000_raw <- read.delim(here("data/ANES/2000/anes_2000prepost/anes_2000prepost_dat.txt"), 
                            sep = ",", header = TRUE)  # comma separated




# filter 2016 ------------------------------------------------

ANES_2016_clean <- ANES_2016_raw %>%
  mutate(V161027_V162034a_summary = ifelse(V161027 %in% c(1,2,3,4,5), V161027,
                                           ifelse(V162034a %in% c(1,2,3,4,5), V162034a,
                                                  NA)))






# fancy ting -------------------------------------------------------


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
  
  
  
# dataset <- ANES_2016_clean
# var_to_find <- "votedpres_yn"
# year_of_dset <- 2016


ANES_2016_filtered <- ANES_2016_clean %>%
  filter_ANES_dset("votedpres_yn", 2016)


ANES_2016_filtered

## 2000 \------------------------- ------------------------------

### 2000 Presidential  ------------------------------------
# V001241 - Did R Vote
#1. I DID NOT VOTE (IN THE ELECTION THIS NOVEMBER).
#2. I THOUGHT ABOUT VOTING THIS TIME, BUT DIDN'T.
#3. I USUALLY VOTE, BUT DIDN'T THIS TIME.
#4. I AM SURE I VOTED

# V001248 - Did R vote for a presidential candidate
#1. YES, VOTED FOR PRESIDENT
#5. NO, DIDN'T VOTE FOR PRESIDENT

# V001249 - Who did R vote for in Presidential race
#1. AL GORE
#3. GEORGE W. BUSH
#5. PAT BUCHANAN
#6. RALPH NADER
#2. HOWARD PHILLIPS - CONSTITUTION PARTY CANDIDATE 4. HARRY BROWN - LIBERTARIAN CANDIDATE
#7. R REPORTS VOTING FOR SELF

### 2000 Respondent Attributes -------------------------------------------

# for visual scanning of variables available in dataset
#raw_lines <- readLines(here("data/ANES/2000/anes_2000prepost/anes_2000prepost_var.txt"))
# Use a regular expression to detect lines that start with V + digits
#v_lines <- grep("^V\\d+", raw_lines, value = TRUE)
# Convert to a data frame
#v_df <- tibble(variable_code = v_lines)



# "belief_in_science"
# V000777 How important is envir regulation
# 1. NOT AT ALL IMPORTANT 2. NOT TOO IMPORTANT 3. SOMEWHAT IMPORTANT 4. VERY IMPORTANT 5. EXTREMELY IMPORTANT 8. DK 9. RF0. NA

# "climate_fears"8
# V000682 Should federal spending on ENVIRONMENTAL PROTECTION be increased, decreased, or kept about the same
# 1. INCREASED 3. DECREASED 5. KEPT ABOUT THE SAME 7. CUT OUT ENTIRELY [VOL]

# "economic_hardship"
# V000994 HH income -all HHs (not sure which var is correct here)
# V000993 Pre-Tax Household Income
# 1.NONE OR LESS THAN $4,999 2.$5,000-$9,999 3. C. $10,000-$14,999 4. D. $15,000-$24,999 5. $25,000-$34,999 6. $35,000-$49,999 7. $50,000-$64,999 8. $65,000-$74,999 9. $75,000-$84,999 10. $85,000-$94,999 11. $95,000-$104,999 12. $105,000-$114,999 13. $115,000-$124,999 14. $125,000-$134,999 15. $135,000-$144,999 16. $145,000-$154,999 17. $155,000-$164,999 18. $165,000-$174,999 19. $175,000-$184,999 20. $185,000-$194,999 21. $195,000-$199,999 22. $200,000 and over
# V000491 Summary US econ btr/worse last year

# "political_cynicism"
# V001307 Thermometer federal govt in Wash DC
# V000369 Thermometer Dem Party
# V000370 Thermometer Rep Party
# V000372 Thermometer parties in general

# "democratic_apathy"
# V001651 Is R satisfied with US Democracy
# V001304 Thermometer supreme court
# V001429 How much can you trust the media


## 2004 \------------------------- ----------------------------------------------
### 2004 Presidential ------------------------------------
# V045017a - Did R vote in this election (standard q)
#1. Yes, voted
#5. No, didn't vote

# V045017b - Did R vote in this election (experimental q)
#1. I did not vote (in the election this November)
#2. I thought about voting this time, but didn't
#3. I usually vote, but didn't this time
#4. I am sure I voted

# V045025 - Did R vote for a presidential candidate
# 1. Yes, voted for President
# 5. No, didn't vote for President

# V045026 - R's vote for President
# 1. John Kerry
# 3. George W. Bush
# 5. Ralph Nader
# 7. Other {SPECIFY}



### 2004 Respondent Attributes ----------------------------------


# "belief_in_science"
# V043167 Federal Budget Spending: science and technology
# V045072 Feeling Thermometer: environmentalists

# "climate_fears"
#V043182 Environment vs. jobs tradeoff scale - self-placement


# "economic_hardship"
# V043293x Summary: Household income
# V045064 Feeling Thermometer: Labor Unions
# V043214 How much national economy better/worse last 4 years

# "political_cynicism"
# V045060 Feeling Thermometer: Federal Government in Washington
# V043049 Feeling Thermometer: Democratic party
# V043050 Feeling Thermometer: Republican party

# "democratic_apathy"
# V045241 How satisfied with democracy in US
# V045242 Makes a difference who is in power
# V045244 Democracy is best form of govt
# V045073 Feeling Thermometer: U.S. Supreme Court






## 2008 \------------------------- ----------------------------------------------
### 2008 Presidential  ------------------------------------

# V085044 - Did R vote for candidate for President
# 1. Yes, voted for President
# 5. No, didn't vote for President

#V085044a - For whom did R vote for President
# 1. Barack Obama
# 3. John McCain
# 7. Other {SPECIFY}


### 2008 Respondent Attributes -------------------------------------

# "belief_in_science"
# V084405s ENVIRONMENTALISTS thermometer
# V085064s Feeling thermometer: ENVIRONMENTALISTS
# V084405e Federal Budget Spending: science and technology

# "climate_fears"
# V083157 [NEW] Favor/oppose lower emission standards
# V083157x [NEW] SUMMARY: favor/oppose lower emission standards ds
# V083158 [NEW] Importance of emission standards issue
# V083151x SUMMARY: increase or decrease spending on environment

# "economic_hardship"
# V084405j LABOR UNIONS thermometer
# V083248 Family income
# V083248x SUMMARY: FAMILY INCOME
# V085080 Income gap today more or less than 20 years ago
# V085080x SUMMARY: INCOME GAP COMPARED TO 20 YRS AGO


# "political_cynicism"
# V083044a Feeling Thermometer: Democratic Party
# V083044b Feeling Thermometer: Republican Party
# V085148 Govt run by a few big interests or for benefit of all

# "democratic_apathy"
# V084405t U.S. SUPREME COURT thermometer (random order q... idk)
# V085064t Feeling thermometer: THE U.S. SUPREME COURT
# V085182 Does/doesn't make a difference who is in power







## 2012 \------------------------- ----------------------------------------------

### 2012 Presidential  ------------------------------------
# postvote_regpty - which party is R registered to vote for
#1. Democratic party
#2. Republican party
#4. None or 'independent'
#5. Other party (SPECIFY)

# postvote_rvote - Did R vote
# 1. I did not vote (in the election this november)
# 2. I thought about voting this time, but didn't
# 3. I usually vote, but didn't this time
# 4. I am sure I voted

# postvote_presvt - Did R vote for President
# 1. Yes, voted for President
# 2. No, didn't vote for President

# postvote_presvtwho - For whom did R vote for President
# 1. <preload: dem_pcname>
# 2. <preload: rep_pcname
# 5. Other candidate (SPECIFY)

### 2012 Respondent Attributes -------------------------------------

# "belief_in_science"
# envir_gwarm - Is global warming happening or not
# envir_gwhow - Anthropogenic climate change (ie: humans cause it)


# "climate_fears"
# envir_gwgood - temperatures continue to go up in the future, would this be good, bad, or neither good nor bad
# envjob_self - environment-jobs tradeoff self-placement

# "economic_hardship"
# ftgr_unions - Feeling thermometer: LABOR UNIONS
# cses_econ - State of economy
# econ_ecpast_x - PRE: SUMMARY- U.S. economy better or worse than 1 year ago
# ineq_incgap_x - PRE: SUMMARY- Income gap size compared to 20 years ago
# finance_finpast_x - PRE: SUMMARY- Better or worse off than 1 year ago
#  ################Family income RESTRICTED IN 2012


# "political_cynicism"
# cses_selfleft - left-right self placement
# cses_diffpower - make a difference who is in power
# cses_closepty - Close to any political party
# ft_dem - Feeling Thermometer: Republican Party
# ft_rep - Feeling Thermometer: Republican Party

# "democratic_apathy"
#  ftgr_ussc - Feeling thermometer: THE U.S. SUPREME COURT
#  cses_diffpower - make a difference who is in power
#  cses_satisdem - Satisfied with way democracy works in the U.S



## 2016 \------------------------- -----------------------------

#length(unique(df$V160001)) 
# gives 4270
# unique identifier column

### 2016 Presidential ----------------------------------------------
# V161019 - PRE: Party of registration
# 1. Democratic party 
# 2. Republican party
# 4. None or ‘independent’
# 5. Other SPECIFY

# V162031x - PRE-POST: SUMMARY -Did R vote in 2016
# 0 did not vote in 2016
# 1 did vote in 2016

# V162034 - POST: Did R vote for President
#1. Yes, voted for President 
# 2. No, didn’t vote for President

# V161027 - PRE: For whom did R vote for President
# 1. Hillary Clinton 2. Donald Trump 3. Gary Johnson 4. Jill Stein 5. Other candidate (SPECIFY)

# V162034a - POST: For whom did R vote for President
# 1. Hillary Clinton
# 2. Donald Trump
# 3. Gary Johnson
# 4. Jill Steiin
# 5. Other candidate SPECIFY
# 7. Other specify given as: none
# 9. Other specify given as: RF



### 2016 Respondent Attributes -------------------------------------
# "belief_in_science"
# V162112 POST: Feeling thermometer: SCIENTISTS
# V161207 PRE: Federal Budget Spending: science and technology
# V161221 - PRE: Is global warming happening or not
# V161222 PRE: Anthropogenic climate change


# "climate_fears"
# V161225x - PRE: SUMMARY - Govt action about rising temperatures


# "economic_hardship"
# V161110 - PRE: R how much better worse off than 1 year ago
# V161140x - PRE: SUMMARY - economy better/worse in last year
# V161138x - PRE: SUMMARY - larger/smaller income gap today
# V162136x - POST: SUMMARY- Economic mobility easier/harder compared to 20 yrs ago
# V161361x - PRE: income summary

# "political_cynicism"
# V161126 - PRE: 7pt scale Liberal conservative self-placement
# V161155 - PRE: Party ID: Does R think of self as Dem, Rep, Ind or what
# V161095 - PRE: Feeling Thermometer: Democratic Party
# V161096 - PRE: Feeling Thermometer: Republican Party
# V161215 - PRE: REV How often trust govt in Wash to do what is right
# V161216 - PRE: Govt run by a few big interests or for benefit of all


# "democratic_apathy"
# V161151x - PRE: SUMMARY - Voting as duty or choice
# V162290 - POST: CSES: Satisfied with way democracy works in the U.S




