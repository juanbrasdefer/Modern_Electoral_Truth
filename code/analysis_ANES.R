# libraries, directory ------------------------------------------------------------
library(tidyverse)
library(here)
library(patchwork)

here::i_am("code/analysis_ANES.R")
options(scipen = 999)


# load data -----------------------------------------------------------------------

ANES_2020_raw <- read_csv(here("data/ANES/2020/anes_timeseries_2020_csv_20220210/anes_timeseries_2020_csv_20220210.csv"))






# Clean 2020 RAW ANES  ------------------------------------------------------------------

ANES_2020_clean <- ANES_2020_raw %>%
  mutate(across(everything(), ~ ifelse(. < 0, # re-coding the ANES missing values as NA
                                       NA, .))) %>% # ANES uses -9, -8, -7 etc to mean different NA reasons
  mutate(across(everything(), ~ ifelse(. > 997, # also, in feeling-thermometer questions, a value of 998 or 999 means they didnt
                                       NA, .))) # know of the subject being discussed (like: do you like feminism? i dont recognize that word)




# Experiment Cont: Taking only select columns from full dset ------------------------------------

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
        V202440, #V202440 POST: CSES5‐Q21: Satisfaction with democratic process 1. Very satisfied 2. Fairly satisfied 4. Not very satisfied 5. Not at all satisfied
      )



colSums(is.na(ANES_2020_clean_slct)) # of our voter belief/ attribute columns, max missing is ~1060
# average is about 850 missing values, so about 10%
# aka no hugely worrisome columns




# Experiment: Voted for Change in Pres, 2020 and two prev elecs -------------------------
change_cand_20_20 <- 1 # Change candidate in 2020 in the 2020 ANES dataset
# Joe Biden is coded as 1 in this dataset
change_cand_16_20 <- 2 # Change candidate in 2016 in the 2020 ANES dataset
# Donald Trump is code as 2 in this dataset
change_cand_12_20 <- 1 # Change candidate in 2012 in the 2020 ANES dataset
# Barack Obama is code as 1 in this dataset

# df: find voters that voted Obama -> Trump -> Biden
voted_change_20_allelecs_slct <- ANES_2020_clean_slct %>%
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
  filter(V201105 == change_cand_12_20)        # V201105 - PRE: recall of 2012 presidential vote choice
# Filter: VOTED for Barack Obama
# N: 156->89 (57% voted Obama)
# meaning that we have 89 people in this 
# dataset that voted Obama -> Trump -> Biden  
# let's see who they are!!


# 2020 Select Variables - Comparison between full dataset and change voters -------------------------------------------------
voted_ANES_2020_clean_slct <- ANES_2020_clean_slct %>%
  filter(V202072 == 1) 
# since we are going to be looking for indicators that tell us why people vote the way they do
# it makes sense to drop all observations that didnt vote in 2020
# since we don't have the counterfactual for these people of "who would they have voted for"

# Drop the ID column
fulldset <- voted_ANES_2020_clean_slct[ , -1]
subset <- voted_change_20_allelecs_slct[ , -1]

# Function to compute summary stats depending on variable type
get_summary <- function(vec) {
  vec <- na.omit(vec)
  n_unique <- length(unique(vec))
  # Treat as ordinal/categorical
  return(as.list(prop.table(table(vec))))
  # Continuous
  #return(list(mean = mean(vec), sd = sd(vec), median = median(vec)))
}

# Wrapper to apply across dataset
summarize_dataset <- function(df) {
  map(df, get_summary)
}

# Apply to both datasets
fulldset_summary <- summarize_dataset(fulldset)
subset_summary <- summarize_dataset(subset)

# Combine into a tibble with variable names
combined_summary <- tibble(
  variable = names(fulldset_summary),
  fulldset = fulldset_summary,
  subset = subset_summary
)

# Expand into long format with new naming scheme
comparison_df <- combined_summary %>%
  unnest_wider(fulldset, names_sep = "_") %>%
  unnest_wider(subset, names_sep = "_")

# Rename columns so numbers come first
colnames(comparison_df) <- ifelse(
  colnames(comparison_df) == "variable",
  "variable",
  paste0(str_extract(colnames(comparison_df), 
                     "\\d+"), "_", 
         str_extract(colnames(comparison_df), "fulldset|subset"))
)

# Get the names of all columns except the 'variable' column
all_cols <- colnames(comparison_df)
data_cols <- setdiff(all_cols, "variable")

# Extract the number and type (fulldset/subset) to sort properly
col_order <- tibble(
  name = data_cols,
  varnum = as.numeric(str_extract(data_cols, "^\\d+")),
  type = str_extract(data_cols, "fulldset|subset")
)

# Arrange by varnum then type so we get: 1_fulldset, 1_subset, 2_fulldset, 2_subset...
sorted_names <- col_order %>%
  arrange(varnum, type) %>%
  pull(name)

# Reorder the original dataframe
comparison_df <- comparison_df %>%
  select(variable, all_of(sorted_names))


# 2020 Select Variables - Graphing Comparison between full dataset and change voters -------------------------------------------------

# manually defining types
var_types_slct_2020 <- tibble::tibble(
  variable = c(
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



# Add variable names as a column first
fulldset_long <- fulldset %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(dataset = "fulldset")

subset_long <- subset %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(dataset = "subset")

# Combine them
long_df <- bind_rows(fulldset_long, subset_long)

# Merge with type info
long_df <- long_df %>%
  left_join(var_types_slct_2020, by = "variable")



# graphing 

plot_bar_comparison <- function(var_name) {
  long_df %>%
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

plot_density_comparison <- function(var_name) {
  long_df %>%
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






# big patchwork plot ----------------------
# Get all variable names
var_list <- var_types_slct_2020$variable

# Generate the list of plots
plot_list <- lapply(var_list, plot_by_type)

# Combine all plots into 5 rows x 6 columns
ggsave(here("outputs/ANES_comparison_grid_newvars.png"), wrap_plots(plot_list, nrow = 5, ncol = 6), width = 20, height = 15)

# T-TEST experiments -----------------------------------------------------------------

# Run t-test for variable V202430 comparing fulldset vs subset
t_test_result <- long_df %>%
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




# TO CREATE A LARGER TABLE WITH ALL VARS

# Step 1: Get a list of variables you'd like to run t-tests on
# (assuming your variable column is called "variable" and dataset is long-form)
variables_to_test <- long_df %>%
  filter(type %in% c("ordinal","continuous")) %>% # or "continuous", or omit this if you want all
  distinct(variable) %>%
  pull(variable)

# Step 2: Run t-tests in a loop, collect results
t_test_summary <- map_dfr(variables_to_test, function(var) {
  
  test_data <- long_df %>%
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



# BINARY VAR COMPARISONS Experiment ---------------------------------------------------



# Step 1: Define binary variables (only 1 and 2, no 3+ levels)
binary_vars <- long_df %>%
  group_by(variable) %>%
  filter(!is.na(value)) %>%
  summarise(n_levels = n_distinct(value)) %>%
  filter(n_levels == 2) %>%
  pull(variable)

# Step 2: Function to run Fisher's Test and extract results
run_fisher_test <- function(var_name) {
  tab <- long_df %>%
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

# Step 3: Apply across all binary variables
fisher_results_df <- map_dfr(binary_vars, run_fisher_test)

# View results
print(fisher_results_df)





