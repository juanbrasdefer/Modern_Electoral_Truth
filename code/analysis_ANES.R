
# Script Setup ----------------------------------------------------------------------
### libraries, directory ------------------------------------------------------------
library(tidyverse)
library(here)
library(patchwork)
library(DT)
library(webshot)
library(webshot2)
library(gt)
library(gtExtras) 
library(flextable)
library(officer)

here::i_am("code/analysis_ANES.R")
options(scipen = 999)



### load data -----------------------------------------------------------------------
ANES_2020_raw <- read_csv(here("data/ANES/2020/anes_timeseries_2020_csv_20220210/anes_timeseries_2020_csv_20220210.csv"))

source(here("code/scripts/var_attributes_2020.R"))


# DATASET Cleaning and Filtering ----------------------------------------------------------------------

### ANES_2020_clean - Standardizing alt codings of NAs  ------------------------------------------------------------------
ANES_2020_clean <- ANES_2020_raw %>%
  mutate(V200001 = as.character(V200001)) %>%
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
  select(V200001, # Case ID
      # Voter election history variables
      V202072, # V202072 - POST: Did R vote for President in this election
      V202073, # V202073 - POST: For whom did R vote for President
      V201101_V201102_summary, # Joint V201101 and V201102 - PRE: Did R vote for President in 2016 
      V201103, # V201103 - PRE: Recall of last (2016) Presidential vote choice
      V201104, # V201104 - PRE: Did R vote for president in 2012 election
      V201105, # V201105 - PRE: recall of 2012 presidential vote choice
      # Variables of interest, taken from source() at start of script
      all_of(var_attributes_2020$variable))



colSums(is.na(ANES_2020_clean_slct)) 
# of our voter belief/ attribute columns, max missing is ~1060
# average is about 850 missing values, so about 10%
# aka no hugely worrisome columns




### voted_change_20_allelecs_slct - Create CVoters: Voted for Change in 2020, 16, 12 -------------------------
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
# note: this droplist is used here and later on MVoters as well

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
MVoters <- voted_ANES_2020_clean_slct[ , -1]   # drop the ID column
MVoters_summary <- summarize_dataset(MVoters)   # and apply custom function (wrapper and get_summary())

CVoters <- voted_change_20_allelecs_slct[ , -1] # repeat for sub
CVoters_summary <- summarize_dataset(CVoters) # same wrapper and summary function


# combine into tibble (similar to list) for storage
# variable names as rows, nested structure for values
combined_summary <- tibble(
  variable = names(MVoters_summary),
  MVoters = MVoters_summary,
  CVoters = CVoters_summary)

# then turn into long format
comparison_sumstats <- combined_summary %>%
  unnest_wider(MVoters, names_sep = "_") %>% # new naming scheme for variables
  unnest_wider(CVoters, names_sep = "_")

# for ease of reordering columns:
# rename so number in var name come first
colnames(comparison_sumstats) <- ifelse(
  colnames(comparison_sumstats) == "variable", "variable",  # ifelse() is used to skip over our first column, called 'variable'
  paste0(str_extract(colnames(comparison_sumstats), "\\d+"), "_", str_extract(colnames(comparison_sumstats), "MVoters|CVoters"))
  # for all other columns, rename to bin_var naming convention
  # where bin is ordinal, binary, semi-continuous bin
  )

# fetch names of all columns except the 'variable' column
all_cols <- colnames(comparison_sumstats)
data_cols <- setdiff(all_cols, "variable") # drop col named "variable"

# extract the number and type (MVoters/CVoters) to sort properly
col_order <- tibble(name = data_cols,
  varnum = as.numeric(str_extract(data_cols, "^\\d+")),
  dset = str_extract(data_cols, "MVoters|CVoters"))

# arrange columns by bins and dataset, meaning we get: 
  # 'variable', '1_MVoters', '1_CVoters', '2_MVoters', '2_CVoters'...
  # where variable has all our variables as rows
  # and the '1_MVoters' column holds values for all variables in MVoters that have '1' in their scale
sorted_names <- col_order %>%
  arrange(varnum, dset) %>%
  pull(name)

# finally, reorder the original dataframe
comparison_sumstats <- comparison_sumstats %>%
  select(variable, all_of(sorted_names))


# COMPARISON - Graph Overlays -------------------------------------------------------------------------

# 2020 Select Variables - Graphing Comparison between datasets of MVoters and CVoters (change voters)

### annotate variable numerical types (binary, ordinal, cont.) --------------------------------------------------
# 


### joint_long_df - format comparison data for further use ----------------------------------------------------------------------
# add variable names as a column first
MVoters_long <- MVoters %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(Voter_Group = "MVoters")
# repeat for CVoters
CVoters_long <- CVoters %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(Voter_Group = "CVoters")

# combine both datasets
joint_long_df <- bind_rows(MVoters_long, CVoters_long)

# merge with annotated (manual) 'variable type' info
joint_long_df <- joint_long_df %>%
  left_join(var_attributes_2020, by = "variable")


### function - plot ordinal data (side-by-side bar chart) ---------------------------------------
plot_bar_comparison <- function(var_name, data_df_long = joint_long_df, var_attributes_df = var_attributes_2020) {
  var_name <- as.character(var_name)
  plot_title <- var_attributes_df %>% 
    filter(variable == var_name) %>% 
    pull(description)
  
  data_df_long %>%
    filter(variable == var_name) %>%
    ggplot(aes(x = as.factor(value), fill = Voter_Group)) +
    geom_bar(
      aes(y = after_stat(prop), group = Voter_Group),
      stat = "count",
      position = "dodge"
    ) +
    scale_fill_manual(values = c("MVoters" = "#3c5cf9", 
                                 "CVoters" = "#7ed994")) +
    labs(title = plot_title, subtitle = var_name,x = "Response", y = "Proportion") +
    theme_minimal()
}

### function - plot continuous data (overlayed density plots) ---------------------------------------
plot_density_comparison <- function(var_name, data_df_long = joint_long_df, var_attributes_df = var_attributes_2020) {
  var_name <- as.character(var_name)
  
  plot_title <- var_attributes_df %>% 
    filter(variable == var_name) %>% 
    pull(description)
  
  data_df_long %>%
    filter(variable == var_name, !is.na(value)) %>%
    ggplot(aes(x = as.numeric(value), fill = Voter_Group, color = Voter_Group)) +
    geom_density(alpha = 0.4, adjust = 0.5) +
    #geom_histogram(aes(y = after_stat(density)), bins = 50, position = "identity", alpha = 0.2) +
    scale_fill_manual(values = c("MVoters" = "#3a6ad1", 
                                 "CVoters" = "#36914c")) +
    scale_color_manual(values = c("MVoters" = "#3a6ad1", 
                                  "CVoters" = "#36914c")) +
    labs(title = plot_title, subtitle = var_name, x = "Score", y = "Density") +
    theme_minimal()
}



### function - triage variable into graph type -----------------------------------------------------
plot_by_type <- function(var_name, var_attributes_df = var_attributes_2020, numerical_type = var_attributes_2020$numerical_type) {
  var_name <- as.character(var_name)
  var_type <- var_attributes_df %>% 
    filter(variable == var_name) %>% 
    pull(numerical_type)
  
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
var_list <- var_attributes_2020$variable

# Generate the list of plots
#plot_list <- lapply(var_list, plot_by_type)

# Combine all plots into 8 rows x 7 columns
#ggsave(here("outputs/ANES_comparison_grid_all.png"), wrap_plots(plot_list, nrow = 8, ncol = 7), width = 30, height = 40)






# COMPARISON - Statistical Independence -------------------------------------------------------------------------

### Past - T-Test on one variable -----------------------------------------------------------------
# Run t-test for variable V202430 comparing MVoters vs CVoters (econ better or worse)
#t_test_result <- joint_long_df %>%
 # filter(variable == "V202430") %>%       # focus on just this variable
  #t.test(value ~ Voter_Group, data = .)       # run the t-test comparing the two groups

# View the results
#t_test_result

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
#This is the confidence interval for the difference in means (MVoters - CVoters), 
#and since the whole interval is less than 0, this reinforces that CVoters tends to score higher on this item.

#MVoters mean = 3.664
#CVoters mean = 3.930



### Past - T-Test on all variables -----------------------------------------------------------------
#A t-test makes strong assumptions:
#It assumes interval-level measurement (equal distances between points).
#It assumes normal distribution (bell curve shape).
#It assumes mean is a meaningful summary.

# Step 1: Get a list of variables you'd like to run t-tests on
# (assuming your variable column is called "variable" and dataset is long-form)
variables_to_test <- joint_long_df %>%
  filter(numerical_type %in% c("ordinal","continuous", "binary")) %>% # doesn't filter any out
  distinct(variable) %>%
  pull(variable)

# Step 2: Run t-tests in a loop, collect results
comparison_ttest <- map_dfr(variables_to_test, function(var) {
  
  test_data <- joint_long_df %>%
    filter(variable == var)
  
  # If fewer than 2 groups, skip (helps prevent errors)
  if (length(unique(test_data$Voter_Group)) < 2) return(NULL)
  
  # Run Welch's t-test
  t_test <- tryCatch({
    t.test(value ~ Voter_Group, data = test_data)
  }, error = function(e) return(NULL))
  
  if (is.null(t_test)) return(NULL)
  
  # Extract stats using broom::tidy
  tidy_result <- broom::tidy(t_test)
  
  tibble(
    variable = var,
    t_tstat = round(tidy_result$statistic, 2),
    t_degfree = round(tidy_result$parameter, 1),
    t_pval = round(tidy_result$p.value, 4),
    t_meanMVoters = round(mean(test_data$value[test_data$Voter_Group == "MVoters"], na.rm = TRUE), 2),
    t_meanCVoters = round(mean(test_data$value[test_data$Voter_Group == "CVoters"], na.rm = TRUE), 2),
    t_conflow = round(tidy_result$conf.low, 2),
    t_confhigh = round(tidy_result$conf.high, 2)
  )
})



### Binary - Fisher's Exact  -----------------------------------------------------------

# p values
# Small values (e.g. < 0.05) mean meaningful differences in the distribution of 1s and 2s between the Voter_Groups.
# Big values (e.g. > 0.10) mean likely similar distributions in your fullset vs CVoters.

# odds ratio
# How much more likely the outcome is in one group compared to the other.
# ==1: No difference in likelihood between groups.
# > 1: Outcome is more likely in one group (often CVoters if coded that way).
# < 1: Outcome is less likely in that group.
# For example, if odds_ratio = 2.5, then respondents in one group are 2.5x more 
# likely to select "Yes" (or 1) than the other group.

# confidence interval
# tells you A range of plausible values for the true odds ratio with 95% confidence.
# How to interpret:
# If the interval includes 1, it suggests no significant effect, 
# even if the point estimate (odds_ratio) looks dramatic.

# If the interval does not include 1, the odds ratio is likely a real difference.
# Wide intervals suggest uncertainty in the effect size, often due to small sample sizes (like your CVoters with 89 obs).

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
    count(Voter_Group, value) %>%
    pivot_wider(names_from = Voter_Group, values_from = n, values_fill = 0) %>%
    column_to_rownames("value") %>%
    as.matrix()
  
  test_result <- fisher.test(tab)
  
  tibble(
    variable = var_name,
    f_pval = round(test_result$p.value, 4),
    f_oddsratio = round(test_result$estimate, 2),
    f_conflow = round(test_result$conf.int[1], 2),
    f_confhigh = round(test_result$conf.int[2], 2)
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
# meaningfully shifted between fullset and CVoters — without assuming the data looks like 
# a normal curve or that “Favor a great deal” is exactly 6 steps away from “Oppose a great deal”.

# Caveat:
# It doesn’t tell you how the distributions differ — just that one is generally “higher” or “lower.”
# You might still want to plot distributions or calculate medians to add context.


# Step 1: Filter to ordinal variables (e.g., 3–10 levels, all integers)
ordinal_vars <- joint_long_df %>%
  group_by(variable) %>%
  filter(!is.na(value)) %>%
  summarise(n_levels = n_distinct(value)) %>%
  filter(n_levels >= 2, n_levels <= 101) %>%  # adjust this range as needed
  pull(variable)

### function - apply Wilcoxon's test to ordinal variables -----------------------------------
# Step 2: Function to run Wilcoxon Test and extract results
run_wilcox_test <- function(var_name) {
  data <- joint_long_df %>%
    filter(variable == var_name, !is.na(value))
  
  test_result <- wilcox.test(value ~ Voter_Group, data = data)
  
  means <- data %>% # add mean as well
    group_by(Voter_Group) %>%
    summarise(mean = mean(value)) %>%
    pivot_wider(names_from = Voter_Group, values_from = mean)
  
  tibble(
    variable = var_name,
    w_pval = round(test_result$p.value, 4),
    w_meanMVoters = round(means$MVoters, 2),
    w_meanCVoters = round(means$CVoters, 2)
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



### violin graphing wilcox ------------------------------------------------------------------------
plot_shift_violin <- function(var_name, df = joint_long_df) {
  df %>%
    filter(variable == var_name, !is.na(value)) %>%
    ggplot(aes(x = Voter_Group, y = value, fill = Voter_Group)) +
    # Violin for distribution
    geom_violin(alpha = 0.4, color = NA, trim = FALSE) +
    # Stripchart of individual points
    geom_jitter(aes(color = Voter_Group), width = 0.15, alpha = 0.6, size = 0.5) +
    # Median lines
    stat_summary(fun = median, geom = "crossbar", width = 0.5,
                 color = "black", fatten = 0, linetype = "dashed") +
    labs(title = var_name,
         y = "Response (Ordinal Scale)",
         x = NULL) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("MVoters" = "#3a6ad1", "CVoters" = "#36914c")) +
    scale_color_manual(values = c("MVoters" = "#3a6ad1", "CVoters" = "#36914c"))
}

#plot_shift_violin("V202259x")

# "#1f77b4"
# "#3a6ad1", 













# THEORY FORMING ------------------------------------------------------------------

### Joining together all comparison results -------------------------------------------------------

joint_comparison <- comparison_ttest %>%
  select(-t_confhigh,
         -t_conflow) %>%
  rename(mean_MVoters = "t_meanMVoters",
         mean_CVoters = "t_meanCVoters") %>%
  select(variable,
         mean_MVoters,
         mean_CVoters,
         t_tstat,
         t_pval,
         t_degfree) %>%
  left_join(comparison_wilcox, by = "variable") %>%
  select(-w_meanMVoters,
         -w_meanCVoters) %>%
  left_join(comparison_fisher, by = "variable") %>%
  arrange(desc(t_pval))



### Filtering - only p-value < 0.05 and < 0.10 -------------------------------------------------------

# p-value < 0.05
joint_comparison_significant <- joint_comparison %>%
  filter(t_pval <= 0.05 | w_pval <= 0.05 | f_pval <= 0.05) %>%
  arrange(t_pval) %>%
  left_join(var_attributes_2020, by = "variable") %>%
  select(variable, 
         description_original,
         scale,
         everything())

joint_comparison_significant %>%
  write_csv(here("data/results/voters/ANES_2020_CVote_StatIndepVars.csv"))
# ANES 2020 ChangeVoter StatisticallyIndependent Variables

joint_comparison_significant %>%  
  saveRDS(file = here("data/results/voters/ANES_2020_CVote_StatIndepVars.RDS"))

# just for curiosity
# p-value < 0.10
joint_comparison_significant_p10 <- joint_comparison %>%
  filter(t_pval <= 0.10 | w_pval <= 0.10 | f_pval <= 0.10) %>%
  filter(!variable %in% joint_comparison_significant$variable)
  


### Table for Visualization and Printing --------------------------------------------
# summarize all numeric columns in the dataframe

joint_comparison_significant_fortable <-joint_comparison_significant %>%
  select(variable,
         description,
         scale,
         mean_MVoters,
         mean_CVoters,
         t_tstat,
         t_pval,
         w_pval,
         f_pval,
         f_oddsratio)



# GT TABLE ------------------------------------------------------------
# Create gt table
joint_comparison_significant_fortable %>%
  gt() %>%
  tab_header(title = "Statistically Independent Attributes: C-Voters (2012, 2016, 2020 - ANES 2020)") %>%
  fmt_number(columns = names(joint_comparison_significant_fortable),
             decimals = 2) %>%
  tab_style(style = cell_fill(color = "#e5f5dc"),locations = cells_body(
    columns = c(t_pval), rows = t_pval < 0.05)) %>%
  tab_style(style = cell_fill(color = "#e5f5dc"), locations = cells_body(
    columns = c(w_pval), rows = w_pval < 0.05)) %>%
  tab_style(style = cell_fill(color = "#e5f5dc"), locations = cells_body(
    columns = c(f_pval), rows = f_pval < 0.05)) %>%
  cols_width(variable ~ px(90),
             description ~ px(150),
             scale ~ px(250),
             everything() ~ px(90)
  ) %>%
  opt_table_font(font = list(gt::google_font("Roboto"), default_fonts())) %>%
  gtsave(filename = here("outputs/tables/ANES_2020_CVote_StatIndepVars_table.png"), 
         #expand = TRUE,  
         vwidth = 2600,  # try 1600-2400 depending on number of columns
         vheight = 1000) # adjust as needed



### plot - big patchwork plot (independent vars) ------------------------------------------------------------------
# Get all variable names
var_list <- joint_comparison_significant_fortable$variable

# Generate the list of plots
plot_list <- lapply(var_list, plot_by_type)

# Combine all plots into 5 rows x 5 columns
ggsave(here("outputs/ANES_comparison_grid_indep.png"), 
       wrap_plots(plot_list, nrow = 5, ncol = 4), width = 30, height = 40)


# # FLEXTABLE ------------------------------------------------------------
# # Make a copy and handle NAs (replace with blank or other marker)
# ft_data <- joint_comparison_significant_fortable %>%
#   mutate(across(everything(), ~ ifelse(is.na(.), "", .)))  # replace NA with blank
# 
# # Create the flextable
# ft <- flextable(joint_comparison_significant_fortable) %>%
#   set_table_properties(width = 1, layout = "autofit") %>%
#   theme_booktabs() %>%  # Clean, classic lines
#   fontsize(size = 11, part = "all") %>%
#   font(fontname = "Times New Roman", part = "all") %>%  # You could also try "Calibri", "Helvetica", etc.
#   color(color = "black", part = "all") %>%
#   bg(part = "header", bg = "#aebff5") %>%     # Light pastel blue for headers
#   bg(i = ~ t_pval < 0.05, j = "t_pval", bg = "#e5f5dc") %>%  # Light red highlight for significant p-values
#   bg(i = ~ w_pval < 0.05, j = "w_pval", bg = "#e5f5dc") %>%
#   bg(i = ~ f_pval < 0.05, j = "f_pval", bg = "#e5f5dc") %>%
#   align(align = "left", part = "all") %>%
#   border(border.top = fp_border(color = "darkgrey")) %>%
#   border(border.left = fp_border(color = "darkgrey")) %>%
#   border(border.right = fp_border(color = "darkgrey")) %>%
#   autofit()
# 
# 
# 
# # Print the flextable in RStudio (or include in RMarkdown)
# ft
# 
# 
# save_as_docx(ft, path = "outputs/tables/ANES_2020_CVote_table.docx")
# 
# 
# 
# 
# 



