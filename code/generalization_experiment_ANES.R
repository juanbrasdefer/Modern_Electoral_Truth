
# Script Setup ----------------------------------------------------------------------
### libraries, directory ------------------------------------------------------------
library(tidyverse)
library(here)
library(patchwork)
library(ggrepel)



here::i_am("code/generalization_experiment_ANES.R")

# load custom functions
source(here("code/scripts/functions_datasetfiltering.R")) # for filtering and summarizing ANES dsets

# load reference dataframe that will help us deal with the fact that
# each of the surveys use different naming scheme for variables
validation_ref <- read_csv(here("data/ref/validation_reftable.csv"))

# load composite index
candidate_change_scores <- read_csv(here("data/results/candidate_change_index_results.csv"))

# old one
var_names_ref_old <- read_csv(here("data/ref/old_generalization_ANES_reftable.csv"))



# load data ------------------------------------------------------------------



# load years of ANES surveys
ANES_2020_raw <- read_csv(here("data/ANES/2020/anes_timeseries_2020_csv_20220210/anes_timeseries_2020_csv_20220210.csv"))

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




# BiVariate Experiment -------------------------------------------------
# BiVariate 2000 -------------------------------------------------------
year <- 2000
ANES_2000_clean <- ANES_2000_raw %>%
  filter(V001249 != 0) %>% # unfortunately a lot of responses were written physically, and exist in a 'separate file' so to clean this dataset we get rid of those obs. first
  filter_ANES_vote("votedpres_yn", year) %>%
  clean_ANES_for_scoring("income_gap", year) %>%
  clean_ANES_for_scoring("leftright_self", year) %>%
  clean_ANES_for_scoring("office_recall", year)

year <- 2000
ANES_2000_scored <- ANES_2000_clean %>%
  score_economic("income_gap", year) %>%
  score_leftright("leftright_self", year) %>%
  score_informed("office_recall", year) %>%
  score_changevoter() %>%
  encode_preschoice("pres_choice",year) %>%
  mutate(ANES_year = year) %>%
  select(ANES_year,
         changevoter_score, # 21 unique scores
         score_economic,
         score_leftright,
         score_informed,
         year_preschoice_coded)


# BiVariate 2004 -------------------------------------------------------
year <- 2004
ANES_2004_clean <- ANES_2004_raw %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  clean_ANES_for_scoring("income_gap", year) %>%
  clean_ANES_for_scoring("leftright_self", year) %>%
  clean_ANES_for_scoring("office_recall", year)

year <- 2004
ANES_2004_scored <- ANES_2004_clean %>%
  score_economic("income_gap", year) %>%
  score_leftright("leftright_self", year) %>%
  score_informed("office_recall", year) %>%
  score_changevoter() %>%
  encode_preschoice("pres_choice",year) %>%
  mutate(ANES_year = year) %>%
  select(ANES_year,
         changevoter_score, # 27 unique scores
         score_economic,
         score_leftright,
         score_informed,
         year_preschoice_coded)


# BiVariate 2008 -------------------------------------------------------
year <- 2008
ANES_2008_clean <- ANES_2008_raw %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  clean_ANES_for_scoring("income_gap", year) %>%
  clean_ANES_for_scoring("leftright_self", year) %>%
  clean_ANES_for_scoring("office_recall", year)

year <- 2008
ANES_2008_scored <- ANES_2008_clean %>%
  score_economic("income_gap", year) %>%
  score_leftright("leftright_self", year) %>%
  score_informed("office_recall", year) %>%
  score_changevoter() %>%
  encode_preschoice("pres_choice",year) %>%
  mutate(ANES_year = year) %>%
  select(ANES_year,
         changevoter_score, # 77 unique scores
         score_economic,
         score_leftright,
         score_informed,
         year_preschoice_coded) 


# BiVariate 2012 -------------------------------------------------------
year <- 2012
ANES_2012_clean <- ANES_2012_raw %>%
  filter(presvote2012_x != -2) %>% # unfortunately a lot of responses were written physically, and exist in a 'separate file' so to clean this dataset we get rid of those obs. first
  filter_ANES_vote("votedpres_yn", year) %>%
  clean_ANES_for_scoring("income_gap", year) %>%
  clean_ANES_for_scoring("leftright_self", year) %>%
  clean_ANES_for_scoring("office_recall", year)

year <- 2012
ANES_2012_scored <- ANES_2012_clean %>%
  score_economic("income_gap", year) %>%
  score_leftright("leftright_self", year) %>%
  score_informed("office_recall", year) %>%
  score_changevoter() %>%
  encode_preschoice("pres_choice",year) %>%
  mutate(ANES_year = year) %>%
  select(ANES_year,
         changevoter_score, # 41 unique scores
         score_economic,
         score_leftright,
         score_informed,
         year_preschoice_coded)

# BiVariate 2016 -------------------------------------------------------
year <- 2016
ANES_2016_clean <- ANES_2016_raw %>%
  mutate(V161027_V162034a_summary = ifelse(V161027 %in% c(1,2,3,4,5), V161027, # choice for president
                                           ifelse(V162034a %in% c(1,2,3,4,5), V162034a,
                                                  NA))) %>%
  mutate(V162073a_V162073b_summary = ifelse(V162073a %in% c(1,0), V162073a, # speaker of the house summary
                                            ifelse(V162073b %in% c(1,0.5), 1,
                                                   ifelse(V162073b %in% (0), 0,
                                                          NA)))) %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  clean_ANES_for_scoring("income_gap", year) %>%
  clean_ANES_for_scoring("leftright_self", year) %>%
  clean_ANES_for_scoring("office_recall", year)

year <- 2016
ANES_2016_scored <- ANES_2016_clean %>%
  score_economic("income_gap", year) %>%
  score_leftright("leftright_self", year) %>%
  score_informed("office_recall", year) %>%
  score_changevoter() %>%
  encode_preschoice("pres_choice",year) %>%
  mutate(ANES_year = year) %>%
  select(ANES_year,
         changevoter_score, # 41 unique scores
         score_economic,
         score_leftright,
         score_informed,
         year_preschoice_coded)


# BiVariate 2020 -------------------------------------------------------
year <- 2020
ANES_2020_clean <- ANES_2020_raw %>%
  mutate(V202139y1_V202139y2_summary = ifelse(V202139y1 %in% c(0,1), # V202139y1 and V202139y2, POST: Office recall: Speaker of the House ‐ Nancy Pelosi
                                              V202139y1, ifelse(V202139y2 %in% c(0,1),  # V202139y1 Coded in ANES as: 0. Incorrect, 1. Correct
                                                                V202139y2, ifelse(V202139y2 %in% c(2), # V202139y2 Coded in ANES as: 0. Incorrect, 1. Partially correct, 2. Correct
                                                                                  1, NA)))) %>%  # re-coding 'partially correct' from experimental phrasing to 1        
  filter_ANES_vote("votedpres_yn", year) %>%
  clean_ANES_for_scoring("income_gap", year) %>%
  clean_ANES_for_scoring("leftright_self", year) %>%
  clean_ANES_for_scoring("office_recall", year)

year <- 2020
ANES_2020_scored <- ANES_2020_clean %>%
  score_economic("income_gap", year) %>%
  score_leftright("leftright_self", year) %>%
  score_informed("office_recall", year) %>%
  score_changevoter() %>%
  encode_preschoice("pres_choice",year) %>%
  mutate(ANES_year = year) %>%
  select(ANES_year,
         changevoter_score, # 41 unique scores
         score_economic,
         score_leftright,
         score_informed,
         year_preschoice_coded)



# Joining ANES Scored datasets -----------------------------------------------
ANES_scored_allyears <- rbind(ANES_2000_scored,
                              ANES_2004_scored,
                              ANES_2008_scored,
                              ANES_2012_scored,
                              ANES_2016_scored,
                              ANES_2020_scored)

# Joining ANES Scored and CCI ------------------------------------------------

candidate_change_scores_joinable <- candidate_change_scores %>%
# key measure:
  # finding the score margin between the two candidates in a year
  # for the top one, the margin will be positive,
  # for the bottom one the margin will be negative
  # but the absolute value of the margin would be the same
  group_by(year) %>%
  mutate(max_year_cci = max(change_index_score, na.rm = TRUE)) %>%
  mutate(min_year_cci = min(change_index_score, na.rm = TRUE)) %>%
  mutate(relative_inyear_cci_margin = ifelse(change_index_score == max_year_cci, 
                                         change_index_score - min_year_cci,
                                         change_index_score - max_year_cci)) %>%
  ungroup() %>%
  mutate(year_preschoice_coded = paste0(year,"_",encoding_ANES)) %>%
  select(speaker, 
         #year, 
         year_preschoice_coded,
         encoding_ANES,
         change_index_score, 
         relative_inyear_cci_margin,
         min_year_cci,
         max_year_cci,
         relative_inyear_changegrams)


CCI_ANES_scored_graphable <- ANES_scored_allyears %>% 
  left_join(candidate_change_scores_joinable, by = "year_preschoice_coded")


# plotting ----------------------------------------------------------
# Set custom colors for each year
year_colors <- c(
  "2000" = "#1b9e77",
  "2004" = "#d95f02",
  "2008" = "#7570b3",
  "2012" = "#e7298a",
  "2016" = "#66a61e",
  "2020" = "#e6ab02"
)


# standard scatter graph ------------------------------------------
CCI_ANES_scored_graphable %>%
ggplot(aes(
  x = changevoter_score,
  y = relative_inyear_cci_margin,
  color = factor(ANES_year))) +
  geom_point(alpha = 0.7, size = 0.5) +
  scale_color_manual(values = year_colors,
                     name = "ANES Year") +
  labs(x = "Change Voter Score",
       y = "Relative In-Year CCI Margin") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

ggsave(here("outputs/bivariate.png"))



# lms graph --------------------------------------------------------
ggplot(CCI_ANES_scored_graphable, aes(
  x = changevoter_score,
  y = relative_inyear_cci_margin,
  color = factor(ANES_year)
)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(se = FALSE, method = "loess", span = 1) +  # You can try method = "lm" too
  scale_color_manual(values = year_colors, name = "ANES Year") +
  labs(x = "Change Voter Score", y = "Relative In-Year CCI Margin") +
  theme_minimal(base_size = 14)

ggsave(here("outputs/bivariate_lms.png"))



plot_loess_year <- function(year, show_y = TRUE) {
  data_year <- CCI_ANES_scored_graphable %>%
    filter(ANES_year == year)
  
  ggplot(data_year, aes(x = changevoter_score, y = relative_inyear_cci_margin)) +
    geom_point(color = year_colors[as.character(year)], alpha = 0.5, size = 2) +
    geom_smooth(se = FALSE, method = "loess", span = 1, color = "black", size = 1) +
    labs(
      title = paste("Year:", year),
      x = "Change Voter Score",
      y = if (show_y) "Relative CCI Margin" else NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.y = if (show_y) element_text() else element_blank(),
          axis.ticks.y = if (show_y) element_line() else element_blank(),
          axis.title.y = if (show_y) element_text() else element_blank())
}


years <- c(2000, 2004, 2008, 2012, 2016, 2020)
plots_loess <- mapply(plot_loess_year, years, show_y = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), SIMPLIFY = FALSE)
combined_loess_plot <- wrap_plots(plots_loess, ncol = 2)

ggsave(here("outputs/bivariate_lms_patchwork.png"), width = 14, height = 10, dpi = 300)

# means --------------------------------------------------------------


plot_year <- function(year, show_y = TRUE) {
  data_year <- CCI_ANES_scored_graphable %>%
    filter(ANES_year == year)
  
  medians <- data_year %>%
    group_by(relative_inyear_cci_margin) %>%
    summarise(mean_score = mean(changevoter_score), .groups = "drop") %>%
    arrange(relative_inyear_cci_margin)
  
  # Identify which point is "higher" in x
  label_df <- medians %>%
    filter(mean_score == max(mean_score)) %>%
    mutate(label = "X")
  
  p <- ggplot(data_year, aes(x = changevoter_score, y = relative_inyear_cci_margin)) +
    geom_point(color = year_colors[as.character(year)], alpha = 0.5, size = 2) +
    geom_point(data = medians, aes(x = mean_score, y = relative_inyear_cci_margin), 
               color = year_colors[as.character(year)], size = 3, shape = 18) +
    geom_path(data = medians, aes(x = mean_score, y = relative_inyear_cci_margin, group = 1),
              color = year_colors[as.character(year)], size = 1.1) +
    geom_text(data = label_df, aes(x = mean_score, y = relative_inyear_cci_margin, label = label),
              color = "red", size = 7) +
    labs(
      title = paste("Year:", year),
      x = "Change Voter Score",
      y = if (show_y) "Relative CCI Margin" else NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.y = if (show_y) element_text() else element_blank(),
          axis.ticks.y = if (show_y) element_line() else element_blank(),
          axis.title.y = if (show_y) element_text() else element_blank())
  
  return(p)
}

# Only show Y-axis labels for the first column of plots
years <- c(2000, 2004, 2008, 2012, 2016, 2020)
plots <- mapply(plot_year, years, show_y = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), SIMPLIFY = FALSE)
wrap_plots(plots, ncol = 2)

ggsave(here("outputs/bivariate_means_patchwork.png"), width = 14, height = 10, dpi = 300)





# lms and means graph --------------------------------------------------------

plot_loess_year <- function(year, show_y = TRUE) {
  data_year <- CCI_ANES_scored_graphable %>%
    filter(ANES_year == year)
  
  medians <- data_year %>%
    group_by(relative_inyear_cci_margin) %>%
    summarise(mean_score = mean(changevoter_score), .groups = "drop") %>%
    arrange(relative_inyear_cci_margin)
  
  # Identify which point is "higher" in x
  label_df <- medians %>%
    filter(mean_score == max(mean_score)) %>%
    mutate(label = "X")
  
  ggplot(data_year, aes(x = changevoter_score, y = relative_inyear_cci_margin)) +
    geom_point(color = year_colors[as.character(year)], alpha = 0.5, size = 2) +
    geom_point(data = medians, aes(x = mean_score, y = relative_inyear_cci_margin), 
               color = year_colors[as.character(year)], size = 3, shape = 18) +
    geom_path(data = medians, aes(x = mean_score, y = relative_inyear_cci_margin, group = 1),
              color = year_colors[as.character(year)], size = 1.1) +
    geom_text(data = label_df, aes(x = mean_score, y = relative_inyear_cci_margin, label = label),
              color = "red", size = 7) +
    geom_smooth(se = FALSE, method = "loess", span = 1, color = "black", size = 1) +
    labs(
      title = paste("Year:", year),
      x = "Change Voter Score",
      y = if (show_y) "Relative CCI Margin" else NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.y = element_text() ,
          axis.ticks.y = element_line() ,
          axis.title.y = if (show_y) element_text() else element_blank())
}


years <- c(2000, 2004, 2008, 2012, 2016, 2020)
plots_loess <- mapply(plot_loess_year, years, show_y = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), SIMPLIFY = FALSE)
combined_loess_plot <- wrap_plots(plots_loess, ncol = 2)

ggsave(here("outputs/bivariate_lms+means_patchwork.png"), width = 14, height = 10, dpi = 300)










#24.04 Results ----------------------------------------------------------
# 2000
# pre-filtering: Gore 50%, Bush 45%, Nader 3% 
# post-filtering: Gore 53%, Bush 27%, Other 14%, but only 12 respondents...

# 2004
# pre-filtering: Kerry 46%, Bush 52%, Nader 1%, 0% others
# post-filtering: Kerry 64%, Bush 29%,  Nader 7%... massive!!!!!

# 2008
# pre-filtering: Obama 64%, McCain 32%, Others 2%
# post-filtering: Obama 74%, McCain 21%, Others 1%

# 2012
# pre-filtering: Obama 57%, Romney 39%, Other 3%
# post-filtering: Obama 61%, Romney 33%, Other 4%

# 2016
# pre-filtering: Clinton 47%, Trump 43%, 7% other candidates
# post-filtering:  Clinton 53%, Trump 34%, 11% other candidates

# 2020
# pre-filtering: Biden 55%, Trump 41%, 1% other candidates
# post-filtering: Biden 75%, Trump 19%, 4% other candidates


# Filter and Results 2000 ------------------------------------------------
ANES_2000_clean <- ANES_2000_raw %>%
  filter(V001249 != 0) # unfortunately a lot of responses were
# written physically, and exist in a 'separate file'
# so to clean this dataset we get rid of those obs. first

year <- 2000
ANES_2000_filtered <- ANES_2000_clean %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  filter_ANES_attribute_single("income_gap", year) %>% # 17% increase for Bush, 20% drop for Gore
  #filter_ANES_attribute_single("leftright_self", year) %>% # 6% bump for Gore
  filter_ANES_attribute_single("office_recall", year) # 1% drop for Gore, went to Nader
  
CVoters_2000_p <- ANES_2000_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Gore 50%, Bush 45%, Nader 3% 
# post-filtering: Gore 53%, Bush 27%, Other 14%, but only 12 respondents...
# coded as 1.GORE 2.PHILLIPS 3.BUSH 4.BROWN 5.BUCHANAN 6.NADER 



# if we run only income gap and office recall together, 
# we get 20% Gore and 66% Bush, with ~70 respondents total
# important to note that 2000 did not have a left-right variable,
# instead it had a messy set of ~6 'branching' variables in some form of 
# "Self placement lib-con scale"






# Filter and Results 2004 ------------------------------------------------
year <- 2004
ANES_2004_filtered <- ANES_2004_raw %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  filter_ANES_attribute_single("income_gap", year) %>% # 5% bias for Kerry
  filter_ANES_attribute_single("leftright_self", year) %>% # 2% drop for Kerry, which went to independents
  filter_ANES_attribute_single("office_recall", year) # 2% bump for Bush
  

CVoters_2004_p <- ANES_2004_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Kerry 46%, Bush 52%, Nader 1%, 0% others
# post-filtering: Kerry 64%, Bush 29%,  Nader 7%... massive!!!!!
# coded as 1.Kerry, 3.Bush, 5.Nader, 7.Other



# Filter and Results 2008 ------------------------------------------------
year <- 2008
ANES_2008_filtered <- ANES_2008_raw %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  filter_ANES_attribute_single("income_gap", year) %>% # 4% increase for Obama
  filter_ANES_attribute_single("leftright_self", year) %>% # 3% increase for McCain
  filter_ANES_attribute_single("office_recall", year) # 4% increase for Obama 

CVoters_2008_p <- ANES_2008_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Obama 64%, McCain 32%, Others 2%
# post-filtering: Obama 74%, McCain 21%, Others 1%
# coded as 1. Obama 3. McCain 7. Others



# Filter and Results 2012 ------------------------------------------------
ANES_2012_clean <- ANES_2012_raw %>%
  filter(presvote2012_x != -2) # unfortunately a lot of responses were
# written physically, and exist in a 'separate file'
# so to clean this dataset we get rid of those obs. first

year <- 2012
ANES_2012_filtered <- ANES_2012_clean %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  filter_ANES_attribute_single("income_gap", year) %>% # 6% increase for Obama
  filter_ANES_attribute_single("leftright_self", year) %>% # 2% drop for Obama, but went to indeps (romney stayed)
  filter_ANES_attribute_single("office_recall", year) # 5% increase for Romney, all from Obama


CVoters_2012_p <- ANES_2012_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Obama 57%, Romney 39%, Other 3%
# post-filtering: Obama 61%, Romney 33%, Other 4%
# Coded as: 1.Obama 2.Romney 5.other cands




# Filter and Results 2016 ------------------------------------------------
ANES_2016_clean <- ANES_2016_raw %>%
  mutate(V161027_V162034a_summary = ifelse(V161027 %in% c(1,2,3,4,5), V161027, # choice for president
                                           ifelse(V162034a %in% c(1,2,3,4,5), V162034a,
                                                  NA))) %>%
  mutate(V162073a_V162073b_summary = ifelse(V162073a %in% c(1,0), V162073a, # speaker of the house summary
                                            ifelse(V162073b %in% c(1,0.5), 1,
                                                   ifelse(V162073b %in% (0), 0,
                                                          NA))))

year <- 2016
ANES_2016_filtered <- ANES_2016_clean %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  filter_ANES_attribute_single("income_gap", year) %>% # 4% increase for Clinton
  filter_ANES_attribute_single("leftright_self", year) %>% # 4% increase for other candidates (HUGE), 2% increase for Clinton, 9% drop Trump
  filter_ANES_attribute_single("office_recall", year) # 2% increase for Trump, taken from indeps


CVoters_2016_p <- ANES_2016_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Clinton 47%, Trump 43%, 7% other candidates
# post-filtering:  Clinton 53%, Trump 34%, 11% other candidates
# Coded as: 1.Clinton 2. Trump 3.4.5. other cands







# Filter and Results 2020 ------------------------------------------------
ANES_2020_clean <- ANES_2020_raw %>%
  mutate(V202139y1_V202139y2_summary = ifelse(V202139y1 %in% c(0,1), # V202139y1 and V202139y2, POST: Office recall: Speaker of the House ‐ Nancy Pelosi
                                              V202139y1, ifelse(V202139y2 %in% c(0,1),  # V202139y1 Coded in ANES as: 0. Incorrect, 1. Correct
                                                                V202139y2, ifelse(V202139y2 %in% c(2), # V202139y2 Coded in ANES as: 0. Incorrect, 1. Partially correct, 2. Correct
                                                                                  1, NA))))  # re-coding 'partially correct' from experimental phrasing to 1        


year <- 2020
ANES_2020_filtered <- ANES_2020_clean %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  filter_ANES_attribute_single("income_gap", year) %>% # 11% increase for Biden
  filter_ANES_attribute_single("leftright_self", year) %>% # 9% increase for Biden, 15% drop for Trump, 5% to indeps
  filter_ANES_attribute_single("office_recall", year) # No change at all


CVoters_2020_p <- ANES_2020_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Biden 55%, Trump 41%, 1% other candidates
# post-filtering: Biden 75%, Trump 19%, 4% other candidates
# Coded as: 1.Biden 2.Trump 3.Jorgensen 4.Hawkins 5. Other 8. Libertarian







# OLD --------------------------------------------------------------------
# colloquium 5 results -------------------------------------------------------

#2000
# pre-filtering: Gore 50%, Bush 45%, Nader 3% 
# post-filtering: Gore 40%, Bush 51%, Brown 3%, Buchanan 1%, Nader 3% 

#2004
# pre-filtering: Kerry 46%, Bush 52%, Nader 1%, 0% others
# post-filtering: Kerry 61%, Bush 31%,  Nader 1%, 2% others 

#2008
# pre-filtering: Obama 64%, McCain 32%, Others 2%
# post-filtering: Obama 69%, McCain 27%, Others 3%

#2012
# pre-filtering: Obama 57%, Romney 39%, Other 3%
# post-filtering: Obama 45%, Romney 46%, Other 7%

#2016
# pre-filtering: Clinton 47%, Trump 43%, 7% other candidates
# post-filtering:  Clinton 49%, Trump 37%, 11% other candidates                                    - I guess here you need to check whether Jill stein and Gary Johnson are bigger CHANGE people



# temp results -------------------------------------------------------

#2000
# pre-filtering: Gore 50%, Bush 45%, Nader 3% 
# post-filtering: Gore 40%, Bush 51%, Brown 3%, Buchanan 1%, Nader 3% 
# coded as 1.GORE 2.PHILLIPS 3.BUSH 4.BROWN 5.BUCHANAN 6.NADER
#2004
# pre-filtering: Kerry 46%, Bush 52%, Nader 1%, 0% others
# post-filtering: Kerry 61%, Bush 31%,  Nader 1%, 2% others # massive for others!!!!!
# if science: Kerry 69%, Bush 21%, Nader 2%, 2% others
#2008
# pre-filtering: Obama 64%, McCain 32%, Others 2%
# post-filtering: Obama 69%, McCain 27%, Others 3%
# if we take science:  Obama 71%, McCain 25%, Others 2% - not too exciting
#2012
# pre-filtering: Obama 57%, Romney 39%, Other 3%
# post-filtering: Obama 45%, Romney 46%, Other 7%
#2016
# pre-filtering: Clinton 47%, Trump 43%, 7% other candidates
# post-filtering:  Clinton 49%, Trump 37%, 11% other candidates                     - I guess here you need to check whether Jill stein and Gary Johnson are bigger CHANGE people
# if we take science as well,  Clinton 54%, Trump 34%, 11% other candidates


# Filter and Results 2000 ------------------------------------------------
ANES_2000_clean <- ANES_2000_raw %>%
  filter(V001249 != 0) # unfortunately a lot of responses were
# written physically, and exist in a 'separate file'
# so to clean this dataset we get rid of those obs. first


year <- 2000
ANES_2000_filtered <- ANES_2000_clean %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  #filter_ANES_attributes_OR("science_belief_1", "science_belief_2", year) %>% 
  #filter_ANES_attributes_OR("forward_looking_1", "forward_looking_2", year) %>% 
  filter_ANES_attributes_OR("economic_reality_1", "economic_reality_2", year) %>% # 2% bias for Bush, increase in all OTHERS, but especially BROWN
  filter_ANES_attributes_OR("partisan_detachment_1", "partisan_detachment_2", year) %>% # 4% bias for Bush, nader goes up 1%
  filter_ANES_attributes_OR("democratic_apathy_1", "democratic_apathy_2", year) # 3% drop for gore, bush and nader, brown and buchanan hit 1%


CVoters_2000_p <- ANES_2000_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Gore 50%, Bush 45%, Nader 3% 
# post-filtering: Gore 40%, Bush 51%, Brown 3%, Buchanan 1%, Nader 3% 
# coded as 1.GORE 2.PHILLIPS 3.BUSH 4.BROWN 5.BUCHANAN 6.NADER 




# Filter and Results 2004 ------------------------------------------------
year <- 2004
ANES_2004_filtered <- ANES_2004_raw %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  filter_ANES_attributes_OR("science_belief_1", "science_belief_2", year) %>% # 6% bias for Kerry, 8% loss for bush, 1% of which went nader
  #filter_ANES_attributes_OR("forward_looking_1", "forward_looking_2", year) %>% 
  filter_ANES_attributes_OR("economic_reality_1", "economic_reality_2", year) %>% # 16% bias for Kerry, 18% loss bush 0.5% nader and 0.5% others
  filter_ANES_attributes_OR("partisan_detachment_1", "partisan_detachment_2", year) %>% # 1% bias for Kerry BUT FEEL THERM ENCODING ON MY END IS MESSED UP
  filter_ANES_attributes_OR("democratic_apathy_1", "democratic_apathy_2", year) # 4% bump kerry, 5% drop bush, large jump for others (not much, but biggest so far)


CVoters_2004_p <- ANES_2004_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Kerry 46%, Bush 52%, Nader 1%, 0% others
# post-filtering: Kerry 61%, Bush 31%,  Nader 1%, 2% others # massive for others!!!!!
      # if science: Kerry 69%, Bush 21%, Nader 2%, 2% others
# coded as 1.Kerry, 2.Bush, 5.Nader, 7.Other



# Filter and Results 2008 ------------------------------------------------
year <- 2008
ANES_2008_filtered <- ANES_2008_raw %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  filter_ANES_attributes_OR("science_belief_1", "science_belief_2", year) %>% # 4% bias for Obama
  #filter_ANES_attributes_OR("forward_looking_1", "forward_looking_2", year) %>% 
  filter_ANES_attributes_OR("economic_reality_1", "economic_reality_2", year) %>% # 4% bias for Obama
  filter_ANES_attributes_OR("partisan_detachment_1", "partisan_detachment_2", year) %>% # no real change 
  filter_ANES_attributes_OR("democratic_apathy_1", "democratic_apathy_2", year) # 1% bump for Obama


CVoters_2008_p <- ANES_2008_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Obama 64%, McCain 32%, Others 2%
# post-filtering: Obama 69%, McCain 27%, Others 3%
      # if we take science:  Obama 71%, McCain 25%, Others 2% - not too exciting
# coded as 1. Obama 3. McCain 7. Others



# Filter and Results 2012 ------------------------------------------------
ANES_2012_clean <- ANES_2012_raw %>%
  filter(presvote2012_x != -2) # unfortunately a lot of responses were
                                # written physically, and exist in a 'separate file'
                                  # so to clean this dataset we get rid of those obs. first


year <- 2012
ANES_2012_filtered <- ANES_2012_clean %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  #filter_ANES_attributes_OR("science_belief_1", "science_belief_2", year) %>% # 4% bias for Obama
  #filter_ANES_attributes_OR("forward_looking_1", "forward_looking_2", year) %>% # not using
  filter_ANES_attributes_OR("economic_reality_1", "economic_reality_2", year) %>% # 1% bias for obama
  filter_ANES_attributes_OR("partisan_detachment_1", "partisan_detachment_2", year) %>% # 1% bias for Obama
  filter_ANES_attributes_OR("democratic_apathy_1", "democratic_apathy_2", year) # 11% drop for obamna, 9% increase for Romney; 2% bump for other


CVoters_2012_p <- ANES_2012_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Obama 57%, Romney 39%, Other 3%
# post-filtering: Obama 45%, Romney 46%, Other 7%
      # if we take science as well,  Obama 48%, Romney 43%, Other 7%
# Coded as: 1.Obama 2.Romney 5.other cands




# Filter and Results 2016 ------------------------------------------------
ANES_2016_clean <- ANES_2016_raw %>%
  mutate(V161027_V162034a_summary = ifelse(V161027 %in% c(1,2,3,4,5), V161027, # choice for president
                                           ifelse(V162034a %in% c(1,2,3,4,5), V162034a,
                                                  NA))) %>%
  mutate(V162073a_V162073b_summary = ifelse(V162073a %in% c(1,0), V162073a, # speaker of the house summary
                                         ifelse(V162073b %in% c(1,0.5), 1,
                                                ifelse(V162073b %in% (0), 0,
                                                NA))))

  
temp <- ANES_2016_raw %>%
  select(V162073a)
year <- 2016
ANES_2016_filtered <- ANES_2016_clean %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  filter_ANES_attributes_OR("science_belief_1", "science_belief_2", year) %>% # 9% bias for clinton
  #filter_ANES_attributes_OR("forward_looking_1", "forward_looking_2", year) %>% # 16% bias for clinton
  filter_ANES_attributes_OR("economic_reality_1", "economic_reality_2", year) %>% # 2% bias for clinton
  filter_ANES_attributes_OR("partisan_detachment_1", "partisan_detachment_2", year) %>% # clinton same but 3% drop for Trump
  filter_ANES_attributes_OR("democratic_apathy_1", "democratic_apathy_2", year) # 3% bias for clinton


CVoters_2016_p <- ANES_2016_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Clinton 47%, Trump 43%, 7% other candidates
# post-filtering:  Clinton 49%, Trump 37%, 11% other candidates - I guess here you need to check whether Jill stein and Gary Johnson are bigger CHANGE people
  # if we take science as well,  Clinton 54%, Trump 34%, 11% other candidates

# Coded as: 1.Clinton 2. Trump 3.4.5. other cands







# Filter and Results 2020 ------------------------------------------------
ANES_2020_clean <- ANES_2020_raw %>%
  mutate(V202139y1_V202139y2_summary = ifelse(V202139y1 %in% c(0,1), # V202139y1 and V202139y2, POST: Office recall: Speaker of the House ‐ Nancy Pelosi
                                              V202139y1, ifelse(V202139y2 %in% c(0,1),  # V202139y1 Coded in ANES as: 0. Incorrect, 1. Correct
                                                                V202139y2, ifelse(V202139y2 %in% c(2), # V202139y2 Coded in ANES as: 0. Incorrect, 1. Partially correct, 2. Correct
                                                                                  1, NA))))  # re-coding 'partially correct' from experimental phrasing to 1        
  
  

temp <- ANES_2016_raw %>%
  select(V162073a)
year <- 2016
ANES_2016_filtered <- ANES_2016_clean %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  filter_ANES_attributes_OR("science_belief_1", "science_belief_2", year) %>% # 9% bias for clinton
  #filter_ANES_attributes_OR("forward_looking_1", "forward_looking_2", year) %>% # 16% bias for clinton
  filter_ANES_attributes_OR("economic_reality_1", "economic_reality_2", year) %>% # 2% bias for clinton
  filter_ANES_attributes_OR("partisan_detachment_1", "partisan_detachment_2", year) %>% # clinton same but 3% drop for Trump
  filter_ANES_attributes_OR("democratic_apathy_1", "democratic_apathy_2", year) # 3% bias for clinton


CVoters_2016_p <- ANES_2016_filtered %>%
  summarize_ANES_preschoice("pres_choice", year) %>%
  print()
# pre-filtering: Clinton 47%, Trump 43%, 7% other candidates
# post-filtering:  Clinton 49%, Trump 37%, 11% other candidates - I guess here you need to check whether Jill stein and Gary Johnson are bigger CHANGE people
# if we take science as well,  Clinton 54%, Trump 34%, 11% other candidates

# Coded as: 1.Clinton 2. Trump 3.4.5. other cands


























# Variable Selections ---------------------------------------------
## 2000 \------------------------- ------------------------------
# for visual scanning of variables available in dataset
# raw_lines <- readLines(here("data/ANES/2000/anes_2000prepost/anes_2000prepost_var.txt"))
# v_lines <- grep("^V\\d+", raw_lines, value = TRUE)
# v_df <- tibble(variable_code = v_lines)

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




# "science_belief"
# V000777 How important is envir regulation
# 1. NOT AT ALL IMPORTANT 2. NOT TOO IMPORTANT 3. SOMEWHAT IMPORTANT 4. VERY IMPORTANT 5. EXTREMELY IMPORTANT 8. DK 9. RF0. NA

# "forward_looking"
# V000682 Should federal spending on ENVIRONMENTAL PROTECTION be increased, decreased, or kept about the same
# 1. INCREASED 3. DECREASED 5. KEPT ABOUT THE SAME 7. CUT OUT ENTIRELY [VOL]

# "economic_reality"
# V000994 Pre-Tax Household Income
# V000491 Summary US econ btr/worse last year

# "partisan_detachment"
# V001307 Thermometer federal govt in Wash DC
# V000372 Thermometer parties in general

# "democratic_apathy"
# V001651 Is R satisfied with US Democracy
# V001304 Thermometer supreme court

# "information_level"
# has Cheney, V045162 (speaker of house), SCOTUS chief, tony blair


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


# "science_belief"
# V043167 Federal Budget Spending: science and technology
# V045072 Feeling Thermometer: environmentalists

# "forward_looking"
#not using

# "economic_reality"
# V043293x Summary: Household income
# V043214 How much national economy better/worse last 4 years
# V045064 Feeling Thermometer: Labor Unions

# "partisan_detachment"
# V045060 Feeling Thermometer: Federal Government in Washington
# V045271 CSES Left-Right scale - self placement

# "democratic_apathy"
# V045242 Makes a difference who is in power
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

# "science_belief"
# V085064s Feeling thermometer: ENVIRONMENTALISTS
# V083143 Federal Budget Spending: science and technology

# "forward_looking"
# V083157x [NEW] SUMMARY: favor/oppose lower emission standards ds
# V083158 [NEW] Importance of emission standards issue

# "economic_reality"
# V083248x SUMMARY: FAMILY INCOME
# V085080x SUMMARY: INCOME GAP COMPARED TO 20 YRS AGO
# V084405j LABOR UNIONS thermometer

# "partisan_detachment"
# V085191 CSES: Left-right: self
# V085148 Govt run by a few big interests or for benefit of all

# "democratic_apathy"
# V085064t Feeling thermometer: THE U.S. SUPREME COURT
# V085182 Does/doesn't make a difference who is in power


temp <- ANES_2008_raw %>%
  select(V085120)




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

# "science_belief"
# envir_gwarm - Is global warming happening or not
# envir_gwgood - temperatures continue to go up in the future, would this be good, bad, or neither good nor bad

# "economic_reality"
# finance_finpast_x - PRE: SUMMARY- Better or worse off than 1 year ago
# ineq_incgap_x - PRE: SUMMARY- Income gap size compared to 20 years ago
#  ################Family income RESTRICTED IN 2012


# "partisan_detachment"
# cses_selfleft - left-right self placement
# cses_closepty - Close to any political party


# "democratic_apathy"
#  cses_diffpower - make a difference who is in power
#  cses_satisdem - Satisfied with way democracy works in the U.S
#  ftgr_ussc - Feeling thermometer: THE U.S. SUPREME COURT




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
# "science_belief"
# V162112 POST: Feeling thermometer: SCIENTISTS
# V161221 PRE: Is global warming happening or not

# "forward_looking"
# V161225x - PRE: SUMMARY - Govt action about rising temperatures
# V162207 - POST: Agree/disagree: world is changing and we should adjust

# "economic_reality"
# V161361x - PRE: income summary
# V161138x - PRE: SUMMARY - larger/smaller income gap today

# "partisan_detachment"
# V161126 - PRE: 7pt scale Liberal conservative self-placement
# V161155 - PRE: Party ID: Does R think of self as Dem, Rep, Ind or what


# "democratic_apathy"
# V161151x - PRE: SUMMARY - Voting as duty or choice
# V162290 - POST: CSES: Satisfied with way democracy works in the U.S

#V162281 POST: CSES: 5pt scale: make a difference who is in power
































































# OLD Attributes (all, rather) -----------------------------------------------------------

# 2000
# "science_belief"
# V000777 How important is envir regulation
# 1. NOT AT ALL IMPORTANT 2. NOT TOO IMPORTANT 3. SOMEWHAT IMPORTANT 4. VERY IMPORTANT 5. EXTREMELY IMPORTANT 8. DK 9. RF0. NA

# "forward_looking"8
# V000682 Should federal spending on ENVIRONMENTAL PROTECTION be increased, decreased, or kept about the same
# 1. INCREASED 3. DECREASED 5. KEPT ABOUT THE SAME 7. CUT OUT ENTIRELY [VOL]

# "economic_reality"
# V000994 HH income -all HHs (not sure which var is correct here)
# V000993 Pre-Tax Household Income
# 1.NONE OR LESS THAN $4,999 2.$5,000-$9,999 3. C. $10,000-$14,999 4. D. $15,000-$24,999 5. $25,000-$34,999 6. $35,000-$49,999 7. $50,000-$64,999 8. $65,000-$74,999 9. $75,000-$84,999 10. $85,000-$94,999 11. $95,000-$104,999 12. $105,000-$114,999 13. $115,000-$124,999 14. $125,000-$134,999 15. $135,000-$144,999 16. $145,000-$154,999 17. $155,000-$164,999 18. $165,000-$174,999 19. $175,000-$184,999 20. $185,000-$194,999 21. $195,000-$199,999 22. $200,000 and over
# V000491 Summary US econ btr/worse last year

# "partisan_detachment"
# V001307 Thermometer federal govt in Wash DC
# V000369 Thermometer Dem Party
# V000370 Thermometer Rep Party
# V000372 Thermometer parties in general

# "democratic_apathy"
# V001651 Is R satisfied with US Democracy
# V001304 Thermometer supreme court
# V001429 How much can you trust the media






# 2004
# "science_belief"
# V043167 Federal Budget Spending: science and technology
# V045072 Feeling Thermometer: environmentalists

# "forward_looking"
#V043182 Environment vs. jobs tradeoff scale - self-placement

# "economic_reality"
# V043293x Summary: Household income
# V045064 Feeling Thermometer: Labor Unions
# V043214 How much national economy better/worse last 4 years

# "partisan_detachment"
# V045060 Feeling Thermometer: Federal Government in Washington
# V043049 Feeling Thermometer: Democratic party
# V043050 Feeling Thermometer: Republican party

# "democratic_apathy"
# V045241 How satisfied with democracy in US
# V045242 Makes a difference who is in power
# V045244 Democracy is best form of govt
# V045073 Feeling Thermometer: U.S. Supreme Court









### 2008
# "science_belief"
# V084405s ENVIRONMENTALISTS thermometer
# V085064s Feeling thermometer: ENVIRONMENTALISTS
# V084405e Federal Budget Spending: science and technology

# "forward_looking"
# V083157 [NEW] Favor/oppose lower emission standards
# V083157x [NEW] SUMMARY: favor/oppose lower emission standards ds
# V083158 [NEW] Importance of emission standards issue
# V083151x SUMMARY: increase or decrease spending on environment

# "economic_reality"
# V084405j LABOR UNIONS thermometer
# V083248 Family income
# V083248x SUMMARY: FAMILY INCOME
# V085080 Income gap today more or less than 20 years ago
# V085080x SUMMARY: INCOME GAP COMPARED TO 20 YRS AGO

# "partisan_detachment"
# V083044a Feeling Thermometer: Democratic Party
# V083044b Feeling Thermometer: Republican Party
# V085148 Govt run by a few big interests or for benefit of all

# "democratic_apathy"
# V084405t U.S. SUPREME COURT thermometer (random order q... idk)
# V085064t Feeling thermometer: THE U.S. SUPREME COURT
# V085182 Does/doesn't make a difference who is in power







# 2012
# "science_belief"
# envir_gwarm - Is global warming happening or not
# envir_gwhow - Anthropogenic climate change (ie: humans cause it)

# "forward_looking"
# envir_gwgood - temperatures continue to go up in the future, would this be good, bad, or neither good nor bad
# envjob_self - environment-jobs tradeoff self-placement

# "economic_reality"
# ftgr_unions - Feeling thermometer: LABOR UNIONS
# cses_econ - State of economy
# econ_ecpast_x - PRE: SUMMARY- U.S. economy better or worse than 1 year ago
# ineq_incgap_x - PRE: SUMMARY- Income gap size compared to 20 years ago
# finance_finpast_x - PRE: SUMMARY- Better or worse off than 1 year ago
#  ################Family income RESTRICTED IN 2012

# "partisan_detachment"
# cses_selfleft - left-right self placement
# cses_diffpower - make a difference who is in power
# cses_closepty - Close to any political party
# ft_dem - Feeling Thermometer: Republican Party
# ft_rep - Feeling Thermometer: Republican Party

# "democratic_apathy"
#  ftgr_ussc - Feeling thermometer: THE U.S. SUPREME COURT
#  cses_diffpower - make a difference who is in power
#  cses_satisdem - Satisfied with way democracy works in the U.S







# 2016 
# "science_belief"
# V162112 POST: Feeling thermometer: SCIENTISTS
# V161207 PRE: Federal Budget Spending: science and technology
# V161221 PRE: Is global warming happening or not
# V161222 PRE: Anthropogenic climate change

# "forward_looking"
# V161225x - PRE: SUMMARY - Govt action about rising temperatures

# "economic_reality"
# V161110 - PRE: R how much better worse off than 1 year ago
# V161140x - PRE: SUMMARY - economy better/worse in last year
# V161138x - PRE: SUMMARY - larger/smaller income gap today
# V162136x - POST: SUMMARY- Economic mobility easier/harder compared to 20 yrs ago
# V161361x - PRE: income summary

# "partisan_detachment"
# V161126 - PRE: 7pt scale Liberal conservative self-placement
# V161155 - PRE: Party ID: Does R think of self as Dem, Rep, Ind or what
# V161095 - PRE: Feeling Thermometer: Democratic Party
# V161096 - PRE: Feeling Thermometer: Republican Party
# V161215 - PRE: REV How often trust govt in Wash to do what is right
# V161216 - PRE: Govt run by a few big interests or for benefit of all

# "democratic_apathy"
# V161151x - PRE: SUMMARY - Voting as duty or choice
# V162290 - POST: CSES: Satisfied with way democracy works in the U.S
