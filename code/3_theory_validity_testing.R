
# STUDY THREE, Validity Testing of Theory of Change Voters ----------------------------------

# Method Two: Continuous measure of voter propensity for change
# Voter Change Score


# Scripts setup ----------------------------------------------------------------
### libraries, directory ------------------------------------------------------------
library(tidyverse)
library(here)
library(patchwork)
library(ggrepel)



here::i_am("code/3_theory_validity_testing.R")

### load custom functions -----------------------------------------------------------
source(here("code/scripts/functions_datasetfiltering.R")) # for filtering and summarizing ANES dsets
# many custom functions. proud of them honestly :-))

source(here("code/scripts/graph_colours.R")) # colours for graphs



### load data ------------------------------------------------------------------

# reference dataframe to deal with the fact that each of the surveys use different naming scheme for variables
validation_ref <- read_csv(here("data/ref/validation_reftable.csv"))

# load CCI scores
candidate_change_scores <- read_csv(here("data/results/CCI_results.csv"))


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




# 1.1 Voter Change Scores -------------------------------------------------
### Filtering 2000 -------------------------------------------------------
year <- 2000
ANES_2000_clean <- ANES_2000_raw %>%
  filter(V001249 != 0) %>% # unfortunately a lot of responses were written physically, and exist in a 'separate file' so to clean this dataset we get rid of those obs. first
  filter_ANES_vote("votedpres_yn", year) %>%
  clean_ANES_for_scoring("income_gap", year) %>%
  clean_ANES_for_scoring("leftright_self", year) %>%
  clean_ANES_for_scoring("office_recall", year)

### Scoring 2000 -------------------------------------------------------
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


### Filtering 2004 -------------------------------------------------------
year <- 2004
ANES_2004_clean <- ANES_2004_raw %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  clean_ANES_for_scoring("income_gap", year) %>%
  clean_ANES_for_scoring("leftright_self", year) %>%
  clean_ANES_for_scoring("office_recall", year)

### Scoring 2004 -------------------------------------------------------
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


### Filtering 2008 -------------------------------------------------------
year <- 2008
ANES_2008_clean <- ANES_2008_raw %>%
  filter_ANES_vote("votedpres_yn", year) %>%
  clean_ANES_for_scoring("income_gap", year) %>%
  clean_ANES_for_scoring("leftright_self", year) %>%
  clean_ANES_for_scoring("office_recall", year)

### Scoring 2008 -------------------------------------------------------
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


### Filtering 2012 -------------------------------------------------------
year <- 2012
ANES_2012_clean <- ANES_2012_raw %>%
  filter(presvote2012_x != -2) %>% # unfortunately a lot of responses were written physically, and exist in a 'separate file' so to clean this dataset we get rid of those obs. first
  filter_ANES_vote("votedpres_yn", year) %>%
  clean_ANES_for_scoring("income_gap", year) %>%
  clean_ANES_for_scoring("leftright_self", year) %>%
  clean_ANES_for_scoring("office_recall", year)

### Scoring 2012 -------------------------------------------------------
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


### Filtering 2016 -------------------------------------------------------
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

### Filtering 2016 -------------------------------------------------------
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

#unique(ANES_2016_scored$changevoter_score)

### Filtering 2020 -------------------------------------------------------
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

### Scoring 2020 -------------------------------------------------------
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



# 1.2 Joining Voter Change Scores and CCI  -----------------------------------------------

# join together all ANES scored years
ANES_scored_allyears <- rbind(ANES_2000_scored,
                              ANES_2004_scored,
                              ANES_2008_scored,
                              ANES_2012_scored,
                              ANES_2016_scored,
                              ANES_2020_scored)

# quick changes to CCI df (add new measure of CCI margin)
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
         relative_inyear_changegrams,
         pol_party)


# join ANES Scored and CCI 
CCI_ANES_scored_graphable <- ANES_scored_allyears %>% 
  left_join(candidate_change_scores_joinable, by = "year_preschoice_coded")


# 2. Plotting ----------------------------------------------------------
### 2.1 Voter Change Scores propensty for Change Candidate (LOESS and means graph) --------------------------------------------------------

# 'Dancers Plot'
#'LOESS' plot (Locally Estimated Scatterplot Smoothing)
# with means!
plot_loess_year <- function(year, show_y = TRUE) {
  data_year <- CCI_ANES_scored_graphable %>%
    filter(ANES_year == year)
  
  candidates <- unique(data_year$speaker)

  means <- data_year %>%
    #group_by(relative_inyear_cci_margin) %>%
    group_by(change_index_score) %>%
    summarise(mean_score = mean(changevoter_score), .groups = "drop") %>%
    arrange(change_index_score)
  
  # Identify which point is "higher" in x
  label_df <- means %>%
    filter(mean_score == max(mean_score)) %>%
    mutate(label = "X")
  
  ggplot(data_year, aes(x = changevoter_score, y = change_index_score,
                        color = pol_party)) +
    geom_point(alpha = 0.2, size = 2) +
    scale_color_manual(
      values = c("Democrat" = democrat_blue, "Republican" = republican_red)) +
    geom_point(data = means, aes(x = mean_score, y = change_index_score),
               color = bipartisan_purple_light,
               size = 3, shape = 18) +
    geom_path(data = means, aes(x = mean_score, y = change_index_score, group = 1), # chord
              color = bipartisan_purple_light,
              size = 1.1) +
    geom_text(data = label_df, aes(x = mean_score, y = change_index_score, label = label),
              color = bipartisan_purple_light,
              size = 7) +
    geom_smooth(se = FALSE, method = "loess", span = 1, color = bipartisan_purple_dark, size = 1) +
    labs(
      title = paste0("Election Cycle: ", year),
      subtitle = paste0("Candidates: ", candidates[1], " & ", candidates[2]),
      x = "Voter Change Scores",
      y = if (show_y) "Candidate CCI Score" else NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(  text = element_text(family = "Times New Roman"),  # <-- this line sets all text 
            legend.position = "none",
          axis.text.y = element_text() , # show axis numbering
          axis.ticks.y = element_line() , # show axis ticks
          axis.title.y = if (show_y) element_text() else element_blank()) # only show axis y label for first column
}


years <- c(2000, 2004, 2008, 2012, 2016, 2020)
plots_loess <- mapply(plot_loess_year, years, show_y = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), SIMPLIFY = FALSE)
combined_loess_plot <- wrap_plots(plots_loess, ncol = 2) + 
  plot_annotation(title = "Voter 'Change' Propensities",
                  subtitle = "Voter 'Change' Scores compared with their Presidential Vote",
                  caption = "Data Sources: APP, ANES",
                  theme = theme(
      plot.title = element_text(hjust = 0.5, size = 24, family = "Times New Roman"),
      plot.subtitle = element_text(hjust = 0.5, size = 18, family = "Times New Roman"),
      plot.caption = element_text(size = 11, family = "Times New Roman")
    ))
ggsave(here("outputs/Dancing_Patchwork.png"), width = 14, height = 12, dpi = 300)

# 
# 
# 
# ### 2.2 Dancers, (LOESS and means graph, but with speaker names as y axis) --------------------------------------------------------
# 
# plot_loess_year <- function(year, show_y = TRUE) {
#   data_year <- CCI_ANES_scored_graphable %>%
#     filter(ANES_year == year)
#   
#   candidates <- unique(data_year$speaker)
#   
#   jitter_prep1 <- (unique(data_year$change_index_score))[1]
#   jitter_prep2 <- (unique(data_year$change_index_score))[2]
#   jitter_size <- (0.05*(max(jitter_prep1,jitter_prep2) - min(jitter_prep1, jitter_prep2)))
#   
#   means <- data_year %>%
#     #group_by(relative_inyear_cci_margin) %>%
#     group_by(speaker) %>%
#     summarise(mean_score = mean(changevoter_score), .groups = "drop") %>%
#     arrange(speaker)
#   
#   # Identify which point is "higher" in x
#   label_df <- means %>%
#     filter(mean_score == max(mean_score)) %>%
#     mutate(label = "X")
#   
#   ggplot(data_year, aes(x = changevoter_score, y = speaker,
#                         color = pol_party)) +
#     # geom_point( #color = year_colors[as.character(year)], 
#     #   alpha = 0.5, size = 2) +
#     geom_jitter(alpha = 0.3, size = 2, width = 0, height = 0.5)+
#     scale_color_manual(
#       values = c("Democrat" = "#377eb8", "Republican" = "#c9102d")) +
#     # geom_point(data = means, aes(x = mean_score, y = speaker),
#     #            #color = year_colors[as.character(year)],
#     #            color = "#c04aff",
#     #            size = 3, shape = 18) +
#     geom_path(data = means, aes(x = mean_score, y = speaker, group = 1),
#               #color = year_colors[as.character(year)],
#               color = "#c04aff",
#               size = 1.1) +
#     geom_text(data = label_df, aes(x = mean_score, y = speaker, label = label),
#               color = "#c04aff",
#               #color = "red"
#               size = 7) +
#     geom_smooth(se = FALSE, method = "loess", span = 1, color = "#691496", size = 1) +
#     labs(
#       title = paste0("Election ", year, ": ", candidates[1], " & ", candidates[2]),
#       x = "Voter Change Scores",
#       y = if (show_y) "Candidate Change Margin" else NULL
#     ) +
#     theme_minimal(base_size = 13) +
#     theme(legend.position = "none",
#           axis.text.y = element_text() , # show axis numbering
#           axis.ticks.y = element_line() , # show axis ticks
#           axis.title.y = if (show_y) element_text() else element_blank()) # only show axis y label for first column
# }
# 
# 
# years <- c(2000, 2004, 2008, 2012, 2016, 2020)
# plots_loess <- mapply(plot_loess_year, years, show_y = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), SIMPLIFY = FALSE)
# combined_loess_plot <- wrap_plots(plots_loess, ncol = 2)
# 
# #ggsave(here("outputs/temp.png"), width = 14, height = 10, dpi = 300)


