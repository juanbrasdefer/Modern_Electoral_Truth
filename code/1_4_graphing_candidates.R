# libraries, directory --------------------------------------------------------

library(tidyverse)
library(here)
library(tictoc)
library(ggrepel)

here::i_am("code/1_4_graphing_candidates.R")
source(here("code/scripts/graph_colours.R"))

# load main dataset of frequencies and change %s 
crosstab_campaigndocs_totals <- read_csv(here("data/results/candidates/crosstab_campaigndocs_totals.csv"))

# load composite index results
change_index_results <- read_csv(here("data/results/candidates/CCI_results.csv"))

# GRAPHING ---------------------------------------------------------------


# scatter 1.1: Appendix: length of total text vs % change grams -------------------------------
top_changespeakers <- crosstab_campaigndocs_totals %>%
  arrange(desc(percent_changegrams)) %>%
  head(6)



crosstab_campaigndocs_totals %>%
  ggplot(aes(x = log(nchars), y = percent_changegrams)) +
  geom_point(size = 2, color = "#c54bfa", alpha = 0.5) +  # Simple scatter points
  geom_smooth(method = "lm", formula = y ~ x, color = "#c4167c", se = FALSE) +  # Regression line
  geom_text_repel(data = top_changespeakers, aes(label = speaker_year_id), size = 3, color = "#c54bfa") + # Labels
  labs(
    title = "Duration in Race vs. Degree of 'Change' Vocabulary",
    x = "Log of Total Words in DSet (Proxy for Duration of Campaign)",
    y = "Percent of 'Change' Vocab"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),   # White outer background
    panel.grid.major = element_line(color = "gray90"),  # Light grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) # Format y-axis as percentage


ggsave(here("outputs/nchars_changegrams_candidates.png"))




# scatter 1.2: Nominees Only: '% change vocabulary' across years  -----------------------

crosstab_campaigndocs_totals %>%
  filter(nominee_yn == 1) %>%
  ggplot(aes(x = year, 
             y = percent_changegrams,
             color = pol_party)) +
  geom_point(size = 2, 
             #color = "#c54bfa", # currently replaced by red blue pol_party
             alpha = 0.7) +  # Scatter points
  geom_smooth(method = "lm", formula = y ~ x, color = lm_pink, size = 0.8, se = FALSE) +  # Regression line
  geom_text_repel(aes(label = speaker_year_id), 
                  vjust = -3, size = 2, 
                  color = name_text,
                  family = "Times New Roman") + # Labels
  scale_color_manual(values = c("Democrat" = democrat_violet, 
                                "Republican" = republican_orange)) + # Custom colors
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016, 2020, 2024)) +  # Custom x-axis labels
  labs(
    title = "Use of 'Change' Vocabulary across Electoral Cycles",
    subtitle = "Democratic and Republican Presidential Nominees",
    x = "Electoral Cycle",
    y = "Percent use of 'Change' Vocab",
    color = "Political Party"
  ) +
  theme_minimal() + 
  theme(
    text = element_text(family = "Times New Roman"),  # <-- this line sets all text
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),   # White outer background
    panel.grid.major = element_line(color = "gray90"),  # Light grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  scale_y_continuous(#limits = c(0.05, 0.025),
    labels = scales::percent_format(scale = 100)) # Format y-axis as percentage


ggsave(here("outputs/changegrams_nominees.png"))








# scatter 1.3: All Candidates '% change vocabulary' across years -----------------------------------------
top_changespeaker_byyear <- crosstab_campaigndocs_totals %>%
  group_by(year) %>%
  slice_max(order_by = percent_changegrams, n = 1, with_ties = FALSE)

# WOULD BE NICE TO ADD:
# 2) top change speaker and lowest change speaker
crosstab_campaigndocs_totals %>%
  ggplot(aes(x = year, 
             y = percent_changegrams,
             color = pol_party)) +
  geom_point(size = 2, 
             #color = "#c54bfa", # currently replaced by red blue pol_party
             alpha = 0.6) +  # Scatter points
  geom_text(data = top_changespeaker_byyear, 
            aes(label = speaker), 
            vjust = -1, size = 2, 
            color = "grey") + # Labels
  scale_color_manual(values = c("Democrat" = "#3c5cf9", 
                                "Republican" = "#ff603e",
                                "Libertarian" = "#f3d04b")) + # Custom colors
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016, 2020, 2024)) +  # Custom x-axis labels
  labs(
    title = "Degree of 'Change Vocabulary' use Across Election Years",
    x = "Year",
    y = "Percent of 'Change' Vocab"
  ) +
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),   # White outer background
    panel.grid.major = element_line(color = "gray90"),  # Light grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) # Format y-axis as percentage


ggsave(here("outputs/changengrams_allcandidates.png"))

















# scatter 2: Candidate CCI Scores ------------------------------------------

change_index_results %>%
  #filter(nominee_yn == 1) %>%
  ggplot(aes(x = year, 
             y = change_index_score,
             color = pol_party)) +
  geom_point(size = 2, 
             #color = "#c54bfa", # currently replaced by red blue pol_party
             alpha = 0.7) +  # Scatter points
  geom_smooth(method = "lm", formula = y ~ x, 
              color = lm_pink, size = 0.8,
              se = FALSE) +  # Regression line
  geom_text_repel(aes(label = speaker), 
                  vjust = -3, size = 2, 
                  color = name_text,
                  family = "Times New Roman")+ # Labels
  scale_color_manual(values = c("Democrat" = democrat_violet, 
                                "Republican" = republican_orange,
                                "Libertarian" = libertarian_yellow)) + # Custom colors
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016, 2020, 2024)) +  # Custom x-axis labels
  labs(
    title = "Composite Change (CCI) Scores across Electoral Cycles",
    subtitle = "Democrat and Republican Presidential Nominees",
    x = "Electoral Cycle",
    y = "Composite 'Change' (CCI) Score",
    color = "Political Party"
  ) +
  theme_minimal() + 
  theme(
    text = element_text(family = "Times New Roman"),  # <-- this line sets all text
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),   # White outer background
    panel.grid.major = element_line(color = "gray90"),  # Light grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  scale_y_continuous(#labels = scales::percent_format(scale = 100), # Format y-axis as percentage
                     expand = c(0, 0), limits = c(0.25, 1)) # make sure it begins at 0
scale_y_continuous()

ggsave(here("outputs/CCI_Scores_Nominees.png"))









# Old -------------------------------------------------------------------
# 
# 
# Individual scatter (like Clinton, Trump, Obama on one graph) 
# 
# crosstab_campaigndocs_totals %>%
#   filter(speaker %in% c("Barack Obama",
#                         "Hillary Clinton",
#                         "Donald J. Trump" 
#                         #"Joseph R. Biden, Jr.",
#                         #"Kamala Harris"
#   )) %>%
#   ggplot(aes(x = year, 
#              y = percent_changegrams,
#              color = pol_party)) +
#   geom_point(size = 2, 
#              #color = "#c54bfa", # currently replaced by red blue pol_party
#              alpha = 0.6) +  # Scatter points
#   geom_text_repel(aes(label = speaker_year_id), 
#                   vjust = -1, size = 2, 
#                   color = "grey") + # Labels
#   scale_color_manual(values = c("Democrat" = "#3c5cf9", 
#                                 "Republican" = "#ff603e",
#                                 "Libertarian" = "#f3d04b")) + # Custom colors
#   scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016, 2020, 2024)) +  # Custom x-axis labels
#   labs(
#     title = "Individual 'Change Vocabulary' Across Election Years",
#     #subtitle = "Trump, Biden, Harris",
#     subtitle = "Obama, Clinton, Trump",
#     x = "Year",
#     y = "Percent of 'Change' Vocab"
#   ) +
#   theme_minimal() + 
#   theme(
#     panel.background = element_rect(fill = "white", color = NA),  # White plot background
#     plot.background = element_rect(fill = "white", color = NA),   # White outer background
#     panel.grid.major = element_line(color = "gray90"),  # Light grid lines
#     panel.grid.minor = element_blank()  # Remove minor grid lines
#   ) +
#   scale_y_continuous(labels = scales::percent_format(scale = 100)) # Format y-axis as percentage
# 
# 
# 
# #ggsave(here("outputs/candidates_changengrams_acrossyears_reduced_TBH.png"))
# #ggsave(here("outputs/candidates_changengrams_acrossyears_reduced_OCT.png"))

