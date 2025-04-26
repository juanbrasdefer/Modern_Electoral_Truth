# libraries, directory --------------------------------------------------------

library(tidyverse)
library(here)
library(tictoc)
library(ggrepel)

here::i_am("code/exploring_graphs.R")


# load main dataset of frequencies and change %s 
crosstab_campaigndocs_totals <- read_csv(here("data/results/crosstab_campaigndocs_totals.csv"))

# load composite index results
change_index_results <- read_csv(here("data/results/candidate_change_index_results.csv"))

# GRAPHING ---------------------------------------------------------------

# scatter 1: '% change vocabulary' across years -----------------------------------------
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


ggsave(here("outputs/candidates_changengrams_acrossyears_affil.png"))




# scatter 2: length of total text vs amount of change grams -------------------------------
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


ggsave(here("outputs/candidates_changengrams_ncharsvspctchange.png"))






# scatter 3: individual scatters (progression across years for candidate) -----------------------------------

crosstab_campaigndocs_totals %>%
  filter(speaker %in% c("Barack Obama",
                        "Hillary Clinton",
                        "Donald J. Trump" 
                        #"Joseph R. Biden, Jr.",
                        #"Kamala Harris"
  )) %>%
  ggplot(aes(x = year, 
             y = percent_changegrams,
             color = pol_party)) +
  geom_point(size = 2, 
             #color = "#c54bfa", # currently replaced by red blue pol_party
             alpha = 0.6) +  # Scatter points
  geom_text_repel(aes(label = speaker_year_id), 
                  vjust = -1, size = 2, 
                  color = "grey") + # Labels
  scale_color_manual(values = c("Democrat" = "#3c5cf9", 
                                "Republican" = "#ff603e",
                                "Libertarian" = "#f3d04b")) + # Custom colors
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016, 2020, 2024)) +  # Custom x-axis labels
  labs(
    title = "Individual 'Change Vocabulary' Across Election Years",
    #subtitle = "Trump, Biden, Harris",
    subtitle = "Obama, Clinton, Trump",
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



#ggsave(here("outputs/candidates_changengrams_acrossyears_reduced_TBH.png"))
#ggsave(here("outputs/candidates_changengrams_acrossyears_reduced_OCT.png"))







# scatter 4: all years, nominees only (Dem v Rep final candidates) -----------------------

crosstab_campaigndocs_totals %>%
  filter(nominee_yn == 1) %>%
  ggplot(aes(x = year, 
             y = percent_changegrams,
             color = pol_party)) +
  geom_point(size = 2, 
             #color = "#c54bfa", # currently replaced by red blue pol_party
             alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", formula = y ~ x, color = "#c4167c", se = FALSE) +  # Regression line
  geom_text_repel(aes(label = speaker_year_id), 
                  vjust = -3, size = 2, 
                  color = "grey") + # Labels
  scale_color_manual(values = c("Democrat" = "#3c5cf9", 
                                "Republican" = "#ff603e",
                                "Libertarian" = "#f3d04b")) + # Custom colors
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016, 2020, 2024)) +  # Custom x-axis labels
  labs(
    title = "'Change Vocabulary' Across Election Years",
    subtitle = "Dem and Rep Nominees",
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


ggsave(here("outputs/candidates_changengrams_nominees_repel.png"))












# scatter 5: Candidate CCI Scores ------------------------------------------

change_index_results %>%
  #filter(nominee_yn == 1) %>%
  ggplot(aes(x = year, 
             y = change_index_score,
             color = pol_party)) +
  geom_point(size = 2, 
             #color = "#c54bfa", # currently replaced by red blue pol_party
             alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", formula = y ~ x, color = "#c4167c", se = FALSE) +  # Regression line
  geom_text_repel(aes(label = speaker), 
                  vjust = -3, size = 2, 
                  color = "grey") + # Labels
  scale_color_manual(values = c("Democrat" = "#3c5cf9", 
                                "Republican" = "#ff603e",
                                "Libertarian" = "#f3d04b")) + # Custom colors
  scale_x_continuous(breaks = c(2000, 2004, 2008, 2012, 2016, 2020, 2024)) +  # Custom x-axis labels
  labs(
    title = "Change Index Score in Each Election",
    subtitle = "Dem and Rep Nominees",
    x = "Year",
    y = "Composite 'Change' Score"
  ) +
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),   # White outer background
    panel.grid.major = element_line(color = "gray90"),  # Light grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  scale_y_continuous(#labels = scales::percent_format(scale = 100), # Format y-axis as percentage
                     expand = c(0, 0), limits = c(0.25, 1)) # make sure it begins at 0
scale_y_continuous()

ggsave(here("outputs/composite_change_scores_nominees2.png"))



