# 2012 exploration

# libraries, directory ------------------------------------------------------------
library(tidyverse)
library(here)
library(haven) # for reading SAV files

here::i_am("code/exploring_ANES_2012.R")


# load data -----------------------------------------------------------------------

df2012 <- read.delim(here("data/ANES/2012/anes_timeseries_2012/anes_timeseries_2012_rawdata.txt")
                 , sep = "|", 
                 header = TRUE)  # For tab-separated

df2016 <- read.delim(here("data/ANES/2016/anes_timeseries_2016/anes_timeseries_2016_rawdata.txt")
                     , sep = "|", 
                     header = TRUE)  # For tab-separated


df2020 <- read_csv(here("data/ANES/2020/anes_timeseries_2020_csv_20220210/anes_timeseries_2020_csv_20220210.csv"))


df2024 <- read_csv(here("data/ANES/2024/anes_timeseries_2024_csv_20250219/anes_timeseries_2024_csv_20250219.csv"))
# 2024 has no 2016 id... 
# bruh


dfpanel_2016_2020 = read_sav(here("data/ANES/Panel_2016_2020/2016_2020_mergedpanel.sav"))


# checks ----------------------------------------------------------------------

# check number of common panel individuals between 2016 and 2020

common <- df2016 %>%
  select(V160001_orig) %>%
  inner_join(df2020, by = 'V160001_orig')
