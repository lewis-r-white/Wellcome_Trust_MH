# Load necessary libraries
library(readxl)     # reading Excel files
library(tidyverse)      # data manipulation
library(labelled)   # labeling columns 
library(here) # file paths
library(haven)


# load in the data ----
WBT_daily_max <- read_csv(here("data", "Ghana-Tw-Extract.csv")) %>%
  filter(time > as.Date("2013-01-01"),
         time < as.Date("2016-01-01")) %>%
  select(community, time, twx) %>% # select relevant columns 
  mutate(community = str_remove(community, "_manual$")) #remove _manual from communities so join works (_manual values appear to be same as community without _manual)


era5 <- read_csv(here("data", "GRAPHS-ERA5-CHIRPS", "GRAPHS-ERA5-Heatstress-Tw-1991missing.csv"))

precip <- read_csv(here("data", "GRAPHS-ERA5-CHIRPS", "GRAPHS-CHIRPS-Precip.csv"))



### EXPLORE DIFFERENCES IN TEMP DATA

original_wbt_comparison_test <- WBT_daily_max %>% 
  filter(time > as.Date("2013-01-01"),
         time < as.Date("2013-01-05")) %>% 
  filter(community %in% c("APESIKA", "AMPOMA", "BEPOSO", "AT", "AKORA", "KA")) %>% distinct()

era5_wbt_comparison_test <- era5 %>% 
  filter(date > as.Date("2013-01-01"),
         date < as.Date("2013-01-05")) %>% 
  filter(community %in% c("APESIKA", "AMPOMA", "BEPOSO", "AKORA/AT", "KANDIGE")) %>%
  select(community, date, max_tw)


## WBT from original data has different values than new data. I think this is what Cascade said here:
# "This data will differ from any heat stress metrics produced by KNUST. 
# This is because we are using 25-km ERA5 and they are using 9-km ERA5-land.
# The reason we use the courser ERA5 data is because it contains the entire meteorological conditions (like 100 different metrics, including sea surface temperatures (SST). 
# To do predictive forecasting for early warnings, we need the entire meteorological conditions and ERA5-land does not include even the basics like SST. 
# So we stick with ERA5."

## ERA 5 has all the communities that are in PSS, just need to change name from AKORA/AT to AKORA





### Archive tempt data
# 
# wbgt_graphs_pregnancy <- read_csv(here("data", "WBGT_GRAPHS_pregnancy_wide.csv"))
# 
# wbgt_graphs_pregnancy_preconception <- read_csv(here("data", "WBGT_GRAPHS_pregnancy_and60dayspreconception_wide.csv"))
# 
# tmax_graphs_pregnancy <- read_csv(here("data", "tmax_dry_GRAPHS_pregnancy_wide.csv"))
# 
# tmax_graphs_pregnancy_preconception <- read_csv(here("data", "tmax_dry_GRAPHS_pregnancy_and60dayspreconception_wide.csv"))
# 
# heat_index_graphs_pregnancy <- read_csv(here("data", "HeatIndex_GRAPHS_pregnancy_wide.csv"))
# 
# heat_index_graphs_pregnancy_preconception <- read_csv(here("data", "HeatIndex_GRAPHS_pregnancy_and60dayspreconception_wide.csv"))
# 
# 
# 
# 
# ## calculating trimester averages  ----
# 
# 
# wbgt_averages <- wbgt_graphs_pregnancy %>%
#   rowwise() %>%
#   mutate(
#     # Pre-conception average: days 1-60
#     wbgt_pre_conception_avg = mean(c_across(WBGT_temp.1:WBGT_temp.60), na.rm = TRUE),
#     
#     # First trimester average: days 61-140
#     wbgt_first_trimester_avg = mean(c_across(WBGT_temp.61:WBGT_temp.140), na.rm = TRUE),
#     
#     # Second trimester average: days 141-220
#     wbgt_second_trimester_avg = mean(c_across(WBGT_temp.141:WBGT_temp.220), na.rm = TRUE),
#     
#     # Third trimester average: days 221 onward
#     wbgt_third_trimester_avg = mean(c_across(WBGT_temp.221:ncol(wbgt_graphs_pregnancy)), na.rm = TRUE),
#     
#     # Overall pregnancy average: days 61 onward
#     wbgt_pregnancy_avg = mean(c_across(WBGT_temp.61:ncol(wbgt_graphs_pregnancy)), na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   select(mstudyid, wbgt_pre_conception_avg, wbgt_first_trimester_avg, wbgt_second_trimester_avg, wbgt_third_trimester_avg, wbgt_pregnancy_avg)
# 
# 
# tmax_averages <- tmax_graphs_pregnancy %>%
#   rowwise() %>%
#   mutate(
#     # Pre-conception average: days 1-60
#     tmax_pre_conception_avg = mean(c_across(dry_temp.1:dry_temp.60), na.rm = TRUE),
#     
#     # First trimester average: days 61-140
#     tmax_first_trimester_avg = mean(c_across(dry_temp.61:dry_temp.140), na.rm = TRUE),
#     
#     # Second trimester average: days 141-220
#     tmax_second_trimester_avg = mean(c_across(dry_temp.141:dry_temp.220), na.rm = TRUE),
#     
#     # Third trimester average: days 221 onward
#     tmax_third_trimester_avg = mean(c_across(dry_temp.221:ncol(tmax_graphs_pregnancy)), na.rm = TRUE),
#     
#     # Overall pregnancy average: days 61 onward
#     tmax_pregnancy_avg = mean(c_across(dry_temp.61:ncol(tmax_graphs_pregnancy)), na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   select(mstudyid, tmax_pre_conception_avg, tmax_first_trimester_avg, tmax_second_trimester_avg, tmax_third_trimester_avg, tmax_pregnancy_avg)
# 
# 
# heat_index_averages <- heat_index_graphs_pregnancy %>%
#   rowwise() %>%
#   mutate(
#     # Pre-conception average: days 1-60
#     heat_index_pre_conception_avg = mean(c_across(heat_index_temp.1:heat_index_temp.60), na.rm = TRUE),
#     
#     # First trimester average: days 61-140
#     heat_index_first_trimester_avg = mean(c_across(heat_index_temp.61:heat_index_temp.140), na.rm = TRUE),
#     
#     # Second trimester average: days 141-220
#     heat_index_second_trimester_avg = mean(c_across(heat_index_temp.141:heat_index_temp.220), na.rm = TRUE),
#     
#     # Third trimester average: days 221 onward
#     heat_index_third_trimester_avg = mean(c_across(heat_index_temp.221:ncol(heat_index_graphs_pregnancy)), na.rm = TRUE),
#     
#     # Overall pregnancy average: days 61 onward
#     heat_index_pregnancy_avg = mean(c_across(heat_index_temp.61:ncol(heat_index_graphs_pregnancy)), na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   select(mstudyid, heat_index_pre_conception_avg, heat_index_first_trimester_avg, heat_index_second_trimester_avg, heat_index_third_trimester_avg, heat_index_pregnancy_avg)
# 
# heat_averages <- left_join(wbgt_averages, tmax_averages) %>%
#   left_join(heat_index_averages)
