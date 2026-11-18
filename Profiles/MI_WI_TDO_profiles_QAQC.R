# Explore TDO profile data from MI and WI lakes and QAQCs data to get into final cleaned format
# Created by MM on 11/12/26
# Input: MN_WI_MI_profiles.csv
# Output: WI_MI_profiles_cleaned.csv

setwd("~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/TDO profiles/")
library(tidyverse)
library(readxl)
source("~/Desktop/TDO_LCCMR/Profiles/rMR.R")

All.Data <- read.csv("MN_WI_MI_profiles.csv", stringsAsFactors = F)
MN.Data <- read.csv("updated_MN_profiles_2025.csv", stringsAsFactors = F)
WI.MI.Data <- All.Data %>%
  filter(state %in% c("WI", "MI"))

# profile IDs are just numbers 1 through n, change IDs to a string of a combination
# of the monitoring location ID and day of year (that's how the MN profile IDs are
# formatted)
WI.MI.Data.2 <- WI.MI.Data %>%
  mutate(doy = case_when(is.na(doy) ~ as.numeric(strftime(as.Date(date), format = "%j")),
                         .default = doy),
         profile_ident = paste(monitoringlocationidentifier, format(as.Date(date), "%Y"), doy, sep = "/"))

# flag obviously wonky data
WI.MI.Data.2 <- WI.MI.Data.2 %>%
  mutate(do_perc_sat = DO.unit.convert(do_ppm, DO.units.in = "mg/L", DO.units.out = "pct",  bar.units.in = "atm", bar.press = 1, temp.C = temp_c),
         # temps below 0 and above 40 ºC get flagged
         flag_temp = case_when(temp_c <= 0 |
                                 temp_c >= 40 ~ 1,
                               .default = 0),
         # DO levels below 0 mg/L, above 30 mg/L, and above 300% sat when temp is accurate get flagged
         flag_do = case_when(do_ppm < 0 |
                               do_ppm >= 30 |
                               (do_perc_sat >= 300 & flag_temp  == 0) ~ 1,
                             .default = 0))

# check duplicates
duplicated.profile.depths <- profile.depth[which(duplicated(paste(WI.MI.Data.2$profile_ident, WI.MI.Data.2$depth_m, sep = "_")))]
Duplicated.Profile.Depths.Data <- WI.MI.Data.2 %>%
  mutate(profile_depth = paste(profile_ident, depth_m, sep = "_")) %>%
  filter(profile_depth %in% duplicated.profile.depths)

# fill out how many duplicated measurements at depth there are for temp and do and then take the average of the duplicates
WI.MI.Data.3 <- WI.MI.Data.2 %>%
  group_by(profile_ident, depth_m) %>%
  summarise(measurecount_temp = case_when(!any(is.na(temp_c)) & length(unique(temp_c)) == 1 & !any(flag_temp == 1) ~ 1,
                                          .default = n_distinct(temp_c, na.rm = T)),
            measurecount_doppm = case_when(!any(is.na(do_ppm)) & length(unique(do_ppm)) == 1 ~ 1,
                                          .default = n_distinct(do_ppm, na.rm = T)),
            temp_c = mean(temp_c, na.rm = T),
            do_ppm = mean(do_ppm, na.rm = T),
            do_perc_sat = mean(do_perc_sat, na.rm = T),
            across(everything(), first),
            .groups = "drop") %>%
  group_by(profile_ident) %>%
  mutate(n_dep_inprofile = n(),
         .groups = "drop",
         # need to recalculate %DO since some of the mg/L values were averaged
         do_perc_sat = DO.unit.convert(do_ppm, DO.units.in = "mg/L", DO.units.out = "pct",  bar.units.in = "atm", bar.press = 1, temp.C = temp_c))
  

















