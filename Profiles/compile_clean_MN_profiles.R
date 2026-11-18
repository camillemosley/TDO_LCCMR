# Created by Mike M. on Nov. 3rd 2025
#
# This code is used to compile, format, and clean (filter) MN temp and DO 
# profile data for calculating TDO3 and VHOD. Takes recent profile data from
# 2023-2025 collected by MN DNR and MPCA and combines it with data from 2022
# and before.
#
# Uses parts of Heidi's code (FullMNScript_QAQC_26Oct2025.R) and Mike V's code
# (FullMNScript_hmr.R).
# 
# Inputs:
#   MN_WI_MI_profiles.csv
#   DNR_MPCA_TDO_2023_2025.xlsx
#   mndow_nhdhr_xwalk.csv
# Output: updated_MN_profiles_2025.csv


setwd('~/Google Drive/My Drive/LCCMR Coldwater Habitat Watersheds -MM/Data/')
library(readxl)
library(tidyverse)
rm(list = ls())

oldProfiles <- read.csv('MN_WI_MI_profiles.csv', stringsAsFactors = F)
oldProfilesMN <- oldProfiles[oldProfiles$state == 'MN',]
newProfilesDNR <- read_xlsx('DNR_MPCA_TDO_2023_2025.xlsx', sheet = 'DNR_23_24_25')
newProfilesMPCA <- read_xlsx('DNR_MPCA_TDO_2023_2025.xlsx', sheet = 'MPCA_23_24')
xwalk <- read.csv('mndow_nhdhr_xwalk.csv', stringsAsFactors = F)





# Combine the 2 data sets =====================================================

## Combine new DNR and PCA data ----

# Format columns
names(newProfilesDNR)
names(newProfilesMPCA)
names(newProfilesDNR)[!names(newProfilesDNR) %in% names(newProfilesMPCA)]
names(newProfilesMPCA)[!names(newProfilesMPCA) %in% names(newProfilesDNR)]
newProfilesDNR$ID <- NA
newProfilesDNR$agency <- 'Minnesota Department of Natural Resources'
newProfilesMPCA$agency <- 'Minnesota Pollution Control Agency - Ambient Surface Water'

newProfiles <- rbind(newProfilesDNR, newProfilesMPCA)
rm(newProfilesDNR, newProfilesMPCA, oldProfiles)

## Combine new and old data ----

# Add leading zeros to the lake IDs that have 7 characters in them
newProfiles$ID_NBR[nchar(newProfiles$ID_NBR) == 7] <- paste0('0', newProfiles$ID_NBR[nchar(newProfiles$ID_NBR) == 7])
oldProfilesMN$state_ident[nchar(oldProfilesMN$state_ident) == 7 & !is.na(oldProfilesMN$state_ident)] <- paste0('0', oldProfilesMN$state_ident[nchar(oldProfilesMN$state_ident) == 7 & !is.na(oldProfilesMN$state_ident)])

# Add columns and change names of columns in newProfiles to match oldProfilesMN
newProfiles.1 <- newProfiles %>%
  mutate(state = 'MN',
         year = as.integer(word(SAMPLE_DATE, 1, 1, sep = '-')),
         doy = as.integer(strftime(SAMPLE_DATE, format = '%j')),
         SAMPLE_DATE = as.character(SAMPLE_DATE),
         monitoringlocationidentifier = NA,
         monitoringlocationname = NA,
         do_perc_sat = NA,
         measurecount_doppm = NA, # no clue what this or the other measurecount columns mean, so just put NA for now
         measurecount_dopercsat = NA,
         measurecount_temp = NA,
         flag_do = NA,
         falg_doperc = NA,
         flag_temp = NA,
         flag_dep_diff = NA,
         profile_ident = paste(ID_NBR, SAMPLE_DATE, GEAR, sep = '/')) %>%
  rename(state_ident = 'ID_NBR',
         date = 'SAMPLE_DATE',
         depth_m = 'WATER_DEPTH_M',
         temp_c = 'WATER_TEMPERATURE_C',
         do_ppm = 'DISSOLVED_OXYGEN_PPM') %>%
  select(-c(SURVEY_ID, SURVEY_COMPONENT_ID, FISHERIES_WATERBODY_ID, AREA, CTY, WB_NM, SRVY_DT, SRV_TYPE, SRV_STATUS, GEARTP, GEAR, ID, WATER_DEPTH_FEET))

# Get number of measurements taken in each profile
newProfiles.2 <- newProfiles.1 %>%
  group_by(profile_ident) %>%
  summarise(n_dep_inprofile = n_distinct(depth_m))
newProfiles.3 <- newProfiles.1 %>%
  full_join(newProfiles.2, by = 'profile_ident')

# Use old profile data to add missing lake info to new data
oldProfilesLakeInfo <- oldProfilesMN %>%
  distinct(state_ident, .keep_all = T)
newProfiles.4 <- newProfiles.3 %>%
  left_join(oldProfilesLakeInfo %>% select(state_ident,
                                           usgs_nhdhr_id,
                                           link_nhd_id,
                                           link_lagosid,
                                           latitude,
                                           longitude,
                                           lake_geomratio,
                                           lake_area_m2,
                                           lake_watershdarea_ha,
                                           lake_maxdepth_m,
                                           Max_Depth), by = 'state_ident')

# Combine old and new data
newProfiles.4 <- newProfiles.4[, names(oldProfilesMN)]
allProfiles <- rbind(oldProfilesMN, newProfiles.4)

# There are a lot of rows that don't have lake IDs, try to fill in those gaps
# Identify profiles that have a DOW for all measurements, profiles that have at least 1 measurement with a DOW or nhdhr id,
#   and profiles that don't have any lake IDs for any measurements

allProfiles$usgs_nhdhr_id[allProfiles$usgs_nhdhr_id == ''] <- NA
allProfiles$link_nhd_id[allProfiles$link_nhd_id == ''] <- NA
allProfiles <- allProfiles %>%
  group_by(profile_ident) %>%
  mutate(DOWs = case_when(all(is.na(state_ident)) ~ 'none',
                          all(!is.na(state_ident)) ~ 'all',
                          .default = 'some'),
         NHD_links = case_when(all(is.na(link_nhd_id)) ~ 'none',
                          all(!is.na(link_nhd_id)) ~ 'all',
                          .default = 'some'))
xwalk$site_id <- word(xwalk$site_id, 2, 2, sep = '_')
xwalk$MNDOW_ID <- word(xwalk$MNDOW_ID, 2, 2, sep = '_')
fix <- allProfiles %>%
  filter(DOWs == 'none',
         NHD_links == 'all')
fix <- merge(x = fix, y = xwalk, by.x = 'link_nhd_id', by.y = 'site_id')
# there are duplicates of the nhdr ids in the crosswalk, need to talk to denver about this
# for now just forget about the profiles that don't have a DOW


# Clean
rm(newProfiles, newProfiles.1, newProfiles.2, newProfiles.3, newProfiles.4, oldProfilesLakeInfo, oldProfilesMN)

# Save this data
write.csv(allProfiles, 'updated_MN_profiles_2025.csv', row.names = F)






