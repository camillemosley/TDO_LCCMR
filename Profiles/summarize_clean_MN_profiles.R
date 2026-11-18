# Created by GH  Sept 2026
#
# 

setwd('D:/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data')
library(readxl)
library(tidyverse)
rm(list = ls())



# read in data
updated_MN_profiles_2025 <- read_csv("TDO profiles/updated_MN_profiles_2025.csv", 
                                      col_types = cols(date = col_date(format = "%Y-%m-%d"))) 

good.profiles=updated_MN_profiles_2025%>% filter(!is.na(lake_maxdepth_m)) %>% filter(n_dep_inprofile>4& !is.na(depth_m& !is.na(do_ppm) )) 

#remove lakes that don't cover at least 60% of max depth and those over 1.2 the max depth 
profile.count=good.profiles %>% group_by(link_nhd_id ,profile_ident, lake_maxdepth_m, date, year) %>% summarize(n_depths=length(do_ppm), max_profile_depth=max(depth_m)) %>% mutate(ratio=max_profile_depth/lake_maxdepth_m) %>% filter(ratio>.6&ratio<1.2)
 
good.profiles=good.profiles[good.profiles$profile_ident%in%profile.count$profile_ident,]

lake.year.count= good.profiles %>% group_by(link_nhd_id, year) %>% summarise(n.dates=length(unique(profile_ident)))



