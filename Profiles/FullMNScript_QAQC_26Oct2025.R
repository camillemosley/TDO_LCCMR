# Code sent from Heidi on Oct 27, 2025 to clean and format profile data. Used to
# help write compile_clean_MN_profiles.R script

#MGLP Project
#Jacob Angus 30June2022
#updated Nov/Dec 2024, Rantala and Knoll

#  Script for Minnesota that can be run as a job
#  It can also be run in individual sections
#  It has five major sections: Data Cleaning and Joining, Filtering,
#  Interpolation, Calculating VHOD, Calculating TDO3

library("tidyverse")
library("lubridate")
library("zoo")
library("rLakeAnalyzer")
library("broom")
library("readxl")
library("plotly")
library("skimr")
library("rMR")

###### Data Cleaning, Preparation, and Joining ==================================
#  The goal of this section of the script is to filter lakes for
#  geometric ratio, combine data sets with metadata, and join Minnesota
#  data sets together. It is split into two sections: WQP and MNPCA
#  Run time is about 5 minutes

#setwd("C:/Users/herantal/OneDrive - State of Minnesota - MN365/Documents/Coldwater_MGLP2020/Jacob")
#data<-read.csv("MN_WI_MI_profiles.csv", na.strings = "NA")
setwd('~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/')
data <- read.csv('MN_profiles.csv', stringsAsFactors = F)

###### Filtering the Data in Preparation for Interpolation =================
#  The goal of this section is to filter the data by several factors:
#  Lakes must have a profile at the start of stratification (121 <= DOY <= 166)
#  Lakes must have a profile at the end of stratification (196 <= DOY <= 258)
#  Profiles must be taken from glacial lakes
#  Profiles will be interpolated at 1m depths 
#  Data points where DO < 2 mg/L will be discarded due to non-linearity 
#  Run time is about 3.5 minutes

MN.ALL.Data = data

#Filtering out unrealistic data AND data in dataset because it has multiple names
#for example NPS data with MPCA codes
profiles<-MN.ALL.Data%>%
  distinct(link_nhd_id, year, doy, depth_m, temp_c, do_ppm,.keep_all = TRUE)
#need to estimate ppm DO from % saturation then filter out unrealistic
do_sat<-profiles%>%filter(is.na(do_ppm)&do_perc_sat!="NA")
do_sat$do_ppm<-DO.unit.convert(do_sat$do_perc_sat, DO.units.in="pct",DO.units.out="mg/L", bar.units.in="atm",bar.press=1,temp.C=do_sat$temp_c)
do_ppm1<-profiles%>%filter(do_ppm!="NA")#53,000

profiles2<-bind_rows(do_ppm1, do_sat)%>%
  filter(do_ppm>0&do_ppm<30&
           temp_c>=0&temp_c<=40)
#okay, now average field duplicates
field_dups<-profiles2%>%#field QA/QC
  select(link_nhd_id, monitoringlocationidentifier,year, doy, depth_m, temp_c, do_ppm)%>%
  distinct()%>%
  group_by(link_nhd_id, monitoringlocationidentifier,year, doy,depth_m)%>%
  summarise(n_meas=n())%>%
  filter(n_meas>1)%>%
  left_join(profiles2, by=c("link_nhd_id","monitoringlocationidentifier","year","doy","depth_m"))%>%
  group_by(profile_ident,depth_m)%>%
  summarise(temp_c=mean(temp_c), do_ppm=mean(do_ppm),do_perc_sat=mean(do_perc_sat))
field_dups2<-profiles2%>%filter(profile_ident%in%field_dups$profile_ident)%>%
  select(-c(temp_c,do_ppm,do_perc_sat))%>%
  distinct()%>%
  right_join(field_dups, by=c("profile_ident","depth_m"))%>%
  mutate(lake_yr=paste(monitoringlocationidentifier,year, sep = "/"))%>%
  select(state:depth_m, temp_c:do_perc_sat,measurecount_doppm:lake_yr)
#create a 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))
profiles2.1<-profiles2%>%filter(profile_ident%!in%field_dups2$profile_ident)%>%
  bind_rows(field_dups2)

# let's see how many profiles there are from our current and candidate case study lakes
caseStudies <- read_xlsx('Case_Study_Lakes_Info.xlsx')
caseStudies <- caseStudies[!caseStudies$Monitoring_Org_Program %in% c('SCWRS', '1854TA'),]

profiles2.1$state_ident <- as.character(profiles2.1$state_ident)
profiles2.1$state_ident[nchar(profiles2.1$state_ident) == 7 & !is.na(profiles2.1$state_ident)] <- paste0('0', profiles2.1$state_ident[nchar(profiles2.1$state_ident) == 7 & !is.na(profiles2.1$state_ident)])
all(caseStudies$MN_DOWID %in% profiles2.1$state_ident)

caseStudyProfiles <- profiles2.1[profiles2.1$state_ident %in% caseStudies$MN_DOWID,]
caseStudyProfilesAug <- caseStudyProfiles[word(caseStudyProfiles$date, 2, 2, sep = '-') == '08',]
all(caseStudies$MN_DOWID %in% caseStudyProfilesAug$MN_DOWID)
caseStudies$Lake_Name[caseStudies$MN_DOWID %in% caseStudyProfilesAug$state_ident]
















