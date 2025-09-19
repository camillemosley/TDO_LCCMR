#Coldwater habitat for lakes and watershed info
#CM 9/9/2025, some lines adapted from CC MW Lake Prep R script 

setwd("/Users/cammosley/Library/CloudStorage/GoogleDrive-cmosley@umn.edu/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/")
#quick check MGLP 
library(readxl)
library(data.table)
library(tidyverse)
library(fields)
library(sf)

MGLP_TDO3_individual_lakes_estimates <- read_excel("MGLP_TDO3_individual_lakes_estimates.xlsx")

MN<-MGLP_TDO3_individual_lakes_estimates[MGLP_TDO3_individual_lakes_estimates$STATE=="MN",]
MN$DOW<-substr(MN$`Lake id`)
#Caroline, Nisswa, Black bear  


#look at lagos for forested and developed percentages 
#read in case study lakes 
Lakes<-read.csv("/Users/cammosley/Downloads/Coldwater Habitat Study Lakes - lake info.csv")


# All LAGOS data obtained from 
# LAGOS-US Geo https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1136.3
# LAGOS-US Locus https://portal.edirepository.org/nis/mapbrowse?packageid=edi.854.1
# LAGOS land use data
lagos_lc <- read.csv("/Users/cammosley/Library/CloudStorage/GoogleDrive-cmosley@umn.edu/Shared drives/Hansen Lab/RESEARCH PROJECTS/Isotope cross lake analysis - CM/Data/lake covariate data/zone_landuse.csv")
setDT(lagos_lc)
# watershed spatial division
lagos_ws <- lagos_lc[spatial_division=="ws"]
rm(lagos_lc)

lagos_info <- read.csv("/Users/cammosley/Library/CloudStorage/GoogleDrive-cmosley@umn.edu/Shared drives/Hansen Lab/RESEARCH PROJECTS/Isotope cross lake analysis - CM/Data/lake covariate data/lake_information.csv") 
setDT(lagos_info)

lagos_nhd <- lagos_info[,site_id:=paste0("nhdhr_",lake_nhdid)
                        # filtering to sites within MN fish data
][site_id %in% unique(MWlakes$site_id)
  # selecting columns and mutating data types
][,.(lagoslakeid=as.character(lagoslakeid),
     zoneid=as.character(ws_zoneid),
     lake_elevation_m,
     lon=lake_lon_decdeg,
     lat=lake_lat_decdeg),
  by=site_id]

lagos_char <- read.csv("/Users/cammosley/Library/CloudStorage/GoogleDrive-cmosley@umn.edu/Shared drives/Hansen Lab/RESEARCH PROJECTS/Isotope cross lake analysis - CM/Data/lake covariate data/lake_characteristics (1).csv")
setDT(lagos_char)

# selecting columns and mutating data types
lagos_char_2 <- lagos_char[,.(lagoslakeid=as.character(lagoslakeid),lake_waterarea_ha,lake_perimeter_m,lake_shorelinedevfactor)]

lagos_nhd_2 <- data.table::merge.data.table(lagos_nhd,
                                            lagos_char_2,
                                            by="lagoslakeid")
# merging all LAGOS data for most current data
lagos_landuse <- merge(lagos_nhd_2,
                       lagos_ws,
                       by="zoneid")[,
                                    .(lon,lat,
                                      elevation=lake_elevation_m,
                                      lakearea=lake_waterarea_ha,
                                      lakeperimeter=lake_perimeter_m,
                                      lakeshorelinefactor=lake_shorelinedevfactor,
                                      total_dev = sum(c(nlcd_devopen21_pct, nlcd_devlow22_pct,
                                                        nlcd_devmed23_pct, nlcd_devhi24_pct), na.rm=T),
                                      total_ag = sum(c(nlcd_past81_pct, nlcd_cultcrop82_pct), na.rm=T),
                                      total_for = sum(nlcd_fordec41_pct,nlcd_forcon42_pct,nlcd_formix43_pct,na.rm=T)),
                                    by=c("site_id","year")][year==2016]

setnames(lagos_landuse,"year","NLCDyear")
lagos_CW <- data.table::merge.data.table(Dow_lagos,lagos_landuse,
                                         by=c("site_id"))


DOW<-Lakes$DOW

#nhdhr id for lagos ids
lagosus_nhdhr_xwalk <- readRDS("~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Crosswalks/lagosus_nhdhr_xwalk.rds")

#DOW to nhdhr for lagos 
dow_nhdhr_xwalk<-read.csv("/Users/cammosley/Downloads/Copy of mndow_nhdhr_xwalk.csv")

Dow_lagos<-full_join(lagosus_nhdhr_xwalk,dow_nhdhr_xwalk,by="site_id")
Dow_lagos<-Dow_lagos[!is.na(Dow_lagos$MNDOW_ID),]
Lakes$MN_DOWID<-paste("mndow_",Lakes$DOW,sep = "")
Dow_lagos<-Dow_lagos[Dow_lagos$MNDOW_ID%in%c(Lakes$MN_DOWID),]

CW_lagos<-inner_join(Dow_lagos,lagos_info)

CW_all<-full_join(CW_lagos,lagos_CW, by="site_id")

#MN DNR data for lake

MNDNR<-read.csv("Shortcut to Proposal docs LCCMR 2025 coldwater fish/Data and Scripts/MN_lakes_watershed_lulc_fromDNR.csv")
Lakes$DOW<-as.integer(Lakes$DOW)
MNDNR<-inner_join(MNDNR,Lakes,by="DOW")
write.csv(MNDNR, "Case_Study_Lakes_MNDNR_LakeInfo.csv")
