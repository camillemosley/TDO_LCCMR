setwd('~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/')
library(readxl)
library(tidyverse)
library(ggrepel)
rm(list = ls())

lakeInfo <- read_xlsx('Case_Study_Lakes_MNDNR_LakeInfo.xlsx')
ciscoData <- read.csv('cisco_cpue_MN_cisco.csv', stringsAsFactors = F)
ciscoData$lake_id <- as.character(ciscoData$lake_id)
ciscoData$lake_id[ciscoData$lake_name == 'Boot'] <- '03003000'
ciscoDataCandidates <- ciscoData[ciscoData$lake_id %in% lakeInfo$MN_DOWID & ciscoData$sampling_method == 'gill_net_standard',]
ciscoDataCandidates$date <- as.Date(ciscoDataCandidates$date)
oxyThermalData <- read_xlsx('MGLP_TDO3_individual_lakes_estimates.xlsx')
oxyThermalDataMN <- oxyThermalData[oxyThermalData$STATE == 'MN',]
oxyThermalDataMN$DOW <- substr(oxyThermalDataMN$`Lake id`, start = 3, stop = 10)
oxyThermalDataCandidates <- oxyThermalDataMN[oxyThermalDataMN$DOW %in% lakeInfo$MN_DOWID,]

MN_DOWID <- c()
Cisco_CPUE <- c()
Cisco_Survey_Date <- c()
TDO3 <- c()
Lat <- c()
Long <- c()

for (i in 1:nrow(lakeInfo)) {
  lakeID <- lakeInfo$MN_DOWID[i]
  date <- max(ciscoDataCandidates$date[ciscoDataCandidates$lake_id == lakeID])
  Cisco_CPUE <- c(Cisco_CPUE, ciscoDataCandidates$cpue[ciscoDataCandidates$lake_id == lakeID & ciscoDataCandidates$date == date])
  TDO3 <- c(TDO3, oxyThermalDataCandidates$`TDO3  estimate`[oxyThermalDataCandidates$DOW == lakeID])
  Lat <- c(Lat, oxyThermalDataCandidates$Lat[oxyThermalDataCandidates$DOW == lakeID])
  Long <- c(Long, oxyThermalDataCandidates$Lon[oxyThermalDataCandidates$DOW == lakeID])
  MN_DOWID <- c(MN_DOWID, lakeID)
  Cisco_Survey_Date <- c(as.Date(Cisco_Survey_Date), date)
}

newData <- data.frame(MN_DOWID, Cisco_CPUE, Cisco_Survey_Date, TDO3, Lat, Long)
lakeInfo <- merge(lakeInfo, newData, by = 'MN_DOWID')

ggplot(data = lakeInfo, aes(x = Percent_Disturbed, y = TDO3, colour = Already_Monitored)) +
  geom_point() +
  geom_text_repel(aes(label = Lake_Name))
  
write.csv(lakeInfo, 'Case_Study_Lakes_MNDNR_LakeInfo.csv', row.names = F)



