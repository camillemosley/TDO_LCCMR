# Calculates TDO3 and other oxythermal habitat metrics and generates temperature-dissolved oxygen profile plots
# Created by Derek Bahr, MNDNR, 1/27/2025, updated on 10/29/2025
# Modified by Mike M. to work with our profile data
# To use, you will need to change the working directory (line 7) and then run the code
# If a different dissolved oxygen or temperature threshold is desired for calculations, those can be changed in lines 26 and 27
# Input: updated_MN_profiles_2025.csv
# Output: TDO_report_statistics_20251114.csv

# load required packages
library(tidyverse)
library(pracma)
library(beepr)
library(here)
source("rMR.R")

# load temp/DO and survey data
tempdo <- read.csv("updated_MN_profiles_2025.csv", header = TRUE, stringsAsFactors = F)
tempdo <- tempdo %>%
  rename(SAMPLE_DATE = "date",
         WATER_DEPTH_M = "depth_m",
         WATER_TEMPERATURE_C = "temp_c",
         DISSOLVED_OXYGEN_PPM = "do_ppm",
         SAMPLE_DATE = "date",
         ID_NBR = "state_ident",
         PROFILE = "profile_ident")
tempdo$SAMPLE_DATE <- as.Date(tempdo$SAMPLE_DATE)
tempdo <- tempdo[order(tempdo$ID_NBR, tempdo$SAMPLE_DATE, tempdo$PROFILE, tempdo$WATER_DEPTH_M),]
tempdo$ID_NBR <- as.character(tempdo$ID_NBR)
tempdo$ID_NBR[nchar(tempdo$ID_NBR) == 7 & !is.na(tempdo$ID_NBR)] <- paste0(0, tempdo$ID_NBR[nchar(tempdo$ID_NBR) == 7 & !is.na(tempdo$ID_NBR)])

# filter out profiles we don"t want
# remove duplicated rows
tempdo.1 <- tempdo %>%
  distinct(ID_NBR, year, doy, WATER_DEPTH_M, WATER_TEMPERATURE_C, DISSOLVED_OXYGEN_PPM, .keep_all = T)
# calc ppm from sat values for rows we don"t have ppm for
do_sat <- tempdo.1 %>%
  filter(is.na(DISSOLVED_OXYGEN_PPM) & !is.na(do_perc_sat))
do_sat$DISSOLVED_OXYGEN_PPM <- DO.unit.convert(do_sat$do_perc_sat, DO.units.in="pct", DO.units.out="mg/L", bar.units.in = "atm", bar.press = 1, temp.C = do_sat$WATER_TEMPERATURE_C)
do_ppm1 <- tempdo.1 %>% filter(!is.na(DISSOLVED_OXYGEN_PPM))
# filter out impossible and rows that don"t have any DO measurements
tempdo.2 <- bind_rows(do_ppm1, do_sat) %>%
  filter(DISSOLVED_OXYGEN_PPM > 0 & DISSOLVED_OXYGEN_PPM < 30 &
           WATER_TEMPERATURE_C >= 0 & WATER_TEMPERATURE_C <= 40)
# some profiles are duplicates (intentionally) so average them
field_dups <- tempdo.2 %>% #field QA/QC
  select(link_nhd_id, monitoringlocationidentifier, year, doy, WATER_DEPTH_M, WATER_TEMPERATURE_C, DISSOLVED_OXYGEN_PPM)%>%
  distinct()%>%
  group_by(link_nhd_id, monitoringlocationidentifier, year, doy, WATER_DEPTH_M)%>%
  summarise(n_meas = n())%>%
  filter(n_meas > 1)%>%
  left_join(tempdo.2, by = c("link_nhd_id", "monitoringlocationidentifier", "year", "doy", "WATER_DEPTH_M")) %>%
  group_by(PROFILE, WATER_DEPTH_M)%>%
  summarise(WATER_TEMPERATURE_C = mean(WATER_TEMPERATURE_C), DISSOLVED_OXYGEN_PPM = mean(DISSOLVED_OXYGEN_PPM), do_perc_sat = mean(do_perc_sat))
field_dups2 <- tempdo.2 %>% filter(PROFILE %in% field_dups$PROFILE) %>%
  select(-c(WATER_TEMPERATURE_C, DISSOLVED_OXYGEN_PPM, do_perc_sat)) %>%
  distinct() %>%
  right_join(field_dups, by = c("PROFILE","WATER_DEPTH_M")) %>%
  mutate(lake_yr = paste(ID_NBR, monitoringlocationidentifier, year, sep = "/")) %>%
  select(state:WATER_DEPTH_M, WATER_TEMPERATURE_C:do_perc_sat, measurecount_doppm:lake_yr)
# create a "not in" function
#"%!in%" <- function(x,y)!("%in%"(x,y))
#tempdo.3 <- tempdo.2 %>% filter(PROFILE %!in% field_dups2$PROFILE)%>%
tempdo.3 <- tempdo.2 %>% filter(!PROFILE %in% field_dups2$PROFILE)%>%
  bind_rows(field_dups2)
# Remove Lake Superior Sites, Mines, and Pits ~ We only want Glacial Lakes
tempdo.4 <- tempdo.3 %>%
  filter(!monitoringlocationidentifier %in% c("EPA_GLNPO-SU19", #Lake Superior Site
                                              "EPA_GLNPO-SU18", #Lake Superior Site
                                              "EPA_GLNPO-SU104550", #Lake Superior Site
                                              "21GPBCH-LS_PT_1", #Lake Superior Site
                                              "21GPBCH-LS_PT_9", #Lake Superior Site
                                              "GPC5_WQX-LS_PT_1", #Lake Superior Site
                                              "GPC5_WQX-LS_PT_9", #Lake Superior Site
                                              "GPC5_WQX-LS_PT_8", #Lake Superior Site
                                              "MNPCA-69-1297-00-202", #Sherman Pit
                                              "MNPCA-69-0429-00-201", #Sabin Pit
                                              "21GPBCH-LS_PT_8", #Lake Superior Site
                                              "MNPCA-18-0440-01-100", #Mahnomen Mine
                                              "MNPCA-69-1297-00-201", #Sherman Pit
                                              "MNPCA-18-0439-00-100", #Pennington Mine
                                              "MNPCA-31-1325-04-201", #Canisteo Mine Pit
                                              "MNPCA-69-1298-00-203", #South Twin City Pit
                                              "MNPCA-18-0093-01-202", #Rabbit Lake
                                              "MNPCA-18-0041-01-201", #Crooked Lake - Silver Bay
                                              "MNPCA-69-0429-00-100", #Embarrass Mine
                                              "MNPCA-69-0428-00-201"), #St. James Pit
         !ID_NBR %in% c("18044001",
                        "69129700",
                        "18043900",
                        "31132504",
                        "69129800",
                         "18009301",
                        "18004101",
                        "69042900",
                        "69042800"))

# specify month range for report statistics output file and year for profile plots
month_min <- 8
month_max <- 8

# subset temp data by survey and months (June-September), remove NAs and profiles with <3 depth increments, review outlier values, and add profile name and max depth for each lake
subset_tempdo <- tempdo.3[!is.na(tempdo$SAMPLE_DATE),]
subset_tempdo <- subset_tempdo[!is.na(subset_tempdo$WATER_DEPTH_M),]
subset_tempdo <- subset_tempdo[!is.na(subset_tempdo$WATER_TEMPERATURE_C),]
subset_tempdo <- subset_tempdo[!is.na(subset_tempdo$DISSOLVED_OXYGEN_PPM),]
subset_tempdo <- subset_tempdo %>% filter(month(SAMPLE_DATE) %in% month_min:month_max)
boxplot(subset_tempdo$WATER_TEMPERATURE_C)
boxplot(subset_tempdo$DISSOLVED_OXYGEN_PPM)
summary(subset_tempdo)
str(subset_tempdo)
subset_tempdo.1 <- subset_tempdo %>%
  group_by(PROFILE) %>%
  summarise(COUNT = n_distinct(WATER_DEPTH_M))
subset_tempdo <- subset_tempdo %>%
  full_join(subset_tempdo.1, by = "PROFILE")
#subset_tempdo$COUNT <- as.numeric(ave(subset_tempdo$PROFILE,subset_tempdo[,c("PROFILE","ID_NBR","SAMPLE_DATE")],FUN=length))
subset_tempdo <- subset(subset_tempdo, COUNT > 2)
#subset_tempdo$PROFILE <- paste(subset_tempdo$ID_NBR,subset_tempdo$SAMPLE_DATE,subset_tempdo$GEAR)

# create tables from which to calculate TDOx
agg <- aggregate(. ~ PROFILE + ID_NBR + SAMPLE_DATE, data = subset_tempdo, FUN = length,na.action = na.pass)
levcom <- agg[, c("PROFILE", "ID_NBR", "SAMPLE_DATE")]

# specify dissolved oxygen and temperature thresholds
# DO thresholds to use for TDOx: 1, 2, 3, 4, 5, 6
# Cisco layer thickness thresholds to try:
#   20 ºC and 3 mg/L (Frey 1955)
#   17 ºC and 3 mg/L (Jiang et al. 2012, Magee et al. 2019)
#   22 ºC and 3 mg/L (Dembkowski et al. 2024)
#   22.8 ºC and 6 mg/L (Lyons et al. 2017)
#   Lethal nice boundary: DOlethal = 0.40 + 0.0000060e^(0.59*T) (Jacobson et al. 2008)
DO_Threshold <- 3
Temp_Threshold <- 21.2   # proposed temp threshold for cisco: 21.2, proposed temp threshold for lake whitefish: 17.2, proposed temp threshold for lake trout: 8.8

# calculate TDOx (when DO_Threshold <- 3, this metric is TDO3) and other habitat metrics
TDxOUT <- NULL
f <- function(Temp) 0.4 + 0.000006*exp(0.59*Temp) - (slope*Temp + int)
for(i in 1:nrow(levcom)) {     
  
  PROFILE <- levcom[i, 1]
  ID_NBR <- levcom[i, 2]
  SAMPLE_DATE <- levcom[i, 3]
  tcalc <- subset(subset_tempdo, PROFILE == levcom[i, 1] & ID_NBR == levcom[i, 2] & SAMPLE_DATE == levcom[i, 3])
  tcalc <- tcalc[order(tcalc$WATER_DEPTH_M, decreasing = TRUE),]
  
  slopedo <- diff(tcalc$DISSOLVED_OXYGEN_PPM) / diff(tcalc$WATER_DEPTH_M)
  slopetemp <- diff(tcalc$WATER_TEMPERATURE_C) / diff(tcalc$WATER_DEPTH_M)
  slopedodepth <- diff(tcalc$WATER_DEPTH_M) / diff(tcalc$DISSOLVED_OXYGEN_PPM)
  slopetempdepth <- diff(tcalc$WATER_DEPTH_M) / diff(tcalc$WATER_TEMPERATURE_C)
  dosurface <- tail(tcalc$DISSOLVED_OXYGEN_PPM, 1)
  domax <- max(tcalc$DISSOLVED_OXYGEN_PPM)
  dobottom <- head(tcalc$DISSOLVED_OXYGEN_PPM, 1)
  domin <- min(tcalc$DISSOLVED_OXYGEN_PPM)
  tempsurface <- tail(tcalc$WATER_TEMPERATURE_C, 1)
  tempmax <- max(tcalc$WATER_TEMPERATURE_C)
  tempbottom <- head(tcalc$WATER_TEMPERATURE_C, 1)
  tempmin <- min(tcalc$WATER_TEMPERATURE_C)
  dolethal <- 0.4 + 0.000006*exp(0.59*tcalc$WATER_TEMPERATURE_C) # lethal niche boundary (Jacobson et al. 2008)
  lethal_nonlethal <- c()
  for (j in 1:nrow(tcalc)) {
    if (tcalc$DISSOLVED_OXYGEN_PPM[j] < dolethal[j]) {
      lethal_nonlethal <- c(lethal_nonlethal, "lethal")
    } else {
      lethal_nonlethal <- c(lethal_nonlethal, "nonlethal")
    }
  }
  tcalc$lethal_nonlethal <- lethal_nonlethal
  letha_nonlethal_surface <- tail(tcalc$lethal_nonlethal, 1)
  
  tcalcint <- head(tcalc, -1)
  tcalcint$slopedo <- slopedo
  tcalcint$slopetemp <- slopetemp
  tcalcint$slopedodepth <- slopedodepth
  tcalcint$slopetempdepth <- slopetempdepth
  tcalcint$interceptdo = (tcalcint$DISSOLVED_OXYGEN_PPM) - (tcalcint$WATER_DEPTH_M) * tcalcint$slopedo
  tcalcint$intercepttemp = (tcalcint$WATER_TEMPERATURE_C) - (tcalcint$WATER_DEPTH_M) * tcalcint$slopetemp
  tcalcint$interceptdodepth = (tcalcint$WATER_DEPTH_M) - (tcalcint$DISSOLVED_OXYGEN_PPM) * tcalcint$slopedodepth
  tcalcint$intercepttempdepth = (tcalcint$WATER_DEPTH_M) - (tcalcint$WATER_TEMPERATURE_C) * tcalcint$slopetempdepth
  
  tcalcint$niche.border <- NA
  for (k in 1:nrow(tcalcint)) {
    if (k == nrow(tcalcint)) {
      if (tcalcint$lethal_nonlethal[k] == tail(tcalc$lethal_nonlethal, 1)) {
        tcalcint$niche.border[k] <- "n"
      } else {
        tcalcint$niche.border[k] <- "y"
      }
    } else {
      if (tcalcint$lethal_nonlethal[k] == tcalcint$lethal_nonlethal[k+1]) {
      tcalcint$niche.border[k] <- "n"
      } else {
      tcalcint$niche.border[k] <- "y"
      }
    }
  }
  boundaries <- tcalcint %>%
    filter(niche.border == "y")
  if (nrow(boundaries) > 2) {
    boundaries <- boundaries[c(1, nrow(boundaries)),]
    }
  if (nrow(boundaries) == 0) {
    bottom.layer <- min(tcalc$WATER_DEPTH_M)
    top.layer <- max(tcalc$WATER_DEPTH_M)
    } else if (nrow(boundaries) == 1) {
      if (tcalcint$lethal_nonlethal[which(tcalcint$niche.border == "y")+1] == "nonlethal") {
        slope <- (boundaries$do.after[1]-boundaries$DISSOLVED_OXYGEN_PPM[1])/(boundaries$temp.after[1]-boundaries$WATER_TEMPERATURE_C[1])
        int <- boundaries$do.after[1]-slope*boundaries$temp.after[1]
        bottom.layer.temp <- uniroot(f, c(boundaries$WATER_TEMPERATURE_C[1], boundaries$temp.after[1]))$root
        bottom.layer.DO <- 0.4 + 0.000006*exp(0.59*bottom.layer.temp)
        bottom.layer.depth <- (bottom.layer.DO - boundaries$interceptdo[1])/boundaries$slopedo[1]
        top.layer.depth <- 0
        top.layer.temp <- tempsurface
        top.layer.DO <- dosurface
      } else {
        bottom.layer.depth <- max(tcalc$WATER_DEPTH_M)
        botton.layer.temp <- tempbottom
        bottom.layer.DO <- dobottom
        slope <- (boundaries$do.after[1]-boundaries$DISSOLVED_OXYGEN_PPM[1])/(boundaries$temp.after[1]-boundaries$WATER_TEMPERATURE_C[1])
        int <- boundaries$do.after[1]-slope*boundaries$temp.after[1]
        top.layer.temp <- uniroot(f, c(boundaries$WATER_TEMPERATURE_C[1], boundaries$temp.after[1]))$root
        top.layer.DO <- 0.4 + 0.000006*exp(0.59*top.layer.temp)
        top.layer.depth <- (top.layer.DO - boundaries$interceptdo[1])/boundaries$slopedo[1]
        }
    } else if (nrow(boundaries) == 2) {
      slope <- (boundaries$do.after[1]-boundaries$DISSOLVED_OXYGEN_PPM[1])/(boundaries$temp.after[1]-boundaries$WATER_TEMPERATURE_C[1])
      int <- boundaries$do.after[1]-slope*boundaries$temp.after[1]
      bottom.layer.temp <- uniroot(f, c(boundaries$WATER_TEMPERATURE_C[1], boundaries$temp.after[1]))$root
      bottom.layer.DO <- 0.4 + 0.000006*exp(0.59*bottom.layer.temp)
      bottom.layer.depth <- (bottom.layer.DO - boundaries$interceptdo[1])/boundaries$slopedo[1]
      slope <- (boundaries$do.after[2]-boundaries$DISSOLVED_OXYGEN_PPM[2])/(boundaries$temp.after[2]-boundaries$WATER_TEMPERATURE_C[2])
      int <- boundaries$do.after[2]-slope*boundaries$temp.after[2]
      top.layer.temp <- uniroot(f, c(boundaries$WATER_TEMPERATURE_C[2], boundaries$temp.after[2]))$root
      top.layer.DO <- 0.4 + 0.000006*exp(0.59*top.layer.temp)
      top.layer.depth <- (bottom.layer.DO - boundaries$interceptdo[2])/boundaries$slopedo[2]
      }
  CLT_m <- bottom.layer.depth - top.layer.depth
  
  tcalcint$do.after <- c(tcalcint$DISSOLVED_OXYGEN_PPM[2:(length(tcalcint$DISSOLVED_OXYGEN_PPM))], dosurface)
  tcalcint$temp.after <- c(tcalcint$WATER_TEMPERATURE_C[2:(length(tcalcint$WATER_TEMPERATURE_C))], tempsurface)
  do_threshold_depth.row <- which((tcalcint$DISSOLVED_OXYGEN_PPM <= DO_Threshold) & (tcalcint$do.after > DO_Threshold))
  tcalcint$temp.after <- c(tcalcint$WATER_TEMPERATURE_C[2:(length(tcalcint$WATER_TEMPERATURE_C))], tempsurface)
  temp_threshold_depth.row <- which((tcalcint$WATER_TEMPERATURE_C <= Temp_Threshold) & (tcalcint$temp.after > Temp_Threshold))
  tdox.row <- which((tcalcint$DISSOLVED_OXYGEN_PPM <= DO_Threshold) & (tcalcint$do.after > DO_Threshold))
  
  calcrow <- tdox.row[c(0, 1)]
  tdox <- if (dobottom >= DO_Threshold) {
    head(tcalc$WATER_TEMPERATURE_C, 1)
  } else {
      if (min(tcalc$DISSOLVED_OXYGEN_PPM) > DO_Threshold) {
        min(tcalc$WATER_TEMPERATURE_C)
      } else {
          if (max(tcalc$DISSOLVED_OXYGEN_PPM) < DO_Threshold) {
            "NA"
          } else {
              ((DO_Threshold - (tcalcint[calcrow, "interceptdo"])) / (tcalcint[calcrow, "slopedo"])) * (tcalcint[calcrow, "slopetemp"]) + (tcalcint[calcrow, "intercepttemp"])
            }
        }
    }
  
  calcrow <- do_threshold_depth.row[c(0, 1)]
  do_threshold_depth_m <- if (dobottom >= DO_Threshold) {
    head(tcalc$WATER_DEPTH_M, 1)
  } else {
      if (min(tcalc$DISSOLVED_OXYGEN_PPM) > DO_Threshold) {
        max(tcalc$WATER_DEPTH_M)
      } else {
          if (max(tcalc$DISSOLVED_OXYGEN_PPM) < DO_Threshold) {
            min(tcalc$WATER_DEPTH_M)
          } else {
              (((tcalcint[calcrow, "slopedodepth"])) * DO_Threshold + (tcalcint[calcrow, "interceptdodepth"]))}
      }
    }
  do_threshold_depth_ft <- (do_threshold_depth_m * 3.28)
  
  calcrow <- temp_threshold_depth.row[c(0, 1)]
  temp_threshold_depth_m <- if(tempsurface <= Temp_Threshold) {
    tail(tcalc$WATER_DEPTH_M, 1)
  } else {
      if (max(tcalc$WATER_TEMPERATURE_C) < Temp_Threshold) {
        min(tcalc$WATER_DEPTH_M)
      } else {
          if (min(tcalc$WATER_TEMPERATURE_C) > Temp_Threshold) {
            max(tcalc$WATER_DEPTH_M)
          } else {
              (((tcalcint[calcrow, "slopetempdepth"])) * Temp_Threshold + (tcalcint[calcrow, "intercepttempdepth"]))
            }
        }
    }
  temp_threshold_depth_ft <- (temp_threshold_depth_m * 3.28)

  max_depth_m <- max(tcalc$WATER_DEPTH_M)
  max_depth_ft <- (max_depth_m * 3.28)
  layer_thickness_m <- (do_threshold_depth_m - temp_threshold_depth_m)
  layer_thickness_ft <- (layer_thickness_m * 3.28)
  tdox.stats <- data.frame(PROFILE, ID_NBR, SAMPLE_DATE, tdox, do_threshold_depth_m, do_threshold_depth_ft, temp_threshold_depth_m, temp_threshold_depth_ft, layer_thickness_m, layer_thickness_ft, max_depth_m, max_depth_ft, tempsurface, tempmax, tempbottom, tempmin, dosurface, domax, dobottom, domin)
  
  TDxOUT = rbind(TDxOUT, tdox.stats)
}

# create output file
#TDxOUT$PROFILE <- paste(TDxOUT$PROFILE, TDxOUT$ID_NBR, TDxOUT$SAMPLE_DATE)
tdtable <- TDxOUT[order(TDxOUT$PROFILE, TDxOUT$ID_NBR, TDxOUT$SAMPLE_DATE),]
tdtable$tdox <- as.numeric(tdtable$tdox)
beep(9)

# save output file as .csv (remove # from DNR or MPCA .csv file)
write.csv(tdtable, "TDO_report_statistics_20251114.csv", row.names = FALSE)

# Temperature dissolved oxygen profile plots
# calculate max profile depth taken per lake to scale y axis
subset_tempdo$MAX_DEPTH_FEET <-as.numeric(ave(subset_tempdo$WATER_DEPTH_FEET,subset_tempdo[,c("ID_NBR")],FUN=max))

# left join tdtable, lakes, and subset_tempdo dataframes
subset_tempdo <- merge(subset_tempdo, tdtable, by="PROFILE", all.x=TRUE)
subset_tempdo$PROFILE <- paste(subset_tempdo$Species, subset_tempdo$HUC8, subset_tempdo$Area, subset_tempdo$PROFILE, trimws(format(round(subset_tempdo$tdox,1),nsmall=1)))

# loop plots for each lake, water quality station, and date
profile <- summarise(group_by(subset_tempdo,PROFILE),n=n())

for(i in 1:nrow(profile)) {
  fishplot <- subset(subset_tempdo, PROFILE == as.factor(profile[[1]][i]))
  plotdep <- round(ceiling(max(fishplot$WATER_DEPTH_FEET, na.rm=TRUE)),0)  
  fishplot %>%
    arrange(desc(WATER_DEPTH_FEET))
  fishplot$temp2 <- tryCatch(
    exp = {with(fishplot, interp1(WATER_DEPTH_FEET, WATER_TEMPERATURE_C, WATER_DEPTH_FEET, "linear"))},
    error = function(e) {temp2 = fishplot$WATER_TEMPERATURE_C})
  fishplot$do2 <- tryCatch(
    exp = {with(fishplot, interp1(WATER_DEPTH_FEET, DISSOLVED_OXYGEN_PPM, WATER_DEPTH_FEET, "linear"))},
    error = function(e) {do2 = fishplot$DO})
  maxtemp <- tryCatch(
    exp = {ceiling(max(subset_tempdo$WATER_TEMPERATURE_C/5, na.rm = TRUE))*5},
    warning = function(e){maxtemp=30})
  maxdep <- round(ceiling(max(fishplot$MAX_DEPTH_FEET, na.rm=TRUE)+1),0)
  
  plot <- ggplot(fishplot, aes(x=WATER_DEPTH_FEET)) +
    coord_flip() +
    annotate("rect", xmin = (ifelse(fishplot$layer_thickness_ft<0, 0, fishplot$do_threshold_depth_ft)), xmax = (ifelse(fishplot$layer_thickness_ft<0, 0, (fishplot$do_threshold_depth_ft - fishplot$layer_thickness_ft))), ymin = -Inf, ymax = Inf, 
             fill = "deepskyblue", color = NA) +
    geom_path(aes(y=temp2,color="Temp", linetype="Temp"), linewidth=1.3) +
    geom_path(aes(y=do2, color="DO", linetype="DO"), linewidth=1.3) +
    scale_y_continuous(expand = c(0,0), lim=c(0,maxtemp)) +
    scale_x_reverse(expand = c(0,0), lim=c(maxdep,-2)) +
    labs(title = paste0(fishplot$WB_NM," (",fishplot$ID_NBR.x,") ",fishplot$SAMPLE_DATE.x," ",fishplot$GEAR.x),
         y = "Temperature (\u00b0C)/Dissolved oxygen (mg/L)",
         x = "Depth (ft)") +
    scale_colour_manual("Lines",values = c("Temp" = "black", "DO" = "black")) + 
    scale_linetype_manual("Lines",values = c("Temp" = "solid", 
                                             "DO" = "dotted")) +
    annotate("text", y=(maxtemp/2), x=(fishplot$do_threshold_depth_ft-(fishplot$layer_thickness_ft)/2), 
             label=ifelse(fishplot$layer_thickness_ft <= 0, "", "Coldwater fish oxythermal layer (\u226417.2\u00b0C and \u22653 mg/L)"), size=3) +
    annotate("rect", xmin = plotdep, xmax = Inf, ymin = -Inf, ymax = Inf, 
             fill = "gray60", alpha=0.1, color = NA) +
    annotate("text", y=(maxtemp/2), x=((maxdep - plotdep)/2 + plotdep), 
             label=ifelse((maxdep - plotdep) < 3, "", "lake bottom"), size=3) +
    theme_classic() +
    theme(plot.title = element_text(hjust=0.5))
  print(plot)
  filename=paste0("Plots/",profile[i, 1], ".jpg")
  ggsave(paste0(filename), width=6.5, height=9, dpi=300)
}

# plots will be saved in "Plots" folder within working directory