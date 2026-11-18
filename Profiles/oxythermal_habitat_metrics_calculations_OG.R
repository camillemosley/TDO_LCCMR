# Original R code to calculate TDO3 and other oxythermal habitat metrics and to generate temperature-dissolved oxygen profile plots
# Created by Derek Bahr, MNDNR, 1/27/2025, updated on 10/29/2025
# To use, you will need to change the working directory (line 7) and then run the code
# If a different dissolved oxygen or temperature threshold is desired for calculations, those can be changed in lines 26 and 27

# establish working directory (change this to align with the folder where the .csv and .R files are saved)
setwd("I:/FAW/_Fisheries/3. Habitat/FIBI/_Guidance Documents/R_Scripts/TDO/Shared")

# check for and create subdirectory to save plots to if needed
dir.create(file.path("Plots"))

# install and load required packages
if(!require('ggplot2'))install.packages('ggplot2')
if(!require('ggplot2'))install.packages('tidyverse')
if(!require('pracma'))install.packages('pracma')
library(ggplot2)
library(tidyverse)
library(pracma)

# load temp/DO and survey data
tempdo <- read.csv('TDO_Data.csv', header=TRUE)
tempdo$SAMPLE_DATE <- as.Date(tempdo$SAMPLE_DATE,'%m/%d/%Y')
tempdo <- tempdo[order(tempdo$ID_NBR, tempdo$SAMPLE_DATE, tempdo$GEAR, tempdo$WATER_DEPTH_FEET),]

# specify dissolved oxygen and temperature thresholds
DO_Threshold <- 3
Temp_Threshold <- 8.8   #proposed temp threshold for cisco: 21.2, proposed temp threshold for lake whitefish: 17.2, proposed temp threshold for lake trout: 8.8

# specify month range for report statistics output file and year for profile plots
month_min=6
month_max=9

# subset temp data by survey and months (June-September), remove NAs and profiles with <3 depth increments, review outlier values, and add profile name and max depth for each lake
subset_tempdo <- tempdo[!is.na(tempdo$SAMPLE_DATE),]
subset_tempdo <- subset_tempdo[!is.na(subset_tempdo$WATER_DEPTH_M),]
subset_tempdo <- subset_tempdo[!is.na(subset_tempdo$WATER_TEMPERATURE_C),]
subset_tempdo <- subset_tempdo[!is.na(subset_tempdo$DISSOLVED_OXYGEN_PPM),]
subset_tempdo <- subset_tempdo %>% filter(month(SAMPLE_DATE) %in% month_min:month_max)
boxplot(subset_tempdo$WATER_TEMPERATURE_C)
boxplot(subset_tempdo$DISSOLVED_OXYGEN_PPM)
summary(subset_tempdo)
str(subset_tempdo)
subset_tempdo$COUNT <-as.numeric(ave(subset_tempdo$GEAR,subset_tempdo[,c("ID_NBR","SAMPLE_DATE","GEAR")],FUN=length))
subset_tempdo <- subset(subset_tempdo,COUNT>2)
subset_tempdo$PROFILE <- paste(subset_tempdo$ID_NBR,subset_tempdo$SAMPLE_DATE,subset_tempdo$GEAR)

# create tables from which to calculate TDOx
agg <- aggregate(. ~ ID_NBR + SAMPLE_DATE + GEAR, data = subset_tempdo, FUN = length,na.action = na.pass)
levcom <- agg[, c("ID_NBR", "SAMPLE_DATE", "GEAR")]

# calculate TDOx (when DO_Threshold <- 3, this metric is TDO3) and other habitat metrics
TDxOUT <- NULL
for(i in 1:nrow(levcom)) {     
  
  ID_NBR <- levcom[i, 1]
  SAMPLE_DATE <- levcom[i, 2]
  GEAR <- levcom[i, 3]
  tcalc <- subset(subset_tempdo, ID_NBR == levcom[i, 1] & SAMPLE_DATE == levcom[i, 2] & GEAR == levcom[i, 3])
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
  
  tcalcint <- head(tcalc, -1)
  tcalcint$slopedo <- slopedo
  tcalcint$slopetemp <- slopetemp
  tcalcint$slopedodepth <- slopedodepth
  tcalcint$slopetempdepth <- slopetempdepth
  tcalcint$interceptdo = (tcalcint$DISSOLVED_OXYGEN_PPM) - (tcalcint$WATER_DEPTH_M) * tcalcint$slopedo
  tcalcint$intercepttemp = (tcalcint$WATER_TEMPERATURE_C) - (tcalcint$WATER_DEPTH_M) * tcalcint$slopetemp
  tcalcint$interceptdodepth = (tcalcint$WATER_DEPTH_M) - (tcalcint$DISSOLVED_OXYGEN_PPM) * tcalcint$slopedodepth
  tcalcint$intercepttempdepth = (tcalcint$WATER_DEPTH_M) - (tcalcint$WATER_TEMPERATURE_C) * tcalcint$slopetempdepth
  
  tcalcint$do.after <- c(tcalcint$DISSOLVED_OXYGEN_PPM[2:(length(tcalcint$DISSOLVED_OXYGEN_PPM))], dosurface)
  do_threshold_depth.row <- which((tcalcint$DISSOLVED_OXYGEN_PPM <= DO_Threshold) & (tcalcint$do.after > DO_Threshold))
  tcalcint$temp.after <- c(tcalcint$WATER_TEMPERATURE_C[2:(length(tcalcint$WATER_TEMPERATURE_C))], tempsurface)
  temp_threshold_depth.row <- which((tcalcint$WATER_TEMPERATURE_C <= Temp_Threshold) & (tcalcint$temp.after > Temp_Threshold))
  tdox.row <- which((tcalcint$DISSOLVED_OXYGEN_PPM <= DO_Threshold) & (tcalcint$do.after > DO_Threshold))
  
  calcrow <- tdox.row[c(0, 1)]
  tdox <- if(dobottom >= DO_Threshold) {head(tcalc$WATER_TEMPERATURE_C, 1)} 
  else{ if(min(tcalc$DISSOLVED_OXYGEN_PPM) > DO_Threshold) {min(tcalc$WATER_TEMPERATURE_C)} 
    else{ if(max(tcalc$DISSOLVED_OXYGEN_PPM) < DO_Threshold) {'NA'} 
      else{ ((DO_Threshold - (tcalcint[calcrow, 'interceptdo'])) / (tcalcint[calcrow, 'slopedo'])) * (tcalcint[calcrow, 'slopetemp']) + (tcalcint[calcrow, 'intercepttemp'])}}}
  
  calcrow <- do_threshold_depth.row[c(0, 1)]
  do_threshold_depth_m <- if(dobottom >= DO_Threshold) {head(tcalc$WATER_DEPTH_M, 1)}
  else{ if(min(tcalc$DISSOLVED_OXYGEN_PPM) > DO_Threshold) {max(tcalc$WATER_DEPTH_M)} 
    else{ if(max(tcalc$DISSOLVED_OXYGEN_PPM) < DO_Threshold) {min(tcalc$WATER_DEPTH_M)} 
      else{ (((tcalcint[calcrow, 'slopedodepth'])) * DO_Threshold + (tcalcint[calcrow, 'interceptdodepth']))}}}
  do_threshold_depth_ft <- (do_threshold_depth_m * 3.28)
  
  calcrow <- temp_threshold_depth.row[c(0, 1)]
  temp_threshold_depth_m <- if(tempsurface <= Temp_Threshold) {tail(tcalc$WATER_DEPTH_M, 1)} 
  else{ if(max(tcalc$WATER_TEMPERATURE_C) < Temp_Threshold) {min(tcalc$WATER_DEPTH_M)} 
    else{ if(min(tcalc$WATER_TEMPERATURE_C) > Temp_Threshold) {max(tcalc$WATER_DEPTH_M)} 
      else{ (((tcalcint[calcrow, 'slopetempdepth'])) * Temp_Threshold + (tcalcint[calcrow, 'intercepttempdepth']))}}}
  temp_threshold_depth_ft <- (temp_threshold_depth_m * 3.28)

  max_depth_m <- max(tcalc$WATER_DEPTH_M)
  max_depth_ft <- (max_depth_m * 3.28)
  layer_thickness_m <- (do_threshold_depth_m - temp_threshold_depth_m)
  layer_thickness_ft <- (layer_thickness_m * 3.28)
  tdox.stats <- data.frame(ID_NBR, SAMPLE_DATE, GEAR, tdox, do_threshold_depth_m, do_threshold_depth_ft, temp_threshold_depth_m, temp_threshold_depth_ft, layer_thickness_m, layer_thickness_ft, max_depth_m, max_depth_ft, tempsurface, tempmax, tempbottom, tempmin, dosurface, domax, dobottom, domin)
  
  TDxOUT = rbind(TDxOUT, tdox.stats)
}

# create output file
TDxOUT$PROFILE <- paste(TDxOUT$ID_NBR, TDxOUT$SAMPLE_DATE, TDxOUT$GEAR)
tdtable <- TDxOUT[order(TDxOUT$ID_NBR, TDxOUT$GEAR, TDxOUT$SAMPLE_DATE, TDxOUT$PROFILE),]
tdtable$tdox <- as.numeric(tdtable$tdox)

# save output file as .csv (remove # from DNR or MPCA .csv file)
write.csv(tdtable, "TDO_Report_Statistics.csv", row.names = FALSE)

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