setwd('~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/Science Museum Logger Data/')
library(tidyverse)
rm(list = ls())

# read in all data files
files <- list.files('OG/')
for (i in 1:length(files)) {
  fileName <- sub('.csv', '', files[i])
  data <- read.csv(paste('OG/', files[i], sep = ''))
  assign(fileName, data)
}

# BT4 ----
BT4_buoy_formatted <- BT4_buoy %>%
  pivot_longer(cols = c(DO_bot,
                        Temp_bot,
                        DOadj_bot,
                        DOperc_bot,
                        Temp_bot_2,
                        Temp_1.5,
                        Temp_2.5,
                        Temp_3.5,
                        Temp_4.5,
                        Temp_5.5,
                        DO_6.5,
                        Temp_6.5,
                        DOadj_6.5,
                        DOperc_6.5,
                        Temp_7.5,
                        DO_adj_mod_bot,
                        DO_perc_mod_bot),
               names_to = 'Variable',
               values_to = 'Value') %>%
  add_column(Rep = NA)
BT4_buoy_formatted$Depth <- word(BT4_buoy_formatted$Variable, -1, sep = '_')
BT4_buoy_formatted$Rep[BT4_buoy_formatted$Variable == 'Temp_bot_2'] <- 2
BT4_buoy_formatted$Rep[BT4_buoy_formatted$Variable != 'Temp_bot_2'] <- 1
BT4_buoy_formatted$Depth[BT4_buoy_formatted$Variable == 'Temp_bot_2'] <- 'bot'
BT4_buoy_formatted$Depth[BT4_buoy_formatted$Depth == 'bot'] <- 0
BT4_buoy_formatted$Variable <- word(BT4_buoy_formatted$Variable, 1, -2, sep = '_')
BT4_buoy_formatted$Variable[BT4_buoy_formatted$Variable == 'Temp_bot'] <- 'Temp'
BT4_buoy_formatted$Depth <- as.numeric(BT4_buoy_formatted$Depth)  

plot(BT4_buoy_formatted$Value[BT4_buoy_formatted$Variable == 'Temp' & BT4_buoy_formatted$Depth == 1.5], type = 'l', col = 'purple', ylim = c(0,26),
     ylab = "Temperature and DO", xlab = "Time")
lines(BT4_buoy_formatted$Value[BT4_buoy_formatted$Variable == 'Temp' & BT4_buoy_formatted$Depth == 2.5], col = 'blue')
lines(BT4_buoy_formatted$Value[BT4_buoy_formatted$Variable == 'Temp' & BT4_buoy_formatted$Depth == 3.5], col = 'green')
lines(BT4_buoy_formatted$Value[BT4_buoy_formatted$Variable == 'Temp' & BT4_buoy_formatted$Depth == 4.5], col = 'yellow')
lines(BT4_buoy_formatted$Value[BT4_buoy_formatted$Variable == 'Temp' & BT4_buoy_formatted$Depth == 5.5], col = 'orange')
lines(BT4_buoy_formatted$Value[BT4_buoy_formatted$Variable == 'Temp' & BT4_buoy_formatted$Depth == 7.5], col = 'red')
lines(BT4_buoy_formatted$Value[BT4_buoy_formatted$Variable == 'Temp' & BT4_buoy_formatted$Depth == 0], col = 'black')
lines(BT4_buoy_formatted$Value[BT4_buoy_formatted$Variable == 'DO' & BT4_buoy_formatted$Depth == 6.5], col = 'orange')
lines(BT4_buoy_formatted$Value[BT4_buoy_formatted$Variable == 'DO' & BT4_buoy_formatted$Depth == 0], col = 'black')

# this one measures depth from the bottom

BT4_buoy_formatted <- BT4_buoy_formatted %>%
  rename(Height_m = 'Depth')
BT4_buoy_formatted$Depth_Adj_m <- abs(BT4_buoy_formatted$Height_m - BT4_buoy_formatted$Bot_depth)
BT4_buoy_formatted$Depth_m <- abs(BT4_buoy_formatted$Height_m - 9.905)

BT4_buoy_formatted <- BT4_buoy_formatted %>%
  rename(Bottom_Depth_m = 'Bot_depth',
         Bottom_AbsPres_kPa = 'AbsPres_kPa')
BT4_buoy_formatted <- BT4_buoy_formatted %>%
  select(-X.) %>%
  select(Date, Time, Variable, Depth_m, Depth_Adj_m, Value, Rep, Bottom_AbsPres_kPa, Bottom_Depth_m, Height_m, Notes)

# BT5 2016 ----
# I'm assuming BT5 is the same as BT4, also measured from the bottom

BT5_buoy2016_formatted <- BT5_buoy2016 %>%
  pivot_longer(cols = c(DO_bot,
                        Temp_bot,
                        DOadj_bot,
                        DOperc_bot,
                        Temp_bot_2,
                        Temp_1.5,
                        Temp_2.5,
                        Temp_3.5,
                        Temp_4.5,
                        Temp_5.5,
                        DO_6.5,
                        Temp_6.5,
                        DOadj_6.5,
                        DOperc_6.5,
                        Temp_7.5),
               names_to = 'Variable',
               values_to = 'Value') %>%
  add_column(Rep = NA)
BT5_buoy2016_formatted$Depth <- word(BT5_buoy2016_formatted$Variable, -1, sep = '_')
BT5_buoy2016_formatted$Rep[BT5_buoy2016_formatted$Variable == 'Temp_bot_2'] <- 2
BT5_buoy2016_formatted$Rep[BT5_buoy2016_formatted$Variable != 'Temp_bot_2'] <- 1
BT5_buoy2016_formatted$Depth[BT5_buoy2016_formatted$Variable == 'Temp_bot_2'] <- 'bot'
BT5_buoy2016_formatted$Depth[BT5_buoy2016_formatted$Depth == 'bot'] <- 0
BT5_buoy2016_formatted$Variable <- word(BT5_buoy2016_formatted$Variable, 1, -2, sep = '_')
BT5_buoy2016_formatted$Variable[BT5_buoy2016_formatted$Variable == 'Temp_bot'] <- 'Temp'
BT5_buoy2016_formatted$Depth <- as.numeric(BT5_buoy2016_formatted$Depth)

BT5_buoy2016_formatted <- BT5_buoy2016_formatted %>%
  rename(Height_m = 'Depth')
BT5_buoy2016_formatted$Depth_Adj_m <- abs(BT5_buoy2016_formatted$Height_m - BT5_buoy2016_formatted$Bot_depth)
BT5_buoy2016_formatted$Depth_m <- abs(BT5_buoy2016_formatted$Height_m - 9.905)

BT5_buoy2016_formatted <- BT5_buoy2016_formatted %>%
  rename(Bottom_Depth_m = 'Bot_depth',
         Bottom_AbsPres_kPa = 'AbsPres_kPa')
BT5_buoy2016_formatted <- BT5_buoy2016_formatted %>%
  select(-X.) %>%
  select(Date, Time, Variable, Depth_m, Depth_Adj_m, Value, Rep, Bottom_AbsPres_kPa, Bottom_Depth_m, Height_m, Notes)

# BT5 2019 ----

BT5_buoy2019_formatted <- BT5_buoy2019 %>%
  pivot_longer(cols = c(DO_bot,
                        Temp_bot,
                        DOadj_bot,
                        DOperc_bot,
                        Temp_bot_2,
                        Temp_1.5,
                        Temp_2.5,
                        Temp_3.5,
                        Temp_4.5,
                        Temp_5.5,
                        DO_6.5,
                        Temp_6.5,
                        DOadj_6.5,
                        DOperc_6.5,
                        Temp_7.5),
               names_to = 'Variable',
               values_to = 'Value') %>%
  add_column(Rep = NA)
BT5_buoy2019_formatted$Depth <- word(BT5_buoy2019_formatted$Variable, -1, sep = '_')
BT5_buoy2019_formatted$Rep[BT5_buoy2019_formatted$Variable == 'Temp_bot_2'] <- 2
BT5_buoy2019_formatted$Rep[BT5_buoy2019_formatted$Variable != 'Temp_bot_2'] <- 1
BT5_buoy2019_formatted$Depth[BT5_buoy2019_formatted$Variable == 'Temp_bot_2'] <- 'bot'
BT5_buoy2019_formatted$Depth[BT5_buoy2019_formatted$Depth == 'bot'] <- 0
BT5_buoy2019_formatted$Variable <- word(BT5_buoy2019_formatted$Variable, 1, -2, sep = '_')
BT5_buoy2019_formatted$Variable[BT5_buoy2019_formatted$Variable == 'Temp_bot'] <- 'Temp'
BT5_buoy2019_formatted$Depth <- as.numeric(BT5_buoy2019_formatted$Depth)

BT5_buoy2019_formatted <- BT5_buoy2019_formatted %>%
  rename(Height_m = 'Depth')
BT5_buoy2019_formatted$Depth_Adj_m <- abs(BT5_buoy2019_formatted$Height_m - BT5_buoy2019_formatted$Bot_depth)
BT5_buoy2019_formatted$Depth_m <- abs(BT5_buoy2019_formatted$Height_m - 9.905)

BT5_buoy2019_formatted <- BT5_buoy2019_formatted %>%
  rename(Bottom_Depth_m = 'Bot_depth',
         Bottom_AbsPres_kPa = 'AbsPres_kPa')
BT5_buoy2019_formatted <- BT5_buoy2019_formatted %>%
  select(-X.) %>%
  select(Date, Time, Variable, Depth_m, Depth_Adj_m, Value, Rep, Bottom_AbsPres_kPa, Bottom_Depth_m, Height_m, Notes)
# want to sort by date, time, variable, and depth, but i need to fix the date formatting first (#fml)


# Richie lake ----

#format dates for ploting
#paste time and date columns 
Richie$DateTime <-paste(Richie$Date,Richie$Time)
#make date object of column
Richie$DateTime<-as.POSIXct(Richie$DateTime, format= "%m/%d/%Y %H:%M:%S") 
#make ggplot showing 2 variables over time 
ggplot(Richie, aes(x = DateTime)) +
  geom_line(aes(y = Temperature, color = "Temperature")) +
  geom_line(aes(y = Dissolved.Oxygen, color = "Dissolved Oxygen")) +
  labs(title = "Temp and DO for Richie Lake", color = "Legend") +
  scale_y_continuous(
    name = "Temperature",
    sec.axis = sec_axis(~ ., name = "Dissolved Oxygen")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Temperature" = "#CC6666",
                                "Dissolved Oxygen" = "#9999CC"))

# Sargent lake ----

#paste time and date columns 
Sargent$DateTime <-paste(Sargent$Date,Sargent$Time)
#make date object of column
Sargent$DateTime<-as.POSIXct(Sargent$DateTime, format= "%m/%d/%Y %H:%M:%S") 
#make ggplot showing 2 variables over time 
ggplot(Sargent, aes(x = DateTime)) +
  geom_line(aes(y = Temperature, color = "Temperature")) +
  geom_line(aes(y = Dissolved.Oxygen, color = "Dissolved Oxygen")) +
  labs(title = "Temp and DO for Sargent Lake", color = "Legend") +
  scale_y_continuous(
    name = "Temperature",
    sec.axis = sec_axis(~ ., name = "Dissolved Oxygen")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Temperature" = "#CC6666",
                                "Dissolved Oxygen" = "#9999CC"))

# Desor lake ----

DesorDO$DateTime <-paste(DesorDO$Date,DesorDO$Time)
#make date object of column
DesorDO$DateTime<-as.POSIXct(DesorDO$DateTime, format= "%m/%d/%Y %H:%M:%S") 
#make ggplot showing 2 variables over time 
ggplot(DesorDO, aes(x = DateTime)) +
  geom_line(aes(y = Temperature, color = "Temperature")) +
  geom_line(aes(y = Dissolved.Oxygen, color = "Dissolved Oxygen")) +
  labs(title = "Temp and DO for Desor Lake", color = "Legend") +
  scale_y_continuous(
    name = "Temperature",
    sec.axis = sec_axis(~ ., name = "Dissolved Oxygen")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Temperature" = "#CC6666",
                                "Dissolved Oxygen" = "#9999CC"))

