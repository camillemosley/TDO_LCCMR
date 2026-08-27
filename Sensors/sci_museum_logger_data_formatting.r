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
                        DO_perc_mod_bot,
                        AbsPres_kPa),
               names_to = 'Variable',
               values_to = 'Value') %>%
  mutate(Rep = case_when(grepl('bot_2', Variable) ~ 2,
                         .default = 1),
         Date_Time = as.POSIXct(paste(Date, Time), format = "%m/%d/%Y %H:%M:%S"),
         Depth_m = as.numeric(word(Variable, -1, sep = '_'))) %>%
  select(-Date, -Time, -X.) %>%
  mutate(Depth_m = case_when(grepl('bot', Variable) ~ 0,
                           grepl('kPa', Variable) ~ 0,
                           .default = Depth_m),
         Variable = case_when(grepl('Temp', Variable) ~ 'Temp',
                              grepl('bot', Variable) ~ str_remove(Variable, '_bot'),
                              grepl('_6.5', Variable) ~ str_remove(Variable, '_6.5'),
                              .default = Variable))

ggplot(BT4_buoy_formatted, aes(x = Date_Time, y = Value, linetype = Variable)) +
  geom_line(data = subset(BT4_buoy_formatted, Variable == 'Temp' & Rep == 1), aes(colour = as.factor(Depth_m))) +
  geom_line(data = subset(BT4_buoy_formatted, Variable == 'DO'), aes(colour = as.factor(Depth_m))) +
  scale_linetype_manual(values = c('longdash', 'solid')) +
  labs(colour = 'Depth (m)', y = 'mg/L | ºC')

# this one measures depth from the bottom

BT4_buoy_formatted <- BT4_buoy_formatted %>%
  rename(Height_m = 'Depth_m') %>%
  mutate(Depth_Adj_m = abs(Height_m - Bot_depth)) %>%
  mutate(Depth_m = abs(Height_m - unique(Height_m[Date_Time == max(Date_Time)])))
BT4_buoy_formatted$Depth_m <- abs(BT4_buoy_formatted$Height_m - 9.905)
BT4_buoy_formatted$Depth_m <- abs(BT4_buoy_formatted$Height_m[BT4_buoy_formatted$Depth_m[BT4_buoy_formatted$Date_Time == max(BT4_buoy_formatted$Date_Time)]])

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

# plot data from each logger
plot(BT5_buoy2016_formatted$Value[BT5_buoy2016_formatted$Variable == 'Temp' & BT5_buoy2016_formatted$Depth == 1.5], type = 'l', col = 'purple', ylim = c(0,26),
     ylab = "Temperature and DO", xlab = "Time")
lines(BT5_buoy2016_formatted$Value[BT5_buoy2016_formatted$Variable == 'Temp' & BT5_buoy2016_formatted$Depth == 2.5], col = 'blue')
lines(BT5_buoy2016_formatted$Value[BT5_buoy2016_formatted$Variable == 'Temp' & BT5_buoy2016_formatted$Depth == 3.5], col = 'green')
lines(BT5_buoy2016_formatted$Value[BT5_buoy2016_formatted$Variable == 'Temp' & BT5_buoy2016_formatted$Depth == 4.5], col = 'yellow')
lines(BT5_buoy2016_formatted$Value[BT5_buoy2016_formatted$Variable == 'Temp' & BT5_buoy2016_formatted$Depth == 5.5], col = 'orange')
lines(BT5_buoy2016_formatted$Value[BT5_buoy2016_formatted$Variable == 'Temp' & BT5_buoy2016_formatted$Depth == 7.5], col = 'red')
lines(BT5_buoy2016_formatted$Value[BT5_buoy2016_formatted$Variable == 'Temp' & BT5_buoy2016_formatted$Depth == 0], col = 'black')
lines(BT5_buoy2016_formatted$Value[BT5_buoy2016_formatted$Variable == 'DO' & BT5_buoy2016_formatted$Depth == 6.5], col = 'orange')
lines(BT5_buoy2016_formatted$Value[BT5_buoy2016_formatted$Variable == 'DO' & BT5_buoy2016_formatted$Depth == 0], col = 'black')

# this one also measures depth from the bottom

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

# Wilderness Lakes summer 2022 readings ----
# lakes in this file: Burnt , Dunnigan , E.Twin , Elbow , Finger , Flame , Smoke , W.Twin 
Burnt<-WL_summer23DO_topbot[WL_summer23DO_topbot$site=="Burnt",]
Dunnigan<-WL_summer23DO_topbot[WL_summer23DO_topbot$site=="Dunnigan",]
E.Twin<-WL_summer23DO_topbot[WL_summer23DO_topbot$site=="E.Twin",]
Elbow<-WL_summer23DO_topbot[WL_summer23DO_topbot$site=="Elbow",]
Finger<-WL_summer23DO_topbot[WL_summer23DO_topbot$site=="Finger",]
Flame<-WL_summer23DO_topbot[WL_summer23DO_topbot$site=="Flame",]
Smoke<-WL_summer23DO_topbot[WL_summer23DO_topbot$site=="Smoke",]
W.Twin<-WL_summer23DO_topbot[WL_summer23DO_topbot$site=="W.Twin",]

#Burnt ---
#make date object of column
Burnt$datetime<-as.POSIXct(Burnt$datetime, format= "%Y-%m-%d %H:%M:%S") 
#make ggplot showing 2 variables over time 
ggplot(Burnt, aes(x = datetime)) +
  geom_line(aes(y = surface_temp, color = "Temperature")) +
  geom_line(aes(y = surface_DO_adj_conc, color = "Dissolved Oxygen")) +
  labs(title = "Temp and DO for Burnt Lake", color = "Legend") +
  scale_y_continuous(
    name = "Temperature",
    sec.axis = sec_axis(~ ., name = "Dissolved Oxygen")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Temperature" = "#CC6666",
                                "Dissolved Oxygen" = "#9999CC"))

#Dunnigan ----

Dunnigan$datetime<-as.POSIXct(Dunnigan$datetime, format= "%Y-%m-%d %H:%M:%S") 
#make ggplot showing 2 variables over time 
ggplot(Dunnigan, aes(x = datetime)) +
  geom_line(aes(y = surface_temp, color = "Temperature")) +
  geom_line(aes(y = surface_DO_adj_conc, color = "Dissolved Oxygen")) +
  labs(title = "Temp and DO for Dunnigan Lake", color = "Legend") +
  scale_y_continuous(
    name = "Temperature",
    sec.axis = sec_axis(~ ., name = "Dissolved Oxygen")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Temperature" = "#CC6666",
                                "Dissolved Oxygen" = "#9999CC"))

# Smoke ----
#has a logger for top and bottom of lake + 2023 summer data from joint csv 
#join data 

Smoke_top$datetime<-as.POSIXct(Smoke_top$Date.Time..GMT.05.00, format= "%m/%d/%Y %H:%M") 
#make ggplot showing 2 variables over time 
ggplot(Smoke_top, aes(x = datetime)) +
  geom_line(aes(y = Temp...C..LGR.S.N..21118676..SEN.S.N..21118676..LBL..temp., color = "Temperature")) +
  geom_line(aes(y = DO.Adj.Conc..mg.L..LGR.S.N..21118676., color = "Dissolved Oxygen")) +
  labs(title = "Temp and DO for Smoke Lake", color = "Legend") +
  scale_y_continuous(
    name = "Temperature",
    sec.axis = sec_axis(~ ., name = "Dissolved Oxygen")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Temperature" = "#CC6666",
                                "Dissolved Oxygen" = "#9999CC"))
Smoke$datetime<-as.POSIXct(Smoke$datetime, format= "%Y-%m-%d %H:%M:%S") 
ggplot(Smoke, aes(x = datetime)) +
  geom_line(aes(y = surface_temp, color = "Temperature")) +
  geom_line(aes(y = surface_DO_adj_conc, color = "Dissolved Oxygen")) +
  labs(title = "Temp and DO for Smoke Lake", color = "Legend") +
  scale_y_continuous(
    name = "Temperature",
    sec.axis = sec_axis(~ ., name = "Dissolved Oxygen")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Temperature" = "#CC6666",
                                "Dissolved Oxygen" = "#9999CC"))


# Finger -----
Finger$datetime<-as.POSIXct(Finger$datetime, format= "%Y-%m-%d %H:%M:%S") 
ggplot(Finger, aes(x = datetime)) +
  geom_line(aes(y = surface_temp, color = "Temperature")) +
  geom_line(aes(y = surface_DO_adj_conc, color = "Dissolved Oxygen")) +
  labs(title = "Temp and DO for Finger Lake", color = "Legend") +
  scale_y_continuous(
    name = "Temperature",
    sec.axis = sec_axis(~ ., name = "Dissolved Oxygen")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Temperature" = "#CC6666",
                                "Dissolved Oxygen" = "#9999CC"))
