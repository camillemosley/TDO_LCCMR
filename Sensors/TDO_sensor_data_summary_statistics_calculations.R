### TDO sensor data analysis ###
# Created by Mike M on Jul 21 2026
# Input: Cleaned temp DO sensor data
# Output: Coldwater habitat and limnological statistics calculated from sensor data
# No statistical analysis is conducted here, just summary statistics from data. See other script for statistical analysis.
# Until I figure out how to get this into a parquet file format, need to do one lake at a time due to memory issues with R's global environment due to the size of the files

# Load libraries ----
library(tidyverse) # for data wrangling
library(scales) # for plotting
library(viridisLite) # for plotting
library(beepr)
library(fields) # for plotting
library(lubridate) # for dealing with time series data >:(

# Load and format data ----
setwd("~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/TDO sensors/")
#Sentinel.Lakes.Temp.Data <- read.csv("Sentinel Lakes/temp_data.csv", stringsAsFactors = F)
#Sentinel.Lakes.Temp.Data.2 <- Sentinel.Lakes.Temp.Data %>%
#  mutate(value_type = "actual",
#         depth_m = case_when(is.na(depth_m) ~ 1, .default = depth_m),
#         standardized_date_time = case_when(nchar(standardized_date_time) == 10 ~ paste(standardized_date_time, "00:00:00", sep = " "), .default = standardized_date_time), # the original data doesn't have 00:00:00 included in the date time column for measurements taken at midnight so need to add that in for those measurements
#         standardized_date_time = ymd_hms(standardized_date_time, tz = "Etc/GMT+6") %>% # this and the next line changes the data time to UTC to avoid issues with daylight savings time
#         with_tz(tzone = "UTC")) %>%
#  group_by(basin_name, station, standardized_date_time, depth_m) %>% # this and the next 3 lines are for identifying duplicates and removing them by averaging them together
#  mutate(temp_c = mean(temp_c, na.rm = TRUE)) %>%
#  distinct(basin_name, station, standardized_date_time, depth_m, .keep_all = TRUE) %>%
#  ungroup()
#rm(Sentinel.Lakes.Temp.Data)

Sentinel.Lakes.DO.Data <- read.csv("Sentinel Lakes/do_data.csv", stringsAsFactors = F)
Sentinel.Lakes.DO.Data.2 <- Sentinel.Lakes.DO.Data %>%
  mutate(value_type = "actual",
         depth_m = case_when(is.na(depth_m) ~ 1, .default = depth_m),
         standardized_date_time = case_when(nchar(standardized_date_time) == 10 ~ paste(standardized_date_time, "00:00:00", sep = " "), .default = standardized_date_time), # the original data doesn't have 00:00:00 included in the date time column for measurements taken at midnight so need to add that in for those measurements
         standardized_date_time = ymd_hms(standardized_date_time, tz = "Etc/GMT+6") %>% # this and the next line changes the data time to UTC to avoid issues with daylight savings time
           with_tz(tzone = "UTC")) %>%
  group_by(basin_name, station, standardized_date_time, depth_m) %>% # this and the next 3 lines are for identifying duplicates and removing them by averaging them together
  mutate(temp_c = mean(temp_c, na.rm = TRUE)) %>%
  distinct(basin_name, station, standardized_date_time, depth_m, .keep_all = TRUE) %>%
  ungroup()
rm(Sentinel.Lakes.DO.Data)

Carlos.Temp <- Sentinel.Lakes.Temp.Data.2 %>%
  filter(basin_name == "Carlos")
rm(Sentinel.Lakes.Temp.Data.2)
# let's see how many observations there are per year
#carlos.temp.obs.per.yr <- table(word(word(Carlos.Temp$standardized_date_time, 1, 1, sep = " "), 1, 1, sep = "-"))
Carlos.DO <- Sentinel.Lakes.DO.Data.2 %>%
  filter(basin_name == "Carlos")
rm(Sentinel.Lakes.DO.Data.2)

#Ten.Mile.Temp <- Sentinel.Lakes.Temp.Data.2 %>%
#  filter(basin_name == "Ten Mile")
# Ten Mile has more than 1 measurement at the same depth in some places, just take their average
#Ten.Mile.Temp <- Ten.Mile.Temp %>%
#  group_by(standardized_date_time, depth_m) %>%
#  mutate(temp_c = mean(temp_c, na.rm = TRUE)) %>%
#  distinct(standardized_date_time, depth_m, .keep_all = TRUE) %>%
#  ungroup()
#rm(Sentinel.Lakes.Temp.Data.2)
#ten.mile.temp.obs.per.yr <- table(word(word(Ten.Mile.Temp$standardized_date_time, 1, 1, sep = " "), 1, 1, sep = "-"))
#Ten.Mile.DO <- Sentinel.Lakes.DO.Data.2 %>%
#  filter(basin_name == "Ten Mile")

#Elk.Temp <- Sentinel.Lakes.Temp.Data.2 %>%
#  filter(basin_name == "Elk")
#rm(Sentinel.Lakes.Temp.Data.2)
#elk.temp.obs.per.yr <- table(word(word(Elk.Temp$standardized_date_time, 1, 1, sep = " "), 1, 1, sep = "-"))
#Elk.DO <- Sentinel.Lakes.DO.Data.2 %>%
#  filter(basin_name == "Elk")
#rm(Sentinel.Lakes.DO.Data.2)

#Greenwood.Temp <- Sentinel.Lakes.Temp.Data.2 %>%
#  filter(basin_name == "Greenwood")
#rm(Sentinel.Lakes.Temp.Data.2)
#Greenwood.DO <- Sentinel.Lakes.DO.Data.2 %>%
#  filter(basin_name == "Greenwood")

# Interpolate ----

# For now, just going to use the approx function and do a linear interpolation.
# In the future I will test whether it is better and possible to use the discrete
# profiles to inform the interpolation.

interpolate_profile <- function(data, all_depths, parameter) {
  data$value_type <- "actual"
  depths.to.estimate <- all_depths[!as.character(all_depths) %in% as.character(data$depth_m)]
  if (nrow(data) <= 1) {
    return(data)
  }
  first_row <- data[1, ]
  if (tolower(parameter) %in% c("do", "oxygen", "dissolved oxygen")) {
    ydata <- data$do_mgl
    estimates <- approx(
      x = data$depth_m, # x and y are the actual depth and temp/DO data used to interpolate at the estimated depths
      y = data$do_mgl, 
      xout = depths.to.estimate, # calculates estimates for only the ones to be estimated, leaving the actual depths out
      method = "linear", # simple linear interpolation, may eventually change this to a smooth function informed by single-point profile data
      rule = 2 # rule = 2 applies constants to NAs at ends of the data set (depths above and below the max and min sensor depths)
    )
    Estimated.Data <- tibble(
      basin_name = first_row$basin_name,
      basin_id = first_row$basin_id,
      station = first_row$station,
      depth_m = estimates$x,
      standardized_date_time = first_row$standardized_date_time,
      do_mgl = estimates$y,
      deployment = first_row$deployment,
      logger_model = first_row$logger_model,
      flaggross_do = NA,
      flagspike_do = NA,
      flagroc_do = NA,
      flagflat_do = NA,
      flagvis_do = NA,
      serial_number = NA,
      value_type = "estimated"
    )
  } else if (tolower(parameter) %in% c("temp", "t", "temperature")) {
    estimates <- approx(
    x = data$depth_m, # x and y are the actual depth and temp/DO data used to interpolate at the estimated depths
    y = data$temp_c, 
    xout = depths.to.estimate, # calculates estimates for only the ones to be estimated, leaving the actual depths out
    method = "linear", # simple linear interpolation, may eventually change this to a smooth function informed by single-point profile data
    rule = 2 # rule = 2 applies constants to NAs at ends of the data set (depths above and below the max and min sensor depths)
    )
    Estimated.Data <- tibble(
      basin_name = first_row$basin_name,
      basin_id = first_row$basin_id,
      station = first_row$station,
      depth_m = estimates$x,
      standardized_date_time = first_row$standardized_date_time,
      temp_c = estimates$y,
      deployment = first_row$deployment,
      logger_model = first_row$logger_model,
      flaggross = NA,
      flagspike = NA,
      flagroc = NA,
      flagflat = NA,
      flagvis = NA,
      serial_number = NA,
      value_type = "estimated"
    )
  }
  Interpolated.Data <- bind_rows(data[, names(Estimated.Data)], Estimated.Data) %>%
    arrange(depth_m)
}

Carlos.Temp.Interpolated <- Carlos.Temp %>%
  group_by(standardized_date_time) %>%
  group_modify(~ interpolate_profile(data = .x, all_depths = seq(0, max(Carlos.Temp$depth_m), by = 0.1), parameter = "temp")) %>%
  ungroup()
rm(Carlos.Temp)
#Carlos.DO.Interpolated <- Carlos.DO %>%
#  group_by(standardized_date_time) %>%
#  group_modify(~ interpolate_profile(data = .x, all_depths = seq(0, max(Carlos.DO$depth_m), by = 0.1), parameter = "do")) %>%
#  ungroup()
#rm(Carlos.DO)

#Ten.Mile.Temp.Interpolated <- Ten.Mile.Temp %>%
#  group_by(standardized_date_time) %>%
#  group_modify(~ interpolate_profile(data = .x, all_depths = seq(0, max(Ten.Mile.Temp$depth_m), by = 0.1))) %>%
#  ungroup()
#rm(Ten.Mile.Temp)

#Elk.Temp.Interpolated <- Elk.Temp %>%
#  group_by(standardized_date_time) %>%
#  group_modify(~ interpolate_profile(data = .x, all_depths = seq(0, max(Elk.Temp$depth_m), by = 0.1))) %>%
#  ungroup()
#rm(Elk.Temp)

#Greenwood.Temp.Interpolated <- Greenwood.Temp %>%
#  group_by(standardized_date_time) %>%
#  group_modify(~ interpolate_profile(data = .x, all_depths = seq(0, max(Greenwood.Temp$depth_m), by = 0.1))) %>%
#  ungroup()
#rm(Greenwood.Temp)
#beep(9)

# Visualize data ----

plot_profile <- function(data,
                         date_time_start,
                         date_time_end,
                         title,
                         parameter) {
  Data.Filt <- data %>%
    filter(standardized_date_time >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
           standardized_date_time <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))
  if (tolower(parameter) %in% c("do", "oxygen", "dissolved oxygen")) {
    max.do <- max(data$do_mgl)
    plot <- ggplot(Data.Filt, aes(x = standardized_date_time, y = depth_m, fill = cut(do_mgl, breaks = c(-Inf, 1, 3, 6, Inf), labels = c("< 1", "1 - 3", "3 - 6", "> 6")))) +
      geom_raster() +
      scale_y_reverse() +
      scale_fill_manual(
        values = c(
          "< 1"   = "black",
          "1 - 3" = "red",
          "3 - 6" = "yellow",
          "> 6"   = "darkgreen"
        ),
        drop = FALSE # Keeps all labels in the legend even if a category isn't in Data.Filt
      ) +
      scale_x_datetime(date_breaks = "1 month",
                       date_labels = "%b") +
      labs(x = "Date", y = "Depth (m)", fill = "DO (mg/L)", title = title) +
      theme_bw()
  } else if (tolower(parameter) %in% c("temp", "t", "temperature")) {
    plot <- ggplot(Data.Filt, aes(x = standardized_date_time, y = depth_m, fill = temp_c)) +
      geom_raster() +
      scale_y_reverse() +
      scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "red", "#7a0000"),
                         values = rescale(c(0, 4, 10.5, 17, 22, 30), from = c(0, 30)), # Matches the full legend limits
                         limits = c(0, 30)) + # Displays 0°C to 30°C on the legend
      scale_x_datetime(date_breaks = "1 month",
                       date_labels = "%b") +
      labs(x = "Date", y = "Depth (m)", fill = "Temp (°C)", title = title) +
      theme_bw()
  }
  return(plot)
}

# need to remove actual measurements that are taken at odd depth intervals (1.65, 4.72, etc.) for plotting
#Carlos.Temp.Interpolated.2 <- Carlos.Temp.Interpolated %>%
#    filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)))
#rm(Carlos.Temp.Interpolated)
#CarlosTempProfilePlots <- list()
#min.year <- min(year(Carlos.Temp.Interpolated.2$standardized_date_time))
#max.year <- max(year(Carlos.Temp.Interpolated.2$standardized_date_time))
#for (year in as.character(min.year:max.year)) {
#  CarlosTempProfilePlots[[year]] <- plot_profile(data = Carlos.Temp.Interpolated.2,
#                                                date_time_start = paste(year, "01-01 00:00:00", sep = "-"),
#                                                 date_time_end = paste(year, "12-31 23:00:00", sep = "-"),
#                                                 title = paste(year, "Carlos Temp Profile", sep = " "))
#}
#length(CarlosTempProfilePlots)
#names(CarlosTempProfilePlots)
#for (i in 1:length(CarlosTempProfilePlots)) {
#  ggsave(filename = paste0("Plots/", names(CarlosTempProfilePlots)[i], " Carlos temp profile.png"),
#         plot = CarlosTempProfilePlots[[i]],
#         width = 8, height = 6)
#}
#Carlos.DO.Interpolated.2 <- Carlos.DO.Interpolated %>%
#    filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)))
#rm(Carlos.DO.Interpolated)
#plot_profile(Carlos.DO.Interpolated.2,
#             date_time_start = "2019-01-01 00:00:00",
#             date_time_end = "2019-12-31 23:00:00",
#             title = "2019 Carlos DO Profile",
#             parameter = "do")
#CarlosDOProfilePlots <- list()
#min.year <- min(year(Carlos.DO.Interpolated.2$standardized_date_time))
#max.year <- max(year(Carlos.DO.Interpolated.2$standardized_date_time))
#for (year in as.character(min.year:max.year)) {
#  CarlosDOProfilePlots[[year]] <- plot_profile(data = Carlos.DO.Interpolated.2,
#                                                date_time_start = paste(year, "01-01 00:00:00", sep = "-"),
#                                                 date_time_end = paste(year, "12-31 23:00:00", sep = "-"),
#                                                 title = paste(year, "Carlos DO Profile", sep = " "),
#                                               parameter = "do")
#}
#length(CarlosDOProfilePlots)
#names(CarlosDOProfilePlots)
#for (i in 1:length(CarlosDOProfilePlots)) {
#  ggsave(filename = paste0("Plots/", names(CarlosDOProfilePlots)[i], " Carlos DO profile.png"),
#         plot = CarlosDOProfilePlots[[i]],
#         width = 8, height = 6)
#}

#Ten.Mile.Temp.Interpolated.2 <- Ten.Mile.Temp.Interpolated %>%
#  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)))
#rm(Ten.Mile.Temp.Interpolated)
#TenMileTempProfilePlots <- list()
#min.year <- min(year(Ten.Mile.Temp.Interpolated.2$standardized_date_time))
#max.year <- max(year(Ten.Mile.Temp.Interpolated.2$standardized_date_time))
#for (year in as.character(min.year:max.year)) {
#  TenMileTempProfilePlots[[year]] <- plot_profile(data = Ten.Mile.Temp.Interpolated.2,
#                                                 date_time_start = paste(year, "01-01 00:00:00", sep = "-"),
#                                                 date_time_end = paste(year, "12-31 23:00:00", sep = "-"),
#                                                 title = paste(year, "Ten Mile Temp Profile", sep = " "))
#}
#length(TenMileTempProfilePlots)
#names(TenMileTempProfilePlots)
#for (i in 1:length(TenMileTempProfilePlots)) {
#  ggsave(filename = paste0("Plots/", names(TenMileTempProfilePlots)[i], " Ten Mile temp profile.png"),
#         plot = TenMileTempProfilePlots[[i]],
#         width = 8, height = 6)
#}

#Elk.Temp.Interpolated.2 <- Elk.Temp.Interpolated %>%
#  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)))
#rm(Elk.Temp.Interpolated)
#ElkTempProfilePlots <- list()
#min.year <- min(year(Elk.Temp.Interpolated.2$standardized_date_time))
#max.year <- max(year(Elk.Temp.Interpolated.2$standardized_date_time))
#for (year in as.character(min.year:max.year)) {
#  ElkTempProfilePlots[[year]] <- plot_profile(data = Elk.Temp.Interpolated.2,
#                                              date_time_start = paste(year, "01-01 00:00:00", sep = "-"),
#                                              date_time_end = paste(year, "12-31 23:00:00", sep = "-"),
#                                              title = paste(year, "Elk Temp Profile", sep = " "))
#}
#length(ElkTempProfilePlots)
#names(ElkTempProfilePlots)
#for (i in 1:length(ElkTempProfilePlots)) {
#  ggsave(filename = paste0("Plots/", names(ElkTempProfilePlots)[i], " Elk temp profile.png"),
#         plot = ElkTempProfilePlots[[i]],
#         width = 8, height = 6)
#}

#Greenwood.Temp.Interpolated.2 <- Greenwood.Temp.Interpolated %>%
#  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)))
#rm(Greenwood.Temp.Interpolated)
#GreenwoodTempProfilePlots <- list()
#min.year <- min(year(Greenwood.Temp.Interpolated.2$standardized_date_time))
#max.year <- max(year(Greenwood.Temp.Interpolated.2$standardized_date_time))
#for (year in as.character(min.year:max.year)) {
#  GreenwoodTempProfilePlots[[year]] <- plot_profile(data = Greenwood.Temp.Interpolated.2,
#                                              date_time_start = paste(year, "01-01 00:00:00", sep = "-"),
#                                              date_time_end = paste(year, "12-31 23:00:00", sep = "-"),
#                                              title = paste(year, "Greenwood Temp Profile", sep = " "))
#}
#length(GreenwoodTempProfilePlots)
#names(GreenwoodTempProfilePlots)
#for (i in 1:length(GreenwoodTempProfilePlots)) {
#  ggsave(filename = paste0("Plots/", names(GreenwoodTempProfilePlots)[i], " Greenwood temp profile.png"),
#         plot = GreenwoodTempProfilePlots[[i]],
#         width = 8, height = 6)
#}




Carlos.Temp.2019.Interpolated <- Carlos.Temp.Interpolated %>%
  filter(standardized_date_time >= as.POSIXct("2019-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"),
         standardized_date_time <= as.POSIXct("2019-12-31 23:00:00", format = "%Y-%m-%d %H:%M:%S"))
rm(Carlos.Temp.Interpolated)
Carlos.DO.2019 <- Carlos.DO %>%
  filter(standardized_date_time >= as.POSIXct("2019-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"),
         standardized_date_time <= as.POSIXct("2019-12-31 23:00:00", format = "%Y-%m-%d %H:%M:%S"))
rm(Carlos.DO)

#Carlos.Temp.2019.Interpolated <- Carlos.Temp.2019 %>%
#  group_by(standardized_date_time) %>%
#  group_modify(~ interpolate_profile(data = .x, all_depths = seq(0, max(Carlos.Temp.2019$depth_m), by = 0.1), parameter = "temp")) %>%
#  ungroup()
#rm(Carlos.Temp.2019)
#Carlos.DO.2019.Interpolated <- Carlos.DO.2019 %>%
#  group_by(standardized_date_time) %>%
#  group_modify(~ interpolate_profile(data = .x, all_depths = seq(0, max(Carlos.DO.2019$depth_m), by = 0.1), parameter = "do")) %>%
#  ungroup()
#rm(Carlos.DO.2019)
#Carlos.Temp.2019.Interpolated.2 <- Carlos.Temp.2019.Interpolated %>%
#  mutate(parameter = "temp_c") %>%
#  rename(parameter_value = "temp_c")
#Carlos.DO.2019.Interpolated.2 <- Carlos.DO.2019.Interpolated %>%
#  mutate(parameter = "do_mgl") %>%
#  rename(parameter_value = "do_mgl",
#         flagvis = "flagvis_do",
#         flagflat = "flagflat_do",
#         flagroc = "flagroc_do",
#         flagspike = "flagspike_do",
#         flaggross = "flaggross_do")
#Carlos.DO.2019.Subset <- Carlos.DO.2019.Interpolated %>%
#  filter(standardized_date_time == "2019-08-01 12:00:00")

calc_do_threshold <- function(data, do_threshold) {
  data <- data %>%
    mutate(suitability = case_when(do_mgl >= do_threshold ~ "suitable",
                                   .default = "unsuitable"))
  data <- data[order(data$depth_m, decreasing = F),]
  above <- c()
  below <- c()
  for (i in 1:nrow(data)) {
    if (i == nrow(data)) {
      next
    } else if (data$suitability[i] != data$suitability[i+1]) {
      above <- c(above, data$depth_m[i])
      below <- c(below, data$depth_m[i+1])
    }
  }
  if (length(above) > 1) {
    above <- min(above)
    below <- min(below)
  }
  if (!any(data$suitability == "unsuitable")) {
    depth <- max(data$depth_m)
  } else {
    do_above <- data$do_mgl[data$depth_m == above]
    do_below <- data$do_mgl[data$depth_m == below]
    m <- (do_below - do_above)/(below-above)
    b <- do_below - m*below
    depth <- (do_threshold - b)/m
  }
  return(depth)
}

#Carlos.DO.2019 <- Carlos.DO %>%
#  filter(standardized_date_time >= "2019-01-01 00:00:00",
#         standardized_date_time <= "2019-12-31 23:00:00")
Carlos.DO.2019.Threshold.Depths <- Carlos.DO.2019 %>%
  group_by(standardized_date_time) %>%
  summarise(threshold_depth = calc_do_threshold(pick(everything()), do_threshold = 6),
            .groups = "drop")

ggplot(Carlos.DO.2019.Threshold.Depths, aes(x = standardized_date_time, y = threshold_depth)) +
  scale_y_reverse() +
  geom_line() +
  theme_bw()

Carlos.Temp.2019.Interpolated.2 <- Carlos.Temp.2019.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)))

ggplot(Carlos.Temp.2019.Interpolated.2, aes(x = standardized_date_time, y = depth_m, fill = temp_c)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 22, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30)) + # Displays 0°C to 30°C on the legend
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b") +
  labs(x = "Date", y = "Depth (m)", fill = "Temp (°C)", title = title) +
  theme_bw()

plot_profile(data = Carlos.Temp.2019.Interpolated.2,
             date_time_start = "2019-01-01 00:00:00",
             date_time_end = "2019-12-31 23:00:00",
             title = "Carlos Temp Profile",
             parameter = "temp")


ggplot() +
  geom_raster(data = Carlos.Temp.2019.Interpolated.2, aes(x = standardized_date_time, y = depth_m, fill = temp_c)) +
  scale_y_reverse() +
  geom_point(data = Carlos.DO.2019.Threshold.Depths, aes(x = standardized_date_time, y = threshold_depth)) +
  geom_smooth(data = Carlos.DO.2019.Threshold.Depths, aes(x = standardized_date_time, y = threshold_depth), method = "loess", span = 0.2) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 22, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30)) + # Displays 0°C to 30°C on the legend
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b") +
  labs(x = "Date", y = "Depth (m)", fill = "Temp (°C)", title = "Carlos temp profile") +
  theme_bw()




