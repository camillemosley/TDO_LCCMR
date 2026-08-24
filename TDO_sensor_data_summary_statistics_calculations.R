### TDO sensor data analysis ###
# Created by Mike M on Jul 21 2026
# Updated by Mike M on 
# Input: Cleaned temp DO sensor data
# Output: Coldwater habitat and limnological statistics calculated from sensor data
# No statistical analysis is conducted here, just summary statistics from data. See other script for statistical analysis.

# Load libraries ----
library(tidyverse) # for data wrangling
library(scales) # for plotting
library(viridisLite) # for plotting
library(beepr)
library(fields) # for plotting
library(lubridate) # for dealing with time series data >:(

# Load and format data ----
Sentinel.Lakes.Temp.Data <- read.csv("Data/TDO sensor data/Sentinel Lakes/temp_data.csv", stringsAsFactors = F)
Sentinel.Lakes.Temp.Data.2 <- Sentinel.Lakes.Temp.Data %>%
  mutate(value_type = "actual",
         depth_m = case_when(is.na(depth_m) ~ 1, .default = depth_m),
         standardized_date_time = case_when(nchar(standardized_date_time) == 10 ~ paste(standardized_date_time, "00:00:00", sep = " "), .default = standardized_date_time), # the original data doesn't have 00:00:00 included in the date time column for measurements taken at midnight so need to add that in for those measurements
         standardized_date_time = ymd_hms(standardized_date_time, tz = "Etc/GMT+6") %>%
         with_tz(tzone = "UTC")) %>%
  group_by(basin_name, station, standardized_date_time, depth_m) %>% # this and the next 3 lines are for identifying duplicates and removing them by averaging them together
  mutate(temp_c = mean(temp_c, na.rm = TRUE)) %>%
  distinct(basin_name, station, standardized_date_time, depth_m, .keep_all = TRUE) %>%
  ungroup()
rm(Sentinel.Lakes.Temp.Data)

#Sentinel.Lakes.DO.Data <- read.csv("Data/TDO sensor data/Sentinel Lakes/do_data.csv", stringsAsFactors = F)
#Sentinel.Lakes.DO.Data.2 <- Sentinel.Lakes.DO.Data %>%
#  filter(!is.na(standardized_date_time)) %>%
#  mutate(value_type = "actual",
#         depth_m = case_when(is.na(depth_m) ~ 1, .default = depth_m),
#         standardized_date_time = case_when(nchar(standardized_date_time) == 10 ~ paste(standardized_date_time, "00:00:00", sep = " "), .default = standardized_date_time),
#        standardized_date_time = ymd_hms(standardized_date_time, tz = "Etc/GMT+6"),
#       standardized_date_time = with_tz(standardized_date_time, tzone = "America/Chicago"))

Carlos.Temp <- Sentinel.Lakes.Temp.Data.2 %>%
  filter(basin_name == "Carlos")
#Carlos.DO <- Sentinel.Lakes.DO.Data.2 %>%
#  filter(basin_name == "Carlos")

Ten.Mile.Temp <- Sentinel.Lakes.Temp.Data.2 %>%
  filter(basin_name == "Ten Mile")
# Ten Mile has more than 1 measurement at the same depth in some places, just take their average
#Ten.Mile.Temp <- Ten.Mile.Temp %>%
#  group_by(standardized_date_time, depth_m) %>%
#  mutate(temp_c = mean(temp_c, na.rm = TRUE)) %>%
#  distinct(standardized_date_time, depth_m, .keep_all = TRUE) %>%
#  ungroup()
#Ten.Mile.DO <- Sentinel.Lakes.DO.Data.2 %>%
#  filter(basin_name == "Ten Mile")

Elk.Temp <- Sentinel.Lakes.Temp.Data.2 %>%
  filter(basin_name == "Elk")
#Elk.DO <- Sentinel.Lakes.DO.Data.2 %>%
#  filter(basin_name == "Elk")

Greenwood.Temp <- Sentinel.Lakes.Temp.Data.2 %>%
  filter(basin_name == "Greenwood")
#Greenwood.DO <- Sentinel.Lakes.DO.Data.2 %>%
#  filter(basin_name == "Greenwood")

# Interpolate ----

# For now, just going to use the approx function and do a linear interpolation.
# In the future I will test whether it is better and possible to use the discrete
# profiles to inform the interpolation.

interpolate_profile <- function(data, all_depths) {
  data$value_type <- "actual"
  depths.to.estimate <- all_depths[!as.character(all_depths) %in% as.character(data$depth_m)]
  if (nrow(data) <= 1) {
    return(data)
  }
  estimates <- approx(
    x = data$depth_m, # x and y are the actual depth and temp data used to interpolate at the estimated depths
    y = data$temp_c, 
    xout = depths.to.estimate, # calculates estimates for only the ones to be estimated, leaving the actual depths out
    method = "linear", # simple linear interpolation, may eventually change this to a smooth function informed by single-point profile data
    rule = 2 # rule = 2 applies constants to NAs at ends of the data set (depths above and below the max and min sensor depths)
  )
  first_row <- data[1, ]
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
  Interpolated.Data <- bind_rows(data, Estimated.Data) %>%
    arrange(depth_m)
}

Carlos.Temp.Interpolated <- Carlos.Temp %>%
  group_by(standardized_date_time) %>%
  group_modify(~ interpolate_profile(data = .x, all_depths = seq(0, max(Carlos.Temp$depth_m), by = 0.1))) %>%
  ungroup()
rm(Carlos.Temp)

Ten.Mile.Temp.Interpolated <- Ten.Mile.Temp %>%
  group_by(standardized_date_time) %>%
  group_modify(~ interpolate_profile(data = .x, all_depths = seq(0, max(Ten.Mile.Temp$depth_m), by = 0.1))) %>%
  ungroup()
rm(Ten.Mile.Temp)

Elk.Temp.Interpolated <- Elk.Temp %>%
  group_by(standardized_date_time) %>%
  group_modify(~ interpolate_profile(data = .x, all_depths = seq(0, max(Elk.Temp$depth_m), by = 0.1))) %>%
  ungroup()
rm(Elk.Temp)

Greenwood.Temp.Interpolated <- Greenwood.Temp %>%
  group_by(standardized_date_time) %>%
  group_modify(~ interpolate_profile(data = .x, all_depths = seq(0, max(Greenwood.Temp$depth_m), by = 0.1))) %>%
  ungroup()
rm(Greenwood.Temp)
beep(9)

# Visualize data ----

# let's see how many observations there are per year
obs.per.yr <- table(word(word(Carlos.Temp$standardized_date_time, 1, 1, sep = " "), 1, 1, sep = "-"))

plot_profile <- function(data,
                         date_time_start,
                         date_time_end,
                         title,
                         ref_colors = c("#2c0354", "blue", "green", "yellow", "red", "#7a0000"),
                         ref_temps = c(0, 4, 10.5, 17, 22, 30)) {
  Data.Filt <- data %>%
    filter(standardized_date_time >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
           standardized_date_time <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))
  plot <- ggplot(Data.Filt, aes(x = standardized_date_time, y = depth_m, fill = temp_c)) +
    geom_raster() +
    scale_y_reverse() +
    scale_fill_gradientn(colors = ref_colors,
                         values = rescale(ref_temps, from = c(0, 30)), # Matches the full legend limits
                         limits = c(0, 30)) + # Displays 0°C to 30°C on the legend
    labs(x = "Date", y = "Depth (m)", fill = "Temp (°C)", title = title) +
    theme_bw()
  return(plot)
}

# need to remove actual measurements that are taken at odd depth intervals (1.65, 4.72, etc.)
Carlos.Temp.Interpolated.2 <- Carlos.Temp.Interpolated %>%
    filter(as.character(depth_m) %in% as.character(seq(0, max(Carlos.Temp$depth_m), by = 0.1)))
CarlosTempProfilePlots <- list()
min.year <- min(year(Carlos.Temp$standardized_date_time))
max.year <- max(year(Carlos.Temp$standardized_date_time))
for (year in as.character(min.year:max.year)) {
  CarlosTempProfilePlots[[year]] <- plot_profile(data = Carlos.Temp.Interpolated.2,
                                                 date_time_start = paste(year, "01-01 00:00:00", sep = "-"),
                                                 date_time_end = paste(year, "12-31 23:00:00", sep = "-"),
                                                 title = paste(year, "Carlos Temp Profile", sep = " "))
}
length(CarlosTempProfilePlots)
names(CarlosTempProfilePlots)
p_carlos2008 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2008-01-01 00:00:00",
                             date_time_end = "2008-12-31 23:00:00",
                             title = "2008 Carlos Temp Profile")
p_carlos2009 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2009-01-01 00:00:00",
                             date_time_end = "2009-12-31 23:00:00",
                             title = "2009 Carlos Temp Profile")
p_carlos2010 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2010-01-01 00:00:00",
                             date_time_end = "2010-12-31 23:00:00",
                             title = "2010 Carlos Temp Profile")
p_carlos2011 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2011-01-01 00:00:00",
                             date_time_end = "2011-12-31 23:00:00",
                             title = "2011 Carlos Temp Profile")
p_carlos2013 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2013-01-01 00:00:00",
                             date_time_end = "2013-12-31 23:00:00",
                             title = "2013 Carlos Temp Profile")
p_carlos2014 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2014-01-01 00:00:00",
                             date_time_end = "2014-12-31 23:00:00",
                             title = "2014 Carlos Temp Profile")
p_carlos2015 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2015-01-01 00:00:00",
                             date_time_end = "2015-12-31 23:00:00",
                             title = "2015 Carlos Temp Profile")
p_carlos2016 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2016-01-01 00:00:00",
                             date_time_end = "2016-12-31 23:00:00",
                             title = "2016 Carlos Temp Profile")
p_carlos2017 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2017-01-01 00:00:00",
                             date_time_end = "2017-12-31 23:00:00",
                             title = "2017 Carlos Temp Profile")
p_carlos2018 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2018-01-01 00:00:00",
                             date_time_end = "2018-12-31 23:00:00",
                             title = "2018 Carlos Temp Profile")
p_carlos2019 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2019-01-01 00:00:00",
                             date_time_end = "2019-12-31 23:00:00",
                             title = "2019 Carlos Temp Profile")
p_carlos2020 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2020-01-01 00:00:00",
                             date_time_end = "2020-12-31 23:00:00",
                             title = "2020 Carlos Temp Profile")
p_carlos2021 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2021-01-01 00:00:00",
                             date_time_end = "2021-12-31 23:00:00",
                             title = "2021 Carlos Temp Profile")
p_carlos2022 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2022-01-01 00:00:00",
                             date_time_end = "2022-12-31 23:00:00",
                             title = "2022 Carlos Temp Profile")
p_carlos2023 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2023-01-01 00:00:00",
                             date_time_end = "2023-12-31 23:00:00",
                             title = "2023 Carlos Temp Profile")
p_carlos2024 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2024-01-01 00:00:00",
                             date_time_end = "2024-12-31 23:00:00",
                             title = "2024 Carlos Temp Profile")
p_carlos2025 <- plot_profile(data = Carlos.Temp.Interpolated.2,
                             date_time_start = "2025-01-01 00:00:00",
                             date_time_end = "2025-12-31 23:00:00",
                             title = "2025 Carlos Temp Profile")








