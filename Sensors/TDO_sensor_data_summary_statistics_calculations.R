### TDO sensor data analysis ###
# Created by Mike M on Jul 21 2026
# Input: Cleaned temp DO sensor data
# Output: Coldwater habitat and limnological statistics calculated from sensor data
# No statistical analysis is conducted here, just summary statistics from data. See other script for statistical analysis.
# There is also no data visualization here. See "TDO_sensor_data_visualization.R" for that.

# Load libraries ----
library(tidyverse) # for data wrangling
library(beepr)
library(arrow) # for pulling in data from sensor database
library(rLakeAnalyzer) # for calculate limno metrics
library(weathermetrics)


# Open database ----
### right now it only has the sentinel lakes data
Sentinel.Lakes.Data <- open_dataset("~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/TDO sensors/Sensor hive/source=MN_DNR_Sentinel_Lakes/mn_dnr_sentinel_lakes.parquet")

# Read in covariate and helper data ----
Hypso <- read.csv("~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/Covariates/lake_hypsography.csv", stringsAsFactors = F)
Xwalk <- read.csv("~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/Helper/mndow_nhdhr_xwalk.csv", stringsAsFactors = F)

# Functions ----

## Calculate oxythermal metrics ----
#### cisco layer thickness, depth of temp threshold, depth of do threshold, TDOx

calc_oxythermal_metrics <- function(data, temp_threshold, do_threshold, max_depth_m) {
  data <- data %>%
    mutate(o2_suitability = case_when(do_mgl >= do_threshold ~ "suitable",
                                      .default = "unsuitable"),
           temp_suitability = case_when(temp_c <= temp_threshold ~ "suitable",
                                        .default = "unsuitable"))
  if (nrow(data) < 3) {
    layer_thickness <- NA; depth_temp_threshold <- NA; depth_o2_threshold <- NA; tdox <- NA
    metrics <- data.frame(layer_thickness = NA, depth_temp_threshold = NA, depth_o2_threshold = NA, tdox = NA)
    return(metrics)
  }
  data <- data[order(data$depth_m, decreasing = F),]
  above_o2_depth <- c()
  below_o2_depth <- c()
  above_temp_depth <- c()
  below_temp_depth <- c()
  for (i in 1:nrow(data)) {
    if (i == nrow(data)) {
      next
    } else if (data$o2_suitability[i] != data$o2_suitability[i+1] &
               data$o2_suitability[i] == "suitable" &
               !any("suitable" %in% data$o2_suitability[c(i+2:nrow(data))])) {
      above_o2_depth <- c(above_o2_depth, data$depth_m[i])
      below_o2_depth <- c(below_o2_depth, data$depth_m[i+1])
    }
    if (i == nrow(data)) {
      next
    } else if (data$temp_suitability[i] != data$temp_suitability[i+1] &
               data$temp_suitability[i] == "unsuitable" &
               !any("unsuitable" %in% data$temp_suitability[c(i+2:nrow(data))])) {
      above_temp_depth <- c(above_temp_depth, data$depth_m[i])
      below_temp_depth <- c(below_temp_depth, data$depth_m[i+1])
    }
  }
  if (length(above_o2_depth) > 1) {
    above_o2_depth <- min(above_o2_depth)
    below_o2_depth <- min(below_o2_depth)
  }
  if (length(above_temp_depth) > 1) {
    above_temp_depth <- min(above_temp_depth)
    below_temp_depth <- min(below_temp_depth)
  }
  if (!any(data$o2_suitability == "unsuitable") |
      length(above_o2_depth) < 1) {
    if (all(data$o2_suitability == "unsuitable")) {
      depth_o2_threshold <- 0
    } else {
      depth_o2_threshold <- max_depth_m
    }
  } else if (!any(data$o2_suitability == "suitable")) {
    if (min(data$depth_m) > 3) {
      depth_o2_threshold <- min(data$depth_m)
    } else {
      depth_o2_threshold <- 0
    }
  } else {
    do_above <- data$do_mgl[data$depth_m == above_o2_depth]
    do_below <- data$do_mgl[data$depth_m == below_o2_depth]
    slopeDO <- (do_below - do_above) / (below_o2_depth - above_o2_depth)
    interceptDO <- do_below - slopeDO * below_o2_depth
    depth_o2_threshold <- (do_threshold - interceptDO) / slopeDO
  }
  if (!any(data$temp_suitability == "unsuitable")) {
    depth_temp_threshold <- 0
  } else {
    temp_above <- data$temp_c[data$depth_m == above_temp_depth]
    temp_below <- data$temp_c[data$depth_m == below_temp_depth]
    slopeTemp <- (temp_below - temp_above) / (below_temp_depth - above_temp_depth)
    interceptTemp <- temp_below - slopeTemp * below_temp_depth
    depth_temp_threshold <- (temp_threshold - interceptTemp) / slopeTemp
  }
  layer_thickness <- depth_o2_threshold - depth_temp_threshold
  # TDOx
  tdox <- if (data$do_mgl[data$depth_m == max(data$depth_m)] > do_threshold) {
    data$temp_c <- data$temp_c[data$depth_m == max(data$depth_m)]
  } else {
    if (min(data$do_mgl) >= do_threshold |
        depth_o2_threshold == max_depth_m) {
      min(data$temp_c)
    } else {
      if (max(data$do_mgl) < do_threshold) {
        max(data$temp_c) # originally set tdo to NA if O2 is never high enough to reach threshold but that doesn't allow for including it in degree days, so setting it to the max temp for now until I figure out a better way to do it
      } else {
        temp_above <- data$temp_c[data$depth_m == above_o2_depth]
        temp_below <- data$temp_c[data$depth_m == below_o2_depth]
        slopeTemp <- (temp_below - temp_above) / (below_o2_depth - above_o2_depth)
        interceptTemp <- temp_below - slopeTemp * below_o2_depth
        slopeTemp*depth_o2_threshold + interceptTemp
      }
    }
  }
  metrics <- data.frame(tdox = tdox, layer_thickness = layer_thickness, depth_temp_threshold = depth_temp_threshold, depth_o2_threshold = depth_o2_threshold)
  return(metrics)
}

## Calculate volumetric habitat availability ----

calc_volumetric_habitat <- function (do_threshold_depth, temp_threshold_depth, hypso_data, max_depth) {
  if (max(hypso_data$depths) != max_depth) {
    hypso_data <- hypso_data %>%
      add_row(depths = max_depth,
              areas = 0)
  }
  if (is.na(do_threshold_depth) | is.na(temp_threshold_depth)) {
    total_volume <- NA
    return(total_volume)
  }
  if (do_threshold_depth <= temp_threshold_depth) {
    total_volume <- 0
    return(total_volume)
  }
  # calculate the area at the temperature threshold depth
  if (temp_threshold_depth == 0) {
    temp_threshold_area <- hypso_data$areas[hypso_data$depths == 0]
  } else if (temp_threshold_depth == max_depth) {
    temp_threshold_area <- hypso_data$areas[hypso_data$depths == max_depth]
  } else {
    if (temp_threshold_depth %in% hypso_data$depths) {
      temp_threshold_area <- hypso_data$areas[hypso_data$depths == temp_threshold_depth]
    } else {
      hypso_depth_above_temp <- max(hypso_data$depths[hypso_data$depths <= temp_threshold_depth]) # depth above the temp threshold depth that we have hypso data for
      hypso_depth_below_temp <- min(hypso_data$depths[hypso_data$depths >= temp_threshold_depth]) # depth below the temp threshold depth that we have hypso data for
      hypso_area_above_temp <- hypso_data$areas[hypso_data$depths == hypso_depth_above_temp]
      hypso_area_below_temp <- hypso_data$areas[hypso_data$depths == hypso_depth_below_temp]
      m <- ((hypso_area_above_temp - hypso_area_below_temp) / (hypso_depth_above_temp - hypso_depth_below_temp))
      b <- hypso_area_above_temp - (m * hypso_depth_above_temp)
      temp_threshold_area <- (m * temp_threshold_depth) + b
    }
  }
  # now calculate the area at the oxygen threshold depth
  if (do_threshold_depth == 0) {
    do_threshold_area <- hypso_data$areas[hypso_data$depths == 0]
  } else if (do_threshold_depth == max_depth) {
    do_threshold_area <- hypso_data$areas[hypso_data$depths == max_depth]
  } else {
    if (do_threshold_depth %in% hypso_data$depths) {
      do_threshold_area <- hypso_data$areas[hypso_data$depths == do_threshold_depth]
    } else {
      hypso_depth_above_do <- max(hypso_data$depths[hypso_data$depths <= do_threshold_depth]) # depth above the do threshold depth that we have hypso data for
      hypso_depth_below_do <- min(hypso_data$depths[hypso_data$depths >= do_threshold_depth]) # depth below the do threshold depth that we have hypso data for
      hypso_area_above_do <- hypso_data$areas[hypso_data$depths == hypso_depth_above_do]
      hypso_area_below_do <- hypso_data$areas[hypso_data$depths == hypso_depth_below_do]
      m <- ((hypso_area_above_do - hypso_area_below_do) / (hypso_depth_above_do - hypso_depth_below_do))
      b <- hypso_area_above_do - (m * hypso_depth_above_do)
      do_threshold_area <- (m * do_threshold_depth) + b 
    }
  }
  # now calculate the volume between each depth within suitable habitat range and sum them all to get total volumetric habitat availability
  depths <- c(temp_threshold_depth, hypso_data$depths[hypso_data$depths >= temp_threshold_depth & hypso_data$depths <= do_threshold_depth], do_threshold_depth)
  areas <- c(temp_threshold_area, hypso_data$areas[hypso_data$depths >= temp_threshold_depth & hypso_data$depths <= do_threshold_depth], do_threshold_area)
  volumes <- c()
  for (i in 1:(length(depths)-1)) {
    V <- (1 / 3) * diff(c(depths[i], depths[i+1])) * (areas[i] + areas[i+1] + sqrt(areas[i] * areas[i+1]))
    volumes <- c(volumes, V)
  }
  total_volume <- sum(volumes)
  return(total_volume)
}

## Calculate mean values over a given period of time ----

smooth_metric <- function(data, width) {
  avg <- c()
  for (i in 1:length(data)) {
    if (i %in% 1:width) {
      window <- c(1:i+width)
    } else if (i %in% (length(data) - width):length(data)) {
      window <- c((i-width):length(data))
    } else {
      window <- c((i-width):(i+width))
    }
    avg <- c(avg, mean(data[window], na.rm = T))
  }
  return(avg)
}

## Calculate rate of change in metrics ----

calc_roc <- function(data, metric, start = NA, end = NA, date_time = NA, width = NA) {
  if (is.na(start)) {
    if ("date" %in% names(data)) {
      start <- data$date[1]
    } else {
      start <- format(data$date_time_local[1], "%Y-%m-%d %H:%M:%S")
    }
  } else {
    if (start == end) {
      return(NA)
    }
  }
  if (is.na(end)) {
    if ("date" %in% names(data)) {
      end <- data$date[nrow(data)]
    } else {
      end <- format(data$date_time_local[nrow(data)], "%Y-%m-%d %H:%M:%S")
    }
  } else {
    if (end == "NA") {
      return(NA)
    }
  }
  if (nchar(as.character(start)) == 10 | nchar(as.character(end)) == 10) {
    data <- data %>%
      select(date, all_of(metric)) %>%
      filter(date >= start,
             date <= end)
  } else {
    data <- data %>%
      select(date_time_local, all_of(metric)) %>%
      filter(date_time_local >= start,
             date_time_local <= end)
  }
  data <- as.data.frame(data)
  if (is.na(width)) {
    model <- summary(lm(data[, 2] ~ data[, 1]))
    roc <- model$coefficients[2]
  }
  if (!is.na(width)) {
    roc <- c()
    if (width == "week") {
      data$year <- year(data$date)
      data$week <- week(data$date)
      for (i in unique(data$year)) {
        for (j in unique(data$week[data$year == i])) {
          window <- which(data$year == i & data$week == j)
          model <- summary(lm(data[window, 2] ~ data[window, 1]))
          roc <- c(roc, model$coefficients[2])
        }
      }
    } else {
      for (i in 1:(nrow(data) - 1)) {
        if (width == 0) {
          window <- c(i, i + 1)
        } else if (i %in% 1:width) {
          window <- c(1:(i + width))
        } else if (i %in% (nrow(data) - width):nrow(data)) {
          window <- c((i - width):nrow(data))
        } else {
          window <- c((i - width):(i + width))
        }
        if (any(is.na(data[window, 2]))) {
          roc <- c(roc, NA)
        } else {
          model <- summary(lm(data[window, 2] ~ data[window, 1]))
          roc <- c(roc, model$coefficients[2])
        }
      }
    }
  }
  return(roc)
}

## Interpolate temperature profile ----

interpolate_temp_profile <- function(data, all_depths) {
  data$value_type <- "actual"
  depths.to.estimate <- all_depths[!as.character(all_depths) %in% as.character(data$depth_m)]
  if (nrow(data) <= 1) {
    return(data)
  }
  first_row <- data[1, ]
  estimates <- approx(
      x = data$depth_m, # x and y are the actual depth and temp/DO data used to interpolate at the estimated depths
      y = data$temp_c, 
      xout = depths.to.estimate, # calculates estimates for only the ones to be estimated, leaving the actual depths out
      method = "linear", # simple linear interpolation, may eventually change this to a smooth function informed by single-point profile data
      rule = 2 # rule = 2 applies constants to NAs at ends of the data set (depths above and below the max and min sensor depths)
    )
    Estimated.Data <- tibble(
      lake_name = first_row$basin_name,
      depth_m = estimates$x,
      date_time_local = first_row$date_time_local,
      temp_c = estimates$y,
      value_type = "estimated"
    )
  Interpolated.Data <- bind_rows(data[, names(Estimated.Data)], Estimated.Data) %>%
    arrange(depth_m)
}

# Greenwood ----

## Read in data ----
Greenwood <- Sentinel.Lakes.Data %>%
  filter(lake_name == "Greenwood",
         flaggross_do == "P",
         flagspike_do == "P",
         flagroc_do == "P",
         flagflat_do == "P",
         flagvis_do == "P",
         flaggross_temp == "P",
         flagspike_temp == "P",
         flagroc_temp == "P",
         flagflat_temp == "P",
         flagvis_temp == "P") %>%
  select(lake_name,
         depth_m,
         date_time_local,
         do_mgl,
         temp_c) %>%
  arrange(date_time_local, depth_m) %>%
  collect()

greenwoodMaxDepth <- 34

### Bring in hypsography data ----

greenwoodDOW <- "16007700"
Greenwood.Hypso <- Hypso %>%
  filter(site_id == Xwalk$site_id[Xwalk$MNDOW_ID == paste("mndow", greenwoodDOW, sep = "_")]) %>%
  add_row(site_id = "nhdhr_120020567",
          depths = greenwoodMaxDepth,
          areas = 0,
          site_n = 32)

## Calculate metrics for different oxythermal thresholds ----

### 6 mg/L DO and 17 ºC ----
Greenwood.Stats.1a <- Greenwood %>%
  group_by(date_time_local) %>%
  summarise(tdo6 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = greenwoodMaxDepth)$tdox,
            layer_thickness_do6_temp17 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = greenwoodMaxDepth)$layer_thickness,
            depth_do6 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = greenwoodMaxDepth)$depth_o2_threshold,
            depth_temp17 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = greenwoodMaxDepth)$depth_temp_threshold,
            .groups = "drop")

### 3 mg/L DO and 21 ºC ----
Greenwood.Stats.1b <- Greenwood %>%
  group_by(date_time_local) %>%
  summarise(tdo3 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = greenwoodMaxDepth)$tdox,
            layer_thickness_do3_temp21 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = greenwoodMaxDepth)$layer_thickness,
            depth_do3 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = greenwoodMaxDepth)$depth_o2_threshold,
            depth_temp21 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = greenwoodMaxDepth)$depth_temp_threshold,
            .groups = "drop")

### 1 mg/L and 25 ----
Greenwood.Stats.1c <- Greenwood %>%
  group_by(date_time_local) %>%
  summarise(tdo1 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = greenwoodMaxDepth)$tdox,
            layer_thickness_do1_temp25 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = greenwoodMaxDepth)$layer_thickness,
            depth_do1 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = greenwoodMaxDepth)$depth_o2_threshold,
            depth_temp25 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = greenwoodMaxDepth)$depth_temp_threshold,
            max_temp = max(temp_c),
            min_do = min(do_mgl),
            schmidt_stability = schmidt.stability(wtr = temp_c,
                                                  depths = depth_m,
                                                  bthA = Greenwood.Hypso$areas,
                                                  bthD = Greenwood.Hypso$depth)[, 1],
            .groups = "drop")

Greenwood.CLT <- Greenwood %>%
  group_by(date_time_local) %>%
  summarise(CLT = calc_oxythermal_metrics(pick(everything()), temp_threshold = 22.8, do_threshold = 6, max_depth_m = greenwoodMaxDepth)$layer_thickness,
            depth_temp22.8 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 22.8, do_threshold = 6, max_depth_m = greenwoodMaxDepth)$depth_temp_threshold,
            .groups = "drop")

### Combine all thresholds into one data frame ----
Greenwood.Thresholds <- Greenwood.Stats.1a %>%
  left_join(y = Greenwood.Stats.1b, by = join_by(date_time_local)) %>%
  left_join(y = Greenwood.Stats.1c, by = join_by(date_time_local)) %>%
  mutate(stratified = case_when((word(date_time_local, 2, 2, sep = "-") %in% c("10", "11") & depth_do6 < greenwoodMaxDepth) |
                                schmidt_stability > 20 ~ "yes",
                                .default = "no"))
Greenwood.Thresholds <- Greenwood.Thresholds %>%
  left_join(y = Greenwood.CLT, by = join_by(date_time_local))

# can remove the subsets now that we have them all in one
rm(Greenwood.Stats.1a, Greenwood.Stats.1b, Greenwood.Stats.1c)

## Calculate volumetric habitat availability ----

Greenwood.Thresholds$volumetric_habitat_availability_do6_temp17 <- NA
greenwoodTotalVolume <- calc_volumetric_habitat(max_depth = greenwoodMaxDepth, hypso_data = Greenwood.Hypso, temp_threshold_depth = 0, do_threshold_depth = greenwoodMaxDepth)
for (i in 1:nrow(Greenwood.Thresholds)) {
  Greenwood.Thresholds$volumetric_habitat_availability_do6_temp17[i] <- calc_volumetric_habitat(do_threshold_depth = Greenwood.Thresholds$depth_do6[i],
                                                                                                temp_threshold_depth = Greenwood.Thresholds$depth_temp17[i],
                                                                                                hypso_data = Greenwood.Hypso,
                                                                                                greenwoodMaxDepth)
}
Greenwood.Thresholds$volumetric_habitat_availability_do3_temp21 <- NA
for (i in 1:nrow(Greenwood.Thresholds)) {
  Greenwood.Thresholds$volumetric_habitat_availability_do3_temp21[i] <- calc_volumetric_habitat(do_threshold_depth = Greenwood.Thresholds$depth_do3[i],
                                                                                                temp_threshold_depth = Greenwood.Thresholds$depth_temp21[i],
                                                                                                hypso_data = Greenwood.Hypso,
                                                                                                greenwoodMaxDepth)
}
Greenwood.Thresholds$volumetric_habitat_availability_do1_temp25 <- NA
for (i in 1:nrow(Greenwood.Thresholds)) {
  Greenwood.Thresholds$volumetric_habitat_availability_do1_temp25[i] <- calc_volumetric_habitat(do_threshold_depth = Greenwood.Thresholds$depth_do1[i],
                                                                                                temp_threshold_depth = Greenwood.Thresholds$depth_temp25[i],
                                                                                                hypso_data = Greenwood.Hypso,
                                                                                                greenwoodMaxDepth)
}
Greenwood.Thresholds$volumetric_habitat_CLT <- NA
for (i in 1:nrow(Greenwood.Thresholds)) {
  Greenwood.Thresholds$volumetric_habitat_CLT[i] <- calc_volumetric_habitat(do_threshold_depth = Greenwood.Thresholds$depth_do6[i],
                                                                                                temp_threshold_depth = Greenwood.Thresholds$depth_temp22.8[i],
                                                                                                hypso_data = Greenwood.Hypso,
                                                                                                greenwoodMaxDepth)
}
rm(Greenwood.Hypso)

## Relative habitat availability ----

Greenwood.Thresholds <- Greenwood.Thresholds %>%
  mutate(relative_layer_thickness_do6_temp17 = layer_thickness_do6_temp17 / greenwoodMaxDepth,
         relative_layer_thickness_do3_temp21 = layer_thickness_do3_temp21 / greenwoodMaxDepth,
         relative_layer_thickness_do1_temp25 = layer_thickness_do1_temp25 / greenwoodMaxDepth,
         relative_volumetric_habitat_availability_do6_temp17 = volumetric_habitat_availability_do6_temp17 / greenwoodTotalVolume,
         relative_volumetric_habitat_availability_do3_temp21 = volumetric_habitat_availability_do3_temp21 / greenwoodTotalVolume,
         relative_volumetric_habitat_availability_do1_temp25 = volumetric_habitat_availability_do1_temp25 / greenwoodTotalVolume)

## Interpolate temp profile ----

Greenwood.Temp.Interpolated <- Greenwood %>%
  group_by(date_time_local) %>%
  group_modify(~ interpolate_temp_profile(data = .x, all_depths = seq(0, greenwoodMaxDepth, by = 0.1))) %>%
  ungroup()

## Cumulative stress ----

### Calculate daily averages ----

Greenwood.Thresholds.Daily.Averages <- Greenwood.Thresholds %>%
  mutate(date = as.Date(word(date_time_local, 1, 1, sep = " "))) %>%
  group_by(date) %>%
  select(-date_time_local, -stratified) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  mutate(gdd_tdo6 = NA,
         gdd_tdo3 = NA,
         gdd_tdo1 = NA,
         gdd_tdo6_cumulative = NA,
         gdd_tdo3_cumulative = NA,
         gdd_tdo1_cumulative = NA,
         stratified = case_when((word(date, 2, 2, sep = "-") %in% c("10", "11") & depth_do6 < greenwoodMaxDepth) |
                                schmidt_stability > 20
                                ~ "yes",
                                .default = "no"))


### Calculate growing degree days ----

# Lyons et al. 2017 benchmarks
# Need to use like 11 and TDO6 because these lakes don't get stressed at all
for (i in 1:nrow(Greenwood.Thresholds.Daily.Averages)) {
  Greenwood.Thresholds.Daily.Averages$gdd_tdo6[i] <- if (Greenwood.Thresholds.Daily.Averages$tdo6[i] < 11) {0} else {Greenwood.Thresholds.Daily.Averages$tdo6[i] - 11}
  Greenwood.Thresholds.Daily.Averages$gdd_tdo3[i] <- if (Greenwood.Thresholds.Daily.Averages$tdo3[i] < 21.5) {0} else {Greenwood.Thresholds.Daily.Averages$tdo3[i] - 21.5}
  Greenwood.Thresholds.Daily.Averages$gdd_tdo1[i] <- if (Greenwood.Thresholds.Daily.Averages$tdo1[i] < 19.5) {0} else {Greenwood.Thresholds.Daily.Averages$tdo1[i] - 19.5}
}

### Calculate cumulative growing degree days ----

Greenwood.Thresholds.Daily.Averages$gdd_tdo6_cumulative[Greenwood.Thresholds.Daily.Averages$stratified == "no"] <- 0
Greenwood.Thresholds.Daily.Averages$gdd_tdo3_cumulative[Greenwood.Thresholds.Daily.Averages$stratified == "no"] <- 0
Greenwood.Thresholds.Daily.Averages$gdd_tdo1_cumulative[Greenwood.Thresholds.Daily.Averages$stratified == "no"] <- 0
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2018" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo6[1:i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo3[1:i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo1[1:i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2019" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo6[which(Greenwood.Thresholds.Daily.Averages$date == "2019-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo3[which(Greenwood.Thresholds.Daily.Averages$date == "2019-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo1[which(Greenwood.Thresholds.Daily.Averages$date == "2019-06-01"):i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2020" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo6[which(Greenwood.Thresholds.Daily.Averages$date == "2020-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo3[which(Greenwood.Thresholds.Daily.Averages$date == "2020-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo1[which(Greenwood.Thresholds.Daily.Averages$date == "2020-06-01"):i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2021" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo6[which(Greenwood.Thresholds.Daily.Averages$date == "2021-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo3[which(Greenwood.Thresholds.Daily.Averages$date == "2021-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo1[which(Greenwood.Thresholds.Daily.Averages$date == "2021-06-01"):i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2022" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo6[which(Greenwood.Thresholds.Daily.Averages$date == "2022-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo3[which(Greenwood.Thresholds.Daily.Averages$date == "2022-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo1[which(Greenwood.Thresholds.Daily.Averages$date == "2022-06-01"):i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2023" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo6[which(Greenwood.Thresholds.Daily.Averages$date == "2023-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo3[which(Greenwood.Thresholds.Daily.Averages$date == "2023-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo1[which(Greenwood.Thresholds.Daily.Averages$date == "2023-06-01"):i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2024" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo6[which(Greenwood.Thresholds.Daily.Averages$date == "2024-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo3[which(Greenwood.Thresholds.Daily.Averages$date == "2024-06-01"):i])
  Greenwood.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Greenwood.Thresholds.Daily.Averages$gdd_tdo1[which(Greenwood.Thresholds.Daily.Averages$date == "2024-06-01"):i])
}

## Cumulative volumetric habitat availability ----

Greenwood.Thresholds.Daily.Averages$cumulative_vol_hab_do3_temp17 <- 0
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2018" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$cumulative_vol_hab_do3_temp17[i] <- sum(greenwoodTotalVolume - Greenwood.Thresholds.Daily.Averages$volumetric_habitat_availability_do3_temp21[1:i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2019" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$cumulative_vol_hab_do3_temp17[i] <- sum(greenwoodTotalVolume - Greenwood.Thresholds.Daily.Averages$volumetric_habitat_availability_do3_temp21[which(Greenwood.Thresholds.Daily.Averages$date == "2019-06-01"):i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2020" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$cumulative_vol_hab_do3_temp17[i] <- sum(greenwoodTotalVolume - Greenwood.Thresholds.Daily.Averages$volumetric_habitat_availability_do3_temp21[which(Greenwood.Thresholds.Daily.Averages$date == "2020-06-01"):i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2021" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$cumulative_vol_hab_do3_temp17[i] <- sum(greenwoodTotalVolume - Greenwood.Thresholds.Daily.Averages$volumetric_habitat_availability_do3_temp21[which(Greenwood.Thresholds.Daily.Averages$date == "2021-06-01"):i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2022" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$cumulative_vol_hab_do3_temp17[i] <- sum(greenwoodTotalVolume - Greenwood.Thresholds.Daily.Averages$volumetric_habitat_availability_do3_temp21[which(Greenwood.Thresholds.Daily.Averages$date == "2022-06-01"):i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2023" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$cumulative_vol_hab_do3_temp17[i] <- sum(greenwoodTotalVolume - Greenwood.Thresholds.Daily.Averages$volumetric_habitat_availability_do3_temp21[which(Greenwood.Thresholds.Daily.Averages$date == "2023-06-01"):i])
}
for (i in which(word(Greenwood.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2024" & Greenwood.Thresholds.Daily.Averages$stratified == "yes")) {
  Greenwood.Thresholds.Daily.Averages$cumulative_vol_hab_do3_temp17[i] <- sum(greenwoodTotalVolume - Greenwood.Thresholds.Daily.Averages$volumetric_habitat_availability_do3_temp21[which(Greenwood.Thresholds.Daily.Averages$date == "2024-06-01"):i])
}

## Calculate average values within weekly and biweekly windows ----

Greenwood.Thresholds <- Greenwood.Thresholds %>%
  mutate(tdo6_avg_weekly = smooth_metric(data = Greenwood.Thresholds$tdo6, width = 84),
         tdo3_avg_weekly = smooth_metric(data = Greenwood.Thresholds$tdo3, width = 84),
         tdo1_avg_weekly = smooth_metric(data = Greenwood.Thresholds$tdo1, width = 84),
         layer_thickness_do6_temp17_avg_weekly = smooth_metric(data = Greenwood.Thresholds$layer_thickness_do6_temp17, width = 84),
         layer_thickness_do3_temp21_avg_weekly = smooth_metric(data = Greenwood.Thresholds$layer_thickness_do3_temp21, width = 84),
         layer_thickness_do1_temp25_avg_weekly = smooth_metric(data = Greenwood.Thresholds$layer_thickness_do1_temp25, width = 84),
         relative_layer_thickness_do6_temp17_avg_weekly =  smooth_metric(data = Greenwood.Thresholds$relative_layer_thickness_do6_temp17, width = 84),
         relative_layer_thickness_do3_temp21_avg_weekly =  smooth_metric(data = Greenwood.Thresholds$relative_layer_thickness_do3_temp21, width = 84),
         relative_layer_thickness_do1_temp25_avg_weekly =  smooth_metric(data = Greenwood.Thresholds$relative_layer_thickness_do1_temp25, width = 84),
         depth_do6_avg_weekly = smooth_metric(data = Greenwood.Thresholds$depth_do6, width = 84),
         depth_do3_avg_weekly = smooth_metric(data = Greenwood.Thresholds$depth_do3, width = 84),
         depth_do1_avg_weekly = smooth_metric(data = Greenwood.Thresholds$depth_do1, width = 84),
         depth_temp17_avg_weekly = smooth_metric(data = Greenwood.Thresholds$depth_temp17, width = 84),
         depth_temp21_avg_weekly = smooth_metric(data = Greenwood.Thresholds$depth_temp21, width = 84),
         depth_temp25_avg_weekly = smooth_metric(data = Greenwood.Thresholds$depth_temp25, width = 84),
         max_temp_avg_weekly = smooth_metric(data = Greenwood.Thresholds$max_temp, width = 84),
         min_do_avg_weekly = smooth_metric(data = Greenwood.Thresholds$min_do, width = 84),
         volumetric_habitat_availability_do6_temp17_avg_weekly = smooth_metric(data = Greenwood.Thresholds$volumetric_habitat_availability_do6_temp17, width = 84),
         volumetric_habitat_availability_do3_temp21_avg_weekly = smooth_metric(data = Greenwood.Thresholds$volumetric_habitat_availability_do3_temp21, width = 84),
         volumetric_habitat_availability_do1_temp25_avg_weekly = smooth_metric(data = Greenwood.Thresholds$volumetric_habitat_availability_do1_temp25, width = 84),
         relative_volumetric_habitat_availability_do6_temp17_avg_weekly = smooth_metric(data = Greenwood.Thresholds$relative_volumetric_habitat_availability_do6_temp17, width = 84),
         relative_volumetric_habitat_availability_do3_temp21_avg_weekly = smooth_metric(data = Greenwood.Thresholds$relative_volumetric_habitat_availability_do3_temp21, width = 84),
         relative_volumetric_habitat_availability_do1_temp25_avg_weekly = smooth_metric(data = Greenwood.Thresholds$relative_volumetric_habitat_availability_do1_temp25, width = 84),
         tdo6_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$tdo6, width = 168),
         tdo3_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$tdo3, width = 168),
         tdo1_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$tdo1, width = 168),
         layer_thickness_do6_temp17_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$layer_thickness_do6_temp17, width = 168),
         layer_thickness_do3_temp21_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$layer_thickness_do3_temp21, width = 168),
         layer_thickness_do1_temp25_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$layer_thickness_do1_temp25, width = 168),
         relative_layer_thickness_do6_temp17_avg_biweekly =  smooth_metric(data = Greenwood.Thresholds$relative_layer_thickness_do6_temp17, width = 168),
         relative_layer_thickness_do3_temp21_avg_biweekly =  smooth_metric(data = Greenwood.Thresholds$relative_layer_thickness_do3_temp21, width = 168),
         relative_layer_thickness_do1_temp25_avg_biweekly =  smooth_metric(data = Greenwood.Thresholds$relative_layer_thickness_do1_temp25, width = 168),
         depth_do6_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$depth_do6, width = 168),
         depth_do3_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$depth_do3, width = 168),
         depth_do1_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$depth_do1, width = 168),
         depth_temp17_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$depth_temp17, width = 168),
         depth_temp21_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$depth_temp21, width = 168),
         depth_temp25_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$depth_temp25, width = 168),
         max_temp_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$max_temp, width = 168),
         min_do_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$min_do, width = 168),
         volumetric_habitat_availability_do6_temp17_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$volumetric_habitat_availability_do6_temp17, width = 168),
         volumetric_habitat_availability_do3_temp21_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$volumetric_habitat_availability_do3_temp21, width = 168),
         volumetric_habitat_availability_do1_temp25_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$volumetric_habitat_availability_do1_temp25, width = 168),
         relative_volumetric_habitat_availability_do6_temp17_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$relative_volumetric_habitat_availability_do6_temp17, width = 168),
         relative_volumetric_habitat_availability_do3_temp21_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$relative_volumetric_habitat_availability_do3_temp21, width = 168),
         relative_volumetric_habitat_availability_do1_temp25_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$relative_volumetric_habitat_availability_do1_temp25, width = 168))

Greenwood.Thresholds <- Greenwood.Thresholds %>%
  mutate(volumetric_habitat_CLT_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$volumetric_habitat_CLT, width = 168),
         CLT_avg_biweekly = smooth_metric(data = Greenwood.Thresholds$CLT, width = 168))

## Calculate rates of change in metrics over different time scales ----

Greenwood.Thresholds.ROCs.Inst <- data.frame(date_time_local = Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)],
                                             date = as.Date(word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 1, 1, sep = " ")),
                                             year = word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 1, 1, sep = "-"),
                                             month = word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 2, 2, sep = "-"),
                                             dom = word(word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 3, 3, sep = "-"), 1, 1, sep = " "),
                                             doy = yday(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)]),
                                             week = week(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)]),
                                             tdo6 = calc_roc(data = Greenwood.Thresholds, metric = "tdo6", width = 0),
                                             layer_thickness_do6_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do6_temp17", width = 0),
                                             depth_do6 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do6", width = 0),
                                             depth_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "tdo3", width = 0),
                                             tdo3 = calc_roc(data = Greenwood.Thresholds, metric = "tdo6", width = 0),
                                             layer_thickness_do3_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do3_temp21", width = 0),
                                             depth_do3 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do3", width = 0),
                                             depth_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "depth_temp21", width = 0),
                                             tdo1 = calc_roc(data = Greenwood.Thresholds, metric = "tdo1", width = 0),
                                             layer_thickness_do1_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do1_temp25", width = 0),
                                             depth_do1 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do1", width = 0),
                                             depth_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "depth_temp25", width = 0),
                                             max_temp = calc_roc(data = Greenwood.Thresholds, metric = "max_temp", width = 0),
                                             min_do = calc_roc(data = Greenwood.Thresholds, metric = "min_do", width = 0),
                                             volumetric_habitat_availability_do6_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", width = 0),
                                             volumetric_habitat_availability_do3_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", width = 0),
                                             volumetric_habitat_availability_do1_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", width = 0))

Greenwood.Thresholds.Daily.Averages.ROCs <- data.frame(year = word(Greenwood.Thresholds.Daily.Averages$date[1:(nrow(Greenwood.Thresholds.Daily.Averages)-1)], 1, 1, sep = "-"),
                                                       month = word(Greenwood.Thresholds.Daily.Averages$date[1:(nrow(Greenwood.Thresholds.Daily.Averages)-1)], 2, 2, sep = "-"),
                                                       dom = word(word(Greenwood.Thresholds.Daily.Averages$date[1:(nrow(Greenwood.Thresholds.Daily.Averages)-1)], 3, 3, sep = "-"), 1, 1, sep = " "),
                                                       doy = yday(Greenwood.Thresholds.Daily.Averages$date[1:(nrow(Greenwood.Thresholds.Daily.Averages)-1)]),
                                                       week = week(Greenwood.Thresholds.Daily.Averages$date[1:(nrow(Greenwood.Thresholds.Daily.Averages)-1)]),
                                                       tdo6 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "tdo6", width = 0),
                                                       layer_thickness_do6_temp17 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "layer_thickness_do6_temp17", width = 0),
                                                       depth_do6 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "depth_do6", width = 0),
                                                       depth_temp17 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "tdo3", width = 0),
                                                       tdo3 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "tdo6", width = 0),
                                                       layer_thickness_do3_temp21 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "layer_thickness_do3_temp21", width = 0),
                                                       depth_do3 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "depth_do3", width = 0),
                                                       depth_temp21 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "depth_temp21", width = 0),
                                                       tdo1 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "tdo1", width = 0),
                                                       layer_thickness_do1_temp25 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "layer_thickness_do1_temp25", width = 0),
                                                       depth_do1 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "depth_do1", width = 0),
                                                       depth_temp25 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "depth_temp25", width = 0),
                                                       max_temp = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "max_temp", width = 0),
                                                       min_do = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "min_do", width = 0),
                                                       volumetric_habitat_availability_do6_temp17 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "volumetric_habitat_availability_do6_temp17", width = 0),
                                                       volumetric_habitat_availability_do3_temp21 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "volumetric_habitat_availability_do3_temp21", width = 0),
                                                       volumetric_habitat_availability_do1_temp25 = calc_roc(data = Greenwood.Thresholds.Daily.Averages, metric = "volumetric_habitat_availability_do1_temp25", width = 0))
Greenwood.Thresholds.Daily.Averages.ROCs <- Greenwood.Thresholds.Daily.Averages.ROCs %>%
  mutate(date_time_local = NA)


Greenwood.Thresholds.ROCs.Roll.Week <- data.frame(date_time_local = Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)],
                                                  date = as.Date(word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 1, 1, sep = " ")),
                                                  year = word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 1, 1, sep = "-"),
                                                  month = word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 2, 2, sep = "-"),
                                                  dom = word(word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 3, 3, sep = "-"), 1, 1, sep = " "),
                                                  doy = yday(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)]),
                                                  week = week(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)]),
                                                  tdo6 = calc_roc(data = Greenwood.Thresholds, metric = "tdo6", width = 84),
                                                  layer_thickness_do6_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do6_temp17", width = 84),
                                                  depth_do6 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do6", width = 84),
                                                  depth_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "tdo3", width = 84),
                                                  tdo3 = calc_roc(data = Greenwood.Thresholds, metric = "tdo6", width = 84),
                                                  layer_thickness_do3_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do3_temp21", width = 84),
                                                  depth_do3 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do3", width = 84),
                                                  depth_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "depth_temp21", width = 84),
                                                  tdo1 = calc_roc(data = Greenwood.Thresholds, metric = "tdo1", width = 84),
                                                  layer_thickness_do1_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do1_temp25", width = 84),
                                                  depth_do1 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do1", width = 84),
                                                  depth_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "depth_temp25", width = 84),
                                                  max_temp = calc_roc(data = Greenwood.Thresholds, metric = "max_temp", width = 84),
                                                  min_do = calc_roc(data = Greenwood.Thresholds, metric = "min_do", width = 84),
                                                  volumetric_habitat_availability_do6_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", width = 84),
                                                  volumetric_habitat_availability_do3_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", width = 84),
                                                  volumetric_habitat_availability_do1_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", width = 84))

Greenwood.Thresholds.ROCs.Roll.2Weeks <- data.frame(date_time_local = Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)],
                                                    date = as.Date(word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 1, 1, sep = " ")),
                                                    year = word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 1, 1, sep = "-"),
                                                    month = word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 2, 2, sep = "-"),
                                                    dom = word(word(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)], 3, 3, sep = "-"), 1, 1, sep = " "),
                                                    doy = yday(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)]),
                                                    week = week(Greenwood.Thresholds$date_time_local[1:(nrow(Greenwood.Thresholds)-1)]),
                                                    tdo6 = calc_roc(data = Greenwood.Thresholds, metric = "tdo6", width = 168),
                                                    layer_thickness_do6_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do6_temp17", width = 168),
                                                    depth_do6 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do6", width = 168),
                                                    depth_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "tdo3", width = 168),
                                                    tdo3 = calc_roc(data = Greenwood.Thresholds, metric = "tdo6", width = 168),
                                                    layer_thickness_do3_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do3_temp21", width = 168),
                                                    depth_do3 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do3", width = 168),
                                                    depth_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "depth_temp21", width = 168),
                                                    tdo1 = calc_roc(data = Greenwood.Thresholds, metric = "tdo1", width = 168),
                                                    layer_thickness_do1_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do1_temp25", width = 168),
                                                    depth_do1 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do1", width = 168),
                                                    depth_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "depth_temp25", width = 168),
                                                    max_temp = calc_roc(data = Greenwood.Thresholds, metric = "max_temp", width = 168),
                                                    min_do = calc_roc(data = Greenwood.Thresholds, metric = "min_do", width = 168),
                                                    volumetric_habitat_availability_do6_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", width = 168),
                                                    volumetric_habitat_availability_do3_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", width = 168),
                                                    volumetric_habitat_availability_do1_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", width = 168))

Greenwood.Thresholds.ROCs.Block.Week <- data.frame(date_time_local = c(rep(NA, length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"])))),
                                                                       rep(NA, length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"])))),
                                                                       rep(NA, length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"])))),
                                                                       rep(NA, length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"])))),
                                                                       rep(NA, length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"])))),
                                                                       rep(NA, length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"])))),
                                                                       rep(NA, length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"]))))),
                                                   year = c(rep("2018", length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"])))),
                                                            rep("2019", length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"])))),
                                                            rep("2020", length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"])))),
                                                            rep("2021", length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"])))),
                                                            rep("2022", length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"])))),
                                                            rep("2023", length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"])))),
                                                            rep("2024", length(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"]))))),
                                                   week = c(unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"])),
                                                            unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"])),
                                                            unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"])),
                                                            unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"])),
                                                            unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"])),
                                                            unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"])),
                                                            unique(week(Greenwood.Thresholds$date_time_local[word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"]))),
                                                  tdo6 = calc_roc(data = Greenwood.Thresholds, metric = "tdo6", width = "week"),
                                                  layer_thickness_do6_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do6_temp17", width = "week"),
                                                  depth_do6 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do6", width = "week"),
                                                  depth_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "tdo3", width = "week"),
                                                  tdo3 = calc_roc(data = Greenwood.Thresholds, metric = "tdo6", width = "week"),
                                                  layer_thickness_do3_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do3_temp21", width = "week"),
                                                  depth_do3 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do3", width = "week"),
                                                  depth_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "depth_temp21", width = "week"),
                                                  tdo1 = calc_roc(data = Greenwood.Thresholds, metric = "tdo1", width = "week"),
                                                  layer_thickness_do1_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do1_temp25", width = "week"),
                                                  depth_do1 = calc_roc(data = Greenwood.Thresholds, metric = "depth_do1", width = "week"),
                                                  depth_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "depth_temp25", width = "week"),
                                                  max_temp = calc_roc(data = Greenwood.Thresholds, metric = "max_temp", width = "week"),
                                                  min_do = calc_roc(data = Greenwood.Thresholds, metric = "min_do", width = "week"),
                                                  volumetric_habitat_availability_do6_temp17 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", width = "week"),
                                                  volumetric_habitat_availability_do3_temp21 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", width = "week"),
                                                  volumetric_habitat_availability_do1_temp25 = calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", width = "week"))
Greenwood.Thresholds.ROCs.Block.Week <- Greenwood.Thresholds.ROCs.Block.Week %>%
  mutate(date = ymd(paste0(year, "0101")) + weeks(week - 1),
         month = month(date),
         dom = word(date, 3, 3, sep = "-"),
         doy = yday(date))

Greenwood.Thresholds.ROCs.Season <- data.frame(year = c("2018", "2019", "2020", "2021", "2022", "2023", "2024"),
                                                   tdo6 = c(calc_roc(data = Greenwood.Thresholds, metric = "tdo6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   layer_thickness_do6_temp17 = c(calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   depth_do6 = c(calc_roc(data = Greenwood.Thresholds, metric = "depth_do6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do6", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   depth_temp17 = c(calc_roc(data = Greenwood.Thresholds, metric = "depth_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   tdo3 = c(calc_roc(data = Greenwood.Thresholds, metric = "tdo3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   layer_thickness_do3_temp21 = c(calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   depth_do3 = c(calc_roc(data = Greenwood.Thresholds, metric = "depth_do3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do3", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   depth_temp21 = c(calc_roc(data = Greenwood.Thresholds, metric = "depth_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   tdo1 = c(calc_roc(data = Greenwood.Thresholds, metric = "tdo1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                            calc_roc(data = Greenwood.Thresholds, metric = "tdo1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   layer_thickness_do1_temp25 = c(calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   depth_do1 = c(calc_roc(data = Greenwood.Thresholds, metric = "depth_do1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                 calc_roc(data = Greenwood.Thresholds, metric = "depth_do1", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   depth_temp25 = c(calc_roc(data = Greenwood.Thresholds, metric = "depth_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                    calc_roc(data = Greenwood.Thresholds, metric = "depth_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   max_temp = c(calc_roc(data = Greenwood.Thresholds, metric = "max_temp", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                calc_roc(data = Greenwood.Thresholds, metric = "max_temp", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                calc_roc(data = Greenwood.Thresholds, metric = "max_temp", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                calc_roc(data = Greenwood.Thresholds, metric = "max_temp", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                calc_roc(data = Greenwood.Thresholds, metric = "max_temp", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                calc_roc(data = Greenwood.Thresholds, metric = "max_temp", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                calc_roc(data = Greenwood.Thresholds, metric = "max_temp", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   min_do = c(calc_roc(data = Greenwood.Thresholds, metric = "min_do", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "min_do_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                              calc_roc(data = Greenwood.Thresholds, metric = "min_do", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "min_do_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                              calc_roc(data = Greenwood.Thresholds, metric = "min_do", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "min_do_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                              calc_roc(data = Greenwood.Thresholds, metric = "min_do", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "min_do_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                              calc_roc(data = Greenwood.Thresholds, metric = "min_do", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "min_do_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                              calc_roc(data = Greenwood.Thresholds, metric = "min_do", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "min_do_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                              calc_roc(data = Greenwood.Thresholds, metric = "min_do", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "min_do_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   volumetric_habitat_availability_do6_temp17 = c(calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),    
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   volumetric_habitat_availability_do3_temp21 = c(calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])),
                                                   volumetric_habitat_availability_do1_temp25 = c(calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2018"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2019"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2020"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2021"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2022"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2023"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2023"]),
                                                                                                  calc_roc(data = Greenwood.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Greenwood.Thresholds$date_time_local[Greenwood.Thresholds$stratified == "yes" & word(Greenwood.Thresholds$date_time_local, 1, 1, sep = "-") == "2024"][1], "%Y-%m-%d %H:%M:%S"), end = Greenwood.Max.Stress.Dates$date[Greenwood.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Greenwood.Max.Stress.Dates$year == "2024"])))

Greenwood.Thresholds <- Greenwood.Thresholds %>%
  mutate(roc_tdo3 = c(Greenwood.Thresholds.ROCs.Inst$tdo3, NA),
         roc_tdo3_avg_weekly = c(Greenwood.Thresholds.ROCs.Roll.Week$tdo3, NA),
         roc_tdo3_avg_biweekly = c(Greenwood.Thresholds.ROCs.Roll.2Weeks$tdo3, NA))

## Find max stress values and dates of occurrence ----

Greenwood.Max.Stress.1 <- Greenwood.Thresholds %>%
  mutate(year = word(date_time_local, 1, 1, sep = "-")) %>%
  filter(word(date_time_local, 2, 2, sep = "-") %in% c("06", "07", "08", "09", "10", "11")) %>%
  group_by(year) %>%
  summarise_at(vars(tdo6,
                    tdo3,
                    tdo1,
                    depth_temp17,
                    depth_temp21,
                    depth_temp25,
                    max_temp,
                    tdo6_avg_weekly,
                    tdo3_avg_weekly,
                    tdo1_avg_weekly,
                    depth_temp17_avg_weekly,
                    depth_temp21_avg_weekly,
                    depth_temp25_avg_weekly,
                    max_temp_avg_weekly,
                    tdo6_avg_biweekly,
                    tdo3_avg_biweekly,
                    tdo1_avg_biweekly,
                    depth_temp17_avg_biweekly,
                    depth_temp21_avg_biweekly,
                    depth_temp25_avg_biweekly,
                    max_temp_avg_biweekly,
                    roc_tdo3,
                    roc_tdo3_avg_weekly,
                    roc_tdo3_avg_biweekly), max, na.rm = T)

Greenwood.Max.Stress.2 <- Greenwood.Thresholds %>%
  mutate(year = word(date_time_local, 1, 1, sep = "-")) %>%
  filter(word(date_time_local, 2, 2, sep = "-") %in% c("06", "07", "08", "09", "10", "11")) %>%
  group_by(year) %>%
  summarise_at(vars(layer_thickness_do6_temp17,
                    relative_layer_thickness_do6_temp17,
                    volumetric_habitat_availability_do6_temp17,
                    relative_volumetric_habitat_availability_do6_temp17,
                    depth_do6,
                    layer_thickness_do3_temp21,
                    relative_layer_thickness_do3_temp21,
                    volumetric_habitat_availability_do3_temp21,
                    relative_volumetric_habitat_availability_do3_temp21,
                    depth_do3,
                    layer_thickness_do1_temp25,
                    relative_layer_thickness_do1_temp25,
                    volumetric_habitat_availability_do1_temp25,
                    relative_volumetric_habitat_availability_do1_temp25,
                    depth_do1,
                    min_do,
                    layer_thickness_do6_temp17_avg_weekly,
                    relative_layer_thickness_do6_temp17_avg_weekly,
                    volumetric_habitat_availability_do6_temp17_avg_weekly,
                    relative_volumetric_habitat_availability_do6_temp17_avg_weekly,
                    depth_do6_avg_weekly,
                    layer_thickness_do3_temp21_avg_weekly,
                    relative_layer_thickness_do3_temp21_avg_weekly,
                    volumetric_habitat_availability_do3_temp21_avg_weekly,
                    relative_volumetric_habitat_availability_do3_temp21_avg_weekly,
                    depth_do3_avg_weekly,
                    layer_thickness_do1_temp25_avg_weekly,
                    relative_layer_thickness_do1_temp25_avg_weekly,
                    volumetric_habitat_availability_do1_temp25_avg_weekly,
                    relative_volumetric_habitat_availability_do1_temp25_avg_weekly,
                    depth_do1_avg_weekly,
                    min_do_avg_weekly,
                    layer_thickness_do6_temp17_avg_biweekly,
                    relative_layer_thickness_do6_temp17_avg_biweekly,
                    volumetric_habitat_availability_do6_temp17_avg_biweekly,
                    relative_volumetric_habitat_availability_do6_temp17_avg_biweekly,
                    depth_do6_avg_biweekly,
                    layer_thickness_do3_temp21_avg_biweekly,
                    relative_layer_thickness_do3_temp21_avg_biweekly,
                    volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    relative_volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    depth_do3_avg_biweekly,
                    layer_thickness_do1_temp25_avg_biweekly,
                    relative_layer_thickness_do3_temp21_avg_biweekly,
                    volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    relative_volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    depth_do1_avg_biweekly,
                    min_do_avg_biweekly), min, na.rm = T)

Greenwood.Max.Stress <- cbind(Greenwood.Max.Stress.1, Greenwood.Max.Stress.2[, -1])
Greenwood.Max.Stress.Dates <- Greenwood.Max.Stress 
Greenwood.Max.Stress.Dates[1:nrow(Greenwood.Max.Stress.Dates), 2:ncol(Greenwood.Max.Stress.Dates)] <- NA

for (j in 2:ncol(Greenwood.Max.Stress.Dates)) {
  var <- names(Greenwood.Max.Stress.Dates)[j]
  for (i in 1:nrow(Greenwood.Max.Stress.Dates)) {
    year <- Greenwood.Max.Stress.Dates$year[i]
    if (var %in% names(Greenwood.Max.Stress.1)) {
      if (max(Greenwood.Thresholds[Greenwood.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Greenwood.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) == 0) {
        Greenwood.Max.Stress.Dates[i,j] <- "NA"
      } else {
        Greenwood.Max.Stress.Dates[i,j] <- format(Greenwood.Thresholds$date_time_local[which(Greenwood.Thresholds[, var] == max(Greenwood.Thresholds[Greenwood.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Greenwood.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) & Greenwood.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Greenwood.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"))], "%Y-%m-%d %H:%M:%S")[1]
      }
    } else {
      if (min(Greenwood.Thresholds[Greenwood.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Greenwood.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) == greenwoodMaxDepth) {
        Greenwood.Max.Stress.Dates[i,j] <- "NA"
      } else {
        Greenwood.Max.Stress.Dates[i,j] <- format(Greenwood.Thresholds$date_time_local[which(Greenwood.Thresholds[, var] == min(Greenwood.Thresholds[Greenwood.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Greenwood.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) & Greenwood.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Greenwood.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"))], "%Y-%m-%d %H:%M:%S")[1]
      }
    }
  }
}

Greenwood.Max.Stress <- Greenwood.Max.Stress %>%
  pivot_longer(cols = c(tdo6:min_do_avg_biweekly),
               names_to = "metric",
               values_to = "value") %>%
  mutate(lake = "Greenwood")

Greenwood.Max.Stress.Dates <- Greenwood.Max.Stress.Dates %>%
  pivot_longer(cols = c(tdo6:min_do_avg_biweekly),
               names_to = "metric",
               values_to = "date") %>%
  mutate(lake = "Greenwood")

Greenwood.Max.Stress <- Greenwood.Max.Stress.Dates %>%
  left_join(Greenwood.Max.Stress, join_by(lake, year, metric)) %>%
  select(lake, year, metric, date, value)

Greenwood.Max.Stress <- Greenwood.Max.Stress %>%
  mutate(date = case_when(date == "NA" ~ NA,
                          .default = date),
         date = as.POSIXct(date))

Greenwood.Max.Stress <- Greenwood.Max.Stress %>%
  mutate(doy = yday(date),
         dom = mday(date),
         month = month(date),
         week = week(date))

rm(Greenwood.Max.Stress.1, Greenwood.Max.Stress.2)




# Carlos ----

## Read in data ----
Carlos <- Sentinel.Lakes.Data %>%
  filter(lake_name == "Carlos",
         flaggross_do == "P",
         flagspike_do == "P",
         flagroc_do == "P",
         flagflat_do == "P",
         flagvis_do == "P",
         flaggross_temp == "P",
         flagspike_temp == "P",
         flagroc_temp == "P",
         flagflat_temp == "P",
         flagvis_temp == "P",
         date_time_local <= "2020-06-30 18:00:00") %>% # there isn't any data we can use after this time
  select(lake_name,
         depth_m,
         date_time_local,
         do_mgl,
         temp_c) %>%
  arrange(date_time_local, depth_m) %>%
  collect()

carlosMaxDepth <- 49

## Bring in hypso data ----

carlosDOW <- "21005700"
carlosNHDHR <- Xwalk$site_id[Xwalk$MNDOW_ID == paste("mndow", carlosDOW, sep = "_")]
Carlos.Hypso <- Hypso %>%
  filter(site_id == carlosNHDHR)

## Calculate metrics for different oxythermal thresholds ----

### 6 mg/L DO and 17 ºC ----
Carlos.Stats.1a <- Carlos %>%
  group_by(date_time_local) %>%
  summarise(tdo6 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = carlosMaxDepth)$tdox,
            layer_thickness_do6_temp17 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = carlosMaxDepth)$layer_thickness,
            depth_do6 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = carlosMaxDepth)$depth_o2_threshold,
            depth_temp17 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = carlosMaxDepth)$depth_temp_threshold,
            .groups = "drop")

### 3 mg/L DO and 21 ºC ----
Carlos.Stats.1b <- Carlos %>%
  group_by(date_time_local) %>%
  summarise(tdo3 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = carlosMaxDepth)$tdox,
            layer_thickness_do3_temp21 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = carlosMaxDepth)$layer_thickness,
            depth_do3 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = carlosMaxDepth)$depth_o2_threshold,
            depth_temp21 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = carlosMaxDepth)$depth_temp_threshold,
            .groups = "drop")

### 1 mg/L and 25 ----
Carlos.Stats.1c <- Carlos %>%
  group_by(date_time_local) %>%
  summarise(tdo1 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = carlosMaxDepth)$tdox,
            layer_thickness_do1_temp25 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = carlosMaxDepth)$layer_thickness,
            depth_do1 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = carlosMaxDepth)$depth_o2_threshold,
            depth_temp25 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = carlosMaxDepth)$depth_temp_threshold,
            max_temp = max(temp_c),
            min_do = min(do_mgl),
            schmidt_stability = schmidt.stability(wtr = temp_c,
                                                  depths = depth_m,
                                                  bthA = Carlos.Hypso$areas,
                                                  bthD = Carlos.Hypso$depth)[, 1],
            .groups = "drop")

Carlos.CLT <- Carlos %>%
  group_by(date_time_local) %>%
  summarise(CLT = calc_oxythermal_metrics(pick(everything()), temp_threshold = 22.8, do_threshold = 6, max_depth_m = carlosMaxDepth)$layer_thickness,
            depth_temp22.8 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 22.8, do_threshold = 6, max_depth_m = carlosMaxDepth)$depth_temp_threshold,
            .groups = "drop")

### Combine all thresholds into one data frame ----
Carlos.Thresholds <- Carlos.Stats.1a %>%
  left_join(y = Carlos.Stats.1b, by = join_by(date_time_local)) %>%
  left_join(y = Carlos.Stats.1c, by = join_by(date_time_local)) %>%
  mutate(stratified = case_when((word(date_time_local, 2, 2, sep = "-") %in% c("10", "11") & depth_do6 < carlosMaxDepth) |
                                  schmidt_stability > 20 ~ "yes",
                                .default = "no"))
Carlos.Thresholds <- Carlos.Thresholds %>%
  left_join(y = Carlos.CLT, by = join_by(date_time_local))

# can remove the subsets now that we have them all in one
rm(Carlos.Stats.1a, Carlos.Stats.1b, Carlos.Stats.1c)

## Calculate volumetric habitat availability ----

Carlos.Thresholds$volumetric_habitat_availability_do6_temp17 <- NA
carlosTotalVolume <- calc_volumetric_habitat(max_depth = carlosMaxDepth, hypso_data = Carlos.Hypso, temp_threshold_depth = 0, do_threshold_depth = carlosMaxDepth)
for (i in 1:nrow(Carlos.Thresholds)) {
  Carlos.Thresholds$volumetric_habitat_availability_do6_temp17[i] <- calc_volumetric_habitat(do_threshold_depth = Carlos.Thresholds$depth_do6[i],
                                                                                                temp_threshold_depth = Carlos.Thresholds$depth_temp17[i],
                                                                                                hypso_data = Carlos.Hypso,
                                                                                                carlosMaxDepth)
}
Carlos.Thresholds$volumetric_habitat_availability_do3_temp21 <- NA
for (i in 1:nrow(Carlos.Thresholds)) {
  Carlos.Thresholds$volumetric_habitat_availability_do3_temp21[i] <- calc_volumetric_habitat(do_threshold_depth = Carlos.Thresholds$depth_do3[i],
                                                                                                temp_threshold_depth = Carlos.Thresholds$depth_temp21[i],
                                                                                                hypso_data = Carlos.Hypso,
                                                                                                carlosMaxDepth)
}
Carlos.Thresholds$volumetric_habitat_availability_do1_temp25 <- NA
for (i in 1:nrow(Carlos.Thresholds)) {
  Carlos.Thresholds$volumetric_habitat_availability_do1_temp25[i] <- calc_volumetric_habitat(do_threshold_depth = Carlos.Thresholds$depth_do1[i],
                                                                                                temp_threshold_depth = Carlos.Thresholds$depth_temp25[i],
                                                                                                hypso_data = Carlos.Hypso,
                                                                                                carlosMaxDepth)
}
Carlos.Thresholds$volumetric_habitat_CLT <- NA
for (i in 1:nrow(Carlos.Thresholds)) {
  Carlos.Thresholds$volumetric_habitat_CLT[i] <- calc_volumetric_habitat(do_threshold_depth = Carlos.Thresholds$depth_do6[i],
                                                                            temp_threshold_depth = Carlos.Thresholds$depth_temp22.8[i],
                                                                            hypso_data = Carlos.Hypso,
                                                                            carlosMaxDepth)
}

## Relative habitat availability ----

Carlos.Thresholds <- Carlos.Thresholds %>%
  mutate(relative_layer_thickness_do6_temp17 = layer_thickness_do6_temp17 / carlosMaxDepth,
         relative_layer_thickness_do3_temp21 = layer_thickness_do3_temp21 / carlosMaxDepth,
         relative_layer_thickness_do1_temp25 = layer_thickness_do1_temp25 / carlosMaxDepth,
         relative_volumetric_habitat_availability_do6_temp17 = volumetric_habitat_availability_do6_temp17 / carlosTotalVolume,
         relative_volumetric_habitat_availability_do3_temp21 = volumetric_habitat_availability_do3_temp21 / carlosTotalVolume,
         relative_volumetric_habitat_availability_do1_temp25 = volumetric_habitat_availability_do1_temp25 / carlosTotalVolume)

## Interpolate temp profile ----

Carlos.Temp.Interpolated <- Carlos %>%
  group_by(date_time_local) %>%
  group_modify(~ interpolate_temp_profile(data = .x, all_depths = seq(0, carlosMaxDepth, by = 0.1))) %>%
  ungroup()

## Cumulative stress ----

### Calculate daily averages ----

Carlos.Thresholds.Daily.Averages <- Carlos.Thresholds %>%
  mutate(date = as.Date(word(date_time_local, 1, 1, sep = " "))) %>%
  group_by(date) %>%
  select(-date_time_local) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  mutate(gdd_tdo6 = NA,
         gdd_tdo3 = NA,
         gdd_tdo1 = NA,
         gdd_tdo6_cumulative = NA,
         gdd_tdo3_cumulative = NA,
         gdd_tdo1_cumulative = NA,
         stratified = case_when((word(date, 2, 2, sep = "-") %in% c("10", "11") & depth_do6 < carlosMaxDepth) |
                                  schmidt_stability > 20
                                ~ "yes",
                                .default = "no"))

### Calculate growing degree days ----

for (i in 1:nrow(Carlos.Thresholds.Daily.Averages)) {
  Carlos.Thresholds.Daily.Averages$gdd_tdo6[i] <- if (Carlos.Thresholds.Daily.Averages$tdo6[i] < 11) {0} else {Carlos.Thresholds.Daily.Averages$tdo6[i] - 11}
  Carlos.Thresholds.Daily.Averages$gdd_tdo3[i] <- if (Carlos.Thresholds.Daily.Averages$tdo3[i] < 21.5) {0} else {Carlos.Thresholds.Daily.Averages$tdo3[i] - 10}
  Carlos.Thresholds.Daily.Averages$gdd_tdo1[i] <- if (Carlos.Thresholds.Daily.Averages$tdo1[i] < 19) {0} else {Carlos.Thresholds.Daily.Averages$tdo1[i] - 7}
}

### Calculate cumulative growing degree days ----

Carlos.Thresholds.Daily.Averages$gdd_tdo6_cumulative[Carlos.Thresholds.Daily.Averages$stratified == "no"] <- 0
Carlos.Thresholds.Daily.Averages$gdd_tdo3_cumulative[Carlos.Thresholds.Daily.Averages$stratified == "no"] <- 0
Carlos.Thresholds.Daily.Averages$gdd_tdo1_cumulative[Carlos.Thresholds.Daily.Averages$stratified == "no"] <- 0
for (i in which(word(Carlos.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2018" & Carlos.Thresholds.Daily.Averages$stratified == "yes")) {
  Carlos.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Carlos.Thresholds.Daily.Averages$gdd_tdo6[1:i])
  Carlos.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Carlos.Thresholds.Daily.Averages$gdd_tdo3[1:i])
  Carlos.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Carlos.Thresholds.Daily.Averages$gdd_tdo1[1:i])
}
for (i in which(word(Carlos.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2019" & Carlos.Thresholds.Daily.Averages$stratified == "yes")) {
  Carlos.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Carlos.Thresholds.Daily.Averages$gdd_tdo6[which(Carlos.Thresholds.Daily.Averages$date == "2019-06-01"):i])
  Carlos.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Carlos.Thresholds.Daily.Averages$gdd_tdo3[which(Carlos.Thresholds.Daily.Averages$date == "2019-06-01"):i])
  Carlos.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Carlos.Thresholds.Daily.Averages$gdd_tdo1[which(Carlos.Thresholds.Daily.Averages$date == "2019-06-01"):i])
}
for (i in which(word(Carlos.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2020" & Carlos.Thresholds.Daily.Averages$stratified == "yes")) {
  Carlos.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Carlos.Thresholds.Daily.Averages$gdd_tdo6[which(Carlos.Thresholds.Daily.Averages$date == "2020-06-01"):i])
  Carlos.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Carlos.Thresholds.Daily.Averages$gdd_tdo3[which(Carlos.Thresholds.Daily.Averages$date == "2020-06-01"):i])
  Carlos.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Carlos.Thresholds.Daily.Averages$gdd_tdo1[which(Carlos.Thresholds.Daily.Averages$date == "2020-06-01"):i])
}

## Calculate average values within weekly and biweekly windows ----

Carlos.Thresholds <- Carlos.Thresholds %>%
  mutate(tdo6_avg_weekly = smooth_metric(data = Carlos.Thresholds$tdo6, width = 84),
         tdo3_avg_weekly = smooth_metric(data = Carlos.Thresholds$tdo3, width = 84),
         tdo1_avg_weekly = smooth_metric(data = Carlos.Thresholds$tdo1, width = 84),
         layer_thickness_do6_temp17_avg_weekly = smooth_metric(data = Carlos.Thresholds$layer_thickness_do6_temp17, width = 84),
         layer_thickness_do3_temp21_avg_weekly = smooth_metric(data = Carlos.Thresholds$layer_thickness_do3_temp21, width = 84),
         layer_thickness_do1_temp25_avg_weekly = smooth_metric(data = Carlos.Thresholds$layer_thickness_do1_temp25, width = 84),
         relative_layer_thickness_do6_temp17_avg_weekly =  smooth_metric(data = Carlos.Thresholds$relative_layer_thickness_do6_temp17, width = 84),
         relative_layer_thickness_do3_temp21_avg_weekly =  smooth_metric(data = Carlos.Thresholds$relative_layer_thickness_do3_temp21, width = 84),
         relative_layer_thickness_do1_temp25_avg_weekly =  smooth_metric(data = Carlos.Thresholds$relative_layer_thickness_do1_temp25, width = 84),
         depth_do6_avg_weekly = smooth_metric(data = Carlos.Thresholds$depth_do6, width = 84),
         depth_do3_avg_weekly = smooth_metric(data = Carlos.Thresholds$depth_do3, width = 84),
         depth_do1_avg_weekly = smooth_metric(data = Carlos.Thresholds$depth_do1, width = 84),
         depth_temp17_avg_weekly = smooth_metric(data = Carlos.Thresholds$depth_temp17, width = 84),
         depth_temp21_avg_weekly = smooth_metric(data = Carlos.Thresholds$depth_temp21, width = 84),
         depth_temp25_avg_weekly = smooth_metric(data = Carlos.Thresholds$depth_temp25, width = 84),
         max_temp_avg_weekly = smooth_metric(data = Carlos.Thresholds$max_temp, width = 84),
         min_do_avg_weekly = smooth_metric(data = Carlos.Thresholds$min_do, width = 84),
         volumetric_habitat_availability_do6_temp17_avg_weekly = smooth_metric(data = Carlos.Thresholds$volumetric_habitat_availability_do6_temp17, width = 84),
         volumetric_habitat_availability_do3_temp21_avg_weekly = smooth_metric(data = Carlos.Thresholds$volumetric_habitat_availability_do3_temp21, width = 84),
         volumetric_habitat_availability_do1_temp25_avg_weekly = smooth_metric(data = Carlos.Thresholds$volumetric_habitat_availability_do1_temp25, width = 84),
         relative_volumetric_habitat_availability_do6_temp17_avg_weekly = smooth_metric(data = Carlos.Thresholds$relative_volumetric_habitat_availability_do6_temp17, width = 84),
         relative_volumetric_habitat_availability_do3_temp21_avg_weekly = smooth_metric(data = Carlos.Thresholds$relative_volumetric_habitat_availability_do3_temp21, width = 84),
         relative_volumetric_habitat_availability_do1_temp25_avg_weekly = smooth_metric(data = Carlos.Thresholds$relative_volumetric_habitat_availability_do1_temp25, width = 84),
         tdo6_avg_biweekly = smooth_metric(data = Carlos.Thresholds$tdo6, width = 168),
         tdo3_avg_biweekly = smooth_metric(data = Carlos.Thresholds$tdo3, width = 168),
         tdo1_avg_biweekly = smooth_metric(data = Carlos.Thresholds$tdo1, width = 168),
         layer_thickness_do6_temp17_avg_biweekly = smooth_metric(data = Carlos.Thresholds$layer_thickness_do6_temp17, width = 168),
         layer_thickness_do3_temp21_avg_biweekly = smooth_metric(data = Carlos.Thresholds$layer_thickness_do3_temp21, width = 168),
         layer_thickness_do1_temp25_avg_biweekly = smooth_metric(data = Carlos.Thresholds$layer_thickness_do1_temp25, width = 168),
         relative_layer_thickness_do6_temp17_avg_biweekly =  smooth_metric(data = Carlos.Thresholds$relative_layer_thickness_do6_temp17, width = 168),
         relative_layer_thickness_do3_temp21_avg_biweekly =  smooth_metric(data = Carlos.Thresholds$relative_layer_thickness_do3_temp21, width = 168),
         relative_layer_thickness_do1_temp25_avg_biweekly =  smooth_metric(data = Carlos.Thresholds$relative_layer_thickness_do1_temp25, width = 168),
         depth_do6_avg_biweekly = smooth_metric(data = Carlos.Thresholds$depth_do6, width = 168),
         depth_do3_avg_biweekly = smooth_metric(data = Carlos.Thresholds$depth_do3, width = 168),
         depth_do1_avg_biweekly = smooth_metric(data = Carlos.Thresholds$depth_do1, width = 168),
         depth_temp17_avg_biweekly = smooth_metric(data = Carlos.Thresholds$depth_temp17, width = 168),
         depth_temp21_avg_biweekly = smooth_metric(data = Carlos.Thresholds$depth_temp21, width = 168),
         depth_temp25_avg_biweekly = smooth_metric(data = Carlos.Thresholds$depth_temp25, width = 168),
         max_temp_avg_biweekly = smooth_metric(data = Carlos.Thresholds$max_temp, width = 168),
         min_do_avg_biweekly = smooth_metric(data = Carlos.Thresholds$min_do, width = 168),
         volumetric_habitat_availability_do6_temp17_avg_biweekly = smooth_metric(data = Carlos.Thresholds$volumetric_habitat_availability_do6_temp17, width = 168),
         volumetric_habitat_availability_do3_temp21_avg_biweekly = smooth_metric(data = Carlos.Thresholds$volumetric_habitat_availability_do3_temp21, width = 168),
         volumetric_habitat_availability_do1_temp25_avg_biweekly = smooth_metric(data = Carlos.Thresholds$volumetric_habitat_availability_do1_temp25, width = 168),
         relative_volumetric_habitat_availability_do6_temp17_avg_biweekly = smooth_metric(data = Carlos.Thresholds$relative_volumetric_habitat_availability_do6_temp17, width = 168),
         relative_volumetric_habitat_availability_do3_temp21_avg_biweekly = smooth_metric(data = Carlos.Thresholds$relative_volumetric_habitat_availability_do3_temp21, width = 168),
         relative_volumetric_habitat_availability_do1_temp25_avg_biweekly = smooth_metric(data = Carlos.Thresholds$relative_volumetric_habitat_availability_do1_temp25, width = 168))

Carlos.Thresholds <- Carlos.Thresholds %>%
  mutate(volumetric_habitat_CLT_avg_biweekly = smooth_metric(data = Carlos.Thresholds$volumetric_habitat_CLT, width = 168),
         CLT_avg_biweekly = smooth_metric(data = Carlos.Thresholds$CLT, width = 168))


## Find max stress values and dates of occurrence ----

Carlos.Max.Stress.1 <- Carlos.Thresholds %>%
  mutate(year = word(date_time_local, 1, 1, sep = "-")) %>%
  filter(word(date_time_local, 2, 2, sep = "-") %in% c("06", "07", "08", "09", "10", "11")) %>%
  group_by(year) %>%
  summarise_at(vars(tdo6,
                    tdo3,
                    tdo1,
                    depth_temp17,
                    depth_temp21,
                    depth_temp25,
                    max_temp,
                    tdo6_avg_weekly,
                    tdo3_avg_weekly,
                    tdo1_avg_weekly,
                    depth_temp17_avg_weekly,
                    depth_temp21_avg_weekly,
                    depth_temp25_avg_weekly,
                    max_temp_avg_weekly,
                    tdo6_avg_biweekly,
                    tdo3_avg_biweekly,
                    tdo1_avg_biweekly,
                    depth_temp17_avg_biweekly,
                    depth_temp21_avg_biweekly,
                    depth_temp25_avg_biweekly,
                    max_temp_avg_biweekly), max, na.rm = T)

Carlos.Max.Stress.2 <- Carlos.Thresholds %>%
  mutate(year = word(date_time_local, 1, 1, sep = "-")) %>%
  filter(word(date_time_local, 2, 2, sep = "-") %in% c("06", "07", "08", "09", "10", "11")) %>%
  group_by(year) %>%
  summarise_at(vars(layer_thickness_do6_temp17,
                    relative_layer_thickness_do6_temp17,
                    volumetric_habitat_availability_do6_temp17,
                    relative_volumetric_habitat_availability_do6_temp17,
                    depth_do6,
                    layer_thickness_do3_temp21,
                    relative_layer_thickness_do3_temp21,
                    volumetric_habitat_availability_do3_temp21,
                    relative_volumetric_habitat_availability_do3_temp21,
                    depth_do3,
                    layer_thickness_do1_temp25,
                    relative_layer_thickness_do1_temp25,
                    volumetric_habitat_availability_do1_temp25,
                    relative_volumetric_habitat_availability_do1_temp25,
                    depth_do1,
                    min_do,
                    layer_thickness_do6_temp17_avg_weekly,
                    relative_layer_thickness_do6_temp17_avg_weekly,
                    volumetric_habitat_availability_do6_temp17_avg_weekly,
                    relative_volumetric_habitat_availability_do6_temp17_avg_weekly,
                    depth_do6_avg_weekly,
                    layer_thickness_do3_temp21_avg_weekly,
                    relative_layer_thickness_do3_temp21_avg_weekly,
                    volumetric_habitat_availability_do3_temp21_avg_weekly,
                    relative_volumetric_habitat_availability_do3_temp21_avg_weekly,
                    depth_do3_avg_weekly,
                    layer_thickness_do1_temp25_avg_weekly,
                    relative_layer_thickness_do1_temp25_avg_weekly,
                    volumetric_habitat_availability_do1_temp25_avg_weekly,
                    relative_volumetric_habitat_availability_do1_temp25_avg_weekly,
                    depth_do1_avg_weekly,
                    min_do_avg_weekly,
                    layer_thickness_do6_temp17_avg_biweekly,
                    relative_layer_thickness_do6_temp17_avg_biweekly,
                    volumetric_habitat_availability_do6_temp17_avg_biweekly,
                    relative_volumetric_habitat_availability_do6_temp17_avg_biweekly,
                    depth_do6_avg_biweekly,
                    layer_thickness_do3_temp21_avg_biweekly,
                    relative_layer_thickness_do3_temp21_avg_biweekly,
                    volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    relative_volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    depth_do3_avg_biweekly,
                    layer_thickness_do1_temp25_avg_biweekly,
                    relative_layer_thickness_do3_temp21_avg_biweekly,
                    volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    relative_volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    depth_do1_avg_biweekly,
                    min_do_avg_biweekly), min, na.rm = T)

Carlos.Max.Stress <- cbind(Carlos.Max.Stress.1, Carlos.Max.Stress.2[, -1])
Carlos.Max.Stress.Dates <- Carlos.Max.Stress 
Carlos.Max.Stress.Dates[1:nrow(Carlos.Max.Stress.Dates), 2:ncol(Carlos.Max.Stress.Dates)] <- NA

for (j in 2:ncol(Carlos.Max.Stress.Dates)) {
  var <- names(Carlos.Max.Stress.Dates)[j]
  for (i in 1:nrow(Carlos.Max.Stress.Dates)) {
    year <- Carlos.Max.Stress.Dates$year[i]
    if (var %in% names(Carlos.Max.Stress.1)) {
      if (max(Carlos.Thresholds[Carlos.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Carlos.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) == 0) {
        Carlos.Max.Stress.Dates[i,j] <- "NA"
      } else {
        Carlos.Max.Stress.Dates[i,j] <- format(Carlos.Thresholds$date_time_local[which(Carlos.Thresholds[, var] == max(Carlos.Thresholds[Carlos.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Carlos.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) & Carlos.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Carlos.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"))], "%Y-%m-%d %H:%M:%S")[1]
      }
    } else {
      if (min(Carlos.Thresholds[Carlos.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Carlos.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) == carlosMaxDepth) {
        Carlos.Max.Stress.Dates[i,j] <- "NA"
      } else {
        Carlos.Max.Stress.Dates[i,j] <- format(Carlos.Thresholds$date_time_local[which(Carlos.Thresholds[, var] == min(Carlos.Thresholds[Carlos.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Carlos.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) & Carlos.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Carlos.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"))], "%Y-%m-%d %H:%M:%S")[1]
      }
    }
  }
}

Carlos.Max.Stress <- Carlos.Max.Stress %>%
  pivot_longer(cols = c(tdo6:min_do_avg_biweekly),
               names_to = "metric",
               values_to = "value") %>%
  mutate(lake = "Carlos")

Carlos.Max.Stress.Dates <- Carlos.Max.Stress.Dates %>%
  pivot_longer(cols = c(tdo6:min_do_avg_biweekly),
               names_to = "metric",
               values_to = "date") %>%
  mutate(lake = "Carlos")

Carlos.Max.Stress <- Carlos.Max.Stress.Dates %>%
  left_join(Carlos.Max.Stress, join_by(lake, year, metric)) %>%
  select(lake, year, metric, date, value)

Carlos.Max.Stress <- Carlos.Max.Stress %>%
  mutate(date = case_when(date == "NA" ~ NA,
                          .default = date),
         date = as.POSIXct(date))

Carlos.Max.Stress <- Carlos.Max.Stress %>%
  mutate(doy = yday(date),
         dom = mday(date),
         month = month(date),
         week = week(date))

rm(Carlos.Max.Stress.1, Carlos.Max.Stress.2)

## Calculate rates of change in metrics over different time scales ----

Carlos.Thresholds.ROCs.Inst <- data.frame(date_time_local = Carlos.Thresholds$date_time_local[1:(nrow(Carlos.Thresholds)-1)],
                                             tdo6 = calc_roc(data = Carlos.Thresholds, metric = "tdo6", width = 0),
                                             layer_thickness_do6_temp17 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do6_temp17", width = 0),
                                             depth_do6 = calc_roc(data = Carlos.Thresholds, metric = "depth_do6", width = 0),
                                             depth_temp17 = calc_roc(data = Carlos.Thresholds, metric = "tdo3", width = 0),
                                             tdo3 = calc_roc(data = Carlos.Thresholds, metric = "tdo6", width = 0),
                                             layer_thickness_do3_temp21 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do3_temp21", width = 0),
                                             depth_do3 = calc_roc(data = Carlos.Thresholds, metric = "depth_do3", width = 0),
                                             depth_temp21 = calc_roc(data = Carlos.Thresholds, metric = "depth_temp21", width = 0),
                                             tdo1 = calc_roc(data = Carlos.Thresholds, metric = "tdo1", width = 0),
                                             layer_thickness_do1_temp25 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do1_temp25", width = 0),
                                             depth_do1 = calc_roc(data = Carlos.Thresholds, metric = "depth_do1", width = 0),
                                             depth_temp25 = calc_roc(data = Carlos.Thresholds, metric = "depth_temp25", width = 0),
                                             max_temp = calc_roc(data = Carlos.Thresholds, metric = "max_temp", width = 0),
                                             min_do = calc_roc(data = Carlos.Thresholds, metric = "min_do", width = 0),
                                             volumetric_habitat_availability_do6_temp17 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", width = 0),
                                             volumetric_habitat_availability_do3_temp21 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", width = 0),
                                             volumetric_habitat_availability_do1_temp25 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", width = 0))

Carlos.Thresholds.Daily.Averages.ROCs <- data.frame(date = Carlos.Thresholds.Daily.Averages$date[1:(nrow(Carlos.Thresholds.Daily.Averages)-1)],
                                                       tdo6 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "tdo6", width = 0),
                                                       layer_thickness_do6_temp17 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "layer_thickness_do6_temp17", width = 0),
                                                       depth_do6 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "depth_do6", width = 0),
                                                       depth_temp17 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "tdo3", width = 0),
                                                       tdo3 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "tdo6", width = 0),
                                                       layer_thickness_do3_temp21 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "layer_thickness_do3_temp21", width = 0),
                                                       depth_do3 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "depth_do3", width = 0),
                                                       depth_temp21 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "depth_temp21", width = 0),
                                                       tdo1 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "tdo1", width = 0),
                                                       layer_thickness_do1_temp25 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "layer_thickness_do1_temp25", width = 0),
                                                       depth_do1 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "depth_do1", width = 0),
                                                       depth_temp25 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "depth_temp25", width = 0),
                                                       max_temp = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "max_temp", width = 0),
                                                       min_do = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "min_do", width = 0),
                                                       volumetric_habitat_availability_do6_temp17 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "volumetric_habitat_availability_do6_temp17", width = 0),
                                                       volumetric_habitat_availability_do3_temp21 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "volumetric_habitat_availability_do3_temp21", width = 0),
                                                       volumetric_habitat_availability_do1_temp25 = calc_roc(data = Carlos.Thresholds.Daily.Averages, metric = "volumetric_habitat_availability_do1_temp25", width = 0))

Carlos.Thresholds.ROCs.Roll.Week <- data.frame(date_time_local = Carlos.Thresholds$date_time_local[1:(nrow(Carlos.Thresholds)-1)],
                                                  tdo6 = calc_roc(data = Carlos.Thresholds, metric = "tdo6", width = 84),
                                                  layer_thickness_do6_temp17 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do6_temp17", width = 84),
                                                  depth_do6 = calc_roc(data = Carlos.Thresholds, metric = "depth_do6", width = 84),
                                                  depth_temp17 = calc_roc(data = Carlos.Thresholds, metric = "tdo3", width = 84),
                                                  tdo3 = calc_roc(data = Carlos.Thresholds, metric = "tdo6", width = 84),
                                                  layer_thickness_do3_temp21 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do3_temp21", width = 84),
                                                  depth_do3 = calc_roc(data = Carlos.Thresholds, metric = "depth_do3", width = 84),
                                                  depth_temp21 = calc_roc(data = Carlos.Thresholds, metric = "depth_temp21", width = 84),
                                                  tdo1 = calc_roc(data = Carlos.Thresholds, metric = "tdo1", width = 84),
                                                  layer_thickness_do1_temp25 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do1_temp25", width = 84),
                                                  depth_do1 = calc_roc(data = Carlos.Thresholds, metric = "depth_do1", width = 84),
                                                  depth_temp25 = calc_roc(data = Carlos.Thresholds, metric = "depth_temp25", width = 84),
                                                  max_temp = calc_roc(data = Carlos.Thresholds, metric = "max_temp", width = 84),
                                                  min_do = calc_roc(data = Carlos.Thresholds, metric = "min_do", width = 84),
                                                  volumetric_habitat_availability_do6_temp17 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", width = 84),
                                                  volumetric_habitat_availability_do3_temp21 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", width = 84),
                                                  volumetric_habitat_availability_do1_temp25 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", width = 84))

Carlos.Thresholds.ROCs.Roll.2Weeks <- data.frame(date_time_local = Carlos.Thresholds$date_time_local[1:(nrow(Carlos.Thresholds)-1)],
                                                    tdo6 = calc_roc(data = Carlos.Thresholds, metric = "tdo6", width = 168),
                                                    layer_thickness_do6_temp17 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do6_temp17", width = 168),
                                                    depth_do6 = calc_roc(data = Carlos.Thresholds, metric = "depth_do6", width = 168),
                                                    depth_temp17 = calc_roc(data = Carlos.Thresholds, metric = "tdo3", width = 168),
                                                    tdo3 = calc_roc(data = Carlos.Thresholds, metric = "tdo6", width = 168),
                                                    layer_thickness_do3_temp21 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do3_temp21", width = 168),
                                                    depth_do3 = calc_roc(data = Carlos.Thresholds, metric = "depth_do3", width = 168),
                                                    depth_temp21 = calc_roc(data = Carlos.Thresholds, metric = "depth_temp21", width = 168),
                                                    tdo1 = calc_roc(data = Carlos.Thresholds, metric = "tdo1", width = 168),
                                                    layer_thickness_do1_temp25 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do1_temp25", width = 168),
                                                    depth_do1 = calc_roc(data = Carlos.Thresholds, metric = "depth_do1", width = 168),
                                                    depth_temp25 = calc_roc(data = Carlos.Thresholds, metric = "depth_temp25", width = 168),
                                                    max_temp = calc_roc(data = Carlos.Thresholds, metric = "max_temp", width = 168),
                                                    min_do = calc_roc(data = Carlos.Thresholds, metric = "min_do", width = 168),
                                                    volumetric_habitat_availability_do6_temp17 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", width = 168),
                                                    volumetric_habitat_availability_do3_temp21 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", width = 168),
                                                    volumetric_habitat_availability_do1_temp25 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", width = 168))

Carlos.Thresholds.ROCs.Block.Week <- data.frame(year = c(rep("2018", length(unique(week(Carlos.Thresholds$date_time_local[word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"])))),
                                                            rep("2019", length(unique(week(Carlos.Thresholds$date_time_local[word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"])))),
                                                            rep("2020", length(unique(week(Carlos.Thresholds$date_time_local[word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"]))))),
                                                   week = c(unique(week(Carlos.Thresholds$date_time_local[word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"])),
                                                            unique(week(Carlos.Thresholds$date_time_local[word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"])),
                                                            unique(week(Carlos.Thresholds$date_time_local[word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"]))),
                                                   tdo6 = calc_roc(data = Carlos.Thresholds, metric = "tdo6", width = "week"),
                                                   layer_thickness_do6_temp17 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do6_temp17", width = "week"),
                                                   depth_do6 = calc_roc(data = Carlos.Thresholds, metric = "depth_do6", width = "week"),
                                                   depth_temp17 = calc_roc(data = Carlos.Thresholds, metric = "tdo3", width = "week"),
                                                   tdo3 = calc_roc(data = Carlos.Thresholds, metric = "tdo6", width = "week"),
                                                   layer_thickness_do3_temp21 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do3_temp21", width = "week"),
                                                   depth_do3 = calc_roc(data = Carlos.Thresholds, metric = "depth_do3", width = "week"),
                                                   depth_temp21 = calc_roc(data = Carlos.Thresholds, metric = "depth_temp21", width = "week"),
                                                   tdo1 = calc_roc(data = Carlos.Thresholds, metric = "tdo1", width = "week"),
                                                   layer_thickness_do1_temp25 = calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do1_temp25", width = "week"),
                                                   depth_do1 = calc_roc(data = Carlos.Thresholds, metric = "depth_do1", width = "week"),
                                                   depth_temp25 = calc_roc(data = Carlos.Thresholds, metric = "depth_temp25", width = "week"),
                                                   max_temp = calc_roc(data = Carlos.Thresholds, metric = "max_temp", width = "week"),
                                                   min_do = calc_roc(data = Carlos.Thresholds, metric = "min_do", width = "week"),
                                                   volumetric_habitat_availability_do6_temp17 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", width = "week"),
                                                   volumetric_habitat_availability_do3_temp21 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", width = "week"),
                                                   volumetric_habitat_availability_do1_temp25 = calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", width = "week"))
Carlos.Thresholds.ROCs.Block.Week$week_start_date <- ymd(paste0(Carlos.Thresholds.ROCs.Block.Week$year, "0101")) + weeks(Carlos.Thresholds.ROCs.Block.Week$week - 1)


Carlos.Thresholds.ROCs.Season <- data.frame(year = c("2018", "2019", "2020"),
                                               tdo6 = c(calc_roc(data = Carlos.Thresholds, metric = "tdo6", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                        calc_roc(data = Carlos.Thresholds, metric = "tdo6", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                        calc_roc(data = Carlos.Thresholds, metric = "tdo6", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               layer_thickness_do6_temp17 = c(calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                                              calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                                              calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               depth_do6 = c(calc_roc(data = Carlos.Thresholds, metric = "depth_do6", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                             calc_roc(data = Carlos.Thresholds, metric = "depth_do6", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                             calc_roc(data = Carlos.Thresholds, metric = "depth_do6", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               depth_temp17 = c(calc_roc(data = Carlos.Thresholds, metric = "depth_temp17", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                                calc_roc(data = Carlos.Thresholds, metric = "depth_temp17", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                                calc_roc(data = Carlos.Thresholds, metric = "depth_temp17", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               tdo3 = c(calc_roc(data = Carlos.Thresholds, metric = "tdo3", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                        calc_roc(data = Carlos.Thresholds, metric = "tdo3", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                        calc_roc(data = Carlos.Thresholds, metric = "tdo3", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               layer_thickness_do3_temp21 = c(calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                                              calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                                              calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               depth_do3 = c(calc_roc(data = Carlos.Thresholds, metric = "depth_do3", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                             calc_roc(data = Carlos.Thresholds, metric = "depth_do3", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                             calc_roc(data = Carlos.Thresholds, metric = "depth_do3", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               depth_temp21 = c(calc_roc(data = Carlos.Thresholds, metric = "depth_temp21", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                                calc_roc(data = Carlos.Thresholds, metric = "depth_temp21", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                                calc_roc(data = Carlos.Thresholds, metric = "depth_temp21", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               tdo1 = c(calc_roc(data = Carlos.Thresholds, metric = "tdo1", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                        calc_roc(data = Carlos.Thresholds, metric = "tdo1", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                        calc_roc(data = Carlos.Thresholds, metric = "tdo1", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               layer_thickness_do1_temp25 = c(calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                                              calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                                              calc_roc(data = Carlos.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               depth_do1 = c(calc_roc(data = Carlos.Thresholds, metric = "depth_do1", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                             calc_roc(data = Carlos.Thresholds, metric = "depth_do1", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                             calc_roc(data = Carlos.Thresholds, metric = "depth_do1", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               depth_temp25 = c(calc_roc(data = Carlos.Thresholds, metric = "depth_temp25", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                                calc_roc(data = Carlos.Thresholds, metric = "depth_temp25", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                                calc_roc(data = Carlos.Thresholds, metric = "depth_temp25", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               max_temp = c(calc_roc(data = Carlos.Thresholds, metric = "max_temp", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                            calc_roc(data = Carlos.Thresholds, metric = "max_temp", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                            calc_roc(data = Carlos.Thresholds, metric = "max_temp", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               min_do = c(calc_roc(data = Carlos.Thresholds, metric = "min_do", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "min_do_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                          calc_roc(data = Carlos.Thresholds, metric = "min_do", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "min_do_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                          calc_roc(data = Carlos.Thresholds, metric = "min_do", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "min_do_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               volumetric_habitat_availability_do6_temp17 = c(calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                                                              calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                                                              calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               volumetric_habitat_availability_do3_temp21 = c(calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                                                              calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                                                              calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])),
                                               volumetric_habitat_availability_do1_temp25 = c(calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2018"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Carlos.Max.Stress.Dates$year == "2018"]),
                                                                                              calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2019"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Carlos.Max.Stress.Dates$year == "2019"]),
                                                                                              calc_roc(data = Carlos.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Carlos.Thresholds$date_time_local[Carlos.Thresholds$stratified == "yes" & word(Carlos.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Carlos.Max.Stress.Dates$date[Carlos.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Carlos.Max.Stress.Dates$year == "2020"])))

# Ten Mile ----

Tenmile <- Sentinel.Lakes.Data %>%
  filter(lake_name == "Ten Mile",
         flaggross_do == "P",
         flagspike_do == "P",
         flagroc_do == "P",
         flagflat_do == "P",
         flagvis_do == "P",
         flaggross_temp == "P",
         flagspike_temp == "P",
         flagroc_temp == "P",
         flagflat_temp == "P",
         flagvis_temp == "P") %>%
  select(lake_name,
         depth_m,
         date_time_local,
         do_mgl,
         temp_c) %>%
  arrange(date_time_local, depth_m) %>%
  collect()

tenmileMaxDepth <- 63

## Bring in hypso data ----

tenmileDOW <- "11041300"
tenmileNHDHR <- Xwalk$site_id[Xwalk$MNDOW_ID == paste("mndow", tenmileDOW, sep = "_")]
Tenmile.Hypso <- Hypso %>%
  filter(site_id == tenmileNHDHR)

## Calculate metrics for different oxythermal thresholds ----

### 6 mg/L DO and 17 ºC ----
Tenmile.Stats.1a <- Tenmile %>%
  group_by(date_time_local) %>%
  summarise(tdo6 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = tenmileMaxDepth)$tdox,
            layer_thickness_do6_temp17 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = tenmileMaxDepth)$layer_thickness,
            depth_do6 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = tenmileMaxDepth)$depth_o2_threshold,
            depth_temp17 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 17, do_threshold = 6, max_depth_m = tenmileMaxDepth)$depth_temp_threshold,
            .groups = "drop")

### 3 mg/L DO and 21 ºC ----
Tenmile.Stats.1b <- Tenmile %>%
  group_by(date_time_local) %>%
  summarise(tdo3 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = tenmileMaxDepth)$tdox,
            layer_thickness_do3_temp21 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = tenmileMaxDepth)$layer_thickness,
            depth_do3 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = tenmileMaxDepth)$depth_o2_threshold,
            depth_temp21 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 21, do_threshold = 3, max_depth_m = tenmileMaxDepth)$depth_temp_threshold,
            .groups = "drop")

### 1 mg/L and 25 ----
Tenmile.Stats.1c <- Tenmile %>%
  group_by(date_time_local) %>%
  summarise(tdo1 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = tenmileMaxDepth)$tdox,
            layer_thickness_do1_temp25 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = tenmileMaxDepth)$layer_thickness,
            depth_do1 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = tenmileMaxDepth)$depth_o2_threshold,
            depth_temp25 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 25, do_threshold = 1, max_depth_m = tenmileMaxDepth)$depth_temp_threshold,
            max_temp = max(temp_c),
            min_do = min(do_mgl),
            schmidt_stability = schmidt.stability(wtr = temp_c,
                                                  depths = depth_m,
                                                  bthA = Tenmile.Hypso$areas,
                                                  bthD = Tenmile.Hypso$depth)[, 1],
            .groups = "drop")

Tenmile.CLT <- Tenmile %>%
  group_by(date_time_local) %>%
  summarise(CLT = calc_oxythermal_metrics(pick(everything()), temp_threshold = 22.8, do_threshold = 6, max_depth_m = tenmileMaxDepth)$layer_thickness,
            depth_temp22.8 = calc_oxythermal_metrics(pick(everything()), temp_threshold = 22.8, do_threshold = 6, max_depth_m = carlosMaxDepth)$depth_temp_threshold,
            .groups = "drop")

### Combine all thresholds into one data frame ----
Tenmile.Thresholds <- Tenmile.Stats.1a %>%
  left_join(y = Tenmile.Stats.1b, by = join_by(date_time_local)) %>%
  left_join(y = Tenmile.Stats.1c, by = join_by(date_time_local)) %>%
  mutate(stratified = case_when((word(date_time_local, 2, 2, sep = "-") %in% c("10", "11") & depth_do6 < tenmileMaxDepth) |
                                  schmidt_stability > 20 ~ "yes",
                                .default = "no"))    
Tenmile.Thresholds <- Tenmile.Thresholds %>%
  left_join(y = Tenmile.CLT, by = join_by(date_time_local))

# can remove the subsets now that we have them all in one
rm(Tenmile.Stats.1a, Tenmile.Stats.1b, Tenmile.Stats.1c)

## Calculate volumetric habitat availability ----

Tenmile.Thresholds$volumetric_habitat_availability_do6_temp17 <- NA
tenmileTotalVolume <- calc_volumetric_habitat(max_depth = tenmileMaxDepth, hypso_data = Tenmile.Hypso, temp_threshold_depth = 0, do_threshold_depth = tenmileMaxDepth)
for (i in 1:nrow(Tenmile.Thresholds)) {
  Tenmile.Thresholds$volumetric_habitat_availability_do6_temp17[i] <- calc_volumetric_habitat(do_threshold_depth = Tenmile.Thresholds$depth_do6[i],
                                                                                             temp_threshold_depth = Tenmile.Thresholds$depth_temp17[i],
                                                                                             hypso_data = Tenmile.Hypso,
                                                                                             tenmileMaxDepth)
}
Tenmile.Thresholds$volumetric_habitat_availability_do3_temp21 <- NA
for (i in 1:nrow(Tenmile.Thresholds)) {
  Tenmile.Thresholds$volumetric_habitat_availability_do3_temp21[i] <- calc_volumetric_habitat(do_threshold_depth = Tenmile.Thresholds$depth_do3[i],
                                                                                             temp_threshold_depth = Tenmile.Thresholds$depth_temp21[i],
                                                                                             hypso_data = Tenmile.Hypso,
                                                                                             tenmileMaxDepth)
}
Tenmile.Thresholds$volumetric_habitat_availability_do1_temp25 <- NA
for (i in 1:nrow(Tenmile.Thresholds)) {
  Tenmile.Thresholds$volumetric_habitat_availability_do1_temp25[i] <- calc_volumetric_habitat(do_threshold_depth = Tenmile.Thresholds$depth_do1[i],
                                                                                             temp_threshold_depth = Tenmile.Thresholds$depth_temp25[i],
                                                                                             hypso_data = Tenmile.Hypso,
                                                                                             tenmileMaxDepth)
}
Tenmile.Thresholds$volumetric_habitat_CLT <- NA
for (i in 1:nrow(Tenmile.Thresholds)) {
  Tenmile.Thresholds$volumetric_habitat_CLT[i] <- calc_volumetric_habitat(do_threshold_depth = Tenmile.Thresholds$depth_do6[i],
                                                                                              temp_threshold_depth = Tenmile.Thresholds$depth_temp22.8[i],
                                                                                              hypso_data = Tenmile.Hypso,
                                                                                              tenmileMaxDepth)
}

Tenmile.Thresholds <- Tenmile.Thresholds %>%
  mutate(relative_layer_thickness_do6_temp17 = layer_thickness_do6_temp17 / tenmileMaxDepth,
         relative_layer_thickness_do3_temp21 = layer_thickness_do3_temp21 / tenmileMaxDepth,
         relative_layer_thickness_do1_temp25 = layer_thickness_do1_temp25 / tenmileMaxDepth,
         relative_volumetric_habitat_availability_do6_temp17 = volumetric_habitat_availability_do6_temp17 / tenmileTotalVolume,
         relative_volumetric_habitat_availability_do3_temp21 = volumetric_habitat_availability_do3_temp21 / tenmileTotalVolume,
         relative_volumetric_habitat_availability_do1_temp25 = volumetric_habitat_availability_do1_temp25 / tenmileTotalVolume)

## Interpolate temp profile ----

Tenmile.Temp.Interpolated <- Tenmile %>%
  group_by(date_time_local) %>%
  group_modify(~ interpolate_temp_profile(data = .x, all_depths = seq(0, tenmileMaxDepth, by = 0.1))) %>%
  ungroup()

## Cumulative stress ----

### Calculate daily averages ----

Tenmile.Thresholds.Daily.Averages <- Tenmile.Thresholds %>%
  mutate(date = as.Date(word(date_time_local, 1, 1, sep = " "))) %>%
  group_by(date) %>%
  select(-date_time_local) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  mutate(gdd_tdo6 = NA,
         gdd_tdo3 = NA,
         gdd_tdo1 = NA,
         gdd_tdo6_cumulative = NA,
         gdd_tdo3_cumulative = NA,
         gdd_tdo1_cumulative = NA,
         stratified = case_when((word(date, 2, 2, sep = "-") %in% c("10", "11") & depth_do6 < tenmileMaxDepth) |
                                  schmidt_stability > 20
                                ~ "yes",
                                .default = "no"))

### Calculate stressful degree days ----

for (i in 1:nrow(Tenmile.Thresholds.Daily.Averages)) {
  Tenmile.Thresholds.Daily.Averages$gdd_tdo6[i] <- if (Tenmile.Thresholds.Daily.Averages$tdo6[i] < 11) {0} else {Tenmile.Thresholds.Daily.Averages$tdo6[i] - 11}
  Tenmile.Thresholds.Daily.Averages$gdd_tdo3[i] <- if (Tenmile.Thresholds.Daily.Averages$tdo3[i] < 21.5) {0} else {Tenmile.Thresholds.Daily.Averages$tdo3[i] - 10}
  Tenmile.Thresholds.Daily.Averages$gdd_tdo1[i] <- if (Tenmile.Thresholds.Daily.Averages$tdo1[i] < 19) {0} else {Tenmile.Thresholds.Daily.Averages$tdo1[i] - 7}
}

### Calculate cumulative stressful degree days ----

Tenmile.Thresholds.Daily.Averages$gdd_tdo6_cumulative[Tenmile.Thresholds.Daily.Averages$stratified == "no"] <- 0
Tenmile.Thresholds.Daily.Averages$gdd_tdo3_cumulative[Tenmile.Thresholds.Daily.Averages$stratified == "no"] <- 0
Tenmile.Thresholds.Daily.Averages$gdd_tdo1_cumulative[Tenmile.Thresholds.Daily.Averages$stratified == "no"] <- 0
for (i in which(word(Tenmile.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2020" & Tenmile.Thresholds.Daily.Averages$stratified == "yes")) {
  Tenmile.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Tenmile.Thresholds.Daily.Averages$gdd_tdo6[1:i])
  Tenmile.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Tenmile.Thresholds.Daily.Averages$gdd_tdo3[1:i])
  Tenmile.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Tenmile.Thresholds.Daily.Averages$gdd_tdo1[1:i])
}
for (i in which(word(Tenmile.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2021" & Tenmile.Thresholds.Daily.Averages$stratified == "yes")) {
  Tenmile.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Tenmile.Thresholds.Daily.Averages$gdd_tdo6[which(Tenmile.Thresholds.Daily.Averages$date == "2021-06-01"):i])
  Tenmile.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Tenmile.Thresholds.Daily.Averages$gdd_tdo3[which(Tenmile.Thresholds.Daily.Averages$date == "2021-06-01"):i])
  Tenmile.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Tenmile.Thresholds.Daily.Averages$gdd_tdo1[which(Tenmile.Thresholds.Daily.Averages$date == "2021-06-01"):i])
}
for (i in which(word(Tenmile.Thresholds.Daily.Averages$date, 1, 1, sep = "-") == "2022" & Tenmile.Thresholds.Daily.Averages$stratified == "yes")) {
  Tenmile.Thresholds.Daily.Averages$gdd_tdo6_cumulative[i] <- sum(Tenmile.Thresholds.Daily.Averages$gdd_tdo6[which(Tenmile.Thresholds.Daily.Averages$date == "2022-06-01"):i])
  Tenmile.Thresholds.Daily.Averages$gdd_tdo3_cumulative[i] <- sum(Tenmile.Thresholds.Daily.Averages$gdd_tdo3[which(Tenmile.Thresholds.Daily.Averages$date == "2022-06-01"):i])
  Tenmile.Thresholds.Daily.Averages$gdd_tdo1_cumulative[i] <- sum(Tenmile.Thresholds.Daily.Averages$gdd_tdo1[which(Tenmile.Thresholds.Daily.Averages$date == "2022-06-01"):i])
}

## Calculate average values within weekly and biweekly windows ----

Tenmile.Thresholds <- Tenmile.Thresholds %>%
  mutate(tdo6_avg_weekly = smooth_metric(data = Tenmile.Thresholds$tdo6, width = 84),
         tdo3_avg_weekly = smooth_metric(data = Tenmile.Thresholds$tdo3, width = 84),
         tdo1_avg_weekly = smooth_metric(data = Tenmile.Thresholds$tdo1, width = 84),
         layer_thickness_do6_temp17_avg_weekly = smooth_metric(data = Tenmile.Thresholds$layer_thickness_do6_temp17, width = 84),
         layer_thickness_do3_temp21_avg_weekly = smooth_metric(data = Tenmile.Thresholds$layer_thickness_do3_temp21, width = 84),
         layer_thickness_do1_temp25_avg_weekly = smooth_metric(data = Tenmile.Thresholds$layer_thickness_do1_temp25, width = 84),
         relative_layer_thickness_do6_temp17_avg_weekly =  smooth_metric(data = Tenmile.Thresholds$relative_layer_thickness_do6_temp17, width = 84),
         relative_layer_thickness_do3_temp21_avg_weekly =  smooth_metric(data = Tenmile.Thresholds$relative_layer_thickness_do3_temp21, width = 84),
         relative_layer_thickness_do1_temp25_avg_weekly =  smooth_metric(data = Tenmile.Thresholds$relative_layer_thickness_do1_temp25, width = 84),
         depth_do6_avg_weekly = smooth_metric(data = Tenmile.Thresholds$depth_do6, width = 84),
         depth_do3_avg_weekly = smooth_metric(data = Tenmile.Thresholds$depth_do3, width = 84),
         depth_do1_avg_weekly = smooth_metric(data = Tenmile.Thresholds$depth_do1, width = 84),
         depth_temp17_avg_weekly = smooth_metric(data = Tenmile.Thresholds$depth_temp17, width = 84),
         depth_temp21_avg_weekly = smooth_metric(data = Tenmile.Thresholds$depth_temp21, width = 84),
         depth_temp25_avg_weekly = smooth_metric(data = Tenmile.Thresholds$depth_temp25, width = 84),
         max_temp_avg_weekly = smooth_metric(data = Tenmile.Thresholds$max_temp, width = 84),
         min_do_avg_weekly = smooth_metric(data = Tenmile.Thresholds$min_do, width = 84),
         volumetric_habitat_availability_do6_temp17_avg_weekly = smooth_metric(data = Tenmile.Thresholds$volumetric_habitat_availability_do6_temp17, width = 84),
         volumetric_habitat_availability_do3_temp21_avg_weekly = smooth_metric(data = Tenmile.Thresholds$volumetric_habitat_availability_do3_temp21, width = 84),
         volumetric_habitat_availability_do1_temp25_avg_weekly = smooth_metric(data = Tenmile.Thresholds$volumetric_habitat_availability_do1_temp25, width = 84),
         relative_volumetric_habitat_availability_do6_temp17_avg_weekly = smooth_metric(data = Tenmile.Thresholds$relative_volumetric_habitat_availability_do6_temp17, width = 84),
         relative_volumetric_habitat_availability_do3_temp21_avg_weekly = smooth_metric(data = Tenmile.Thresholds$relative_volumetric_habitat_availability_do3_temp21, width = 84),
         relative_volumetric_habitat_availability_do1_temp25_avg_weekly = smooth_metric(data = Tenmile.Thresholds$relative_volumetric_habitat_availability_do1_temp25, width = 84),
         tdo6_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$tdo6, width = 168),
         tdo3_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$tdo3, width = 168),
         tdo1_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$tdo1, width = 168),
         layer_thickness_do6_temp17_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$layer_thickness_do6_temp17, width = 168),
         layer_thickness_do3_temp21_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$layer_thickness_do3_temp21, width = 168),
         layer_thickness_do1_temp25_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$layer_thickness_do1_temp25, width = 168),
         relative_layer_thickness_do6_temp17_avg_biweekly =  smooth_metric(data = Tenmile.Thresholds$relative_layer_thickness_do6_temp17, width = 168),
         relative_layer_thickness_do3_temp21_avg_biweekly =  smooth_metric(data = Tenmile.Thresholds$relative_layer_thickness_do3_temp21, width = 168),
         relative_layer_thickness_do1_temp25_avg_biweekly =  smooth_metric(data = Tenmile.Thresholds$relative_layer_thickness_do1_temp25, width = 168),
         depth_do6_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$depth_do6, width = 168),
         depth_do3_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$depth_do3, width = 168),
         depth_do1_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$depth_do1, width = 168),
         depth_temp17_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$depth_temp17, width = 168),
         depth_temp21_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$depth_temp21, width = 168),
         depth_temp25_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$depth_temp25, width = 168),
         max_temp_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$max_temp, width = 168),
         min_do_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$min_do, width = 168),
         volumetric_habitat_availability_do6_temp17_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$volumetric_habitat_availability_do6_temp17, width = 168),
         volumetric_habitat_availability_do3_temp21_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$volumetric_habitat_availability_do3_temp21, width = 168),
         volumetric_habitat_availability_do1_temp25_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$volumetric_habitat_availability_do1_temp25, width = 168),
         relative_volumetric_habitat_availability_do6_temp17_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$relative_volumetric_habitat_availability_do6_temp17, width = 168),
         relative_volumetric_habitat_availability_do3_temp21_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$relative_volumetric_habitat_availability_do3_temp21, width = 168),
         relative_volumetric_habitat_availability_do1_temp25_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$relative_volumetric_habitat_availability_do1_temp25, width = 168))

Tenmile.Thresholds <- Tenmile.Thresholds %>%
  mutate(volumetric_habitat_CLT_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$volumetric_habitat_CLT, width = 168),
         CLT_avg_biweekly = smooth_metric(data = Tenmile.Thresholds$CLT, width = 168))

## Find max stress values and dates of occurrence ----

Tenmile.Max.Stress.1 <- Tenmile.Thresholds %>%
  mutate(year = word(date_time_local, 1, 1, sep = "-")) %>%
  filter(word(date_time_local, 2, 2, sep = "-") %in% c("06", "07", "08", "09", "10", "11")) %>%
  group_by(year) %>%
  summarise_at(vars(tdo6,
                    tdo3,
                    tdo1,
                    depth_temp17,
                    depth_temp21,
                    depth_temp25,
                    max_temp,
                    tdo6_avg_weekly,
                    tdo3_avg_weekly,
                    tdo1_avg_weekly,
                    depth_temp17_avg_weekly,
                    depth_temp21_avg_weekly,
                    depth_temp25_avg_weekly,
                    max_temp_avg_weekly,
                    tdo6_avg_biweekly,
                    tdo3_avg_biweekly,
                    tdo1_avg_biweekly,
                    depth_temp17_avg_biweekly,
                    depth_temp21_avg_biweekly,
                    depth_temp25_avg_biweekly,
                    max_temp_avg_biweekly), max, na.rm = T)

Tenmile.Max.Stress.2 <- Tenmile.Thresholds %>%
  mutate(year = word(date_time_local, 1, 1, sep = "-")) %>%
  filter(word(date_time_local, 2, 2, sep = "-") %in% c("06", "07", "08", "09", "10", "11")) %>%
  group_by(year) %>%
  summarise_at(vars(layer_thickness_do6_temp17,
                    relative_layer_thickness_do6_temp17,
                    volumetric_habitat_availability_do6_temp17,
                    relative_volumetric_habitat_availability_do6_temp17,
                    depth_do6,
                    layer_thickness_do3_temp21,
                    relative_layer_thickness_do3_temp21,
                    volumetric_habitat_availability_do3_temp21,
                    relative_volumetric_habitat_availability_do3_temp21,
                    depth_do3,
                    layer_thickness_do1_temp25,
                    relative_layer_thickness_do1_temp25,
                    volumetric_habitat_availability_do1_temp25,
                    relative_volumetric_habitat_availability_do1_temp25,
                    depth_do1,
                    min_do,
                    layer_thickness_do6_temp17_avg_weekly,
                    relative_layer_thickness_do6_temp17_avg_weekly,
                    volumetric_habitat_availability_do6_temp17_avg_weekly,
                    relative_volumetric_habitat_availability_do6_temp17_avg_weekly,
                    depth_do6_avg_weekly,
                    layer_thickness_do3_temp21_avg_weekly,
                    relative_layer_thickness_do3_temp21_avg_weekly,
                    volumetric_habitat_availability_do3_temp21_avg_weekly,
                    relative_volumetric_habitat_availability_do3_temp21_avg_weekly,
                    depth_do3_avg_weekly,
                    layer_thickness_do1_temp25_avg_weekly,
                    relative_layer_thickness_do1_temp25_avg_weekly,
                    volumetric_habitat_availability_do1_temp25_avg_weekly,
                    relative_volumetric_habitat_availability_do1_temp25_avg_weekly,
                    depth_do1_avg_weekly,
                    min_do_avg_weekly,
                    layer_thickness_do6_temp17_avg_biweekly,
                    relative_layer_thickness_do6_temp17_avg_biweekly,
                    volumetric_habitat_availability_do6_temp17_avg_biweekly,
                    relative_volumetric_habitat_availability_do6_temp17_avg_biweekly,
                    depth_do6_avg_biweekly,
                    layer_thickness_do3_temp21_avg_biweekly,
                    relative_layer_thickness_do3_temp21_avg_biweekly,
                    volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    relative_volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    depth_do3_avg_biweekly,
                    layer_thickness_do1_temp25_avg_biweekly,
                    relative_layer_thickness_do3_temp21_avg_biweekly,
                    volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    relative_volumetric_habitat_availability_do3_temp21_avg_biweekly,
                    depth_do1_avg_biweekly,
                    min_do_avg_biweekly), min, na.rm = T)

Tenmile.Max.Stress <- cbind(Tenmile.Max.Stress.1, Tenmile.Max.Stress.2[, -1])
Tenmile.Max.Stress.Dates <- Tenmile.Max.Stress 
Tenmile.Max.Stress.Dates[1:nrow(Tenmile.Max.Stress.Dates), 2:ncol(Tenmile.Max.Stress.Dates)] <- NA

for (j in 2:ncol(Tenmile.Max.Stress.Dates)) {
  var <- names(Tenmile.Max.Stress.Dates)[j]
  for (i in 1:nrow(Tenmile.Max.Stress.Dates)) {
    year <- Tenmile.Max.Stress.Dates$year[i]
    if (var %in% names(Tenmile.Max.Stress.1)) {
      if (max(Tenmile.Thresholds[Tenmile.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Tenmile.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) == 0) {
        Tenmile.Max.Stress.Dates[i,j] <- "NA"
      } else {
        Tenmile.Max.Stress.Dates[i,j] <- format(Tenmile.Thresholds$date_time_local[which(Tenmile.Thresholds[, var] == max(Tenmile.Thresholds[Tenmile.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Tenmile.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) & Tenmile.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Tenmile.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"))], "%Y-%m-%d %H:%M:%S")[1]
      }
    } else {
      if (min(Tenmile.Thresholds[Tenmile.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Tenmile.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) == tenmileMaxDepth) {
        Tenmile.Max.Stress.Dates[i,j] <- "NA"
      } else {
        Tenmile.Max.Stress.Dates[i,j] <- format(Tenmile.Thresholds$date_time_local[which(Tenmile.Thresholds[, var] == min(Tenmile.Thresholds[Tenmile.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Tenmile.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"), var], na.rm = T) & Tenmile.Thresholds$date_time_local >= paste0(year, "-06-01 00:00:00") & Tenmile.Thresholds$date_time_local <= paste0(year, "-11-30 23:00:00"))], "%Y-%m-%d %H:%M:%S")[1]
      }
    }
  }
}

Tenmile.Max.Stress <- Tenmile.Max.Stress %>%
  pivot_longer(cols = c(tdo6:min_do_avg_biweekly),
               names_to = "metric",
               values_to = "value") %>%
  mutate(lake = "Ten Mile")

Tenmile.Max.Stress.Dates <- Tenmile.Max.Stress.Dates %>%
  pivot_longer(cols = c(tdo6:min_do_avg_biweekly),
               names_to = "metric",
               values_to = "date") %>%
  mutate(lake = "Ten Mile")

Tenmile.Max.Stress <- Tenmile.Max.Stress.Dates %>%
  left_join(Tenmile.Max.Stress, join_by(lake, year, metric)) %>%
  select(lake, year, metric, date, value)

Tenmile.Max.Stress <- Tenmile.Max.Stress %>%
  mutate(date = case_when(date == "NA" ~ NA,
                          .default = date),
         date = as.POSIXct(date))

Tenmile.Max.Stress <- Tenmile.Max.Stress %>%
  mutate(doy = yday(date),
         dom = mday(date),
         month = month(date),
         week = week(date))

rm(Tenmile.Max.Stress.1, Tenmile.Max.Stress.2)

## Calculate rates of change in metrics over different time scales ----

Tenmile.Thresholds.ROCs.Inst <- data.frame(date_time_local = Tenmile.Thresholds$date_time_local[1:(nrow(Tenmile.Thresholds)-1)],
                                          tdo6 = calc_roc(data = Tenmile.Thresholds, metric = "tdo6", width = 0),
                                          layer_thickness_do6_temp17 = calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do6_temp17", width = 0),
                                          depth_do6 = calc_roc(data = Tenmile.Thresholds, metric = "depth_do6", width = 0),
                                          depth_temp17 = calc_roc(data = Tenmile.Thresholds, metric = "tdo3", width = 0),
                                          tdo3 = calc_roc(data = Tenmile.Thresholds, metric = "tdo6", width = 0),
                                          layer_thickness_do3_temp21 = calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do3_temp21", width = 0),
                                          depth_do3 = calc_roc(data = Tenmile.Thresholds, metric = "depth_do3", width = 0),
                                          depth_temp21 = calc_roc(data = Tenmile.Thresholds, metric = "depth_temp21", width = 0),
                                          tdo1 = calc_roc(data = Tenmile.Thresholds, metric = "tdo1", width = 0),
                                          layer_thickness_do1_temp25 = calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do1_temp25", width = 0),
                                          depth_do1 = calc_roc(data = Tenmile.Thresholds, metric = "depth_do1", width = 0),
                                          depth_temp25 = calc_roc(data = Tenmile.Thresholds, metric = "depth_temp25", width = 0),
                                          max_temp = calc_roc(data = Tenmile.Thresholds, metric = "max_temp", width = 0),
                                          min_do = calc_roc(data = Tenmile.Thresholds, metric = "min_do", width = 0),
                                          volumetric_habitat_availability_do6_temp17 = calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", width = 0),
                                          volumetric_habitat_availability_do3_temp21 = calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", width = 0),
                                          volumetric_habitat_availability_do1_temp25 = calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", width = 0))

Tenmile.Thresholds.Daily.Averages.ROCs <- data.frame(date = Tenmile.Thresholds.Daily.Averages$date[1:(nrow(Tenmile.Thresholds.Daily.Averages)-1)],
                                                    tdo6 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "tdo6", width = 0),
                                                    layer_thickness_do6_temp17 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "layer_thickness_do6_temp17", width = 0),
                                                    depth_do6 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "depth_do6", width = 0),
                                                    depth_temp17 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "tdo3", width = 0),
                                                    tdo3 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "tdo6", width = 0),
                                                    layer_thickness_do3_temp21 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "layer_thickness_do3_temp21", width = 0),
                                                    depth_do3 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "depth_do3", width = 0),
                                                    depth_temp21 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "depth_temp21", width = 0),
                                                    tdo1 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "tdo1", width = 0),
                                                    layer_thickness_do1_temp25 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "layer_thickness_do1_temp25", width = 0),
                                                    depth_do1 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "depth_do1", width = 0),
                                                    depth_temp25 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "depth_temp25", width = 0),
                                                    max_temp = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "max_temp", width = 0),
                                                    min_do = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "min_do", width = 0),
                                                    volumetric_habitat_availability_do6_temp17 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "volumetric_habitat_availability_do6_temp17", width = 0),
                                                    volumetric_habitat_availability_do3_temp21 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "volumetric_habitat_availability_do3_temp21", width = 0),
                                                    volumetric_habitat_availability_do1_temp25 = calc_roc(data = Tenmile.Thresholds.Daily.Averages, metric = "volumetric_habitat_availability_do1_temp25", width = 0))

Tenmile.Thresholds.ROCs.Roll.Week <- data.frame(date_time_local = Tenmile.Thresholds$date_time_local[1:(nrow(Tenmile.Thresholds)-1)],
                                               tdo6 = calc_roc(data = Tenmile.Thresholds, metric = "tdo6", width = 84),
                                               layer_thickness_do6_temp17 = calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do6_temp17", width = 84),
                                               depth_do6 = calc_roc(data = Tenmile.Thresholds, metric = "depth_do6", width = 84),
                                               depth_temp17 = calc_roc(data = Tenmile.Thresholds, metric = "tdo3", width = 84),
                                               tdo3 = calc_roc(data = Tenmile.Thresholds, metric = "tdo6", width = 84),
                                               layer_thickness_do3_temp21 = calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do3_temp21", width = 84),
                                               depth_do3 = calc_roc(data = Tenmile.Thresholds, metric = "depth_do3", width = 84),
                                               depth_temp21 = calc_roc(data = Tenmile.Thresholds, metric = "depth_temp21", width = 84),
                                               tdo1 = calc_roc(data = Tenmile.Thresholds, metric = "tdo1", width = 84),
                                               layer_thickness_do1_temp25 = calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do1_temp25", width = 84),
                                               depth_do1 = calc_roc(data = Tenmile.Thresholds, metric = "depth_do1", width = 84),
                                               depth_temp25 = calc_roc(data = Tenmile.Thresholds, metric = "depth_temp25", width = 84),
                                               max_temp = calc_roc(data = Tenmile.Thresholds, metric = "max_temp", width = 84),
                                               min_do = calc_roc(data = Tenmile.Thresholds, metric = "min_do", width = 84),
                                               volumetric_habitat_availability_do6_temp17 = calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", width = 84),
                                               volumetric_habitat_availability_do3_temp21 = calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", width = 84),
                                               volumetric_habitat_availability_do1_temp25 = calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", width = 84))

Tenmile.Thresholds.ROCs.Roll.2Weeks <- data.frame(date_time_local = Tenmile.Thresholds$date_time_local[1:(nrow(Tenmile.Thresholds)-1)],
                                                 tdo6 = calc_roc(data = Tenmile.Thresholds, metric = "tdo6", width = 168),
                                                 layer_thickness_do6_temp17 = calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do6_temp17", width = 168),
                                                 depth_do6 = calc_roc(data = Tenmile.Thresholds, metric = "depth_do6", width = 168),
                                                 depth_temp17 = calc_roc(data = Tenmile.Thresholds, metric = "tdo3", width = 168),
                                                 tdo3 = calc_roc(data = Tenmile.Thresholds, metric = "tdo6", width = 168),
                                                 layer_thickness_do3_temp21 = calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do3_temp21", width = 168),
                                                 depth_do3 = calc_roc(data = Tenmile.Thresholds, metric = "depth_do3", width = 168),
                                                 depth_temp21 = calc_roc(data = Tenmile.Thresholds, metric = "depth_temp21", width = 168),
                                                 tdo1 = calc_roc(data = Tenmile.Thresholds, metric = "tdo1", width = 168),
                                                 layer_thickness_do1_temp25 = calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do1_temp25", width = 168),
                                                 depth_do1 = calc_roc(data = Tenmile.Thresholds, metric = "depth_do1", width = 168),
                                                 depth_temp25 = calc_roc(data = Tenmile.Thresholds, metric = "depth_temp25", width = 168),
                                                 max_temp = calc_roc(data = Tenmile.Thresholds, metric = "max_temp", width = 168),
                                                 min_do = calc_roc(data = Tenmile.Thresholds, metric = "min_do", width = 168),
                                                 volumetric_habitat_availability_do6_temp17 = calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", width = 168),
                                                 volumetric_habitat_availability_do3_temp21 = calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", width = 168),
                                                 volumetric_habitat_availability_do1_temp25 = calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", width = 168))

Tenmile.Thresholds.ROCs.Block.Week <- data.frame(year = c(rep("2020", length(unique(week(Tenmile.Thresholds$date_time_local[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"])))),
                                                         rep("2021", length(unique(week(Tenmile.Thresholds$date_time_local[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"])))),
                                                         rep("2022", length(unique(week(Tenmile.Thresholds$date_time_local[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"]))))),
                                                week = c(unique(week(Tenmile.Thresholds$date_time_local[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"])),
                                                         unique(week(Tenmile.Thresholds$date_time_local[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"])),
                                                         unique(week(Tenmile.Thresholds$date_time_local[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"]))),
                                                tdo6 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "tdo6", width = "week"),
                                                layer_thickness_do6_temp17 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "layer_thickness_do6_temp17", width = "week"),
                                                depth_do6 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "depth_do6", width = "week"),
                                                depth_temp17 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "tdo3", width = "week"),
                                                tdo3 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "tdo6", width = "week"),
                                                layer_thickness_do3_temp21 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "layer_thickness_do3_temp21", width = "week"),
                                                depth_do3 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "depth_do3", width = "week"),
                                                depth_temp21 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "depth_temp21", width = "week"),
                                                tdo1 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "tdo1", width = "week"),
                                                layer_thickness_do1_temp25 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "layer_thickness_do1_temp25", width = "week"),
                                                depth_do1 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "depth_do1", width = "week"),
                                                depth_temp25 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "depth_temp25", width = "week"),
                                                max_temp = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "max_temp", width = "week"),
                                                min_do = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "min_do", width = "week"),
                                                volumetric_habitat_availability_do6_temp17 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "volumetric_habitat_availability_do6_temp17", width = "week"),
                                                volumetric_habitat_availability_do3_temp21 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "volumetric_habitat_availability_do3_temp21", width = "week"),
                                                volumetric_habitat_availability_do1_temp25 = calc_roc(data = Tenmile.Thresholds[word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") != "2019",], metric = "volumetric_habitat_availability_do1_temp25", width = "week"))
Tenmile.Thresholds.ROCs.Block.Week$week_start_date <- ymd(paste0(Tenmile.Thresholds.ROCs.Block.Week$year, "0101")) + weeks(Tenmile.Thresholds.ROCs.Block.Week$week - 1)


Tenmile.Thresholds.ROCs.Season <- data.frame(year = c("2020", "2021", "2022"),
                                            tdo6 = c(calc_roc(data = Tenmile.Thresholds, metric = "tdo6", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                     calc_roc(data = Tenmile.Thresholds, metric = "tdo6", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                     calc_roc(data = Tenmile.Thresholds, metric = "tdo6", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "tdo6_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            layer_thickness_do6_temp17 = c(calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                                           calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                                           calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do6_temp17", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "layer_thickness_do6_temp17_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            depth_do6 = c(calc_roc(data = Tenmile.Thresholds, metric = "depth_do6", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                          calc_roc(data = Tenmile.Thresholds, metric = "depth_do6", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                          calc_roc(data = Tenmile.Thresholds, metric = "depth_do6", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_do6_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            depth_temp17 = c(calc_roc(data = Tenmile.Thresholds, metric = "depth_temp17", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                             calc_roc(data = Tenmile.Thresholds, metric = "depth_temp17", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                             calc_roc(data = Tenmile.Thresholds, metric = "depth_temp17", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_temp17_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            tdo3 = c(calc_roc(data = Tenmile.Thresholds, metric = "tdo3", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                     calc_roc(data = Tenmile.Thresholds, metric = "tdo3", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                     calc_roc(data = Tenmile.Thresholds, metric = "tdo3", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "tdo3_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            layer_thickness_do3_temp21 = c(calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                                           calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                                           calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do3_temp21", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "layer_thickness_do3_temp21_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            depth_do3 = c(calc_roc(data = Tenmile.Thresholds, metric = "depth_do3", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                          calc_roc(data = Tenmile.Thresholds, metric = "depth_do3", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                          calc_roc(data = Tenmile.Thresholds, metric = "depth_do3", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_do3_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            depth_temp21 = c(calc_roc(data = Tenmile.Thresholds, metric = "depth_temp21", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                             calc_roc(data = Tenmile.Thresholds, metric = "depth_temp21", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                             calc_roc(data = Tenmile.Thresholds, metric = "depth_temp21", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_temp21_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            tdo1 = c(calc_roc(data = Tenmile.Thresholds, metric = "tdo1", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                     calc_roc(data = Tenmile.Thresholds, metric = "tdo1", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                     calc_roc(data = Tenmile.Thresholds, metric = "tdo1", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "tdo1_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            layer_thickness_do1_temp25 = c(calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                                           calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                                           calc_roc(data = Tenmile.Thresholds, metric = "layer_thickness_do1_temp25", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "layer_thickness_do1_temp25_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            depth_do1 = c(calc_roc(data = Tenmile.Thresholds, metric = "depth_do1", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                          calc_roc(data = Tenmile.Thresholds, metric = "depth_do1", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                          calc_roc(data = Tenmile.Thresholds, metric = "depth_do1", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_do1_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            depth_temp25 = c(calc_roc(data = Tenmile.Thresholds, metric = "depth_temp25", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                             calc_roc(data = Tenmile.Thresholds, metric = "depth_temp25", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                             calc_roc(data = Tenmile.Thresholds, metric = "depth_temp25", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "depth_temp25_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            max_temp = c(calc_roc(data = Tenmile.Thresholds, metric = "max_temp", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                         calc_roc(data = Tenmile.Thresholds, metric = "max_temp", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                         calc_roc(data = Tenmile.Thresholds, metric = "max_temp", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "max_temp_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            min_do = c(calc_roc(data = Tenmile.Thresholds, metric = "min_do", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "min_do_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                       calc_roc(data = Tenmile.Thresholds, metric = "min_do", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "min_do_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                       calc_roc(data = Tenmile.Thresholds, metric = "min_do", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "min_do_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            volumetric_habitat_availability_do6_temp17 = c(calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                                                           calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                                                           calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do6_temp17", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "volumetric_habitat_availability_do6_temp17_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            volumetric_habitat_availability_do3_temp21 = c(calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                                                           calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                                                           calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do3_temp21", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "volumetric_habitat_availability_do3_temp21_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])),
                                            volumetric_habitat_availability_do1_temp25 = c(calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2020"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2020"]),
                                                                                           calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2021"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2021"]),
                                                                                           calc_roc(data = Tenmile.Thresholds, metric = "volumetric_habitat_availability_do1_temp25", start = format(Tenmile.Thresholds$date_time_local[Tenmile.Thresholds$stratified == "yes" & word(Tenmile.Thresholds$date_time_local, 1, 1, sep = "-") == "2022"][1], "%Y-%m-%d %H:%M:%S"), end = Tenmile.Max.Stress.Dates$date[Tenmile.Max.Stress.Dates$metric == "volumetric_habitat_availability_do1_temp25_avg_weekly" & Tenmile.Max.Stress.Dates$year == "2022"])))

Weather.Data <- read.csv("~/Downloads/asos.csv", stringsAsFactors = F)
Weather.Data <- Weather.Data %>%
  select(station, valid, tmpf, p01i) %>%
  mutate(station = case_when(station == "Y49" ~ "Ten Mile",
                             station == "AXN" ~ "Carlos",
                             .default = "Greenwood"),
         temp_c = fahrenheit.to.celsius(tmpf),
         p01i = as.numeric(p01i),
         date = as.Date(word(valid, 1, 1, sep = " "))) %>%
  rename(lake = "station",
         date_time = "valid",
         temp_f = "tmpf",
         precip_1hr_in = "p01i") %>%
  group_by(lake, date) %>%
  summarise(temp_c = mean(temp_c, na.rm = TRUE),
            temp_f = mean(temp_f, na.rm = TRUE),
            precip_1day_in = sum(precip_1hr_in, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(year = as.numeric(word(date, 1, 1, sep = "-")),
         moy = as.numeric(word(date, 2, 2, sep = "-")),
         doy = as.numeric(word(date, 3, 3, sep = "-")))

Carlos.Weather.Data <- Weather.Data %>%
  filter(lake == "Carlos",
         year %in% c(2018, 2019, 2020),
         moy >= 3,
         moy <= 12)
ggplot(Carlos.Weather.Data %>% filter(year == 2018), aes(x = date, y = precip_1day_in)) +
  geom_col() +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

















