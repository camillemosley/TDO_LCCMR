### TDO sensor data analysis ###
# Created by Mike M on Jul 21 2026

# Load libraries ----
library(tidyverse)
library(svglite)

# Read in data ----


# Plots ----

## Greenwood oxythermal habitat ----

Greenwood.Thresholds.Longer <- Greenwood.Thresholds %>%
  pivot_longer(cols = c(depth_do1, depth_do3, depth_do6),
               names_to = "Threshold_Type",
               values_to = "depth") %>%
  mutate(Threshold_Type = factor(Threshold_Type, 
                                 levels = c("depth_do1", "depth_do3", "depth_do6"),
                                 labels = c("1 mg/L DO", "3 mg/L DO", "6 mg/L DO")))

### 2018 ----

date_time_start <- "2018-01-01 00:00:00"
date_time_end <- "2018-12-31 23:00:00"
title <- "2018"

Greenwood.Temp.Filt.2018 <- Greenwood.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Greenwood.Thresholds.Longer.2018 <- Greenwood.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Greenwood.Temp.Filt.2018, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = "Date", y = "Depth (m)", fill = "Temp (°C)", title = "2018") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data = Greenwood.Thresholds.Longer.2018,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9 ) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black"))

### 2019 ----

date_time_start <- "2019-01-01 00:00:00"
date_time_end <- "2019-12-31 23:00:00"
title <- "2019"

Greenwood.Temp.Filt.2019 <- Greenwood.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Greenwood.Thresholds.Longer.2019 <- Greenwood.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Greenwood.Temp.Filt.2019, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "Depth (m)", fill = "Temp (c)", title = "Greenwood 2019") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  geom_line(data = Greenwood.Thresholds.Longer.2019,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9 ) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black"))

# plot just temp
ggplot(Greenwood.Temp.Filt.2019, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "Depth (m)", fill = "Temp (°C)", title = "Greenwood 2019") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14))

### 2020 ----

date_time_start <- "2020-01-01 00:00:00"
date_time_end <- "2020-12-31 23:00:00"
title <- "2020"

Greenwood.Temp.Filt.2020 <- Greenwood.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Greenwood.Thresholds.Longer.2020 <- Greenwood.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Greenwood.Temp.Filt.2020, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = "Temp (°C)", title = "2020") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14)) +
  geom_line(data = Greenwood.Thresholds.Longer.2020,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9 ) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black")) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())

### 2021 ----

date_time_start <- "2021-01-01 00:00:00"
date_time_end <- "2021-12-31 23:00:00"
title <- "2021"

Greenwood.Temp.Filt.2021 <- Greenwood.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Greenwood.Thresholds.Longer.2021 <- Greenwood.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Greenwood.Temp.Filt.2021, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = "Temp (°C)", title = "2021") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14)) +
  geom_line(data = Greenwood.Thresholds.Longer.2021,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9 ) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black")) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())

### 2022 ----

date_time_start <- "2022-01-01 00:00:00"
date_time_end <- "2022-12-31 23:00:00"
title <- "2022"

Greenwood.Temp.Filt.2022 <- Greenwood.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Greenwood.Thresholds.Longer.2022 <- Greenwood.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Greenwood.Temp.Filt.2022, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = "Temp (°C)", title = "2022") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14)) +
  geom_line(data = Greenwood.Thresholds.Longer.2022,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9 ) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black")) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())

### 2023 ----

date_time_start <- "2023-01-01 00:00:00"
date_time_end <- "2023-12-31 23:00:00"
title <- "2023"

Greenwood.Temp.Filt.2023 <- Greenwood.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Greenwood.Thresholds.Longer.2023 <- Greenwood.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Greenwood.Temp.Filt.2023, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = "Temp (°C)", title = "2023") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14)) +
  geom_line(data = Greenwood.Thresholds.Longer.2023,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9 ) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black")) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())

### 2024 ----

date_time_start <- "2024-01-01 00:00:00"
date_time_end <- "2024-12-31 23:00:00"
title <- "2024"

Greenwood.Temp.Filt.2024 <- Greenwood.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Greenwood.Thresholds.Longer.2024 <- Greenwood.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Greenwood.Temp.Filt.2024, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0),
                   limits = c(as.POSIXct("2024-01-01"), as.POSIXct("2024-12-31"))) +
  labs(x = NULL, y = NULL, fill = "Temp (°C)", title = "2024") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14)) +
  geom_line(data = Greenwood.Thresholds.Longer.2024,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9 ) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black")) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggplot(data = Greenwood.Thresholds) +
  geom_line(aes(x = date_time_local, y = tdo1_avg_weekly), colour = "black") +
  geom_line(aes(x = date_time_local, y = tdo3_avg_weekly), colour = "red") +
  geom_line(aes(x = date_time_local, y = tdo6_avg_weekly), colour = "blue")

ggplot(data = Greenwood.Thresholds) +
  geom_line(aes(x = date_time_local, y = layer_thickness_do1_temp25_avg_weekly), colour = "black") +
  geom_line(aes(x = date_time_local, y = layer_thickness_do3_temp21_avg_weekly), colour = "red") +
  geom_line(aes(x = date_time_local, y = layer_thickness_do6_temp17_avg_weekly), colour = "blue")

ggplot(data = Greenwood.Thresholds) +
  geom_line(aes(x = date_time_local, y = tdo3), colour = "red") +
  geom_line(aes(x = date_time_local, y = tdo3_avg_weekly), colour = "black") +
  geom_line(aes(x = date_time_local, y = tdo3_avg_biweekly), colour = "white")

ggplot(data = Greenwood.Thresholds) +
  geom_line(aes(x = date_time_local, y = depth_do3), colour = "red") +
  geom_line(aes(x = date_time_local, y = depth_do3_avg_weekly), colour = "black") +
  geom_line(aes(x = date_time_local, y = depth_do3_avg_biweekly), colour = "blue") +
  scale_y_reverse()

rm(Data.Filt,
   Greenwood.Temp.Filt.2018,
   Greenwood.Temp.Filt.2019,
   Greenwood.Temp.Filt.2020,
   Greenwood.Temp.Filt.2021,
   Greenwood.Temp.Filt.2022,
   Greenwood.Temp.Filt.2023,
   Greenwood.Temp.Filt.2024,
   Greenwood.Thresholds.Longer,
   Greenwood.Thresholds.Longer.2018,
   Greenwood.Thresholds.Longer.2019,
   Greenwood.Thresholds.Longer.2020,
   Greenwood.Thresholds.Longer.2021,
   Greenwood.Thresholds.Longer.2022,
   Greenwood.Thresholds.Longer.2023,
   Greenwood.Thresholds.Longer.2024)

## Carlos oxythermal habitat ----

Carlos.Thresholds.Longer <- Carlos.Thresholds %>%
  pivot_longer(cols = c(depth_do1, depth_do3, depth_do6),
               names_to = "Threshold_Type",
               values_to = "depth") %>%
  mutate(Threshold_Type = factor(Threshold_Type, 
                                 levels = c("depth_do1", "depth_do3", "depth_do6"),
                                 labels = c("1 mg/L DO", "3 mg/L DO", "6 mg/L DO")))

### 2018 ----

date_time_start <- "2018-01-01 00:00:00"
date_time_end <- "2018-12-31 23:00:00"

Carlos.Temp.Filt.2018 <- Carlos.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Carlos.Thresholds.Longer.2018 <- Carlos.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Carlos.Temp.Filt.2018, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0),
                   limits = c(as.POSIXct("2018-01-01"), as.POSIXct("2018-12-31"))) +
  labs(x = NULL, y = NULL, fill = "Temp (°C)", title = "2018") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14)) +
  geom_line(data = Carlos.Thresholds.Longer.2018,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9 ) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black")) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())

### 2019 ----

date_time_start <- "2019-01-01 00:00:00"
date_time_end <- "2019-12-31 23:00:00"

Carlos.Temp.Filt.2019 <- Carlos.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Carlos.Thresholds.Longer.2019 <- Carlos.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Carlos.Temp.Filt.2019, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = "Temp (°C)", title = "2019") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14)) +
  geom_line(data = Carlos.Thresholds.Longer.2019,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black")) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())

### 2020 ----

date_time_start <- "2020-01-01 00:00:00"
date_time_end <- "2020-12-31 23:00:00"

Carlos.Temp.Filt.2020 <- Carlos.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Carlos.Thresholds.Longer.2020 <- Carlos.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Carlos.Temp.Filt.2020, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = "Date", y = "Depth (m)", fill = "Temp (°C)", title = "2020") +
  theme_bw() +
  geom_line(data = Carlos.Thresholds.Longer.2020,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9 ) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black"))

rm(Carlos.Temp.Filt.2018,
   Carlos.Temp.Filt.2019,
   Carlos.Temp.Filt.2020,
   Carlos.Thresholds.Longer,
   Carlos.Thresholds.Longer.2018,
   Carlos.Thresholds.Longer.2019,
   Carlos.Thresholds.Longer.2020)

## Ten Mile oxythermal habitat ----

Tenmile.Thresholds.Longer <- Tenmile.Thresholds %>%
  pivot_longer(cols = c(depth_do1, depth_do3, depth_do6),
               names_to = "Threshold_Type",
               values_to = "depth") %>%
  mutate(Threshold_Type = factor(Threshold_Type, 
                                 levels = c("depth_do1", "depth_do3", "depth_do6"),
                                 labels = c("1 mg/L DO", "3 mg/L DO", "6 mg/L DO")))

### 2019 ----

date_time_start <- "2019-01-01 00:00:00"
date_time_end <- "2019-12-31 23:00:00"

Tenmile.Temp.Filt.2019 <- Tenmile.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Tenmile.Thresholds.Longer.2019 <- Tenmile.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Tenmile.Temp.Filt.2019, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = "Temp (°C)", title = "2019") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14)) +
  geom_line(data = Tenmile.Thresholds.Longer.2019,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black")) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())

### 2020 ----

date_time_start <- "2020-01-01 00:00:00"
date_time_end <- "2020-12-31 23:00:00"

Tenmile.Temp.Filt.2020 <- Tenmile.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Tenmile.Thresholds.Longer.2020 <- Tenmile.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Tenmile.Temp.Filt.2020, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = "Temp (°C)", title = "2020") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14)) +
  geom_line(data = Tenmile.Thresholds.Longer.2020,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black")) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())

### 2021 ----

date_time_start <- "2021-01-01 00:00:00"
date_time_end <- "2021-12-31 23:00:00"

Tenmile.Temp.Filt.2021 <- Tenmile.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Tenmile.Thresholds.Longer.2021 <- Tenmile.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Tenmile.Temp.Filt.2021, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = "Temp (°C)", title = "2021") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14)) +
  geom_line(data = Tenmile.Thresholds.Longer.2021,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black")) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())

### 2022 ----

date_time_start <- "2022-01-01 00:00:00"
date_time_end <- "2022-12-31 23:00:00"

Tenmile.Temp.Filt.2022 <- Tenmile.Temp.Interpolated %>%
  filter(as.character(depth_m) %in% as.character(seq(0, max(depth_m), by = 0.1)),
         date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

Tenmile.Thresholds.Longer.2022 <- Tenmile.Thresholds.Longer %>%
  filter(date_time_local >= as.POSIXct(date_time_start, format = "%Y-%m-%d %H:%M:%S"),
         date_time_local <= as.POSIXct(date_time_end, format = "%Y-%m-%d %H:%M:%S"))

ggplot(Tenmile.Temp.Filt.2022, aes(x = date_time_local, y = depth_m, fill = temp_c)) +
  geom_tile() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("#2c0354", "blue", "green", "yellow", "orange","red", "#7a0000"),
                       values = rescale(c(0, 4, 10.5, 17, 21, 25, 30), from = c(0, 30)), # Matches the full legend limits
                       limits = c(0, 30),
                       breaks = c(0, 4, 10, 17, 21, 25, 30)) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = "Temp (°C)", title = "2022") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14)) +
  geom_line(data = Tenmile.Thresholds.Longer.2022,
            aes(x = date_time_local, y = round(depth, digits = 1), colour = Threshold_Type),
            inherit.aes = FALSE,
            linewidth = 0.9) +
  scale_colour_manual(name = "DO Threshold",
                      values = c("1 mg/L DO" = "pink",
                                 "3 mg/L DO" = "white",
                                 "6 mg/L DO" = "black")) +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())

rm(Tenmile.Temp.Filt.2019,
   Tenmile.Temp.Filt.2020,
   Tenmile.Temp.Filt.2021,
   Tenmile.Temp.Filt.2022,
   Tenmile.Thresholds.Longer,
   Tenmile.Thresholds.Longer.2019,
   Tenmile.Thresholds.Longer.2020,
   Tenmile.Thresholds.Longer.2021,
   Tenmile.Thresholds.Longer.2022)

# Visualize metrics ----

## Greenwood ----

Greenwood.Thresholds <- Greenwood.Thresholds %>%
  #mutate(year = word(date_time_local, 1, 1, sep = "-"),
  #       moy = month(date_time_local),
  #       day = day(date_time_local),
  #       doy = yday(date_time_local),
  #       woy = week(date_time_local),
  #       dummy_date_time = update(date_time_local, year = 2000)) %>%
  filter(year %in% c(2019:2024))

Greenwood.Max.Stress.Dates <- Greenwood.Max.Stress.Dates %>%
  mutate(date = case_when(date == "NA" ~ NA,
                          .default = date),
         dummy_date = update(as.POSIXct(date), year = 2000))

### tdo3 ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = tdo3_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "TDO3", colour = "Year") +
  scale_y_continuous(breaks = 0:11, limits = c(0, 11)) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(r = 0, l = 1, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = tdo3_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "tdo3_avg_biweekly", year != "2024"), aes(xintercept = dummy_date, colour = as.factor(year)), linewidth = 1) +
  labs(x = NULL, y = "TDO3", colour = "Year") +
  theme(text = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(r = 0, l = 1, unit = "pt")) +
  scale_y_continuous(breaks = 0:11, limits = c(0, 11)) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

### tdo6 ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = tdo6_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "TDO6", colour = "Year") +
  scale_y_continuous(breaks = 0:17, limits = c(0, 17)) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(r = 0, l = 1, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = tdo6_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "tdo6_avg_biweekly", year != "2024"), aes(xintercept = dummy_date, colour = as.factor(year)), linewidth = 1) +
  labs(x = NULL, y = "TDO6", colour = "Year") +
  theme(text = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(r = 0, l = 1, unit = "pt")) +
  scale_y_continuous(breaks = 0:17, limits = c(0, 17)) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = tdo6, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "TDO6", colour = "Year") +
  #scale_y_continuous(breaks = 0:17, limits = c(0, 17)) +
  theme(text = element_text(size = 14)) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

### tdo1 ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = tdo1_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = tdo1_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "tdo1_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### CLT 6 do 22.8 temp ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = CLT_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "CLT", colour = "Year") +
  ylim(c(0, 63)) +
  geom_hline(aes(yintercept = 34), linetype = 2) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(r = 0, l = 1, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

### CLT 6 do 17 temp ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do6_temp17_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "CLT", colour = "Year") +
  ylim(c(0, 63)) +
  geom_hline(aes(yintercept = 34), linetype = 2) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(r = 0, l = 1, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do6_temp17_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "layer_thickness_do6_temp17_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### CLT 3 do 21 temp ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do3_temp21_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "CLT", colour = "Year") +
  ylim(c(0, 63)) +
  geom_hline(aes(yintercept = 34), linetype = 2) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(r = 0, l = 1, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do3_temp21_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "layer_thickness_do3_temp21_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### CLT 1 do 25 temp ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do1_temp25_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do1_temp25_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "layer_thickness_do1_temp25_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### max temp ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = max_temp_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = max_temp_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "max_temp_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### min do ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = min_do_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = min_do_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "min_do_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### depth do 3 ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = depth_do3_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = depth_do3_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "depth_do3_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### total volumetric habitat ----

#### 3 do 22.8 temp ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = volumetric_habitat_CLT_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "VHA", colour = "Year") +
  geom_hline(aes(yintercept = max(volumetric_habitat_CLT_avg_biweekly)), linetype = 2) +
  scale_y_continuous(limits = c(0, max(Tenmile.Thresholds$volumetric_habitat_CLT_avg_biweekly))) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(r = 0, l = 1, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

#### do 3 temp 21 ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = volumetric_habitat_availability_do3_temp21_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "Volumetric habitat availability (DO > 3 mg/L, temp < 21 ºC)", colour = "Year") +
  scale_y_continuous(limits = c(4e+07, 8.1e+07)) +
  theme(text = element_text(size = 14)) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = volumetric_habitat_availability_do3_temp21_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "volumetric_habitat_availability_do3_temp21_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year))) +
  labs(x = NULL, y = "Volumetric habitat (DO > 3 mg/L, temp < 21 mg/L)", colour = "Year") +
  scale_y_continuous(limits = c(4e+07, 8.1e+07)) +
  theme(text = element_text(size = 14), legend.position = "none") +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = tdo3_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "tdo3_avg_biweekly", year != "2024"), aes(xintercept = dummy_date, colour = as.factor(year))) +
  labs(x = NULL, y = "TDO3", colour = "Year") +
  theme(text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = 0:11, limits = c(0, 11)) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

Greenwood.Max.Stress.Wide <- Greenwood.Max.Stress %>%
  select(-c(lake, date, doy, dom, month, week)) %>%
  pivot_wider(names_from = metric, values_from = value)

ggplot(Greenwood.Max.Stress.Wide, aes(x = tdo3, y = volumetric_habitat_availability_do3_temp21)) +
  geom_point() +
  theme_bw() +
  labs(x = "Maximum TDO3", y = "Minimum volumetric habitat availability") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(Greenwood.Max.Stress.Wide, aes(x = tdo3_avg_biweekly, y = volumetric_habitat_availability_do3_temp21_avg_biweekly)) +
  geom_point() +
  theme_bw() +
  labs(x = "Maximum TDO3", y = "Minimum volumetric habitat availability") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

greenwood.mean.aug <- Greenwood.Thresholds %>%
  filter(moy == 8) %>%
  group_by(year) %>%
  summarise(mean_aug_tdo3 = mean(tdo3),
            mean_aug_tdo6 = mean(tdo6),
            mean_aug_CLT_3_21 = mean(layer_thickness_do3_temp21),
            mean_aug_CLT_6_17 = mean(layer_thickness_do6_temp17))

vhab_vs_mean_aug <- Greenwood.Max.Stress.Wide %>%
  inner_join(y = greenwood.mean.aug, by = join_by(year))

ggplot(Greenwood.Max.Stress.Wide, aes(x = tdo3_avg_biweekly, y = volumetric_habitat_availability_do3_temp21_avg_biweekly)) +
  geom_point() +
  theme_bw() +
  labs(x = "Maximum TDO3", y = "Minimum volumetric habitat availability") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(Greenwood.Thresholds.Daily.Averages, aes(x = dummy_date_time, y = cumulative_vol_hab_do3_temp17, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "Cumulative volumetric habitat reduction", colour = "Year") +
  scale_y_continuous(limits = c(0, max(Greenwood.Thresholds.Daily.Averages$cumulative_vol_hab_do3_temp17))) +
  theme(text = element_text(size = 14)) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

greenwood.cum.vol <- Greenwood.Thresholds.Daily.Averages %>%
  group_by(year) %>%
  summarise(max_cum_vol_3_17 = max(cumulative_vol_hab_do3_temp17))

cum.vol_max.tdo3 <- vhab_vs_mean_aug %>%
  inner_join(y = greenwood.cum.vol, by = join_by(year))

ggplot(cum.vol_max.tdo3, aes(x = tdo3_avg_biweekly, y = max_cum_vol_3_17)) +
  geom_point() +
  theme_bw() +
  labs(x = "Maximum TDO3", y = "Maximum cumulative volumetric habitat reduction") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(cum.vol_max.tdo3, aes(x = mean_aug_tdo3, y = max_cum_vol_3_17)) +
  geom_point() +
  theme_bw() +
  labs(x = "Mean August TDO3", y = "Maximmum cumulative volumetric habitat reduction") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(cum.vol_max.tdo3, aes(x = tdo6_avg_biweekly, y = max_cum_vol_3_17)) +
  geom_point() +
  theme_bw() +
  labs(x = "Maximum TDO6", y = "Maximum cumulative volumetric habitat reduction") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(cum.vol_max.tdo3, aes(x = mean_aug_tdo6, y = max_cum_vol_3_17)) +
  geom_point() +
  theme_bw() +
  labs(x = "Mean August TDO6", y = "Maximmum cumulative volumetric habitat reduction") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(cum.vol_max.tdo3, aes(x = mean_aug_tdo3, y = volumetric_habitat_availability_do3_temp21_avg_biweekly)) +
  geom_point() +
  theme_bw() +
  labs(x = "Mean August TDO3", y = "Minimum volumetric habitat") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(cum.vol_max.tdo3, aes(x = tdo3_avg_biweekly, y = volumetric_habitat_availability_do3_temp21_avg_biweekly)) +
  geom_point() +
  theme_bw() +
  labs(x = "Maximum TDO3", y = "Minimum volumetric habitat") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(cum.vol_max.tdo3, aes(x = mean_aug_tdo6, y = volumetric_habitat_availability_do3_temp21_avg_biweekly)) +
  geom_point() +
  theme_bw() +
  labs(x = "Mean August TDO6", y = "Minimum volumetric habitat") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(cum.vol_max.tdo3, aes(x = tdo6_avg_biweekly, y = volumetric_habitat_availability_do3_temp21_avg_biweekly)) +
  geom_point() +
  theme_bw() +
  labs(x = "Maximum TDO6", y = "Minimum volumetric habitat") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")



greenwood.cum.stress <- Greenwood.Thresholds.Daily.Averages %>%
  group_by(year) %>%
  summarise(max_cum_stress_tdo6 = max(gdd_tdo6_cumulative))

cum.vol_max.tdo3 <- vhab_vs_mean_aug %>%
  inner_join(y = greenwood.cum.stress, by = join_by(year))

ggplot(cum.vol_max.tdo3, aes(x = mean_aug_tdo3, y = max_cum_stress_tdo6)) +
  geom_point() +
  theme_bw() +
  labs(x = "Mean August TDO3", y = "Maximum cumulative stress") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(cum.vol_max.tdo3, aes(x = tdo3_avg_biweekly, y = max_cum_stress_tdo6)) +
  geom_point() +
  theme_bw() +
  labs(x = "Maximum TDO3", y = "Maximum cumulative stress") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(cum.vol_max.tdo3, aes(x = mean_aug_tdo6, y = max_cum_stress_tdo6)) +
  geom_point() +
  theme_bw() +
  labs(x = "Mean August TDO6", y = "Maximum cumulative stress") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")

ggplot(cum.vol_max.tdo3, aes(x = tdo6_avg_biweekly, y = max_cum_stress_tdo6)) +
  geom_point() +
  theme_bw() +
  labs(x = "Maximum TDO6", y = "Maximum cumulative stress") +
  geom_smooth(method = "lm", se = FALSE) + # Optional trendline
  stat_cor(method = "pearson", label.x.npc = "middle", label.y.npc = "top")


#### do 6 temp 17 ----



### relative volumetric habitat ----

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = relative_volumetric_habitat_availability_do3_temp21_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Greenwood.Thresholds, aes(x = dummy_date_time, y = relative_volumetric_habitat_availability_do3_temp21_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "relative_volumetric_habitat_availability_do3_temp21_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### stress ----

Greenwood.Thresholds.Daily.Averages <- Greenwood.Thresholds.Daily.Averages %>%
  mutate(dummy_date = update(date, year = 2000),
         year = word(date, 1, 1, sep = "-"))

ggplot(Greenwood.Thresholds.Daily.Averages, aes(x = dummy_date, y = gdd_tdo6, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Greenwood.Thresholds.Daily.Averages, aes(x = dummy_date, y = gdd_tdo6, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "gdd_tdo6"), aes(xintercept = dummy_date, colour = as.factor(year)))

#### cumulative ----

ggplot(Greenwood.Thresholds.Daily.Averages, aes(x = dummy_date, y = gdd_tdo6_cumulative, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0),
                   limits = c(as.POSIXct("2000-06-01"), as.POSIXct("2000-12-01"))) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink")) +
  labs(x = NULL, y = "CSDD", colour = "Year") +
  scale_y_continuous(limits = c(0, 250)) +
  theme(text = element_text(size = 14))

ggplot(Greenwood.Thresholds.Daily.Averages, aes(x = dummy_date, y = gdd_tdo6_cumulative, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Max.Stress.Dates %>% filter(metric == "gdd_tdo6_cumulative"), aes(xintercept = dummy_date, colour = as.factor(year)))


### rate of change ----

#### tdo3 ----

Greenwood.Thresholds.ROCs.Roll.2Weeks <- Greenwood.Thresholds.ROCs.Roll.2Weeks %>%
  mutate(dummy_date_time = update(date_time_local, year = 2000),
         year = word(date_time_local, 1, 1, sep = "-"))

ggplot(Greenwood.Thresholds.ROCs.Roll.2Weeks, aes(x = dummy_date_time, y = tdo3, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  facet_wrap(vars(as.factor(year)))

ggplot(Greenwood.Thresholds.ROCs.Roll.2Weeks, aes(x = dummy_date_time, y = tdo3, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Greenwood.Thresholds.ROCs.Roll.2Weeks %>% filter(metric == "tdo3"), aes(xintercept = dummy_date_time, colour = as.factor(year)))

#### CLT 3 do 21 temp ----

ggplot(Greenwood.Thresholds.ROCs.Roll.2Weeks, aes(x = dummy_date_time, y = layer_thickness_do3_temp21, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

### weather ----

ggplot(Greenwood.Thresholds.2019, aes(x = date_time_local, y = tdo3)) +
  geom_line()
Weather.Data$doy <- yday(Weather.Data$date)
ggplot(Greenwood.Thresholds.Daily.Averages.ROCs, aes(x = doy, y = tdo3)) +
  geom_line() +
  theme_bw() +
  geom_col(data = Weather.Data %>% filter(lake == "Greenwood", year == 2019), aes(x = doy, y = precip_1day_in), fill = "red")

ggplot(Greenwood.Thresholds %>% filter(year == "2021"), aes(x = doy, y = tdo3)) +
  geom_line() +
  theme_bw() +
  geom_col(data = Weather.Data %>% filter(lake == "Greenwood", year == 2019), aes(x = doy, y = precip_1day_in), fill = "red")


mult <- 10
ggplot(Greenwood.Thresholds %>% filter(year == "2021"), aes(x = doy, y = tdo3)) +
  # Primary dataset (TDO3 line)
  geom_line(colour = "black", linewidth = 1) +
  
  # Secondary dataset (Precipitation bars) — multiply y by 'mult'
  geom_col(data = Weather.Data %>% filter(lake == "Greenwood", year == 2019), 
           aes(x = doy, y = precip_1day_in * mult), 
           fill = "red") +
  
  # Primary Y axis on left, transformed Secondary Y axis on right
  scale_y_continuous(
    name = "TDO3",
    sec.axis = sec_axis(~ . / mult, name = "1-Day Precipitation (in)")
  ) +
  
  labs(x = "Day of Year") +
  theme_bw()

mult <- 15
ggplot(Greenwood.Thresholds %>% filter(year == "2021"), aes(x = doy, y = tdo6)) +
  # Primary dataset (TDO3 line)
  geom_line(colour = "black", linewidth = 1) +
  
  # Secondary dataset (Precipitation bars) — multiply y by 'mult'
  geom_col(data = Weather.Data %>% filter(lake == "Greenwood", year == 2019), 
           aes(x = doy, y = precip_1day_in * mult), 
           fill = "red") +
  
  # Primary Y axis on left, transformed Secondary Y axis on right
  scale_y_continuous(
    name = "TDO6",
    sec.axis = sec_axis(~ . / mult, name = "1-Day Precipitation (in)")
  ) +
  
  labs(x = "Day of Year", title = "Greenwood 2021") +
  theme_bw()

Weather.Data$doy <- yday(Weather.Data$date)
ggplot(Greenwood.Thresholds.Daily.Averages.ROCs %>% filter(year == "2021"), aes(x = doy, y = tdo3)) +
  geom_line() +
  theme_bw() +
  geom_col(data = Weather.Data %>% filter(lake == "Greenwood", year == 2019), aes(x = doy, y = precip_1day_in), fill = "red")

mult <- 1
ggplot(Carlos.Thresholds %>% filter(year == "2018"), aes(x = doy, y = tdo6)) +
  # Primary dataset (TDO3 line)
  geom_line(colour = "black", linewidth = 1) +
  
  # Secondary dataset (Precipitation bars) — multiply y by 'mult'
  geom_col(data = Weather.Data %>% filter(lake == "Carlos", year == 2018), 
           aes(x = doy, y = precip_1day_in * mult), 
           fill = "red") +
  
  # Primary Y axis on left, transformed Secondary Y axis on right
  scale_y_continuous(
    name = "TDO6",
    sec.axis = sec_axis(~ . / mult, name = "1-Day Precipitation (in)")
  ) +
  
  labs(x = "Day of Year", title = "Carlos 2018") +
  theme_bw()


## Carlos ----

Carlos.Thresholds <- Carlos.Thresholds %>%
  #mutate(year = word(date_time_local, 1, 1, sep = "-"),
  #       moy = month(date_time_local),
  #       day = day(date_time_local),
  #      doy = yday(date_time_local),
  #      woy = week(date_time_local),
  #      dummy_date_time = update(date_time_local, year = 2000)) %>%
  filter(year %in% c(2018, 2019))

Carlos.Max.Stress.Dates <- Carlos.Max.Stress.Dates %>%
  mutate(date = as.POSIXct(case_when(date == "NA" ~ NA,
                                     .default = date)),
         dummy_date = update(date, year = 2000))

### tdo3 ----

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = tdo3_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "TDO3", colour = "Year") +
  scale_y_continuous(breaks = 0:11, limits = c(0, 11)) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = tdo3_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Carlos.Max.Stress.Dates %>% filter(metric == "tdo3_avg_biweekly", year != "2020"), aes(xintercept = dummy_date, colour = as.factor(year)), linewidth = 1) +
  labs(x = NULL, y = "TDO3", colour = "Year") +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_y_continuous(breaks = 0:11, limits = c(0, 11)) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

### tdo6 ----

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = tdo6_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "TDO6", colour = "Year") +
  scale_y_continuous(breaks = 0:17, limits = c(0, 17)) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_color_manual(values = c("2018" = "red", "2019" = "blue"))

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = tdo6_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Carlos.Max.Stress.Dates %>% filter(metric == "tdo6_avg_biweekly", year %in% c("2019", "2018")), aes(xintercept = dummy_date, colour = as.factor(year)), linewidth = 1) +
  labs(x = NULL, y = "TDO6", colour = "Year") +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_y_continuous(breaks = 0:17, limits = c(0, 17)) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

### tdo1 ----

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = tdo1_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = tdo1_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Carlos.Max.Stress.Dates %>% filter(metric == "tdo1_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### CLT 6 do 22.8 temp ----

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = CLT_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "CLT", colour = "Year") +
  ylim(c(0, 63)) +
  geom_hline(aes(yintercept = 49), linetype = 2) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

### CLT 6 do 17 temp ----

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do6_temp17_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "CLT", colour = "Year") +
  ylim(c(0, 63)) +
  geom_hline(aes(yintercept = 49), linetype = 2) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(r = 0, l = 1, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do6_temp17_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Carlos.Max.Stress.Dates %>% filter(metric == "layer_thickness_do6_temp17_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### CLT 3 do 21 temp ----

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do3_temp21_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "CLT", colour = "Year") +
  ylim(c(0, 63)) +
  geom_hline(aes(yintercept = 49), linetype = 2) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(r = 0, l = 10, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do3_temp21_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Carlos.Max.Stress.Dates %>% filter(metric == "layer_thickness_do3_temp21_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### CLT 1 do 25 temp ----

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do1_temp25_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do1_temp25_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Carlos.Max.Stress.Dates %>% filter(metric == "layer_thickness_do3_temp21_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### total volumetric habitat ----

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = volumetric_habitat_CLT_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "VHA", colour = "Year") +
  geom_hline(aes(yintercept = max(volumetric_habitat_CLT_avg_biweekly)), linetype = 2) +
  scale_y_continuous(limits = c(0, max(Tenmile.Thresholds$volumetric_habitat_CLT_avg_biweekly))) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(r = 0, l = 10, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = volumetric_habitat_availability_do3_temp21_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Carlos.Max.Stress.Dates %>% filter(metric == "volumetric_habitat_availability_do3_temp21_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### relative volumetric habitat ----

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = relative_volumetric_habitat_availability_do3_temp21_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Carlos.Thresholds, aes(x = dummy_date_time, y = relative_volumetric_habitat_availability_do3_temp21_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Carlos.Max.Stress.Dates %>% filter(metric == "relative_volumetric_habitat_availability_do3_temp21_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### stress ----

Carlos.Thresholds.Daily.Averages <- Carlos.Thresholds.Daily.Averages %>%
  mutate(dummy_date = update(date, year = 2000),
         year = word(date, 1, 1, sep = "-"))

ggplot(Carlos.Thresholds.Daily.Averages, aes(x = dummy_date, y = gdd_tdo6, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Carlos.Thresholds.Daily.Averages, aes(x = dummy_date, y = gdd_tdo6, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Carlos.Max.Stress.Dates %>% filter(metric == "gdd_tdo6"), aes(xintercept = dummy_date, colour = as.factor(year)))

#### cumulative ----

ggplot(Carlos.Thresholds.Daily.Averages, aes(x = dummy_date, y = gdd_tdo6_cumulative, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0),
                   limits = c(as.POSIXct("2000-06-01"), as.POSIXct("2000-12-01"))) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink")) +
  labs(x = NULL, y = "Cumulative SDD (TDO6)", colour = "Year") +
  theme(text = element_text(size = 14)) +
  scale_y_continuous(limits = c(0, 250))

ggplot(Carlos.Thresholds.Daily.Averages, aes(x = dummy_date, y = gdd_tdo6_cumulative, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Carlos.Max.Stress.Dates %>% filter(metric == "gdd_tdo6_cumulative"), aes(xintercept = dummy_date, colour = as.factor(year)))


## Ten Mile ----

Tenmile.Thresholds <- Tenmile.Thresholds %>%
  #mutate(year = word(date_time_local, 1, 1, sep = "-"),
  #       moy = month(date_time_local),
  #       day = day(date_time_local),
  #       doy = yday(date_time_local),
  #       woy = week(date_time_local),
  #       dummy_date_time = update(date_time_local, year = 2000))
  filter(year %in% c("2020", "2021"))

Tenmile.Max.Stress.Dates <- Tenmile.Max.Stress.Dates %>%
  mutate(date = as.POSIXct(case_when(date == "NA" ~ NA,
                                     .default = date)),
         dummy_date = update(date, year = 2000))

### tdo3 ----

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = tdo3_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "TDO3", colour = "Year") +
  scale_y_continuous(breaks = 0:11, limits = c(0, 11)) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = tdo3_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Tenmile.Max.Stress.Dates %>% filter(metric == "tdo3_avg_biweekly", year %in% c("2020", "2021")), aes(xintercept = dummy_date, colour = as.factor(year)), linewidth = 1) +
  labs(x = NULL, y = "TDO3", colour = "Year") +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_y_continuous(breaks = 0:11, limits = c(0, 11)) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))


### tdo6 ----

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = tdo6_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  labs(x = NULL, y = "TDO3", colour = "Year") +
  scale_y_continuous(breaks = 0:17, limits = c(0,17)) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_colour_manual(values = c("2020" = "darkgreen", "2021" = "purple"))

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = tdo6_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Tenmile.Max.Stress.Dates %>% filter(metric == "tdo6_avg_biweekly", year %in% c("2020", "2021")), aes(xintercept = dummy_date, colour = as.factor(year)), linewidth = 1) +
  labs(x = NULL, y = "TDO6", colour = "Year") +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_y_continuous(breaks = 0:17, limits = c(0, 17)) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

### tdo1 ----

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = tdo1_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = tdo1_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Tenmile.Max.Stress.Dates %>% filter(metric == "tdo1_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### CLT 6 do 22.8 temp ----

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = CLT_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  ylim(c(0, 63)) +
  geom_hline(aes(yintercept = 63), linetype = 2) +
  labs(x = NULL, y = "CLT", colour = "Year") +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

### CLT 6 do 17 temp ----

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do6_temp17_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  ylim(c(0, 63)) +
  geom_hline(aes(yintercept = 63), linetype = 2) +
  labs(x = NULL, y = "CLT", colour = "Year") +
  theme(text = element_text(size = 14),
        legend.position = "none",
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do6_temp17_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Tenmile.Max.Stress.Dates %>% filter(metric == "layer_thickness_do6_temp17_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### CLT 3 do 21 temp ----

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do3_temp21_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  ylim(c(0, 63)) +
  geom_hline(aes(yintercept = 63), linetype = 2) +
  labs(x = NULL, y = "CLT", colour = "Year") +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(l = 10, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do3_temp21_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Tenmile.Max.Stress.Dates %>% filter(metric == "layer_thickness_do3_temp21_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### CLT 1 do 25 temp ----

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do1_temp25_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = layer_thickness_do1_temp25_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Tenmile.Max.Stress.Dates %>% filter(metric == "layer_thickness_do1_temp25_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### total volumetric habitat ----

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = volumetric_habitat_CLT_avg_biweekly, colour = as.factor(year))) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_hline(aes(yintercept = max(volumetric_habitat_CLT_avg_biweekly)), linetype = 2) +
  scale_y_continuous(limits = c(0, max(Tenmile.Thresholds$volumetric_habitat_CLT_avg_biweekly))) +
  theme(text = element_text(size = 14),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = margin(r = 0, l = 10, unit = "pt")) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink"))

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = volumetric_habitat_availability_do3_temp21_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Tenmile.Max.Stress.Dates %>% filter(metric == "volumetric_habitat_availability_do3_temp21_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

### relative volumetric habitat ----

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = relative_volumetric_habitat_availability_do3_temp21_avg_biweekly, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0))

ggplot(Tenmile.Thresholds, aes(x = dummy_date_time, y = relative_volumetric_habitat_availability_do3_temp21_avg_biweekly, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Tenmile.Max.Stress.Dates %>% filter(metric == "relative_volumetric_habitat_availability_do3_temp21_avg_biweekly"), aes(xintercept = dummy_date, colour = as.factor(year)))

#### cumulative ----

Tenmile.Thresholds.Daily.Averages <- Tenmile.Thresholds.Daily.Averages %>%
  mutate(dummy_date = update(date, year = 2000),
         year = word(date, 1, 1, sep = "-"))

ggplot(Tenmile.Thresholds.Daily.Averages, aes(x = dummy_date, y = gdd_tdo6_cumulative, colour = as.factor(year))) +
  geom_line() +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0),
                   limits = c(as.POSIXct("2000-06-01"), as.POSIXct("2000-12-01"))) +
  scale_colour_manual(values = c("2018" = "red", "2019" = "blue", "2020" = "darkgreen", "2021" = "purple", "2022" = "orange", "2023" = "lightblue", "2024" = "pink")) +
  labs(x = NULL, y = "Cumulative SDD (TDO6)", colour = "Year") +
  theme(text = element_text(size = 14))# +
  #scale_y_continuous(limits = c(0, 250))

ggplot(Tenmile.Thresholds.Daily.Averages, aes(x = dummy_date, y = gdd_tdo6_cumulative, group = as.factor(year))) +
  geom_line(colour = "black") +
  theme_bw() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0, 0)) +
  geom_vline(data = Tenmile.Max.Stress.Dates %>% filter(metric == "gdd_tdo6_cumulative"), aes(xintercept = dummy_date, colour = as.factor(year)))















