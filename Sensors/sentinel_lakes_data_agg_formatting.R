### Aggregating and standardizing sentinel lakes data, creating parquet file ###
# Input: Cleaned QA/QC'd sentinel lakes data (all lakes)
# Output: Parquet file of standardized data of all sentinel lakes

library(tidyverse)
library(readxl)
library(arrow)

# Read in data
Data <- read.csv("~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/TDO sensors/Sentinel Lakes/do_data.csv", stringsAsFactors = F)
# Read in helper files
Metadata <- read_xlsx("~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/TDO sensors/TDO Sensor Data Column Names and Descriptions.xlsx")
Xwalk <- read.csv("~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/Helper/mndow_nhdhr_xwalk.csv", stringsAsFactors = F)
Xwalk <- Xwalk %>%
  mutate(MNDOW_ID = word(Xwalk$MNDOW_ID, 2, 2, sep = "_"),
         site_id = word(Xwalk$site_id, 2, 2, sep = "_")) %>%
  rename(NHDHR_ID = "site_id")

# Format data ----
#   Follow formatting in "TDO Sensor Data Column Names and Descriptions.gsheet" in Data/TDO sensors folder

Data.Formatted <- Data %>%
  rename(lake_name = "basin_name",
         lake_id_source = "basin_id",
         date_time = "standardized_date_time") %>%
  mutate(lake_id_source = as.character(lake_id_source),
         lake_id_state = as.character(lake_id_source), # in this case the lake id from the source org is the same as the lake id of the state
         date_time_cst = case_when(nchar(date_time) == 10 ~ paste(date_time, "00:00:00", sep = " "), .default = date_time), # the original data doesn't have 00:00:00 included in the date time column for measurements taken at midnight so need to add that in for those measurements
         date_time_cst = ymd_hms(date_time_cst, tz = "Etc/GMT+6"), # convert original date times to POSIXct format
         date_time_utc = with_tz(date_time_cst, tzone = "UTC"), # convert to UTC to avoid issues with DST
         date_time_local = with_tz(date_time_utc, tzone = "America/Chicago"), # convert back to central time, but this accounts for shifting times due to DST
         source = "Sentinel Lakes",
         state = "MN",
         station = NULL,
         serial_number = as.character(serial_number)) %>%
  left_join(Xwalk, by = join_by(lake_id_state == MNDOW_ID)) %>%
  rename(lake_id_nhdhr = "NHDHR_ID") %>%
  arrange(lake_name, date_time_local, depth_m)

column.names <- Metadata$fields_in_all_parquet
Data.Formatted <- Data.Formatted[, column.names]

# Save formatted data as a parquet ----

write_parquet(Data.Formatted, "~/Google Drive/Shared drives/Hansen Lab/RESEARCH PROJECTS/LCCMR Coldwater Habitat Watersheds - MM/Data/TDO sensors/Hive/source-MN_DNR_Sentinel_Lakes.parquet")














