# This code is a modification of Mike V's code FullMNScript_hmr.R (on github: https://github.com/FisheriesEcologyLab/VHOD/blob/main/data%26scripts/scripts/archive/FullMNScript_hmr.R)
# Didn't actually end up using to calculate TDO3 because this script was not originally written for that
# purpose, so used it to help write compile_clean_MN_profiles.R
# 
# Modified by Mike M.


library("lubridate")
library("zoo")
library("rLakeAnalyzer")
library("broom")
library("plotly")
library("skimr")

profiles <- read.csv('MN_profiles.csv')

#Filter the dataset
filter1.mn <- profiles %>% #Remove Impossible Values
  mutate(Location_Year = as.character(paste(monitoringlocationidentifier,year, sep = "/")),
         Profile = paste(monitoringlocationidentifier, year, doy, sep = "/")) %>%
  filter(temp_c < 40) #Jane et al 2021

rm(profiles)

filter2.mn = filter1.mn %>%            #Profiles must have a minimum depth less than 3 meters; 
  group_by(Profile)%>%                 #otherwise the profiles shallowest read could be in 
  summarise(min.depth = min(depth_m))%>% #the metalimnion and capture none of the epilimnion
  filter(min.depth < 3) #Jane et al 2021

filter3.mn = filter1.mn %>%
  semi_join(filter2.mn, by = "Profile") 

filter4.mn = filter3.mn %>%
  filter(doy >= 121 &
           doy <= 258)%>%
  group_by(year, monitoringlocationidentifier)%>% 
  summarise(Late=max(doy))%>%
  filter(Late >= 196 & #this adds that the last sample was taken between DOY 196 and 258
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(monitoringlocationidentifier,year, sep = "/"))) 

#  Put it all together
filter5.mn = filter3.mn %>%
  semi_join(filter4.mn, by = "Location_Year") %>% 
  filter(doy >= 121 &
           doy <= 258) #Removes profiles where it may have been sampled while ice is on or during turnover

# Remove Lake Superior Sites, Mines, and Pits ~ We only want Glacial Lakes
filter6.mn = filter5.mn %>%
  filter(monitoringlocationidentifier != "EPA_GLNPO-SU19") %>% #Lake Superior Site
  filter(monitoringlocationidentifier != "EPA_GLNPO-SU18") %>% #Lake Superior Site
  filter(monitoringlocationidentifier != "EPA_GLNPO-SU104550")%>% #Lake Superior Site
  filter(monitoringlocationidentifier != "21GPBCH-LS_PT_1") %>% #Lake Superior Site
  filter(monitoringlocationidentifier != "21GPBCH-LS_PT_9") %>% #Lake Superior Site
  filter(monitoringlocationidentifier != "GPC5_WQX-LS_PT_1") %>% #Lake Superior Site
  filter(monitoringlocationidentifier != "GPC5_WQX-LS_PT_9") %>% #Lake Superior Site
  filter(monitoringlocationidentifier != "GPC5_WQX-LS_PT_8") %>% #Lake Superior Site
  filter(monitoringlocationidentifier != "MNPCA-69-1297-00-202") %>% #Sherman Pit
  filter(monitoringlocationidentifier != "MNPCA-69-0429-00-201") %>% #Sabin Pit
  filter(monitoringlocationidentifier != "21GPBCH-LS_PT_8") %>%      #Lake Superior Site
  filter(monitoringlocationidentifier != "MNPCA-18-0440-01-100") %>% #Mahnomen Mine
  filter(monitoringlocationidentifier != "MNPCA-69-1297-00-201") %>% #Sherman Pit
  filter(monitoringlocationidentifier != "MNPCA-18-0439-00-100") %>% #Pennington Mine
  filter(monitoringlocationidentifier != "MNPCA-31-1325-04-201") %>% #Canisteo Mine Pit
  filter(monitoringlocationidentifier != "MNPCA-69-1298-00-203") %>% #South Twin City Pit
  filter(monitoringlocationidentifier != "MNPCA-18-0093-01-202") %>% #Rabbit Lake
  filter(monitoringlocationidentifier != "MNPCA-18-0041-01-201") %>% #Crooked Lake - Silver Bay
  filter(monitoringlocationidentifier != "MNPCA-69-0429-00-100") %>% #Embarrass Mine
  filter(monitoringlocationidentifier != "MNPCA-69-0428-00-201") %>% #St. James Pit
  filter(state_ident != "18044001")%>%
  filter(state_ident != "69129700")%>%
  filter(state_ident != "18043900")%>%
  filter(state_ident != "31132504")%>%
  filter(state_ident != "69129800")%>%
  filter(state_ident != "18009301")%>%
  filter(state_ident != "18004101")%>%
  filter(state_ident != "69042900")%>%
  filter(state_ident != "69042800")

filter7.1mn = filter6.mn %>%            #Profiles must have at least 3 reads in them
  group_by(Profile)%>%
  summarise(n_depths = n_distinct(depth_m))%>%
  filter(n_depths >= 3) #Jane et al 2021

filter7.mn = filter6.mn %>%
  semi_join(filter7.1mn, by = "Profile")

filter8.mn = filter7.mn %>%
  group_by(Profile)%>%
  filter(!any(duplicated(depth_m))) %>% # remove profiles that have more than 1 measurement of temp and/or DO for any depth
  mutate(Meta = list(meta.depths(wtr = temp_c, depths = depth_m, slope = 0.1, seasonal = F)))%>%
  separate(Meta, into = c("top.meta", "top.hypo"), sep = ",")%>%
  mutate(top.meta = gsub("[^0-9.-]", "", top.meta),
         top.hypo = as.numeric(gsub("[^0-9.-]", "", top.hypo)),
         Depth = as.numeric(depth_m))%>%
  drop_na(top.hypo)%>%  #Jane et al 2021
  filter(top.hypo != "0")

filter9.1mn = filter8.mn %>%
  filter(doy >= 121 &
           doy <= 258)%>%
  group_by(year, monitoringlocationidentifier)%>% 
  summarise(Late=max(doy))%>%
  filter(Late >= 196 & #and this adds that the last sample was taken between DOY 196 and 258
           Late <= 258)%>%
  mutate(Location_Year = as.character(paste(monitoringlocationidentifier,year, sep = "/")))
filter9.mn = filter8.mn %>%
  semi_join(filter9.1mn, by = "Location_Year")%>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))

filter10.1mn = filter9.mn %>%
  group_by(state_ident, monitoringlocationidentifier)%>%
  summarise(n = n())%>%
  group_by(state_ident)%>%
  mutate(MostSampled = max(n))%>%
  ungroup()%>%
  filter(n == MostSampled)%>%
  distinct(state_ident, .keep_all = T)

MN.Filtered.Obs = filter9.mn %>%
  semi_join(filter10.1mn)

#  Clean
rm(filter9.1mn, filter9.mn, filter8.mn, filter7.mn, filter7.1mn, filter6.mn,
   filter5.mn, filter4.mn, filter3.mn, filter2.mn, filter1.mn, filter10.1mn)

### The next step is to interpolate data at 1 m increments
Depth.df = tibble(Depth = 0:90) #This creates a data frame to make sure we have data for the bottom of larger lakes

#  First DO

# try doing it in chunks with multiple data frames
MN.Filtered.Obs$profile_number <- NA
num <- 1:length(unique(MN.Filtered.Obs$profile_ident))
i <- 1
for (profile in unique(MN.Filtered.Obs$profile_ident)) {
  MN.Filtered.Obs$profile_number[MN.Filtered.Obs$profile_ident == profile] <- num[i]
  i <- i+1
}
MN.Filtered.Obs <- ungroup(MN.Filtered.Obs)
MN.Filtered.Obs.1 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% 1:floor(length(num)/7),]
Wide.MN.DO.Obs.1 <- MN.Filtered.Obs.1 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, do_ppm) %>%
  pivot_wider(names_from = ID, values_from = do_ppm, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.1)
MN.Filtered.Obs.2 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)+1):(floor(length(num)/7)*2),]
Wide.MN.DO.Obs.2 <- MN.Filtered.Obs.2 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, do_ppm) %>%
  pivot_wider(names_from = ID, values_from = do_ppm, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.2)
MN.Filtered.Obs.3 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)*2+1):(floor(length(num)/7)*3),]
Wide.MN.DO.Obs.3 <- MN.Filtered.Obs.3 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, do_ppm) %>%
  pivot_wider(names_from = ID, values_from = do_ppm, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.3)
MN.Filtered.Obs.4 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)*3+1):(floor(length(num)/7)*4),]
Wide.MN.DO.Obs.4 <- MN.Filtered.Obs.4 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, do_ppm) %>%
  pivot_wider(names_from = ID, values_from = do_ppm, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.4)
MN.Filtered.Obs.5 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)*4+1):(floor(length(num)/7)*5),]
Wide.MN.DO.Obs.5 <- MN.Filtered.Obs.5 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, do_ppm) %>%
  pivot_wider(names_from = ID, values_from = do_ppm, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.5)
MN.Filtered.Obs.6 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)*5+1):(floor(length(num)/7)*6),]
Wide.MN.DO.Obs.6 <- MN.Filtered.Obs.6 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, do_ppm) %>%
  pivot_wider(names_from = ID, values_from = do_ppm, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.6)
MN.Filtered.Obs.7 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)*6+1):length(num),]
Wide.MN.DO.Obs.7 <- MN.Filtered.Obs.7 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, do_ppm) %>%
  pivot_wider(names_from = ID, values_from = do_ppm, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.7)

#  Interpolate
Wide.Inter.MN.DO.1 = na.approx(Wide.MN.DO.Obs.1, x = Wide.MN.DO.Obs.1$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.DO.Obs.1)
Wide.Inter.MN.DO.2 = na.approx(Wide.MN.DO.Obs.2, x = Wide.MN.DO.Obs.2$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.DO.Obs.2)
Wide.Inter.MN.DO.3 = na.approx(Wide.MN.DO.Obs.3, x = Wide.MN.DO.Obs.3$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.DO.Obs.3)
Wide.Inter.MN.DO.4 = na.approx(Wide.MN.DO.Obs.4, x = Wide.MN.DO.Obs.4$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.DO.Obs.4)
Wide.Inter.MN.DO.5 = na.approx(Wide.MN.DO.Obs.5, x = Wide.MN.DO.Obs.5$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.DO.Obs.5)
Wide.Inter.MN.DO.6 = na.approx(Wide.MN.DO.Obs.6, x = Wide.MN.DO.Obs.6$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.DO.Obs.6)
Wide.Inter.MN.DO.7 = na.approx(Wide.MN.DO.Obs.7, x = Wide.MN.DO.Obs.7$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.DO.Obs.7)

#  Pivot Long
Long.Inter.MN.DO.1 = Wide.Inter.MN.DO.1 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.DO.1)
Long.Inter.MN.DO.2 = Wide.Inter.MN.DO.2 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.DO.2)
Long.Inter.MN.DO.3 = Wide.Inter.MN.DO.3 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.DO.3)
Long.Inter.MN.DO.4 = Wide.Inter.MN.DO.4 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.DO.4)
Long.Inter.MN.DO.5 = Wide.Inter.MN.DO.5 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.DO.5)
Long.Inter.MN.DO.6 = Wide.Inter.MN.DO.6 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.DO.6)
Long.Inter.MN.DO.7 = Wide.Inter.MN.DO.7 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "DO", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.DO.7)

# combine them all
Long.Inter.MN.DO <- rbind(Long.Inter.MN.DO.1, Long.Inter.MN.DO.2, Long.Inter.MN.DO.3, Long.Inter.MN.DO.4, Long.Inter.MN.DO.5, Long.Inter.MN.DO.6, Long.Inter.MN.DO.7)
rm(Long.Inter.MN.DO.1, Long.Inter.MN.DO.2, Long.Inter.MN.DO.3, Long.Inter.MN.DO.4, Long.Inter.MN.DO.5, Long.Inter.MN.DO.6, Long.Inter.MN.DO.7)

### Next is Temperature
#  Pivot Wide
MN.Filtered.Obs.1 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% 1:floor(length(num)/7),]
Wide.MN.Temp.Obs.1 <- MN.Filtered.Obs.1 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, temp_c) %>%
  pivot_wider(names_from = ID, values_from = temp_c, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.1)
MN.Filtered.Obs.2 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)+1):(floor(length(num)/7)*2),]
Wide.MN.Temp.Obs.2 <- MN.Filtered.Obs.2 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, temp_c) %>%
  pivot_wider(names_from = ID, values_from = temp_c, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.2)
MN.Filtered.Obs.3 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)*2+1):(floor(length(num)/7)*3),]
Wide.MN.Temp.Obs.3 <- MN.Filtered.Obs.3 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, temp_c) %>%
  pivot_wider(names_from = ID, values_from = temp_c, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.3)
MN.Filtered.Obs.4 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)*3+1):(floor(length(num)/7)*4),]
Wide.MN.Temp.Obs.4 <- MN.Filtered.Obs.4 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, temp_c) %>%
  pivot_wider(names_from = ID, values_from = temp_c, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.4)
MN.Filtered.Obs.5 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)*4+1):(floor(length(num)/7)*5),]
Wide.MN.Temp.Obs.5 <- MN.Filtered.Obs.5 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, temp_c) %>%
  pivot_wider(names_from = ID, values_from = temp_c, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.5)
MN.Filtered.Obs.6 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)*5+1):(floor(length(num)/7)*6),]
Wide.MN.Temp.Obs.6 <- MN.Filtered.Obs.6 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, temp_c) %>%
  pivot_wider(names_from = ID, values_from = temp_c, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.6)
MN.Filtered.Obs.7 <- MN.Filtered.Obs[MN.Filtered.Obs$profile_number %in% (floor(length(num)/7)*6+1):length(num),]
Wide.MN.Temp.Obs.7 <- MN.Filtered.Obs.7 %>%
  mutate(ID = paste(monitoringlocationidentifier, year, doy, lake_maxdepth_m, sep = "/"))%>%
  select(ID, Depth, temp_c) %>%
  pivot_wider(names_from = ID, values_from = temp_c, values_fn = mean)%>%
  full_join(Depth.df)
rm(MN.Filtered.Obs.7, Depth.df)

# Interpolate
Wide.Inter.MN.Temp.1 = na.approx(Wide.MN.Temp.Obs.1, x = Wide.MN.Temp.Obs.1$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.Temp.Obs.1)
Wide.Inter.MN.Temp.2 = na.approx(Wide.MN.Temp.Obs.2, x = Wide.MN.Temp.Obs.2$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.Temp.Obs.2)
Wide.Inter.MN.Temp.3 = na.approx(Wide.MN.Temp.Obs.3, x = Wide.MN.Temp.Obs.3$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.Temp.Obs.3)
Wide.Inter.MN.Temp.4 = na.approx(Wide.MN.Temp.Obs.4, x = Wide.MN.Temp.Obs.4$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.Temp.Obs.4)
Wide.Inter.MN.Temp.5 = na.approx(Wide.MN.Temp.Obs.5, x = Wide.MN.Temp.Obs.5$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.Temp.Obs.5)
Wide.Inter.MN.Temp.6 = na.approx(Wide.MN.Temp.Obs.6, x = Wide.MN.Temp.Obs.6$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.Temp.Obs.6)
Wide.Inter.MN.Temp.7 = na.approx(Wide.MN.Temp.Obs.7, x = Wide.MN.Temp.Obs.7$Depth)%>% #interpolates data
  na.locf()%>% #fills out the rest of the lake profile with the previous value
  na.locf(fromLast = T)%>%
  as_tibble()
rm(Wide.MN.Temp.Obs.7)

#  Pivot Long
Long.Inter.MN.Temp.1 = Wide.Inter.MN.Temp.1 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.Temp.1)
Long.Inter.MN.Temp.2 = Wide.Inter.MN.Temp.2 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.Temp.2)
Long.Inter.MN.Temp.3 = Wide.Inter.MN.Temp.3 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.Temp.3)
Long.Inter.MN.Temp.4 = Wide.Inter.MN.Temp.4 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.Temp.4)
Long.Inter.MN.Temp.5 = Wide.Inter.MN.Temp.5 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.Temp.5)
Long.Inter.MN.Temp.6 = Wide.Inter.MN.Temp.6 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.Temp.6)
Long.Inter.MN.Temp.7 = Wide.Inter.MN.Temp.7 %>% #pivots to a longer format
  pivot_longer(cols = -"Depth",
               names_to = "ID", values_to = "Temperature", values_drop_na = T)%>%
  separate(ID, into = c("MonitoringLocationIdentifier", "Year", "DOY", "Max_Depth"), 
           sep = "/", remove = F)%>%
  filter(Depth <= Max_Depth)
rm(Wide.Inter.MN.Temp.7)

# combine them all
Long.Inter.MN.Temp <- rbind(Long.Inter.MN.Temp.1, Long.Inter.MN.Temp.2, Long.Inter.MN.Temp.3, Long.Inter.MN.Temp.4, Long.Inter.MN.Temp.5, Long.Inter.MN.Temp.6, Long.Inter.MN.Temp.7)
rm(Long.Inter.MN.Temp.1, Long.Inter.MN.Temp.2, Long.Inter.MN.Temp.3, Long.Inter.MN.Temp.4, Long.Inter.MN.Temp.5, Long.Inter.MN.Temp.6, Long.Inter.MN.Temp.7)

### Join our results together
MN.joined.inter = inner_join(Long.Inter.MN.Temp, Long.Inter.MN.DO)

#  We need to get the Lat and Lon
MN.Filtered.Obs <- MN.Filtered.Obs %>% rename(MonitoringLocationIdentifier = 'monitoringlocationidentifier')
MN.Depth.Specific = MN.Filtered.Obs %>%
  distinct(MonitoringLocationIdentifier, .keep_all = T)%>%
  select(c(MonitoringLocationIdentifier, latitude, longitude, state_ident))%>%
  inner_join(MN.joined.inter)

rm(Long.Inter.MN.DO, Long.Inter.MN.Temp, MN.joined.inter)

###  Calculate TDO3

#MN.TDO3 = MN.Depth.Specific %>%
#  filter(DO >= 3)%>%
#  group_by(MonitoringLocationIdentifier, Year)%>%
#  summarise(TDO3 = min(Temperature))

MN.TDO3 = MN.Depth.Specific %>%
  filter(DO >= 3)%>%
  group_by(ID)%>%
  filter(DO == min(DO)) %>%
  filter(Temperature == min(Temperature)) %>%
  filter(DO < 4) %>%
  mutate(unique = paste(ID, Temperature, DO, sep = '_')) %>%
  filter(!duplicated(unique))

# clean

rm(MN.Depth.Specific.weird, MN.Filtered.Obs.weird, MN.TDO3.2, MN.TDO3.2.Counts, MN.TDO3.Counts, MN.TDO3.Counts.2, MN.TDO3.Counts.2.Counts,
   MN.TDO3.Counts.Counts, MN.TDO3.weird)

# format and clean up data frame

MN.TDO3.final <- MN.TDO3 %>%
  ungroup() %>%
  rename(DOW = 'state_ident', TDO3 = 'Temperature') %>%
  mutate(Date = as.Date(as.numeric(DOY) - 1, origin = paste0(Year, '-01-01'))) %>%
  select(DOW, Year, Date, Depth, DO, TDO3)

write.csv(MN.TDO3.final, 'TDO3_New.csv', row.names = F)
















