library(tidyverse)

df <- read_csv("output/soilCore1998To2015ShallowDeepMergedByHorizon_20180926.csv")

carbon <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, TCConc, TCConcAcidWashed) %>% 
  mutate(TCConc = case_when(
    !is.na(TCConcAcidWashed) ~ TCConcAcidWashed,
    is.na(TCConcAcidWashed) ~ TCConc)) %>% 
  select(-TCConcAcidWashed) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  mutate(key = row_number()) %>% 
  spread(Year, TCConc) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  fill(-ID2, TopDepth, BottomDepth) %>% 
  mutate(maxKey = max(key)) %>% 
  filter(key == maxKey) %>% 
  select(-key, -maxKey)

nitrogen <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, TNConc, TNConcAcidWashed) %>% 
  mutate(TNConc = case_when(
    !is.na(TNConcAcidWashed) ~ TNConcAcidWashed,
    is.na(TNConcAcidWashed) ~ TNConc)) %>% 
  select(-TNConcAcidWashed) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  mutate(key = row_number()) %>% 
  spread(Year, TNConc) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  fill(-ID2, TopDepth, BottomDepth) %>% 
  mutate(maxKey = max(key)) %>% 
  filter(key == maxKey) %>% 
  select(-key, -maxKey)

d13C <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, dC13, dC13AcidWashed) %>% 
  mutate(dC13 = as.double(dC13)) %>% 
  mutate(dC13 = case_when(
    !is.na(dC13AcidWashed) ~ dC13AcidWashed,
    is.na(dC13AcidWashed) ~ dC13)) %>% 
  select(-dC13AcidWashed) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  mutate(key = row_number()) %>% 
  spread(Year, dC13) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  fill(-ID2, TopDepth, BottomDepth) %>% 
  mutate(maxKey = max(key)) %>% 
  filter(key == maxKey) %>% 
  select(-key, -maxKey)

pH <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, pH) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  mutate(key = row_number()) %>% 
  spread(Year, pH) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  fill(-ID2, TopDepth, BottomDepth) %>% 
  mutate(maxKey = max(key)) %>% 
  filter(key == maxKey) %>% 
  select(-key, -maxKey)

bulkDensity <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, BulkDensity) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  mutate(key = row_number()) %>% 
  spread(Year, BulkDensity) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  fill(-ID2, TopDepth, BottomDepth) %>% 
  mutate(maxKey = max(key)) %>% 
  filter(key == maxKey) %>% 
  select(-key, -maxKey)

dateToday <- format(Sys.Date(), "%Y%m%d")

write_csv(carbon, paste("output/wideFormatByYearCarbon_", dateToday, ".csv", sep = ""))
write_csv(nitrogen, paste("output/wideFormatByYearNitrogen_", dateToday, ".csv", sep = ""))
write_csv(d13C, paste("output/wideFormatByYeardC13_", dateToday, ".csv", sep = ""))
write_csv(pH, paste("output/wideFormatByYearpH_", dateToday, ".csv", sep = ""))
write_csv(bulkDensity, paste("output/wideFormatByYearBulkDensity_", dateToday, ".csv", sep = ""))