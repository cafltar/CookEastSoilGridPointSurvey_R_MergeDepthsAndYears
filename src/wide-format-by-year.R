library(tidyverse)

df <- read_csv("output/soilCore1998To2015ShallowDeepMergedByHorizon_20180926.csv") %>% 
  mutate(dC13 = as.numeric(dC13)) %>% 
  mutate(TCConc = case_when(
    !is.na(TCConcAcidWashed) ~ TCConcAcidWashed,
    is.na(TCConcAcidWashed) ~ TCConc)) %>% 
  mutate(TNConc = case_when(
    !is.na(TNConcAcidWashed) ~ TNConcAcidWashed,
    is.na(TNConcAcidWashed) ~ TNConc)) %>%
  mutate(dC13 = case_when(
    !is.na(dC13AcidWashed) ~ dC13AcidWashed,
    is.na(dC13AcidWashed) ~ dC13)) %>%
  mutate(CN = TCConc / TNConc)

carbon <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, TCConc, Horizon) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  mutate(key = row_number()) %>% 
  spread(Year, TCConc) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  fill(-ID2, TopDepth, BottomDepth) %>% 
  mutate(maxKey = max(key)) %>% 
  filter(key == maxKey) %>% 
  select(-key, -maxKey) %>% 
  rename("c1998" = "1998", "c2008" = "2008", "c2015" = "2015")

nitrogen <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, TNConc, Horizon) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  mutate(key = row_number()) %>% 
  spread(Year, TNConc) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  fill(-ID2, TopDepth, BottomDepth) %>% 
  mutate(maxKey = max(key)) %>% 
  filter(key == maxKey) %>% 
  select(-key, -maxKey) %>% 
  rename("n1998" = "1998", "n2008" = "2008", "n2015" = "2015")

d13C <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, dC13, Horizon) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  mutate(key = row_number()) %>% 
  spread(Year, dC13) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  fill(-ID2, TopDepth, BottomDepth) %>% 
  mutate(maxKey = max(key)) %>% 
  filter(key == maxKey) %>% 
  select(-key, -maxKey) %>% 
  rename("d13C1998" = "1998", "d13C2008" = "2008", "d13C2015" = "2015")

pH <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, pH, Horizon) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  mutate(key = row_number()) %>% 
  spread(Year, pH) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  fill(-ID2, TopDepth, BottomDepth) %>% 
  mutate(maxKey = max(key)) %>% 
  filter(key == maxKey) %>% 
  select(-key, -maxKey) %>% 
  rename("pH1998" = "1998", "pH2008" = "2008", "pH2015" = "2015")

bulkDensity <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, BulkDensity, Horizon) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  mutate(key = row_number()) %>% 
  spread(Year, BulkDensity) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  fill(-ID2, TopDepth, BottomDepth) %>% 
  mutate(maxKey = max(key)) %>% 
  filter(key == maxKey) %>% 
  select(-key, -maxKey) %>% 
  rename("BD1998" = "1998", "BD2008" = "2008", "BD2015" = "2015")

CN <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, CN, Horizon) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  mutate(key = row_number()) %>% 
  spread(Year, CN) %>% 
  group_by(ID2, TopDepth, BottomDepth) %>% 
  fill(-ID2, TopDepth, BottomDepth) %>% 
  mutate(maxKey = max(key)) %>% 
  filter(key == maxKey) %>% 
  select(-key, -maxKey) %>% 
  rename("CN1998" = "1998", "CN2008" = "2008", "CN2015" = "2015")

join.by <- c("ID2", "TopDepth", "BottomDepth", "Latitude", "Longitude", "Horizon")
wide <- carbon %>% 
  left_join(nitrogen, by = join.by) %>% 
  left_join(d13C, by = join.by) %>% 
  left_join(pH, by = join.by) %>% 
  left_join(bulkDensity, by = join.by) %>%
  left_join(CN, by = join.by)

dateToday <- format(Sys.Date(), "%Y%m%d")

write_csv(wide, paste("output/wideFormatByYear_", dateToday, ".csv", sep = ""), na = "")
