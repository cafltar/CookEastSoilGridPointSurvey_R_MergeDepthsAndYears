library(tidyverse)

df <- read_csv("output/soilCore1998To2015ShallowDeepMergedByHorizon_20180926.csv")

pH.wide.horizon <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, Horizon) %>%
  spread(Year, Horizon) %>% 
  rename(Horizon1998 = `1998`,
         Horizon2008 = `2008`,
         Horizon2015 = `2015`)

pH.wide.pH <- df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, 
         BottomDepth, pH) %>%
  spread(Year, pH) %>% 
  rename(pH1998 = `1998`,
         pH2008 = `2008`,
         pH2015 = `2015`)

wide <- full_join(pH.wide.horizon, pH.wide.pH, by = c("ID2", 
                                                      "Latitude", 
                                                      "Longitude", 
                                                      "TopDepth", 
                                                      "BottomDepth")) %>% 
  select(ID2, Latitude, Longitude, TopDepth, BottomDepth,
         Horizon1998, pH1998,
         Horizon2008, pH2008,
         Horizon2015, pH2015) %>% 
  arrange(by = ID2)

dateToday <- format(Sys.Date(), "%Y%m%d")

write_csv(wide, paste("output/horizon-pH-wide_", dateToday, ".csv", sep = ""), na = "")
