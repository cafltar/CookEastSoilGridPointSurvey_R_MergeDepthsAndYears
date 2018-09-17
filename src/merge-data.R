library(tidyverse)
library(readxl)

# Load data ----



# Prepare 2015 ----
df2015 <- read_excel(
  "input/2015 soil samples  merge 0-30 and below profile data organization.xlsx", 
  sheet = "Merged Profile (2)",
  na = c("NA"))

clean15 <- df2015 %>% 
  select("Label__1",
    "top (cm)",
    "bottom(cm)",
    "ID2",
    "longitude",
    "latitude",
    "General Horizon",
    "Bulk density\r\n(g cm-3)",
    contains("VPDB"),
    "Total N%",
    "Total N%_acid washed",
    "Total C%",
    "Total C%_acid washed",
    "pH_soil:water=1:1)",
    "Carbon stocks (Mg ·ha-1)",
    "Accumulative C stocks(Mg ·ha-1)") %>% 
  rename("SampleId" = "Label__1",
    "TopDepth" = "top (cm)",
    "BottomDepth" = "bottom(cm)",
    "Longitude" = "longitude",
    "Latitude" = "latitude",
    "Horizon" = "General Horizon",
    "BulkDensity" = "Bulk density\r\n(g cm-3)",
    "C13" = 9,
    "C13AcidWashed" = 10,
    "TN" = "Total N%",
    "TNAcidWashed" = "Total N%_acid washed",
    "TC" = "Total C%",
    "TCAcidWashed" = "Total C%_acid washed",
    "pH" = "pH_soil:water=1:1)",
    "CStock" = "Carbon stocks (Mg ·ha-1)",
    "CStockAccum" = "Accumulative C stocks(Mg ·ha-1)")
  
# Prepare 1998-2008 data
df1998_2008 <- read_excel(
  "input/2017_03_10_Soil_Plant_Fert_Data_Combined drh_20180914.xlsx",
  "1998_2008_SoilMerged_2017_03_09",
  na = c(".", "", "NA"))
dfX8 <- df1998_2008 %>% 
  select("1998_ID2",
    "1998_Horizons",
    "1998_TopD_cm",
    "1998_BottomD_cm",
    "1998_SoilpH",
    "Accepted_1998_BD_gcm3",
    "1998_TruSpecC_Prct",
    "1998_TruSpecN_Prct",
    "1998_TruSpecC_kgMg",
    "1998_TruSpecN_kgMg",
    "1998_Soil_C_Stock_Mgha",
    "1998_Soil_N_Stock_Mgha",
    "2008_Horizons",
    "2008_TopD_cm",
    "2008_BottomD_cm",
    "Accepted_2008_BD_gcm3",
    "2008_TruSpecC_Prct",
    "2008_TruSpecN_Prct",
    "2008 pH",
    "2008_TruSpecC_kgMg",
    "2008_TruSpecN_kgMg",
    "2008_Soil_C_Stock_Mgha",
    "2008_Soil_N_Stock_Mgha") %>% 
  rename("ID2" = "1998_ID2",
         "1998_BD" = "Accepted_1998_BD_gcm3",
         "2008_BD" = "Accepted_2008_BD_gcm3",
         "1998_pH" = "1998_SoilpH",
         "2008_pH" = "2008 pH")

df98 <- dfX8 %>% 
  select("ID2", contains("1998")) %>% 
  mutate(Year = 1998) %>% 
  rename_at(vars(contains("1998")), funs(sub("1998_", "", .))) %>% 
  mutate(TopD_cm = as.character(TopD_cm)) %>% 
  mutate(BottomD_cm = as.character(BottomD_cm))

df08 <- dfX8 %>% 
  select("ID2", contains("2008")) %>% 
  mutate(Year = 2008) %>% 
  rename_at(vars(contains("2008")), funs(sub("2008_", "", .))) %>% 
  mutate(TopD_cm = as.character(TopD_cm)) %>% 
  mutate(BottomD_cm = as.character(BottomD_cm))

cleanX8 <- bind_rows(df98, df08)

# Quick checks ----
# Compare points between datasets:
cleanX8$`1998_ID2`[!(cleanX8$`1998_ID2`%in% clean15$ID2)]

