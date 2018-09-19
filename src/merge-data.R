library(tidyverse)
library(readxl)
library(sf)
library(tmap)

# Load data ----
# Georef points are 369 grid points the soil was sampled from
georef <- st_read("input/CookEast_GeoReferencePoints_2015_IL/All_CookEast.shp") %>% 
  st_transform(crs = 4326) %>% 
  select(ID2, COLUMN, ROW2, STRIP, FIELD)

# 2015 soil data include shallow and deep cores, acid washed samples integrated
df2015 <- read_excel(
  "input/2015 soil samples  merge 0-30 and below profile data organization.xlsx", 
  sheet = "Merged Profile (2)",
  na = c("NA"))

# 1998, 2008 soil data include shallow and deep cores, no acid washed integration
df1998_2008 <- read_excel(
  "input/2017_03_10_Soil_Plant_Fert_Data_Combined drh_20180914.xlsx",
  "1998_2008_SoilMerged_2017_03_09",
  na = c(".", "", "NA"))

# Acid washed data for 1998 and 2008
dfAcidX8 <- read_excel(
  "input/acidWashedSamples_1998-2008_20180918.xlsx",
  "Sheet1")


# Prepare 2015 ----
# Check Easting/Northing in Ellen's data with "correct" dataset
#st_as_sf(df2015, 
#         coords = c("Easting", "Northing"), 
#         na.fail = FALSE, 
#         crs = 26911) %>% 
#  st_transform(crs = 4326) %>% 
#  tm_shape() + tm_symbols(size = 1, col = "black", popup.vars = c("ID2", "Column", "Row2")) +
#  tm_shape(georef) + tm_symbols(size = 0.25, col = "red", popup.vars = c("ID2", "COLUMN", "ROW2"))
# Looks like Easting/Northing is off, but ID2 and row2,col is okay

clean15 <- df2015 %>% 
  select("Label__1",
    "top (cm)",
    "bottom(cm)",
    "ID2",
    "Horizons",
    "Bulk density\r\n(g cm-3)",
    contains("VPDB"),
    "Total N%",
    "Total N%_acid washed",
    "Total C%",
    "Total C%_acid washed",
    "pH_soil:water=1:1)",
    "Carbon stocks (Mg ·ha-1)") %>% 
  rename("TopDepth" = "top (cm)",
    "BottomDepth" = "bottom(cm)",
    "Horizon" = "Horizons",
    "BulkDensity" = "Bulk density\r\n(g cm-3)",
    "dC13" = 7,
    "dC13AcidWashed" = 8,
    "TNConc" = "Total N%",
    "TNConcAcidWashed" = "Total N%_acid washed",
    "TCConc" = "Total C%",
    "TCConcAcidWashed" = "Total C%_acid washed",
    "pH" = "pH_soil:water=1:1)",
    "TOCStock" = "Carbon stocks (Mg ·ha-1)") %>% 
  mutate(SampleId = str_replace(Label__1, " ", "_")) %>% 
  mutate(pH = as.double(pH)) %>% 
  mutate(dC13 = as.double(C13)) %>% 
  mutate(TNConc = as.double(TN)) %>% 
  mutate(TCConc = as.double(TC)) %>% 
  select(-Label__1)
  


# Prepare 1998-2008 data ----
# Check Easting/Northing in Tabitha's data with "correct" dataset
#st_as_sf(df1998_2008, 
#         coords = c("1998_Easting", "1998_Northing"), 
#         na.fail = FALSE, 
#         crs = 26911) %>% 
#  st_transform(crs = 4326) %>% 
#  tm_shape() + tm_symbols(size = 1, col = "black", popup.vars = c("1998_ID2", "1998_Col", "1998_Row2")) +
#  tm_shape(georef) + tm_symbols(size = 0.25, col = "red", popup.vars = c("ID2", "COLUMN", "ROW2"))
# Looks like Easting/Northing is ok and ID2 and row2,col is okay

# Select and rename
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
  
# Split year columns into separate dataframes, add Year column, remove year prefix from column names, convert data classes
df98 <- dfX8 %>% 
  select("ID2", contains("1998")) %>% 
  mutate(Year = 1998) %>% 
  rename_at(vars(contains("1998")), funs(sub("1998_", "", .))) %>% 
  mutate(TopD_cm = as.double(TopD_cm)) %>% 
  mutate(BottomD_cm = as.double(BottomD_cm)) %>% 
  mutate(pH = as.double(pH))
df08 <- dfX8 %>% 
  select("ID2", contains("2008")) %>% 
  mutate(Year = 2008) %>% 
  rename_at(vars(contains("2008")), funs(sub("2008_", "", .))) %>% 
  mutate(TopD_cm = as.double(TopD_cm)) %>% 
  mutate(BottomD_cm = as.double(BottomD_cm)) %>% 
  mutate(pH = as.double(pH))

# Recombine 1998 and 2008 data in long format, rename columns, add SampleId and calculate carbon stocks
cleanX8 <- bind_rows(df98, df08) %>% 
  rename("Horizon" = "Horizons",
    "TopDepth" = "TopD_cm",
    "BottomDepth" = "BottomD_cm",
    "BulkDensity" = "BD",
    "TCConc" = "TruSpecC_Prct",
    "TNConc" = "TruSpecN_Prct",
    "TOCStock" = "Soil_C_Stock_Mgha") %>% 
  mutate(SampleId = paste("CF",
    as.character(str_sub(Year, 3, 4)),
    "GP_",
    as.character(ID2), 
    "_", 
    as.character(TopDepth),
    "-",
    as.character(BottomDepth),
    sep = ""))

# Merge acid washed with cleanX8
cleanAcidX8 <- dfAcidX8  %>% 
  rename("dC13AcidWashed" = "dC13",
         "TNConcAcidWashed" = "TN",
         "TCConcAcidWashed" = "TC") %>% 
  separate(SampleId, 
           into = c("Field", "Year", "ID2", "ColRowDepth"),
           sep = "_") %>% 
  separate(ColRowDepth,
           int = c("ColRow", "Depth"),
           sep = " ") %>% 
  separate(Depth,
           into = c("TopDepth", "BottomDepth"),
           sep = "-",
           convert = TRUE) %>% 
  separate(ColRow,
           into = c("Column", "Row"),
           sep = -1) %>% 
  mutate(ID2 = as.numeric(str_remove(ID2, "GP")),
         Year = as.numeric(Year))

cleanX8AcidWash <- full_join(cleanX8, cleanAcidX8, by = c("ID2", "BottomDepth", "Year")) %>% 
  select(-TopDepth.y, -Field, -Column, -Row) %>% 
  rename(TopDepth = TopDepth.x)

cleanX8AWCStock <- cleanX8AcidWash %>% 
  mutate(TocStock = case_when(
    is.na(TCConcAcidWashed) ~ (BottomDepth - TopDepth) * TCConc/100 * BulkDensity * 100,
    !is.na(TCConcAcidWashed) ~ (BottomDepth - TopDepth) * TCConcAcidWashed/100 * BulkDensity * 100))

# Merge years
cleanAllYears <- bind_rows(cleanX8AWCStock, clean15) %>% 
  filter(!is.na(.$ID2))

# Add coordinates
df <- cleanAllYears %>% 
  full_join(data.frame(st_coordinates(georef), 
                       st_set_geometry(georef, NULL)),
            by = c("ID2")) %>% 
  select(-COLUMN, -ROW2, -STRIP, -FIELD) %>% 
  rename(Latitude = Y, Longitude = X)

# Add C and N related columns
df <- df %>% 
  mutate(TocConc = case_when(
    is.na(TCConcAcidWashed) & !is.na(TCConc) ~ TCConc,
    !is.na(TCConcAcidWashed) ~ TCConcAcidWashed)) %>% 
  mutate(TicConc = case_when(
    !is.na(TCConcAcidWashed) & !is.na(TCConc) ~ TCConc - TCConcAcidWashed,
    TRUE ~ 0)) %>% 
  mutate(TicStock = (BottomDepth - TopDepth) * TicConc / 100 * BulkDensity * 100) %>% 
  mutate(TNStock = (BottomDepth - TopDepth) * TNConc / 100 * BulkDensity * 100)

# Reorganize columns and write csv
dateToday <- format(Sys.Date(), "%y%m%d")
outPath <- paste("output/soilCore_1998-2015_shallow_deep_mergedByHorizon_", 
                 dateToday, 
                 ".csv",
                 sep = "")
df %>% 
  select(Year, ID2, Latitude, Longitude, TopDepth, BottomDepth, Horizon, 
         BulkDensity, dC13, dC13AcidWashed, TNConc, TNConcAcidWashed,
         TCConc, TCConcAcidWashed, TocConc, TicConc, TicStock, TNStock, pH) %>% 
  write_csv(outPath)

# Quick checks ----
# Compare points between datasets:
cleanX8$`1998_ID2`[!(cleanX8$`1998_ID2`%in% clean15$ID2)]

# Quick graph -- fairly  meaningless
ggplot(df, aes(ID2, TC, color = Year, alpha = 0.6)) + geom_point()

# Quick map
tmap_mode("view")
df %>% st_as_sf(coords = c("Longitude", "Latitude"), 
                na.fail = FALSE, 
                crs = 4326) %>% 
  tm_shape() + tm_symbols(size = 1, col = "black") +
  tm_shape(georef) + tm_symbols(size = 0.25, col = "red")
