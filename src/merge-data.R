library(tidyverse)
library(readxl)
library(sf)
library(tmap)

# Load data ----
# Georef points are 369 grid points the soil was sampled from
georef <- st_read("input/CookEast_GeoReferencePoints_2015_IL/All_CookEast.shp") %>% 
  st_transform(crs = 4326) %>% 
  select(ID2, COLUMN, ROW2, STRIP, FIELD)

df2015 <- read_excel(
  "input/soilCore2015MergeAcceptedBDWithEllens_ForImport_20180924.xlsx",
  "Sheet1")

# 1998, 2008 soil data include shallow and deep cores, no acid washed integration
df1998_2008 <- read_excel(
  "input/Soil_Plant_Fert_Data_Combined_depthsCorrected_20180919.xlsx",
  "1998_2008_SoilMerged_2017_03_09",
  na = c(".", "", "NA"))

# Acid washed data for 1998 and 2008
dfAcidX8 <- read_excel(
  "input/acidWashedSamples_1998-2008_20180918.xlsx",
  "Sheet1")

clean15 <- df2015 %>% 
  mutate(Year = 2015)

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
    "TocStock" = "Soil_C_Stock_Mgha") %>% 
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

# Finalize dataset ----
# Merge years
cleanAllYears <- bind_rows(cleanX8AcidWash, clean15) %>% 
  filter(!is.na(.$ID2))

# Stretch bottom depth increment to 153 cm
df <- cleanAllYears %>% 
  group_by(ID2, Year) %>% 
  mutate(MaxDepth = max(BottomDepth)) %>% 
  ungroup() %>% 
  mutate(BottomDepth = case_when(BottomDepth == MaxDepth ~ 153,
    TRUE ~ BottomDepth)) %>% 
  select(-MaxDepth)

# Calculate total organic carbon stocks
df <- df %>% 
  mutate(TocStock = case_when(
    is.na(TCConcAcidWashed) ~ (BottomDepth - TopDepth) * TCConc/100 * BulkDensity * 100,
    !is.na(TCConcAcidWashed) ~ (BottomDepth - TopDepth) * TCConcAcidWashed/100 * BulkDensity * 100))

# Add coordinates
df <- df %>% 
  full_join(data.frame(st_coordinates(georef), 
                       st_set_geometry(georef, NULL)),
            by = c("ID2")) %>% 
  select(-COLUMN, -ROW2, -STRIP, -FIELD) %>% 
  rename(Latitude = Y, Longitude = X) %>% 
  filter(!is.na(Year))

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
dateToday <- format(Sys.Date(), "%Y%m%d")
outPathData <- paste("output/soilCore1998To2015ShallowDeepMergedByHorizon_", 
                 dateToday, 
                 ".csv",
                 sep = "")
outPathDict <- paste("output/soilCore1998To2015ShallowDeepMergedByHorizon_Dictionary_", 
                     dateToday, 
                     ".csv",
                     sep = "")
varNames <- c("Year", "ID2", "Latitude", "Longitude", "TopDepth", "BottomDepth", "Horizon", 
              "BulkDensity", "dC13", "dC13AcidWashed", "TNConc", "TNConcAcidWashed",
              "TCConc", "TCConcAcidWashed", "TocConc", "TocStock", "TicConc", "TicStock", "TNStock", "pH")
varUnits <- c("unitless", "unitless", "dd", "dd", "cm", "cm", "unitless",
              "g/cm^3", "%o", "%o", "%", "%",
              "%", "%", "%", "Mg/ha", "%", "Mg/ha", "Mg/ha", "unitless")
varTypes <- c("Int", "Int", "Double", "Double", "Int", "Int", "String",
              "Double", "Double", "Double", "Double", "Double",
              "Double", "Double", "Double", "Double", "Double", "Double", "Double", "Double")
varDesc <- c("Year sample was collected", 
             "Number ID of georeference point near sample collection",
             "Latitude of georeference point near where sample was collected",
             "Longitude of georeference point near where sample was colelcted",
             "Top depth of subsample taken from soil core",
             "Bottom depth of subsample taken from soil core",
             "Horizon designation of the subsample",
             "Bulk density of the subsample",
             "Carbon isotopic signature of the subsample",
             "Carbon isotopic signature of the subsample after acid washed",
             "Total nitrogen concentraion",
             "Total nitrogen concentration after acid washed",
             "Total carbon concentration",
             "Total carbon concentration after acid washed",
             "Total organic carbon concentration",
             "Total organic carbon stock",
             "Total inorganic carbon concentration",
             "Total inorganic carbon stock",
             "Total nitrogen stock",
             "pH of the subsample")
df %>% 
  select(varNames) %>% 
  write_csv(outPathData, na = "")

data.frame(varNames, varUnits, varDesc, varTypes) %>% 
  rename("FieldNames" = varNames,
         "Units" = varUnits,
         "Description" = varDesc,
         "DataType" = varTypes) %>% 
  write_csv(outPathDict)

# Quick checks ----
## Compare points between datasets:
#cleanX8$`1998_ID2`[!(cleanX8$`1998_ID2`%in% clean15$ID2)]
#
## Quick graph -- fairly  meaningless
#ggplot(df, aes(ID2, TC, color = Year, alpha = 0.6)) + geom_point()
#
## Quick map
#tmap_mode("view")
#df %>% st_as_sf(coords = c("Longitude", "Latitude"), 
#                na.fail = FALSE, 
#                crs = 4326) %>% 
#  tm_shape() + tm_symbols(size = 1, col = "black") +
#  tm_shape(georef) + tm_symbols(size = 0.25, col = "red")
