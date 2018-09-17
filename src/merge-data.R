library(tidyverse)
library(readxl)

# Load data ----
df2015 <- read_excel(
  "input/2015 soil samples  merge 0-30 and below profile data organization.xlsx", 
  sheet = "Merged Profile (2)")
df2008s <- read_excel(
  "input/2017_08_29_CAF2008_surface_30cm.xlsx",
  sheet = "2011_12_15_weightedBD_mulch",
  skip = 3,
  na = c("", "NS"),
  col_types = c(
    "text",
    rep("numeric",2),
    "text",
    rep("numeric", 3),
    rep("text", 3),
    "numeric",
    "text",
    rep("numeric", 15),
    rep("text", 2),
    rep("numeric", 3),
    "text",
    "numeric",
    "text",
    rep("numeric", 3)
  ),
  col_names = c(
    "Barcode",
    "UID",
    "COLUMN",
    "Row",
    "EASTING",
    "NORTHING",
    "STRIP",
    "FIELD",
    "CROP",
    "Depth",
    "DepthSort",
    "CurrentCrop",
    "Mulch1",
    "Mulch2",
    "Mulch3",
    "Mulch4",
    "TotalDepth",
    "TotalDepth2_4",
    "AirDryWeightSampleBag",
    "BagWeightBagLabel",
    "AirDryWeightSample",
    "CoreRadius_cm",
    "SampleVolumn",
    "MoistureContent",
    "MoistureCorrectedSampleWeight",
    "BulkDensityByDepthIncrement",
    "WeightedBulkDensityFor0_10cm",
    "Tray",
    "Project",
    "YearSampled",
    "UID",
    "Column",
    "Row",
    "MergeCheck",
    "DepthIncrement_cm",
    "Weight",
    "N%",
    "C%"))
df1998s <- read_excel(
  "input/2017_08_29_CAF1998_surface_30cm.xlsx",
  sheet = "0-30 cm")
df1998_2008 <- read_excel(
  "input/2017_03_10_Soil_Plant_Fert_Data_Combined.xlsx",
  "1998_2008_SoilMerged_2017_03_09")



# Prepare 2015 ----
foo <- df2015 %>% 
  select(
    "Label__1",
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
    "Accumulative C stocks(Mg ·ha-1)")
foo <- foo %>% rename(
  "SampleId" = "Label__1",
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
  
