# BDQT - Percent Disturbed by Human Footprint

library(tidyverse)
library(DBI)
library(RSQLite)

# path to SC drive
sc <- "S:/AB_data_v2019/bdqt/"

# OK, we'll need the SQLite database after all.
con <- dbConnect(SQLite(), paste0(sc, "bdqt-labels-2017hfi_2019-07-18.sqlite"))

# Show list of tables
as.data.frame(dbListTables(con))

# Get table ... uh oh.
bdqt_veghf <- dbReadTable(con, 'veghf') # Wow, it actually worked.

bdqt_veghf_nsr <- bdqt_veghf %>%
  select(-c(Easting, Northing))

# Retrieve Peter's processed RData file
load(paste0(sc, "bdqt-poly-hab_2019-12-10.RData"))

x1 <- x %>%
  rename(VEGHFAGEclass_off = VEGHFAGEclass) %>%
  select(-c(SOILHFclass))

bdqt_veghf_nsr_all <- bdqt_veghf_nsr %>%
  left_join(x1, by = "UID")

rm(bdqt_veghf_nsr, x, x1)

# Find out about categories
veghf_cat <- bdqt_veghf_nsr_all %>%
  select(VEGHFAGEclass_off) %>%
  distinct()

soil_cat <- bdqt_veghf_nsr_all %>%
  select(SOILclass) %>%
  distinct()

# Read in Dave's North Values
north <- read_csv("./data/base/north-values-by-veg.csv") %>%
  mutate(VEGHFAGEclass_off = ifelse(!is.na(Age), paste(VegType, Age, sep = "_"), VegType)) %>%
  pivot_longer(cols = Alpine:`Upper Foothills`, names_to = "NSRNAME", values_to = "VALUE_north") %>%
  select(VEGHFAGEclass_off, NSRNAME, VALUE_north)

south <- read_csv("./data/base/south-values-by-soil.csv") %>%
  pivot_longer(cols = `Central Parkland`:`Peace River Parkland`, names_to = "NSRNAME", values_to = "VALUE_south")

# NSR categories
south_cat <- south %>%
  select(NSRNAME) %>%
  distinct()

north_cat <- north %>%
  select(NSRNAME) %>%
  distinct() # No overlap ...

#-------------------------------------------------------------------------------

# Join info:

bdqt_rank_hf_disturbance_value <- bdqt_veghf_nsr_all %>%
  left_join(north, by = c("NSRNAME", "VEGHFAGEclass_off")) %>%
  left_join(south, by = c("NSRNAME", "SOILHFclass")) %>%
  mutate(VALUE = ifelse(is.na(VALUE_north), VALUE_south, VALUE_north)) %>%
  select(UID, NSRNAME, VEGHFAGEclass_off, SOILHFclass, VALUE) %>%
  rename(VEGHFAGEclass = VEGHFAGEclass_off)

save(bdqt_rank_hf_disturbance_value,
     file = paste0(sc, "final/Results from Marcus/bdqt-poly_percent-disturb-score.RData"))


















