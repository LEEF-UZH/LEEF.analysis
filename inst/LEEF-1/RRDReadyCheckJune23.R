library(tidyverse)
library(patchwork)
## Get the LEEF analysis package
# drat::addRepo("LEEF-UZH")
# install.packages("bemovi.LEEF")
# install.packages("LEEF.measurements.flowcam")
# options(repos = c(
#   leefuzh = 'https://leef-uzh.r-universe.dev',
#   CRAN = 'https://cloud.r-project.org'))
# # Download and install LEEF.analysis in R
# install.packages('LEEF.analysis')
library(LEEF.analysis)
library(DBI)
library(RSQLite)
library(data.table)
library(pracma)
library(parallel)
library(doParallel)
library(here)

set.seed(1)
options(dplyr.summarise.inform = FALSE)


options(RRDdb = "/Volumes/RRD.Reclassification_final/LEEF.RRD.v1.8.5_1.sqlite")

# options(RRDdb = "LEEF.RRD.v1.8.5_4.sqlite")

densities <- db_read_table(table = "density") %>%
  collect() %>%
  arrange(day) 
# CHECK: Code in previous 3 lines give the below message, which seems indicate a problem in the data
# Column `biomass`: mixed type, first seen values of type real, coercing other values of type string
# RMK: Fixed

o2 <- db_read_table(table = "o2") %>% collect()

water_chem <- db_read_toc() %>% collect()

## check that all variables are (species) present

vars <- c("Coleps sp.","Paramecium bursaria","Paramecium caudatum", 
          "Stylonychia sp.","Colpidium striatum","Euplotes daidaleos",
          "Dexiostoma campylum","Loxocephalus sp.","Chlamydomonas reinhardtii",
          "Chlamydomonas clumps","Cosmarium botrytis","Debris",
          "Desmodesmus armatus","Desmodesmus clumps","Digested algae",
          "Dividing Chlamydomonas","Small cells","Small unidentified",
          "Staurastrum gracile","Colpidium vacuoles","Monoraphidium obtusum",
          "Staurastrum polytrichum","Bacteria","Didinium nasutum", "Algae")    

# CHECK: Must be TRUE
all(vars %in% unique(densities$species)) 
idx <- unique(densities$species) %in% vars
idx
# RMK: OK

# CHECK: must be empty
unique(densities$species)[!idx]
# RMK: OK

## Next check: missing timestamps

# table(densities$species, densities$measurement)/123 # dividing by 123 should give an integer value (the number of bottles). Not a valid criterion for all measurements

# CHECK: length should be 123
timestamps <- unique(densities$timestamp)
length(timestamps)
# RMK OK



# CHECK: The following data.frame should be empty
# TODO RMK
missing_timestamps <- densities %>%
  group_by(measurement, species, bottle) %>%
  summarize(missing_timestamps = as.numeric(timestamps[which(!(timestamps %in% unique(timestamp)))])) %>%
  dplyr::filter(measurement!="manualcount", !(measurement=="bemovi_mag_25_non_cropped" & missing_timestamps<20211103), # non_cropped only used from 20211103 onwards
                !(measurement=="flowcytometer" & missing_timestamps %in% c(20220404,20220406,20220415,20220418,20220425))) # For the flowcytometer for the following timestamps there was no data collection: "20220404" "20220406" "20220415" "20220418" "20220425" 
missing_timestamps

# RMK: More detailed tests of missing timestamps
{ 
  # CHECK: should be empty
  timestamps[!(timestamps %in% unique(densities[densities$measurement == "bemovi_mag_16", ]$timestamp))]
  # RMK: OK

  # CHECK: should be empty
  timestamps[!(timestamps %in% unique(densities[densities$measurement == "bemovi_mag_25", ]$timestamp))]
  # RMK: OK

  # CHECK: should be empty
  timestamps[!(timestamps %in% unique(densities[densities$measurement == "bemovi_mag_25_cropped", ]$timestamp))]
  # [1] "20211029"
  # TODO

  # CHECK: should be empty
  # non_cropped only used from 20211103 onwards
  timestamps[!(timestamps[timestamps >= 20211103] %in% unique(densities[densities$measurement == "bemovi_mag_25_non_cropped", ]$timestamp))]
  #  [1] "20211112"
  # TODO WRONG

  # CHECK: should be empty
  timestamps[!(timestamps %in% unique(densities[densities$measurement == "flowcam", ]$timestamp))]
  # RMK: OK

  # CHECK: should be as these are missing "20220404" "20220406" "20220415" "20220418" "20220425"
  timestamps[!(timestamps %in% unique(densities[densities$measurement == "flowcytometer", ]$timestamp))]
  # RMK: OK

  # Not measured regularly
  # timestamps[!(timestamps %in% unique(densities[densities$measurement == "manualcount", ]$timestamp))]
}

### the same for the oxygen and the water chemistry 

# CHECK: The following data.frame should be empty
# RMK: OK b_29 Sensor 4 is mssing in raw data
missing_timestamps_o2 <- o2 %>%
  group_by(sensor, bottle) %>%
  summarize(missing_timestamps = as.numeric(timestamps[which(!(timestamps %in% unique(timestamp)))])) %>%
  dplyr::filter(!(missing_timestamps==20211208 & bottle=="b_01" & sensor==4),
                missing_timestamps!=20211201,
                ) # Previously confirmed missing data is filtered out
missing_timestamps_o2

# CHECK: The following data.frame should be empty
# This check fails (missing_timestamps_wc). Romana confirmed that there is raw data for timestamps 20211001 and 20220624 (note: date is incorrect specified in 20211001 water chem file). Because of this I'm assuming that the other timestamps should also be present (most of them were in previous RRD versions). Please check.
# 
# CONFIRM RMK should be working now
missing_timestamps_wc <- water_chem %>%
  group_by(type, bottle) %>%
  summarize(missing_timestamps = as.numeric(timestamps[which(!(timestamps %in% unique(timestamp)))])) 
table(missing_timestamps_wc$missing_timestamps, missing_timestamps_wc$bottle)
missing_timestamps_wc


## Next check: NAs. 

# CHECK: The following data.frame should be empty
# RMK: Algae has no biomass, therefore added to filter
# RMK added timestamp into grouping
# TODO RMK flowcam biomass calculation is fishy
NAs <- densities %>%
  dplyr::filter(species!="Debris", species!="Digested algae",species!="Small unidentified",species!="Colpidium vacuoles", species!="Didinium nasutum", species != "Algae") %>% #biomass not calculated for these classes
  group_by(measurement, species, bottle, timestamp) %>%
  summarize(densityNA = sum(is.na(density)),
            biomassNA = sum(is.na(biomass))) %>%
  dplyr::filter(biomassNA>0 | densityNA>0,
                !(measurement %in% c("bemovi_mag_25","bemovi_mag_25_cropped","bemovi_mag_25_non_cropped") & densityNA == 1)) # There is 1 NA for all species for all bottles at magnification 25 (20220622 missing)
NAs

# CHECK: The following data.frame should be empty
# RMK: OK
NAs_o2 <- o2 %>%
  group_by(sensor, bottle) %>%
  summarize(o2NA = sum(is.na(percent_o2))) %>%
  dplyr::filter(o2NA>0)
NAs_o2

# CHECK: The following data.frame should be empty
# RMK: OK
NAs_wc <- water_chem %>%
  group_by(type, bottle) %>%
  summarize(concentrationNA = sum(is.na(concentration))) %>%
  dplyr::filter(concentrationNA>0)
NAs_wc

## Next check: are all species present in all the bottles they are supposed to be

# see below for check
# all uncommented checks below were passing at 4pm on 14.6.2023 while using LEEF.RRD.v1.8.5_4.sqlite
# RMK: OK


## TO FIX...
comps <- db_read_table(table = "composition") %>% collect() # still has the old species names... FIXED
design <- db_read_table(table = "experimental_design") %>% collect()

design <- full_join(design, comps)

design_long <- design %>%
  pivot_longer(cols = -c(bottle,temperature,richness,composition,incubator), names_to = "species") %>%
  dplyr::filter(value == 1) %>%
  select(-value)

design_long <- design_long %>%
  mutate(species = case_when(species == "Chlamydomonas" ~ "Chlamydomonas reinhardtii",
                             species == "Cosmarium" ~ "Cosmarium botrytis",
                             species == "Desmodesmus" ~ "Desmodesmus armatus",
                             species == "Tetrahymena" ~ "Tetrahymena thermophila",
                             species == "Loxocephallus" ~ "Loxocephalus sp.",
                             species == "Paramecium_caudatum" ~ "Paramecium caudatum",
                             species == "Coleps_irchel" ~ "Coleps sp.",
                             species == "Paramecium_bursaria" ~ "Paramecium bursaria", 
                             species == "Didinium" ~ "Didinium nasutum",
                             species == "Staurastrum1" ~ "Staurastrum gracile",
                             species == "Euplotes" ~ "Euplotes daidaleos",
                             species == "Colpidium" ~ "Colpidium striatum",
                             species == "Stylonychia2" ~  "Stylonychia sp.",
                             species == "Monoraphidium" ~ "Monoraphidium obtusum",
                             species == "Staurastrum2" ~ "Staurastrum polytrichum",
                             species == "Stylonychia1" ~ "Stylonychia mytilus",
                             species == "Dexiostoma" ~ "Dexiostoma campylum",
                             species == "Cryptomonas" ~ "Cryptomonas sp.")
  ) %>%
  dplyr::filter(species != "Cryptomonas sp.", species != "Stylonychia mytilus")

bottles <- c("b_30", "b_02", "b_09", "b_02", "b_09", "b_14", 
             "b_23", "b_04", "b_07", "b_13", "b_07", "b_13", 
             "b_07", "b_13", "b_10", "b_21", "b_10", "b_21",
             "b_01", "b_11", "b_01", "b_11", "b_20", "b_28",
             "b_28", "b_22", "b_22", "b_27", "b_22", "b_22",
             "b_27", "b_03", "b_03", "b_06" ,"b_03", "b_06", 
             "b_03", "b_06", "b_03", "b_03", "b_06", "b_19", 
             "b_29", "b_19", "b_29", "b_19", "b_29", "b_19", 
             "b_29", "b_05", "b_24" ,"b_05", "b_24", "b_05",
             "b_24", "b_05", "b_24", "b_15", "b_12", "b_15", 
             "b_12", "b_15", "b_12", "b_15", "b_12", "b_15", 
             "b_16", "b_26" ,"b_16", "b_26", "b_16", "b_26", 
             "b_16", "b_16", "b_26", "b_05", "b_12", "b_15",
             "b_16", "b_24", "b_26", "b_08", "b_06", "b_26",
             "b_14", "b_25")

species <- c("Tetrahymena thermophila",     "Tetrahymena thermophila",   
             "Tetrahymena thermophila",     "Euplotes daidaleos",
             "Euplotes daidaleos",          "Loxocephalus sp.", 
             "Euplotes daidaleos",          "Colpidium striatum",
             "Loxocephalus sp.",            "Loxocephalus sp.",
             "Dexiostoma campylum",         "Dexiostoma campylum",
             "Euplotes daidaleos",          "Euplotes daidaleos",    
             "Colpidium striatum",          "Colpidium striatum",
             "Loxocephalus sp.",            "Loxocephalus sp.", 
             "Tetrahymena thermophila",     "Tetrahymena thermophila",
             "Loxocephalus sp.",            "Loxocephalus sp.",
             "Tetrahymena thermophila",     "Tetrahymena thermophila",  
             "Colpidium striatum",          "Colpidium striatum",   
             "Dexiostoma campylum",         "Dexiostoma campylum", 
             "Stylonychia sp.",             "Euplotes daidaleos",    
             "Euplotes daidaleos",          "Staurastrum gracile",
             "Tetrahymena thermophila",     "Tetrahymena thermophila",  
             "Colpidium striatum",          "Colpidium striatum",   
             "Loxocephalus sp.",            "Loxocephalus sp.", 
             "Stylonychia sp.",             "Euplotes daidaleos",   
             "Euplotes daidaleos",          "Colpidium striatum",   
             "Colpidium striatum",          "Loxocephalus sp.",
             "Loxocephalus sp.",            "Dexiostoma campylum",    
             "Dexiostoma campylum",         "Euplotes daidaleos",    
             "Euplotes daidaleos",          "Tetrahymena thermophila",  
             "Tetrahymena thermophila",     "Colpidium striatum",   
             "Colpidium striatum",          "Loxocephalus sp.", 
             "Loxocephalus sp.",            "Euplotes daidaleos",   
             "Euplotes daidaleos",          "Staurastrum gracile", 
             "Colpidium striatum",          "Colpidium striatum",   
             "Loxocephalus sp.",            "Loxocephalus sp.",
             "Dexiostoma campylum",         "Dexiostoma campylum",  
             "Euplotes daidaleos",          "Euplotes daidaleos",    
             "Colpidium striatum",          "Colpidium striatum",  
             "Loxocephalus sp.",            "Loxocephalus sp.",
             "Dexiostoma campylum",         "Dexiostoma campylum",  
             "Stylonychia sp.",             "Euplotes daidaleos",    
             "Euplotes daidaleos",          "Staurastrum polytrichum", 
             "Staurastrum polytrichum",     "Staurastrum polytrichum",
             "Staurastrum polytrichum",     "Staurastrum polytrichum", 
             "Staurastrum polytrichum",     "Tetrahymena thermophila",   
             "Stylonychia sp.",             "Stylonychia sp.",
             "Euplotes daidaleos",          "Euplotes daidaleos") 

timeSeries_toExclude <- data.frame(
  bottle = bottles,
  species = species) %>%
  dplyr::mutate(int = interaction(bottle, species))


design_long <- design_long %>%
  dplyr::mutate(bottle.species = interaction(bottle, species)) %>%
  dplyr::filter(!(bottle.species %in% timeSeries_toExclude$int)) 

# CHECK: densities2 should have the same dimensions as densties data.frame
# TODO RMK
densities2 <- full_join(densities, design_long) # no change = everything that is supposed to be present is present
dim(densities)
dim(densities2)


# CHECK: data.frame "check" should be empty (species time series correctly removed)
# RMK: OK
check <- densities %>%
  dplyr::mutate(bottle.species = interaction(bottle, species)) %>%
  dplyr::filter((bottle.species %in% timeSeries_toExclude$int)) 
check

## Further (older) checks. not run anymore

##### Exclusion of low probability Staurastrum1 

# stau <- db_read_table(table="flowcam__algae_traits") %>%
#   dplyr::filter(species=="Staurastrum gracile",
#                 timestamp == "20210924") %>%
#   collect()
# 
# min(stau$species_probability) # good

#####  Exclusion of 20220622 25x videos

# day20220622 <- densities %>%
#   dplyr::filter(timestamp == "20220622")
# 
# table(day20220622$measurement) # good
# 
# check2 <- db_read_table(table="bemovi_mag_25__morph_mvt") %>%
#   dplyr::filter(timestamp == "20220622") %>%
#   collect()  # good
# 
# check2 <- db_read_table(table="bemovi_mag_25__morph_mvt_cropped") %>%
#   dplyr::filter(timestamp == "20220622") %>%
#   collect() # good
# 
# check2 <- db_read_table(table="bemovi_mag_25__morph_mvt_non_cropped") %>%
#   dplyr::filter(timestamp == "20220622") %>%
#   collect()  # good

##### Adding density=0 for some special cases in flowcam

# b02 <- densities %>%
#   dplyr::filter(bottle=="b_02",
#                 species %in% c("Desmodesmus armatus", "Desmodesmus clumps"))
# 
# table(b02$species, b02$day) # not implemented
# table(b02$species) # not implemented, should be 123
# 
# 
# ChlamydomonasClumps_DesmodesmusClumps <- densities %>%
#   dplyr::filter(species %in% c("Chlamydomonas clumps", "Desmodesmus clumps"))
# 
# table(ChlamydomonasClumps_DesmodesmusClumps$species, ChlamydomonasClumps_DesmodesmusClumps$bottle) # good
