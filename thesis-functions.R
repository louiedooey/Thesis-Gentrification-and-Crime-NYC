## Thesis Code Functions ##
library(sf)
library(dplyr)
library(data.table)
library(tidycensus)
library(ggplot2)
library(maptools)
library(rgdal)
library(showtext)
library(ggthemes)
library(hrbrthemes)
library(cowplot) # for grid plot
library(beepr)
library(GISTools)
library(spdep)
library(spgwr)

# Fonts 
showtext_auto()
font_add(family = "raleway", regular = "fonts/Raleway-Regular.ttf")
font_add(family = "proximanova", regular = "fonts/ProximaNova-Reg.ttf")
font_add(family = "proximanovalight", regular = "fonts/ProximaNova-Light.ttf")
font_add(family = "proximanovaboldlight", regular = "fonts/ProximaNova-Bold.ttf")
font_add(family = "montserrat-semibold", regular = "fonts/Montserrat-SemiBold.otf")
font_add(family = "montserrat-med", regular = "fonts/Montserrat-Medium.otf")
font_add(family = "montserrat", regular = "fonts/Montserrat-Regular.otf")

# ------------------------------------------- # 
# ------------ CRIME IN BROOKYLN ------------ #
# ------------------------------------------- # 

# https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i
# nypd  <- fread("data/NYPD/NYPD.csv", header = TRUE)


# Sorting out NYPD Dataset

# variables renamed 
# nypd_renamed <- nypd %>% 
#   rename("PRECINCT" = "ADDR_PCT_CD",
#          "REPORTED_DATE" = "RPT_DT", # only using crimes reported
#          "OFFENSE_LV" = "LAW_CAT_CD",
#          "OFFENSE_DESC" = "OFNS_DESC")

# nypd_bk <- nypd %>% 
#   filter(BORO_NM == "BROOKLYN") %>%
#   rename("PRECINCT" = "ADDR_PCT_CD",
#          "REPORTED_DATE" = "RPT_DT", # only using crimes reported
#          "OFFENSE_LV" = "LAW_CAT_CD",
#          "OFFENSE_DESC" = "OFNS_DESC")
# write.csv(nypd_bk, "data/NYPD/nypd_brooklyn.csv")

# only brooklyn data and relevant column
nypd_brooklyn_data <- nypd %>% 
  filter(OFFENSE_LV == "FELONY") %>% # filter to offense level of felony, the most severe
  dplyr::select(CMPLNT_NUM, # complaint serial/reference number
                PRECINCT, # police dept precinct of crime occurrence
                REPORTED_DATE, # when the crime first happened
                OFFENSE_LV,
                OFFENSE_DESC, # more details of offense
                X_COORD_CD, Y_COORD_CD, Latitude, Longitude, Lat_Lon) # Geo details

# convert to date format, month/day/year
nypd_brooklyn_data$REPORTED_DATE = as.Date(nypd_brooklyn_data$REPORTED_DATE, "%m/%d/%Y")



# ------------------------------------------------------------------------------ # 
# ------------ GEO DATA FOR BORO, NEIGHBORHOOD CENSUS TRACTS IN NYC ------------ #
# ------------------------------------------------------------------------------ # 

# census json data from data.cityofnewyork   
geo_url <- "https://data.cityofnewyork.us/api/geospatial/fxpq-c8ku?method=export&format=GeoJSON"
# tutorial from https://mattherman.info/blog/point-in-poly/ 

# Get Census Tract Boundary Data, reduce variables
tract_sf <- read_sf(geo_url) %>% 
  st_transform(2263) %>%
  dplyr::select(ntacode, ntaname, # neighborhood code & name (55 for Brooklyn eg Crown Heights, Bushwick)
                boro_code, boro_name, #  borough code & name (Brooklyn, Manhattan, Queens, Bronx...)
                boro_ct2010, # borough census tract defined in 2010, compound key
                ctlabel) %>% # census tract label (number)
  rename(NTA_CODE = "ntacode",
         NTA_NAME = "ntaname",
         BORO_CODE = "boro_code",
         BORO_NAME = "boro_name",
         BORO_CT_KEY = "boro_ct2010",
         TRACT_NO = "ctlabel")

# head(tract_sf)


# ---------------------------------------------------- # 
# ------------ COUNT FOR BROOKLYN CRIME  ------------- #
# ---------------------------------------------------- # 

# ******************** FUNCTION ************************* # 
### Getting the Crime Count from the NYPD Crimes Dataset ### 
# 1. add geo attributes (coord and polgygon) to nypd brooklyn crime data
# 2. calculate count of offense/felonies for each neighborhood and tract 

crimeCount <- function(year) {
  
  df <- nypd_brooklyn_data
  
  filtered_year <- df %>%
    filter(df$REPORTED_DATE >=
             as.Date(paste(year, "-01-01", sep="")) & # from January 1st that year
             df$REPORTED_DATE <=
             as.Date(paste(year, "-12-31", sep=""))) %>%  # to Dec 31st that year
    na.omit()
  
  # rename df to nypd_brooklyn
  nypd_brooklyn <- filtered_year
  
  ### --- adding spatial attributes to crime data --- ##
  # brooklyn crimes into 
  bk_crime_coord <- st_as_sf(nypd_brooklyn, coords = c("X_COORD_CD", "Y_COORD_CD"), crs = 2263) 
  bk_crime_tract <- st_join(bk_crime_coord, tract_sf, join = st_within)
  ## ??? ^^^W hy is there data from other boros? ^^ ### 
  
  # data manipulation to create Crime Count in Brooklyn dataframe 
  bk_crime_count <- as_tibble(bk_crime_tract) %>% 
    # ------------------------------
  # getting felony counts by tracts, neighborhoods
  add_count(NTA_NAME, name = "FEL_NTA_CNT") %>% # felony neighborhood count
    add_count(BORO_CT_KEY, name = "FEL_CT_CNT") %>% # felony census tract count
    # ------------------------------
  # getting overall offense counts by tracts, neighborhoods
  group_by(OFFENSE_DESC) %>% # offense description
    add_count(NTA_NAME, name = "OFN_NTA_CNT") %>% # offense neighborhood count
    add_count(BORO_CT_KEY, name = "OFN_CT_CNT") %>%  # offense census tract count
    rename("FELONY_TYPE" = "OFFENSE_DESC") %>% # type of felony, bc crimes here are felonies only
    # ------------------------------
  filter(BORO_NAME == "Brooklyn") %>% # only for Brooklyn borough
    dplyr::select(NTA_CODE, NTA_NAME, # neighborhood code & name
                  BORO_NAME, # brooklyn 
                  BORO_CT_KEY, TRACT_NO, # census tract code & compound key 
                  FELONY_TYPE,  FEL_NTA_CNT, FEL_CT_CNT, # felonies 
                  OFN_NTA_CNT, OFN_CT_CNT) %>% # can include OFFENSE_LV if we want more than felonies
    # ------------------------------
  distinct() %>% # only distinct ones 
    na.omit() # remove na
  
  return(bk_crime_count)
}

crimeCount_2 <- function(year) {
  
  df <- nypd_brooklyn_data
  
  filtered_year <- df %>%
    filter(df$REPORTED_DATE >=
             as.Date(paste(year, "-01-01", sep="")) & # from January 1st that year
             df$REPORTED_DATE <=
             as.Date(paste(year, "-12-31", sep=""))) %>%  # to Dec 31st that year
    na.omit()
  
  # rename df to nypd_brooklyn
  nypd_brooklyn <- filtered_year
  
  ### --- adding spatial attributes to crime data --- ##
  # brooklyn crimes into 
  bk_crime_coord <- st_as_sf(nypd_brooklyn, coords = c("X_COORD_CD", "Y_COORD_CD"), crs = 2263) 
  bk_crime_tract <- st_join(bk_crime_coord, tract_sf, join = st_within)
  ## ??? ^^^W hy is there data from other boros? ^^ ### 
  
  # data manipulation to create Crime Count in Brooklyn dataframe 
  bk_crime_count <- as_tibble(bk_crime_tract) %>% 
    # ------------------------------
  # getting felony counts by tracts, neighborhoods
  add_count(NTA_NAME, name = "FEL_NTA_CNT") %>% # felony neighborhood count
    add_count(BORO_CT_KEY, name = "FEL_CT_CNT") %>% # felony census tract count
    # ------------------------------
  # getting overall offense counts by tracts, neighborhoods
  group_by(OFFENSE_DESC) %>% # offense description
    add_count(NTA_NAME, name = "OFN_NTA_CNT") %>% # offense neighborhood count
    add_count(BORO_CT_KEY, name = "OFN_CT_CNT") %>%  # offense census tract count
    rename("FELONY_TYPE" = "OFFENSE_DESC") %>% # type of felony, bc crimes here are felonies only
    # ------------------------------
  filter(BORO_NAME == "Brooklyn") %>% # only for Brooklyn borough
    dplyr::select(NTA_CODE, NTA_NAME, # neighborhood code & name
                  BORO_NAME, # brooklyn 
                  BORO_CT_KEY, TRACT_NO, # census tract code & compound key 
                  FELONY_TYPE,  FEL_NTA_CNT, FEL_CT_CNT, # felonies 
                  OFN_NTA_CNT, OFN_CT_CNT) %>% # can include OFFENSE_LV if we want more than felonies
    # ------------------------------
  distinct() %>% # only distinct ones 
    na.omit() # remove na
  
  return(bk_crime_count)
}

# ******************** //////// ************************* # 

# bk_count_2010 <- crimeCount(2010)
# # # head(bk_count_2010)
# View(bk_count_2010)
# 
# bk_count_2018 <- crimeCount(2018)
# # # View(bk_count_2018)
# View(bk_count_2018)
# 
# 


# ---------------------------------------------------------- # 
# ------------ GET CENSUS GENTRIFICATION DATA  ------------- #
# ---------------------------------------------------------- # 


# ******************** FUNCTION ************************* # 

# get census data from the year

bkGentri <- function(year) {
  
  census_data_tract <- get_acs(geography = "tract", # tract  
                               variables = c(TOTAL_POPN = "B01003_001E", # demographic 
                                             WHITE_ALONE = "B03002_003E",
                                             FOREIGN_BORN = "B05012_003E",
                                             NO_HIGHSCH = "B06009_002E"),
                               output = "wide",
                               county = "Kings", 
                               state = "NY",
                               year = year)
  
  census_data_tract %>% 
    mutate(YEAR = year) %>% 
    dplyr::select(GEOID, NAME, TOTAL_POPN, WHITE_ALONE, FOREIGN_BORN, NO_HIGHSCH, YEAR)
  
  
} 


# ******************** /////// ************************* # 

# bk_gentri_2018 <- bkGentri(2018)
# # bk_gentri_2017 <- bk_gentri(2017)
# bk_gentri_2010 <- bkGentri(2010)
# # 
# 

# ---------------------------------------------------------------------------- # 
# ------------ PERCENTAGE CHANGE IN VARIABLES FOR GENTRIFICATION ------------- #
# ---------------------------------------------------------------------------- # 


# ******************** FUNCTION ************************* # 
# Calculate change


calcChange <- function(bk_gentri_1, bk_gentri_2) {
  
  # two years 
  gen_1 <- bk_gentri_1 # earlier year
  gen_2 <- bk_gentri_2 # later year
  
  bk_gen_both <- gen_2 %>% # later year
    left_join(gen_1, by = "GEOID")# %>% #recent year
  
  # calculating percentage change   
  
  bk_gen_change <- bk_gen_both %>% 
    mutate(POPN_CHANGE = (bk_gen_both$TOTAL_POPN.x/bk_gen_both$TOTAL_POPN.y*100)-100) %>% 
    mutate(WHITE_CHANGE = (WHITE_ALONE.x/TOTAL_POPN.x-WHITE_ALONE.y/TOTAL_POPN.y)*100) %>%
    mutate(FOREIGN_CHANGE = (FOREIGN_BORN.x/TOTAL_POPN.x-FOREIGN_BORN.y/TOTAL_POPN.y)*100) %>%
    mutate(NO_HS_CHANGE = (NO_HIGHSCH.x/TOTAL_POPN.x-NO_HIGHSCH.y/TOTAL_POPN.y)*100) %>%
    
    # computing gentrification score
    mutate(GENTRI_SCORE = (WHITE_CHANGE + FOREIGN_CHANGE - NO_HS_CHANGE)/3) %>%
    # 
    # # rename
    rename("CENSUS_TRACT" = "NAME.x", 
           "YEAR" = "YEAR.x", 
           "TOTAL_POPN_1" = "TOTAL_POPN.y",
           "TOTAL_POPN_2" = "TOTAL_POPN.x") %>%
    dplyr::select(GEOID, CENSUS_TRACT, YEAR, GENTRI_SCORE, 
                  TOTAL_POPN_1, TOTAL_POPN_2, POPN_CHANGE, 
                  WHITE_CHANGE, FOREIGN_CHANGE, NO_HS_CHANGE) %>%
    na.omit()
  
  return(bk_gen_change)
  
}

# ******************** /////// ************************* # 

# # testing for % change between 2010-2018
# bk_gen_2010_2018 <- calcChange(bk_gentri_2010, bk_gentri_2018)
# # head(bk_gen_18_10)
# View(bk_gen_10_18)

########### -------- load tracts.shp file ------------------- ########

# has GEOID and TRACT no. 
tracts <- read_sf("data/boundaries/tracts/tl_2018_36_tract.shp")



# ---------------------------------------------------------------------------- # 
# ------------ MERGING GENTRIFICATION AND CRIME DATA + GEO  ------------------ #
# ---------------------------------------------------------------------------- # 

# ******************** FUNCTION ************************* # 
mergeTracts <- function(bk_gen_1_2, bk_count_1, bk_count_2) {
  
  # set year range 
  # year_range <- paste(earlier_year, later_year, sep = "_")
  # 
  # # crime count 
  # # bk_count_1 <- get(paste("bk_count", earlier_year, sep = "_"))
  # # bk_count_2 <- get(paste("bk_count", later_year, sep = "_")) # this only takes latest year 
  # # 
  # # gentrification (change)
  # bk_gen <- get(paste("bk_gen", year_range, sep = "_")) # year range: e.g. 10_18 for 2010-2018
  bk_gen <- bk_gen_1_2
  # 
  # merge spatial data (GEOID, Tract No.) with gentrification (GEOID)
  tracts_geoid <- merge(tracts, bk_gen, by.x="GEOID", by.y="GEOID")
  geoid_df <- as_tibble(tracts_geoid)
  
  
  # ----
  # merge gentrification & spatial with crime 
  bk_gentri_crime_1 <- geoid_df %>%
    rename("TRACT_NO" = "NAME") %>%
    
    # with earlier year crime data 
    left_join(bk_count_1, by = "TRACT_NO") %>% # Merge with Tract No.
    # mutate(OFN_NTA_PAX = OFN_NTA_CNT/TOTAL_POPN) %>%
    # mutate(OFN_CT_PAX = OFN_CT_CNT/TOTAL_POPN) %>%
    mutate(FEL_CT_PAX = FEL_CT_CNT/TOTAL_POPN_1) %>%
    rename(FEL_CT_PAX_1 = FEL_CT_PAX,
           FEL_CT_CNT_1 = FEL_CT_CNT) %>%
    # select variables to prevent repetition
    dplyr::select("YEAR",
                  "GEOID", "TRACT_NO", "CENSUS_TRACT", 
                  "TOTAL_POPN_1", "TOTAL_POPN_2", "POPN_CHANGE", 
                  "GENTRI_SCORE", "WHITE_CHANGE", "FOREIGN_CHANGE", "NO_HS_CHANGE",
                  "FEL_CT_PAX_1", "FEL_CT_CNT_1") 
  
  bk_gentri_crime <- bk_gentri_crime_1 %>%
    # with latest year crime data 
    left_join(bk_count_2, by = "TRACT_NO") %>%
    mutate(FEL_CT_PAX = FEL_CT_CNT/TOTAL_POPN_2) %>%
    rename(FEL_CT_PAX_2 = FEL_CT_PAX,
           FEL_CT_CNT_2 = FEL_CT_CNT) %>%
    
    # select variables
    dplyr::select("BORO_NAME", "YEAR",
                  "GEOID", "NTA_NAME", "NTA_CODE", "BORO_CT_KEY", "TRACT_NO", "CENSUS_TRACT", 
                  "TOTAL_POPN_1", "TOTAL_POPN_2", "POPN_CHANGE", 
                  "GENTRI_SCORE", "WHITE_CHANGE", "FOREIGN_CHANGE", "NO_HS_CHANGE",
                  "FEL_CT_PAX_1", "FEL_CT_CNT_1",
                  "FEL_CT_PAX_2", "FEL_CT_CNT_2") %>%
    # "OFN_NTA_PAX", "OFN_NTA_CNT",
    # "OFN_CT_PAX", "OFN_CT_CNT") %>%
    distinct() %>%
    na.omit()
  
}


# Includes Offenses 

mergeTracts_2 <- function(bk_gen_1_2, bk_count_1, bk_count_2) {
  
 
  bk_gen <- bk_gen_1_2
  # 
  # merge spatial data (GEOID, Tract No.) with gentrification (GEOID)
  tracts_geoid <- merge(tracts, bk_gen, by.x="GEOID", by.y="GEOID")
  geoid_df <- as_tibble(tracts_geoid)
  
  # ----
  # merge gentrification & spatial with crime 
  bk_gentri_crime_1 <- geoid_df %>%
    rename("TRACT_NO" = "NAME") %>%
    
    # with earlier year crime data 
    left_join(bk_count_1, by = "TRACT_NO") %>% # Merge with Tract No.
    mutate(OFN_CT_PAX_1 = OFN_CT_CNT/TOTAL_POPN_1) %>%
    mutate(FEL_CT_PAX_1 = FEL_CT_CNT/TOTAL_POPN_1) %>%
    rename(OFN_CT_CNT_1 = OFN_CT_CNT) %>%
    rename(FEL_CT_CNT_1 = FEL_CT_CNT) %>%
    # select variables to prevent repetition
    dplyr::select("YEAR",
                  "GEOID", "TRACT_NO", "CENSUS_TRACT", 
                  "TOTAL_POPN_1", "TOTAL_POPN_2", "POPN_CHANGE", 
                  "GENTRI_SCORE", "WHITE_CHANGE", "FOREIGN_CHANGE", "NO_HS_CHANGE",
                  "FEL_CT_PAX_1", "FEL_CT_CNT_1",  "FELONY_TYPE", "OFN_CT_PAX_1", "OFN_CT_CNT_1") 
  
  bk_gentri_crime <- bk_gentri_crime_1 %>%
    # with latest year crime data 
    left_join(bk_count_2, by = c("TRACT_NO", "FELONY_TYPE")) %>%
    mutate(FEL_CT_PAX_2 = FEL_CT_CNT/TOTAL_POPN_2) %>%
    mutate(OFN_CT_PAX_2 = OFN_CT_CNT/TOTAL_POPN_2) %>%
    rename(OFN_CT_CNT_2 = OFN_CT_CNT) %>%
    rename(FEL_CT_CNT_2 = FEL_CT_CNT) %>%
    # select variables
    dplyr::select("BORO_NAME", "YEAR",
                  "GEOID", "NTA_NAME", "NTA_CODE", "BORO_CT_KEY", "TRACT_NO", "CENSUS_TRACT", 
                  "TOTAL_POPN_1", "TOTAL_POPN_2", "POPN_CHANGE", 
                  "GENTRI_SCORE", "WHITE_CHANGE", "FOREIGN_CHANGE", "NO_HS_CHANGE", "FELONY_TYPE",
                  "FEL_CT_PAX_1", "FEL_CT_PAX_2", "OFN_CT_PAX_1", "OFN_CT_PAX_2", 
                  "FEL_CT_CNT_1", "OFN_CT_CNT_1", "FEL_CT_CNT_2", "OFN_CT_CNT_2") %>%
    
    distinct() %>%
    na.omit()
  
}


# ******************** /////// ************************* # 


# get the mean for NTA e.g. mean Felony rate and change, mean Gentri score, remove BK99, 
calcScore <- function(bk_gen_crime) {
  
  bk_gen_crime %>%
    filter(NTA_CODE != "BK99") %>%
    group_by(NTA_CODE) %>%
    mutate(NTA_GENTRI_SCORE = mean(GENTRI_SCORE)) %>% 
    mutate(FEL_NTA_CNT_1 = sum(FEL_CT_CNT_1)) %>%  
    mutate(FEL_NTA_CNT_2 = sum(FEL_CT_CNT_2)) %>%
    mutate(TOTAL_POPN_NTA_1 = sum(TOTAL_POPN_1)) %>%
    mutate(TOTAL_POPN_NTA_2 = sum(TOTAL_POPN_2)) %>%
    mutate(FEL_NTA_PAX_1 = FEL_NTA_CNT_1/TOTAL_POPN_NTA_1) %>%  
    mutate(FEL_NTA_PAX_2 = FEL_NTA_CNT_2/TOTAL_POPN_NTA_2) %>% 
    mutate(FEL_NTA_CHANGE = FEL_NTA_PAX_2 - FEL_NTA_PAX_1) %>% 
    # for nta only 
    dplyr::select(BORO_NAME, NTA_NAME, NTA_CODE, NTA_GENTRI_SCORE, FEL_NTA_PAX_1, FEL_NTA_PAX_2, FEL_NTA_CHANGE) %>% 
    distinct()
  
}

mergeTracts_2 <- function(bk_gen_1_2, bk_count_1, bk_count_2) {
  
  # set year range 
  # year_range <- paste(earlier_year, later_year, sep = "_")
  # 
  # # crime count 
  # # bk_count_1 <- get(paste("bk_count", earlier_year, sep = "_"))
  # # bk_count_2 <- get(paste("bk_count", later_year, sep = "_")) # this only takes latest year 
  # # 
  # # gentrification (change)
  # bk_gen <- get(paste("bk_gen", year_range, sep = "_")) # year range: e.g. 10_18 for 2010-2018
  bk_gen <- bk_gen_1_2
  # 
  # merge spatial data (GEOID, Tract No.) with gentrification (GEOID)
  tracts_geoid <- merge(tracts, bk_gen, by.x="GEOID", by.y="GEOID")
  geoid_df <- as_tibble(tracts_geoid)
  
  # ----
  # merge gentrification & spatial with crime 
  bk_gentri_crime_1 <- geoid_df %>%
    rename("TRACT_NO" = "NAME") %>%
    
    # with earlier year crime data 
    left_join(bk_count_1, by = "TRACT_NO") %>% # Merge with Tract No.
    mutate(OFN_CT_PAX_1 = OFN_CT_CNT/TOTAL_POPN_1) %>%
    mutate(FEL_CT_PAX_1 = FEL_CT_CNT/TOTAL_POPN_1) %>%
    rename(OFN_CT_CNT_1 = OFN_CT_CNT) %>%
    rename(FEL_CT_CNT_1 = FEL_CT_CNT) %>%
    # select variables to prevent repetition
    dplyr::select("YEAR",
                  "GEOID", "TRACT_NO", "CENSUS_TRACT", 
                  "TOTAL_POPN_1", "TOTAL_POPN_2", "POPN_CHANGE", 
                  "GENTRI_SCORE", "WHITE_CHANGE", "FOREIGN_CHANGE", "NO_HS_CHANGE",
                  "FEL_CT_PAX_1", "FEL_CT_CNT_1",  "FELONY_TYPE", "OFN_CT_PAX_1", "OFN_CT_CNT_1") 
  
  bk_gentri_crime <- bk_gentri_crime_1 %>%
    # with latest year crime data 
    left_join(bk_count_2, by = c("TRACT_NO", "FELONY_TYPE")) %>%
    mutate(FEL_CT_PAX_2 = FEL_CT_CNT/TOTAL_POPN_2) %>%
    mutate(OFN_CT_PAX_2 = OFN_CT_CNT/TOTAL_POPN_2) %>%
    rename(OFN_CT_CNT_2 = OFN_CT_CNT) %>%
    rename(FEL_CT_CNT_2 = FEL_CT_CNT) %>%
    # select variables
    dplyr::select("BORO_NAME", "YEAR",
                  "GEOID", "NTA_NAME", "NTA_CODE", "BORO_CT_KEY", "TRACT_NO", "CENSUS_TRACT", 
                  "TOTAL_POPN_1", "TOTAL_POPN_2", "POPN_CHANGE", 
                  "GENTRI_SCORE", "WHITE_CHANGE", "FOREIGN_CHANGE", "NO_HS_CHANGE", "FELONY_TYPE",
                  "FEL_CT_PAX_1", "FEL_CT_PAX_2", "OFN_CT_PAX_1", "OFN_CT_PAX_2", 
                  "FEL_CT_CNT_1", "OFN_CT_CNT_1", "FEL_CT_CNT_2", "OFN_CT_CNT_2") %>%
    
    distinct() %>%
    na.omit()
  
}

calcScore_OFN <- function(bk_gen_crime) {
  
  bk_gen_crime %>%
    filter(NTA_CODE != "BK99") %>%
    group_by(NTA_CODE, FELONY_TYPE) %>%
    mutate(NTA_OFN_GENTRI_SCORE = mean(GENTRI_SCORE)) %>% 
    mutate(TOTAL_POPN_OFN_NTA_1 = sum(TOTAL_POPN_1)) %>%
    mutate(TOTAL_POPN_OFN_NTA_2 = sum(TOTAL_POPN_2)) %>%
    mutate(OFN_NTA_CNT_1 = sum(OFN_CT_CNT_1)) %>%  
    mutate(OFN_NTA_CNT_2 = sum(OFN_CT_CNT_2)) %>%
    mutate(OFN_NTA_PAX_1 = OFN_NTA_CNT_1/TOTAL_POPN_OFN_NTA_1) %>%  
    mutate(OFN_NTA_PAX_2 = OFN_NTA_CNT_2/TOTAL_POPN_OFN_NTA_2) %>% 
    mutate(OFN_NTA_CHANGE = OFN_NTA_PAX_2 - OFN_NTA_PAX_1) %>% 
    group_by(NTA_CODE) %>%
    mutate(NTA_GENTRI_SCORE = mean(GENTRI_SCORE)) %>% 
    mutate(TOTAL_POPN_NTA_1 = sum(TOTAL_POPN_1)) %>%
    mutate(TOTAL_POPN_NTA_2 = sum(TOTAL_POPN_2)) %>%
    mutate(FEL_NTA_CNT_1 = sum(FEL_CT_CNT_1)) %>%  
    mutate(FEL_NTA_CNT_2 = sum(FEL_CT_CNT_2)) %>%
    mutate(FEL_NTA_PAX_1 = FEL_NTA_CNT_1/TOTAL_POPN_NTA_1) %>%  
    mutate(FEL_NTA_PAX_2 = FEL_NTA_CNT_2/TOTAL_POPN_NTA_2) %>% 
    mutate(FEL_NTA_CHANGE = FEL_NTA_PAX_2 - FEL_NTA_PAX_1) %>% 
    # for nta only 
    dplyr::select(BORO_NAME, NTA_NAME, NTA_CODE, 
                  NTA_GENTRI_SCORE, NTA_OFN_GENTRI_SCORE, FELONY_TYPE, 
                  FEL_NTA_PAX_1, FEL_NTA_PAX_2, FEL_NTA_CHANGE, 
                  OFN_NTA_PAX_1, OFN_NTA_PAX_2, OFN_NTA_CHANGE) %>% 
    distinct()
  
}


calcScore_CT <- function(bk_gen_crime) {
  
  bk_gen_crime %>%
    filter(NTA_CODE != "BK99") %>%
    group_by(TRACT_NO) %>%
    mutate(CT_GENTRI_SCORE = mean(GENTRI_SCORE)) %>% 
    mutate(FEL_CT_CNT_1 = sum(FEL_CT_CNT_1)) %>%  
    mutate(FEL_CT_CNT_2 = sum(FEL_CT_CNT_2)) %>%
    mutate(TOTAL_POPN_CT_1 = sum(TOTAL_POPN_1)) %>%
    mutate(TOTAL_POPN_CT_2 = sum(TOTAL_POPN_2)) %>%
    mutate(FEL_CT_PAX_1 = FEL_CT_CNT_1/TOTAL_POPN_CT_1) %>%  
    mutate(FEL_CT_PAX_2 = FEL_CT_CNT_2/TOTAL_POPN_CT_2) %>% 
    mutate(FEL_CT_CHANGE = FEL_CT_PAX_2 - FEL_CT_PAX_1) %>% 
    # for ct only 
    dplyr::select(BORO_NAME, GEOID, TRACT_NO, CENSUS_TRACT, NTA_NAME, NTA_CODE, CT_GENTRI_SCORE, FEL_CT_PAX_1, FEL_CT_PAX_2, FEL_CT_CHANGE) %>% 
    distinct()
  
}


ntaMean <- function(bkg, year) {
  
  bkg %>% 
    mutate(gentrified = (NTA_GENTRI_SCORE > 0)) %>% 
    group_by(gentrified) %>% 
    summarise(avg_gentri = mean(NTA_GENTRI_SCORE), 
              avg_crime_rate_1 = mean(FEL_NTA_PAX_1),
              avg_crime_rate_2 = mean(FEL_NTA_PAX_2),
              crime_change = mean(FEL_NTA_CHANGE)) %>%  # crime rate per pax
    mutate(year = year)
  
}

# final_2010_2018<- mergeTracts(2010, 2018)
# # 
# View(final_2010_2018)
# 


# -------------------------------------------------------- # 
# ------------ CONVERT NYPD TO SPATIAL OBJECT ------------ #
# -------------------------------------------------------- #
# getting tract lines

# how to convert nypd dataframe into sf/geo/spatial object 
# https://erinbecker.github.io/r-raster-vector-geospatial/10-vector-csv-to-shapefile-in-r/index.html

# read lines from tract shape file 

tract_lines_bk <- st_read("data/boundaries/tracts/tl_2018_36_tract.shp")# set CRS from line from tracts.shp

nta_lines_ny <- st_read("data/ntashp/nynta.shp")# set CRS from line from tracts.shpfile 2018
# head(lines_bk)

nta_lines_bk <- nta_lines_ny %>% 
  filter(BoroName == "Brooklyn") %>%
  rename(NTA_CODE = NTACode) %>% 
  rename(NTA_NAME = NTAName)


# ******************** FUNCTION ************************* # 
# convert dataframe to plot table spatial object

toSpatial_NTA <- function(df) {
  
  merged <- merge(nta_lines_bk, df, by = c("NTA_CODE", "NTA_NAME"))
  nta_map <- merged %>% st_transform('+proj=longlat +datum=WGS84')  
  
}

toSpatial_tracts <- function(df) {
  
  merged <- merge(tract_lines_bk, df, by = "GEOID")
  tract_map <- merged %>% st_transform('+proj=longlat +datum=WGS84')  
  
}

# ******************** //////// ************************* # 



# -------------------------------------------------------- # 
# -------------------- VISUALIZATIONS -------------------- #
# -------------------------------------------------------- #



# ******************** FUNCTION ************************* # 
# plotting scatterplot of neighborhood name   

plotTextScatter <- function(bkg_year, year_1, year_2) { 
  
  bk_nta <- bkg_year %>%
    dplyr:: select(BORO_NAME, NTA_NAME, NTA_CODE, NTA_GENTRI_SCORE, FEL_NTA_CHANGE) %>% 
    # filter(NTA_GENTRI_SCORE < 0) %>%
    distinct()
  
  ggplot(data = bk_nta) + 
    
    geom_text(aes(label = NTA_NAME), 
              size = 2.5, color = "#333333", family = "raleway",
              x = bk_nta$NTA_GENTRI_SCORE,
              y = bk_nta$FEL_NTA_CHANGE) +
    
    geom_hline(yintercept = 0, color = "cyan3") +
    geom_vline(xintercept = 0, color = "darksalmon") +
    
    ylim(min(bk_nta$FEL_NTA_CHANGE)*1.2, max(bk_nta$FEL_NTA_CHANGE)*1.2) + # felony score 
    xlim(min(bk_nta$NTA_GENTRI_SCORE)*1.2, max(bk_nta$NTA_GENTRI_SCORE)*1.2) + 
    labs(title = paste("\n", year_1, "-", year_2, "\n"),
         # title = paste("Crime and Gentrification in Brooklyn (", year_1, "-", year_2, ")\n", sep = ""),  
         x = "\nGentrification Score\n", 
         y = "\nChange in Felony Crime Rate\n") +
    theme_tufte() +
    theme(text = element_text(family = "raleway"),
          plot.title = element_text(family = "montserrat"))
  
} 

# ******************** FUNCTION ************************* # 


plotMapGentri <- function(df, year_1, year_2) {
  
  ggplot(data = df) +
    geom_sf(aes(fill = NTA_GENTRI_SCORE), color = NA) +
    scale_fill_viridis_c(option = "magma", name = "Gentrification Score") +
    labs(title = paste("\n", year_1, "-", year_2, "\n")) +
    theme_map() +
    theme(text = element_text(family = "raleway", size = 12),
          plot.title = element_text(family = "montserrat", size = 18))
  
}


plotMapCrime <- function(df, year_1, year_2) {
  
  ggplot(data = df) +
    geom_sf(aes(fill = FEL_NTA_CHANGE), color = NA) +
    scale_fill_viridis_c(option = "viridis", name = "Change in Crime Rates", direction = -1) + 
    labs(title = paste("\n", year_1, "-", year_2, "\n")) + 
    theme_map() + 
    theme(text = element_text(family = "raleway", size = 12), 
          plot.title = element_text(family = "montserrat", size = 18))
  
}


# ******************** FUNCTION ************************* # 


mapAnalysis <- function(nta_map, var) {
  
  map_analysis <- nta_map %>% 
    as_Spatial()
  
  # Queens Contiguity Matrix
  spat_matrix <- poly2nb(map_analysis)
  
  # Neighbors list with Spatial Weihgts
  list_w <- nb2listw(spat_matrix)
  
  # calculate local Moran of the distribution 
  lmoran <- localmoran(map_analysis$var, list_w)
  
  # padronize the variable and save it to a new column 
  map_analysis$s_var <- scale(map_analysis$var) %>% as.vector()
  
  # create a spatially lagged variable and save it to a new column 
  map_analysis$lag_s_var <- lag.listw(list_w, map_analysis$s_var)
  
  # moran scatterplot, in basic graphics - with identificaiton of influential observations 
  x <- map_analysis$s_var
  y <- map_analysis$lag_s_var %>% as.vector()
  xx <- data.frame(x, y)
  
  g <- ggplot(xx, aes(x, y)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = F) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed")
  
  # QUADRANTS 
  
  map_analysis$quad_sig <- NA
  
  # high high quadrant 
  map_analysis[(map_analysis$s_var >= 0 & 
                  map_analysis$lag_s_var >= 0) & 
                 (lmoran[, 5] <= 0.05), "quad_sig"] <- "high-high"
  
  # low low  quadrant 
  map_analysis[(map_analysis$s_var <= 0 & 
                  map_analysis$lag_s_var <= 0) & 
                 (lmoran[, 5] <= 0.05), "quad_sig"] <- "low-low"
  
  # high low  quadrant 
  map_analysis[(map_analysis$s_var >= 0 & 
                  map_analysis$lag_s_var <= 0) & 
                 (lmoran[, 5] <= 0.05), "quad_sig"] <- "highlow"
  
  # low low  quadrant 
  map_analysis[(map_analysis$s_var <= 0 & 
                  map_analysis$lag_s_var >= 0) & 
                 (lmoran[, 5] <= 0.05), "quad_sig"] <- "low-high"
  
  # non-significant observations
  
  map_analysis@data[(lmoran[, 5] > 0.05), "quad_sig"] <- "not signif."
  map_analysis$quad_sig <- as.factor(map_analysis$quad_sig)
  map_analysis@data$id <- rownames(map_analysis@data)
  
  
  
  # plotting the map
  df <- fortify(map_analysis, region = "id") # make it ready for mapping
  df <- left_join(df, map_analysis@data)
  
  
  lisa_plot <- df %>% 
    ggplot(aes(long, lat, group = group, fill = quad_sig)) + 
    geom_polygon(color = "white", size = .05) + 
    coord_equal() + 
    labs(title = "\nLISA Clusters\n") +
    # for\nChange in Gentrification Score (2010-2018)") +
    scale_fill_manual(values = c("darksalmon", "palegreen3", "honeydew3"), name = "Quadrant\nSignificance") +
    theme_map() + 
    theme(text = element_text(family = "raleway", size = 12), 
          plot.title = element_text(family = "montserrat", size = 18, color = "grey20"))
  
  return(c(summary(lmoran), 
           # summary of variables to form the analysis 
           summary(map_analysis$s_var),
           summary(map_analysis$lag_s_var),
           g, 
           lisa_plot))
  
}