
## Thesis Code Functions ##
library(sf)
library(dplyr)
library(data.table)
library(tidycensus)

# ------------------------------------------- # 
# ------------ CRIME IN BROOKYLN ------------ #
# ------------------------------------------- # 

# https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i
nypd  <- fread("data/NYPD-2/NYPD.csv", header = TRUE)

# Sorting out NYPD Dataset

# variables renamed 
nypd_renamed <- nypd %>% 
  rename("PRECINCT" = "ADDR_PCT_CD",
         "REPORTED_DATE" = "RPT_DT", # only using crimes reported
         "OFFENSE_LV" = "LAW_CAT_CD",
         "OFFENSE_DESC" = "OFNS_DESC")
  
# only brooklyn data and relevant column
nypd_brooklyn_data <- nypd_renamed %>% 
  filter(BORO_NM == "BROOKLYN") %>% 
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
  select(ntacode, ntaname, # neighborhood code & name (55 for Brooklyn eg Crown Heights, Bushwick)
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
    select(NTA_CODE, NTA_NAME, # neighborhood code & name
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
                                       # NATIVE_BORN = "B05012_002E",
                                       FOREIGN_BORN = "B05012_003E",
                                       NO_HIGHSCH = "B06009_002E"),
                         output = "wide",
                         county = "Kings", 
                         state = "NY",
                         year = year)
  
  census_data %>% 
    mutate(YEAR = year) %>% 
    select(GEOID, NAME, TOTAL_POPN, WHITE_ALONE, FOREIGN_BORN, NO_HIGHSCH, YEAR)
  
  
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
    select(GEOID, CENSUS_TRACT, YEAR, GENTRI_SCORE, 
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


mergeTracts <- function(earlier_year, later_year) {
  
  # set year range 
  year_range <- paste(earlier_year, later_year, sep = "_")
  
  # datasets 
  # crime count 
  bk_count_earlier <- get(paste("bk_count", earlier_year, sep = "_"))
  bk_count_later <- get(paste("bk_count", later_year, sep = "_")) # this only takes latest year 
  
  # gentrification (change)
  bk_gen <- get(paste("bk_gen", year_range, sep = "_")) # year range: e.g. 10_18 for 2010-2018
  
  # merge spatial data (GEOID, Tract No.) with gentrification (GEOID)
  tracts_geoid <- merge(tracts, bk_gen, by.x="GEOID", by.y="GEOID")
  geoid_df <- as_tibble(tracts_geoid)
  
  # ----
  # merge gentrification & spatial with crime 
  bk_gentri_crime_y1 <- geoid_df %>%
    rename("TRACT_NO" = "NAME") %>%
    
    # with earlier year crime data 
    left_join(bk_count_earlier, by = "TRACT_NO") %>% # Merge with Tract No.
    # mutate(OFN_NTA_PAX = OFN_NTA_CNT/TOTAL_POPN) %>%
    # mutate(OFN_CT_PAX = OFN_CT_CNT/TOTAL_POPN) %>%
    mutate(FEL_CT_PAX = FEL_CT_CNT/TOTAL_POPN_1) %>%
    rename(FEL_CT_PAX_1 = FEL_CT_PAX,
           FEL_CT_CNT_1 = FEL_CT_CNT) %>%
    # select variables to prevent repetition
    select("YEAR",
           "GEOID", "TRACT_NO", "CENSUS_TRACT", 
           "TOTAL_POPN_1", "TOTAL_POPN_2", "POPN_CHANGE", 
           "GENTRI_SCORE", "WHITE_CHANGE", "FOREIGN_CHANGE", "NO_HS_CHANGE",
           "FEL_CT_PAX_1", "FEL_CT_CNT_1") 
  
  bk_gentri_crime <- bk_gentri_crime_y1 %>%
    # with latest year crime data 
    left_join(bk_count_later, by = "TRACT_NO") %>%
    mutate(FEL_CT_PAX = FEL_CT_CNT/TOTAL_POPN_2) %>%
    rename(FEL_CT_PAX_2 = FEL_CT_PAX,
           FEL_CT_CNT_2 = FEL_CT_CNT) %>%
    
    # select variables
    select("BORO_NAME", "YEAR",
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

# ******************** /////// ************************* # 


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

# lines_ny <- st_read("data/boundaries/tracts/tl_2018_36_tract.shp")# set CRS from line from tracts.shp
lines_ny <- st_read("data/ntashp/nynta.shp")# set CRS from line from tracts.shpfile 2018
# head(lines_bk)


# ******************** FUNCTION ************************* # 
# convert dataframe to plot table spatial object

toSpatial <- function(df) {
  
  merged <- merge(lines_ny, df)
  tract_map <- merged %>% st_transform('+proj=longlat +datum=WGS84')  
  
}

# ******************** //////// ************************* # 
