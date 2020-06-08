###################################################
# CLEANING CODE
# Data download prep, cleaning and merging
# For non-text features
###################################################


###################################################
# Script for downloading NYT cases and deaths data
# NYT data repo: https://github.com/nytimes/covid-19-data
###################################################
library(tidyverse)

today <- str_sub(Sys.Date(), start = 6L)

counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
us <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")

county.name <- "nyt-us-counties.csv"
write.csv(counties, paste0("data/raw/cases-deaths/nyt/", county.name), row.names=FALSE)
prior.county.name <- paste0("nyt-us-counties-", today, ".csv")
write.csv(counties, paste0("data/raw/cases-deaths/nyt/prior/", prior.county.name), row.names=FALSE)

state.name <- "nyt-us-states.csv"
write.csv(states, paste0("data/raw/cases-deaths/nyt/", state.name), row.names=FALSE)
prior.state.name <- paste0("nyt-us-states-", today, ".csv")
write.csv(states, paste0("data/raw/cases-deaths/nyt/prior/", prior.state.name), row.names=FALSE)

us.name <- "nyt-us.csv"
write.csv(us, paste0("data/raw/cases-deaths/nyt/", us.name), row.names=FALSE)
prior.us.name <- paste0("nyt-us-", today, ".csv")
write.csv(us, paste0("data/raw/cases-deaths/nyt/prior/", prior.us.name), row.names=FALSE)




###################################################
# Script for downloading MIT Election Data and Science Lab (MEDSL) data
# MEDSL package and data repo: https://github.com/MEDSL/elections
###################################################

# if (!require('devtools', quietly=TRUE)) install.packages('devtools')
# devtools::install_github('MEDSL/elections')

library(elections)
library(dplyr)
data(presidential_precincts_2016)
write.csv(presidential_precincts_2016, "data/raw/political/elections/medsl.csv", row.names=FALSE)




###################################################
# Build dataset of case and death counts 
###################################################

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(tidycensus)
Sys.getenv("CENSUS_API_KEY")

# Read in county size
size <- read_xls("data/raw/geographic/counties/LND01.xls") %>% 
  filter(grepl(",", Areaname)) %>% 
  select(STCOU, LND110200D) %>% 
  rename(county_fips = STCOU) %>% 
  mutate(km_sq = 2.58999*LND110200D) %>% 
  select(county_fips, km_sq)

nyc.size <- size %>% 
  filter(county_fips %in% c("36005", "36047", "36061", "36081", "36115")) %>% 
  mutate(county_fips = "36061") %>% 
  group_by(county_fips) %>% 
  summarize(km_sq = sum(km_sq))

size.fix <- size %>% 
  filter(!(county_fips %in% c("36005", "36047", "36061", "36081", "36115"))) %>% 
  rbind(nyc.size) %>% 
  arrange(county_fips)

# Get # of residents 65+ from 2016 ACS by county
v16 <- load_variables(2016, "acs5", cache=TRUE)
age <- get_acs(geography="county", year=2016, survey="acs5",
               variables=c(total="B01001_001", s1="B01001_020", s2="B01001_021", s3="B01001_022",
                           s4="B01001_023", s5="B01001_024", s6="B01001_025", s7="B01001_044",
                           s8="B01001_045", s9="B01001_046", s10="B01001_047", s11="B01001_048",
                           s12="B01001_049")) %>% 
  select(-c(moe)) %>% 
  spread(variable, estimate) %>% 
  mutate(seniors = s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11 + s12,
         share_over_65 = seniors/total) %>% 
  rename(county_fips = GEOID, county_name = NAME)

nyc.age <- age %>% 
  filter(county_fips %in% c("36005", "36047", "36061", "36081", "36115")) %>% 
  mutate(county_fips = "36061", county_name = "New York County, New York") %>% 
  group_by(county_fips, county_name) %>% 
  summarize(total = sum(total), seniors = sum(seniors)) %>% 
  ungroup() %>% 
  mutate(share_over_65 = seniors / total) %>% 
  select(county_fips, share_over_65)

age.fix <- age %>% 
  filter(!(county_fips %in% c("36005", "36047", "36061", "36081", "36115"))) %>% 
  select(county_fips, share_over_65) %>% 
  rbind(nyc.age) %>% 
  arrange(county_fips)

# Read in county population dataframe to get NYC data
nyc.pop <- read_csv("data/raw/demographic/co-est2019-alldata.csv") %>% 
  filter(STNAME=="New York" & CTYNAME %in% c("New York County", "Kings County", "Queens County", "Richmond County", "Bronx County")) %>% 
  select(POPESTIMATE2019) %>% 
  .[[1]] %>% 
  sum()

# Read in county shelter in place orders
responses.county <- read_csv("data/raw/responses/county_level_sip_updated.csv",
                             col_types = cols(state_code = col_character(), county_code = col_character()))

responses.county.fix <- responses.county %>%
  mutate(state_fips = str_pad(state_code, 2, pad = "0"),
         county_fips = str_pad(county_code, 3, pad = "0")) %>% 
  select(-c(citation1, citation2, citation3, citation4, citation5, state_code, county_code, state,
            state_sip_start_date, state_sip_end_date, county_sip_start_date, county_sip_end_date)) %>% 
  rename(state = state_name,
         county = county_name) %>% 
  # We're treating all of New York City as New York County, so drop NY counties for the other 4 boroughs
  # This is OK because all of NYC has the same start/end date for local SIP order
  filter(!(state=="New York" & 
             county %in% c("Kings County", "Queens County", "Richmond County", "Bronx County"))) %>% 
  mutate(popestimate2019 = ifelse(state=="New York" & county=="New York County", nyc.pop, popestimate2019))

# Generate daily time series from 1/27 - 5/31 for all counties
daily.county <- responses.county.fix %>% 
  distinct(state_fips, county_fips, state, county, popestimate2019) %>% 
  group_by(state_fips, county_fips, state, county, popestimate2019) %>% 
  do(data.frame(state=.$state, county=.$county, state_code=.$state_fips, county_fips=.$county_fips, popestimate2019=.$popestimate2019,
                date=seq(as.Date("01-27-20", format="%m-%d-%y"),
                         as.Date("05-31-20", format="%m-%d-%y"),
                         by="day"))) %>% 
  ungroup() %>% 
  mutate(county_fips = paste0(state_fips, county_fips),
         state = str_to_lower(state),
         county = str_to_lower(gsub(" County", "", county))) %>% 
  select(-c(state_fips, state_code))

# Read in NYT cases data
nyt <- read_csv("data/raw/cases-deaths/nyt/nyt-us-counties.csv") %>% 
  mutate(state_code = str_sub(fips, start = 1L, end = 2L),
         county_code = str_sub(fips, start = 3L, end = 5L)) %>% 
  # NYT treats the 5 counties that compose NYC as one, so we'll transform "New York City" to "New York County"
  mutate(county = ifelse(county=="New York City", "New York County", county),
         fips = ifelse(county=="New York County", "36061", fips),
         state_code = ifelse(county=="New York County", "36", state_code),
         county_code = ifelse(county=="New York County", "061", county_code),
         county_fips = paste0(state_code, county_code)) %>% 
  select(date, county_fips, cases, deaths)

# Merge daily time series with NYT case data
cases.daily <- daily.county %>% 
  left_join(nyt, by=c("county_fips", "date")) %>% 
  left_join(age.fix, by=c("county_fips")) %>% 
  left_join(size.fix, by=c("county_fips")) %>% 
  mutate(cases = ifelse(is.na(cases), 0, cases),
         deaths = ifelse(is.na(deaths), 0, deaths),
         log_cases = log(1 + cases),
         log_deaths = log(1 + deaths),
         log_pop_density = log(1 + popestimate2019 / km_sq))

# Write out daily cases
write_csv(cases.daily, "data/interim/cases-deaths/nyt/nyt_cases_deaths_daily.csv.gz")

# Create weekly cases/deaths summary (average of the measure for the week)
cases.weekly <- cases.daily %>% 
  group_by(county_fips) %>% 
  mutate(weeknum = lubridate::week(date),
         weekshift = dplyr::lead(weeknum, n=2L),
         weekshift = ifelse(is.na(weekshift), 22, weekshift)) %>% 
  group_by(county_fips, weekshift) %>% 
  mutate(daynum = row_number()) %>% 
  ungroup() %>% 
  mutate(week_start_date = ifelse(daynum==1, date, NA)) %>% 
  arrange(county_fips, date) %>% 
  group_by(county_fips) %>% 
  fill(week_start_date)

cases.weekly.summary <- cases.weekly %>% 
  arrange(county_fips, state, county, week_start_date, share_over_65, log_pop_density) %>% 
  group_by(county_fips, state, county, week_start_date, share_over_65, log_pop_density) %>% 
  summarize(mean_cases = mean(cases, na.rm=TRUE),
            mean_deaths = mean(deaths, na.rm=TRUE),
            log_cases = log(1 + mean_cases),
            log_deaths = log(1 + mean_deaths)) %>% 
  ungroup() %>% 
  mutate(week_start_date = as_date(week_start_date))

# Write out weekly cases summary
write_csv(cases.weekly.summary, "data/interim/cases-deaths/nyt/nyt_cases_deaths_weekly.csv.gz")



###################################################
# County-level census data build
###################################################

library(stringr)
library(sf)
library(data.table)
library(lubridate)
library(tidyverse)
library(tidycensus)

# Use tidycensus to directly get desired variables
# at the county level
Sys.getenv("CENSUS_API_KEY")
v16 <- load_variables(2016, "acs5", cache=TRUE)

# Find variables using v16 manual inspection

open.census.rep <- get_acs(geography="county", year=2016, survey="acs5",
                           variables=c(bachelors = "B06009_005",
                                       white = "B02001_002",
                                       black = "B02001_003",
                                       asian = "B02001_005",
                                       current_undergrad = "B14001_008",
                                       poverty = "B06012_002",
                                       hhinc_100k = "B25121_092",
                                       total = "B00001_001")) %>% 
  select(-c(moe)) %>% 
  spread(variable, estimate)

occupations <- get_acs(geography="county", year=2016, survey="acs5",
                       variables=c(total_occupations="C24050_001",
                                   msta="C24050_015",
                                   services="C24050_029",
                                   sales="C24050_043",
                                   resources="C24050_057",
                                   public_transit="B08006_008")) %>% 
  select(-c(moe)) %>% 
  spread(variable, estimate) %>% 
  mutate(share_msta = msta / total_occupations,
         share_services = services / total_occupations,
         share_sales = sales / total_occupations,
         share_resources = resources / total_occupations,
         share_public_transit = public_transit / total_occupations) %>% 
  select(GEOID, NAME, share_msta, share_services, share_sales, share_resources, share_public_transit)

total.pop <- read_csv("data/raw/demographic/county_pop_2016.csv") %>% 
  mutate(NAME = str_trim(str_sub(county, start=2L, end=-1L))) %>% 
  select(-c(county))

# Final output with shares
output <- open.census.rep %>% 
  left_join(total.pop, by=c("NAME")) %>% 
  mutate(share_asian = asian / population,
         share_black = black / population,
         share_white = white / population,
         share_bachelors = bachelors / population,
         share_undergrad = current_undergrad / population,
         share_hhinc_100k = hhinc_100k / population,
         share_poverty = poverty / population) %>% 
  left_join(occupations, by=c("NAME", "GEOID")) %>% 
  separate(NAME, into=c("countyname", "statename"), sep=",") %>% 
  mutate(county = str_to_lower(gsub(" County", "", countyname)),
         state = str_to_lower(statename)) %>% 
  rename(county_fips = GEOID) %>% 
  select(county_fips, county, state, starts_with("share"))

write_csv(output, "data/interim/demographic/economic_controls_census.csv")



###################################################
# Script to get county of POIs in safegraph data
# As described in Allcott et al (2020) Appendix A.1.1, step 2
###################################################

library(sp)
library(maps)
library(maptools)
library(data.table)
library(tidyverse)

core1 <- data.table::fread("data/raw/geographic/safegraph/CorePlacesMay/core_poi-part1.csv.gz")
core2 <- data.table::fread("data/raw/geographic/safegraph/CorePlacesMay/core_poi-part2.csv.gz")
core3 <- data.table::fread("data/raw/geographic/safegraph/CorePlacesMay/core_poi-part3.csv.gz")
core4 <- data.table::fread("data/raw/geographic/safegraph/CorePlacesMay/core_poi-part4.csv.gz")
core5 <- data.table::fread("data/raw/geographic/safegraph/CorePlacesMay/core_poi-part5.csv.gz")

# Function for converting lat/lon to county
# Source: https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon per county
  counties <- maps::map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- maptools::map2SpatialPolygons(counties, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- sp::SpatialPoints(pointsDF, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- sp::over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

conversion.wrapper <- function(core.df) {
  # Wrapper function that calls latlong2county on a core POI dataframe,
  # find the county names, and rebinds with the safegraph ID and NAICS code
  core.sub <- core.df %>% select(safegraph_place_id, naics_code, longitude, latitude)
  lat.lon <- core.df %>% select(longitude, latitude)
  
  names <- tibble::enframe(latlong2county(lat.lon), value="result") %>% 
    select(-c(name)) %>% 
    separate(result, c("state", "county"), sep=",")
  
  paired <- cbind(core.sub, names)
  paired
}

core1.processed <- conversion.wrapper(core1)
core2.processed <- conversion.wrapper(core2)
core3.processed <- conversion.wrapper(core3)
core4.processed <- conversion.wrapper(core4)
core5.processed <- conversion.wrapper(core5)

core.final <- rbind(core1.processed, core2.processed, core3.processed, core4.processed, core5.processed)

# As a sanity check, about 4.37% of observations have null county values
nas <- core.final %>% filter(is.na(county)) %>% nrow()
nas / nrow(core.final)

write_csv(core.final, "data/interim/geographic/safegraph/poi_counties.csv.gz")



###################################################
# Script to perform data build for Safegraph social distancing data
###################################################

library(stringr)
library(data.table)
library(lubridate)
library(tidyverse)

# Create list of daily dataframes to process
root.dir <- "data/raw/mobility/safegraph/social-distancing/"
all.files <- list.files(root.dir, recursive=TRUE)
n <- length(all.files)

# Process each daily df and store it in a list
all.dfs <- list()
i <- 1
for (path in all.files) {
  print(paste0("Processing file ", i))
  file.path <- paste0(root.dir, path)
  df <- data.table::fread(file.path) %>% 
    mutate(block_group = str_pad(as.character(origin_census_block_group), 12, side=c("left"), pad="0"),
           county_fips = str_sub(block_group, start=1L, end=5L),
           date = as.Date(str_sub(date_range_start, start=1L, end=10L), "%Y-%m-%d")) %>% 
    select(county_fips, date, device_count, completely_home_device_count, median_home_dwell_time,
           median_non_home_dwell_time, candidate_device_count)
  all.dfs[[i]] <- df
  i <- i + 1
}

# Combine daily dfs into one master df
all.social.distancing <- bind_rows(all.dfs)

# Identify county_fips that don't have full coverage
coverage <- all.social.distancing %>% 
  group_by(county_fips, date) %>% 
  summarize(count = n())

fips.not.full <- as.data.frame(table(coverage$county_fips)) %>% 
  filter(Freq != 128) %>% 
  mutate(Var1 = as.character(Var1)) %>% 
  pull(Var1)

asd.munge <- all.social.distancing %>% 
  mutate(state_fips = str_sub(county_fips, start=1L, end=2L)) %>% 
  # Drop Alaska
  filter(state_fips != "02") %>% 
  # Drop county_fips without full coverage
  filter(!(county_fips %in% fips.not.full)) %>% 
  mutate(share_of_devices = 1 - (completely_home_device_count / device_count),
         share_of_candidate_devices = 1 - (completely_home_device_count / candidate_device_count))

initial_device_count <- asd.munge %>% 
  filter(date >= as.Date("2020-01-27", "%Y-%m-%d") & date <= as.Date("2020-02-02", "%Y-%m-%d")) %>% 
  group_by(county_fips) %>% 
  summarize(initial_device_count = mean(device_count, na.rm=TRUE))


# Create DAILY social distancing measure at county level by aggregating across block groups
social.distancing.daily <- asd.munge %>% 
  select(-c(state_fips)) %>% 
  left_join(initial_device_count, by=c("county_fips")) %>% 
  mutate(numerator = ifelse(completely_home_device_count + initial_device_count - device_count < 0, 0,
                            completely_home_device_count + initial_device_count - device_count),
         share_adj_attrition = 1 - (numerator/initial_device_count)) %>% 
  group_by(county_fips, date) %>% 
  summarize(device_count = sum(device_count, na.rm=TRUE),
            completely_home_device_count = sum(completely_home_device_count, na.rm=TRUE),
            share_of_devices = mean(share_of_devices, na.rm=TRUE),
            share_of_candidate_devices = mean(share_of_candidate_devices, na.rm=TRUE),
            share_adj_attrition = mean(share_adj_attrition, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(county_fips, date)

weighted.mean <- asd.munge %>% 
  rename(device_count_new = device_count) %>% 
  select(-c(completely_home_device_count)) %>% 
  left_join(select(social.distancing.daily, county_fips, date, device_count), by=c("county_fips", "date")) %>% 
  mutate(weight = device_count_new / device_count,
         product1 = weight*median_home_dwell_time,
         product2 = weight*median_non_home_dwell_time) %>% 
  group_by(county_fips, date) %>% 
  summarize(median_home_dwell_time = sum(product1),
            median_non_home_dwell_time = sum(product2)) %>% 
  ungroup()

social.distancing.daily.merge <- social.distancing.daily %>% 
  left_join(weighted.mean, by=c("county_fips", "date")) %>% 
  mutate(devices_leaving_home = device_count - completely_home_device_count,
         log_devices_leaving_home = log(1 + devices_leaving_home),
         log_median_home_dwell_time = log(1 + median_home_dwell_time),
         log_median_non_home_dwell_time = log(1 + median_non_home_dwell_time)) %>% 
  select(county_fips, date, device_count, completely_home_device_count,
         devices_leaving_home, log_devices_leaving_home, 
         median_home_dwell_time, log_median_home_dwell_time,
         median_non_home_dwell_time, log_median_non_home_dwell_time,
         share_of_devices, share_of_candidate_devices, share_adj_attrition)

write_csv(social.distancing.daily.merge, "data/interim/mobility/safegraph/social-distancing/social_distancing_daily.csv.gz")




# Create WEEKLY social distancing measure at county level by aggregating across block groups
social.distancing.weekly <- social.distancing.daily.merge %>% 
  filter(date <= as.Date("2020-05-31", "%Y-%m-%d")) %>% 
  group_by(county_fips) %>% 
  mutate(weeknum = week(date),
         weekshift = dplyr::lead(weeknum, n=2L),
         weekshift = ifelse(is.na(weekshift), 22, weekshift)) %>% 
  group_by(county_fips, weekshift) %>% 
  mutate(daynum = row_number(),
         week_start_date = ifelse(daynum==1, date, NA)) %>% 
  fill(week_start_date)

social.distancing.weekly.summary <- social.distancing.weekly %>% 
  group_by(county_fips, week_start_date) %>% 
  summarize(device_count = sum(device_count, na.rm=TRUE),
            completely_home_device_count = sum(completely_home_device_count, na.rm=TRUE),
            share_of_devices = mean(share_of_devices, na.rm=TRUE),
            share_of_candidate_devices = mean(share_of_candidate_devices, na.rm=TRUE),
            share_adj_attrition = mean(share_adj_attrition, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(county_fips, week_start_date)

weekly.weighted.mean <- social.distancing.weekly %>% 
  rename(device_count_new = device_count) %>% 
  select(-c(completely_home_device_count)) %>% 
  left_join(select(social.distancing.weekly.summary, county_fips, week_start_date, device_count), by=c("county_fips", "week_start_date")) %>% 
  mutate(weight = device_count_new / device_count,
         product1 = weight*median_home_dwell_time,
         product2 = weight*median_non_home_dwell_time) %>% 
  group_by(county_fips, week_start_date) %>% 
  summarize(median_home_dwell_time = sum(product1, na.rm=TRUE),
            median_non_home_dwell_time = sum(product2, na.rm=TRUE))

social.distancing.weekly.merge <- social.distancing.weekly.summary %>% 
  left_join(weekly.weighted.mean, by=c("county_fips", "week_start_date")) %>% 
  mutate(week_start_date = as_date(week_start_date)) %>% 
  filter(week_start_date != as.Date("2020-01-26", "%Y-%m-%d")) %>% 
  mutate(devices_leaving_home = device_count - completely_home_device_count,
         log_devices_leaving_home = log(1 + devices_leaving_home),
         log_median_home_dwell_time = log(1 + median_home_dwell_time),
         log_median_non_home_dwell_time = log(1 + median_non_home_dwell_time)) %>% 
  select(county_fips, week_start_date, device_count, completely_home_device_count,
         devices_leaving_home, log_devices_leaving_home, 
         median_home_dwell_time, log_median_home_dwell_time,
         median_non_home_dwell_time, log_median_non_home_dwell_time,
         share_of_devices, share_of_candidate_devices, share_adj_attrition)

write_csv(social.distancing.weekly.merge, "data/interim/mobility/safegraph/social-distancing/social_distancing_weekly.csv.gz")


###################################################
# Script to produce data build for county-level POI and visits
# As described in Allcott et al (2020) Appendix A.1.1, step 3
# Merge county name from POI data build
###################################################

library(tidyverse)
library(stringr)
library(data.table)

safegraph.counties <- data.table::fread("data/interim/geographic/safegraph/poi_counties.csv.gz")

# Create list of files to process
in.root.dir <- "data/raw/mobility/safegraph/weekly-patterns/main-file/"
out.root.dir <- "data/interim/mobility/safegraph/weekly-patterns/main-file/"
all.files <- list.files(root.dir, recursive=TRUE)
n <- length(all.files)

# Loop through each main file and process
for (path in all.files) {
  print(paste0("Processing file ", i, " of ", n))
  in.file.path <- paste0(in.root.dir, path)
  out.file.path <- paste0(out.root.dir, "processed_", path)
  print(paste0("    File path: ", in.file.path))
  df <- data.table::fread(in.file.path)
  
  print("    Producing weekly aggregates...")
  weekly <- process.weekly(df)
  write_csv(weekly[[1]], paste0(out.root.dir, "processed_weekly_", path))
  write_csv(weekly[[2]], paste0(out.root.dir, "processed_weekly_naics_", path))
  rm(weekly)
  rm(df)
  gc()
  
  print("    Producing daily aggregates...")
  daily <- process.daily(df)
  write_csv(daily[[1]], paste0(out.root.dir, "processed_daily_", path))
  write_csv(daily[[2]], paste0(out.root.dir, "processed_daily_naics_", path))
  rm(daily)
  rm(df)
  gc()
  print(paste0("Current memory size: ", memory.size()))
  i <- i + 1
}

### HELPER FUNCTIONS FOR PROCESSING RAW FILES
process.weekly <- function(patterns) {
  weekly <- short %>% 
    select(safegraph_place_id, date_range_start, visits_by_day,
           raw_visit_counts, raw_visitor_counts, poi_cbg, visitor_home_cbgs) %>% 
    mutate(date_week_start = as.Date(str_sub(date_range_start, start=1L, end=10L), "%Y-%m-%d")) %>% 
    left_join(select(safegraph.counties, safegraph_place_id, naics_code, state, county), by=c("safegraph_place_id"))
  
  visitor.cbg.weekly <- weekly %>% 
    mutate(visitor_cbgs = gsub("[{}]", "", gsub('\"', '', visitor_home_cbgs))) %>% 
    # Keep only count of visitors from same CBG as the POI
    separate(visitor_cbgs, into = c("vc1"), sep = ",") %>% 
    # Keep only the first CBG and count from that CBG. If there are visitors from the same CBG
    # as the POI in question, it is stored first
    separate(vc1, into = c("visitor_home_cbg", "count")) %>% 
    # Only keep counts if the visitor home CBG matches the POI CBG
    # Calculate visitors from same CBG as POI and different CBG
    mutate(count = ifelse(is.na(count), 0, as.integer(count)),
           raw_visitor_counts_same_cbg = ifelse(visitor_home_cbg==poi_cbg, count, 0),
           raw_visitor_counts_same_cbg = pmin(raw_visitor_counts, raw_visitor_counts_same_cbg),
           raw_visitor_counts_diff_cbg = raw_visitor_counts - raw_visitor_counts_same_cbg)
  
  weekly.agg <- visitor.cbg.weekly %>% 
    group_by(state, county, date_week_start) %>% 
    summarize(total_visits = sum(raw_visit_counts, na.rm=TRUE),
              total_visitors = sum(raw_visitor_counts, na.rm=TRUE),
              total_visitors_same_cbg = sum(raw_visitor_counts_same_cbg, na.rm=TRUE),
              total_visitors_diff_cbg = sum(raw_visitor_counts_diff_cbg, na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(state, county, date_week_start)
  
  weekly.naics <- visitor.cbg.weekly %>% 
    mutate(naics = str_sub(naics_code, start=1L, end=2L)) %>% 
    group_by(state, county, naics, date_week_start) %>% 
    summarize(total_visits = sum(raw_visit_counts, na.rm=TRUE),
              total_visitors = sum(raw_visitor_counts, na.rm=TRUE),
              total_visitors_same_cbg = sum(raw_visitor_counts_same_cbg, na.rm=TRUE),
              total_visitors_diff_cbg = sum(raw_visitor_counts_diff_cbg, na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(state, county, naics, date_week_start)
  
  return.list <- list(weekly.agg, weekly.naics)
  return.list
}

process.daily <- function(patterns) {
  
  daily <- patterns %>% 
    select(safegraph_place_id, date_range_start, visits_by_day) %>% 
    mutate(visits_by_day = str_sub(visits_by_day, start=2L, end=-2L)) %>% 
    separate(visits_by_day, into=c("d1", "d2", "d3", "d4", "d5", "d6", "d7"), sep=",") %>% 
    gather(key="day", value="visits", d1:d7) %>% 
    arrange(safegraph_place_id) %>% 
    group_by(safegraph_place_id) %>% 
    mutate(visits = as.integer(visits),
           count = row_number()) %>% 
    ungroup() %>% 
    mutate(week_start_date = as.Date(str_sub(date_range_start, start=1L, end=10L), "%Y-%m-%d"),
           date = week_start_date + count - 1) %>% 
    select(safegraph_place_id, date, visits) %>% 
    left_join(select(safegraph.counties, safegraph_place_id, naics_code, state, county), by=c("safegraph_place_id"))
  
  daily.agg <- daily %>% 
    group_by(state, county, date) %>% 
    summarize(total_visits = sum(visits, na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(state, county, date)
  
  daily.naics <- daily %>% 
    mutate(naics = str_sub(naics_code, start=1L, end=2L)) %>% 
    group_by(state, county, naics, date) %>% 
    summarize(total_visits = sum(visits, na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(state, county, naics, date)
  
  return.list <- list(daily.agg, daily.naics)
  return.list
}

### Read weekly patterns dfs back in and process
weekly.dfs <- list()
interim.root.dir <- "data/interim/mobility/safegraph/weekly-patterns/main-file/"
interim.files <- list.files(interim.root.dir, pattern="^processed_weekly_2020")
i <- 1
for (path in interim.files) {
  df <- read_csv(paste0(interim.root.dir, path))
  weekly.dfs[[i]] <- df
  i <- i + 1
}

countyfips <- read_csv("data/raw/geographic/counties/countyfips.csv.gz")

# Generate alternate dependent variable specifications
all.weekly <- bind_rows(weekly.dfs) %>% 
  mutate(log_total_visits = log(1 + total_visits),
         log_total_visitors = log(1 + total_visitors),
         share_visitors_same_cbg = total_visitors_same_cbg / total_visitors,
         share_visitors_diff_cbg = total_visitors_diff_cbg / total_visitors) %>% 
  select(state, county, date_week_start, 
         total_visits, log_total_visits,
         total_visitors, log_total_visitors,
         total_visitors_same_cbg, share_visitors_same_cbg,
         total_visitors_diff_cbg, share_visitors_diff_cbg) %>% 
  rename(week_start_date = date_week_start)

all.weekly <- all.weekly %>% 
  left_join(countyfips, by=c("state", "county")) %>% 
  select(state, county, county_fips, everything())

write_csv(all.weekly, "data/interim/mobility/safegraph/weekly-patterns/processed_weekly_visits.csv.gz")




###################################################
# Script for cleaning MIT elections data
###################################################

library(tidyverse)
library(elections)

data(presidential_precincts_2016)

county_totals <- presidential_precincts_2016 %>% 
  # It looks like some votes are double-counted, for example when people vote straight party tickets
  # in AL, IN or SC. Filter out to only obs where "US President" is explicitly listed
  filter(office == "US President") %>% 
  select(state, state_fips, state_postal, county_name, county_fips, votes) %>% 
  group_by(state, state_fips, state_postal, county_name, county_fips) %>% 
  summarize(totvotes = sum(votes, na.rm=TRUE)) %>% 
  ungroup() %>% 
  # There's some weird obs with too-long county fips codes, trim these
  mutate(county_fips = str_sub(county_fips, start=1L, end=5L))

# Highlight just trump vote totals
trump <- presidential_precincts_2016 %>% 
  select(state, state_postal, state_fips, county_name, county_fips,
         county_lat, county_long, candidate, party, candidate_normalized, votes) %>% 
  filter(candidate_normalized=="trump") %>% 
  group_by(state, state_fips, state_postal, county_name, county_fips, county_lat, county_long) %>% 
  summarize(trump_votes = sum(votes, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(county_fips = str_sub(county_fips, start=1L, end=5L))

# Merge trump totals with overall totals to calculate percentage; output
final <- county_totals %>% 
  left_join(trump, by=c("state", "state_postal", "state_fips",
                        "county_name", "county_fips")) %>% 
  mutate(trump_vote_share = trump_votes / totvotes,
         state_fips = str_pad(as.character(state_fips), 2, side = c("left"), "0")) %>% 
  select(-c(totvotes, trump_votes))

# Save dataset to intermediate cleaned section
write.csv(final, "data/interim/political/elections/medsl_allcott_clean.csv", row.names=FALSE)




###################################################
# Build dataset of daily and weekly SIPO timeseries
# by county
###################################################

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)

all.counties <- read_csv("data/raw/demographic/co-est2019-alldata.csv") %>% 
  filter(COUNTY != "000") %>% 
  select(STATE:CTYNAME, POPESTIMATE2019) %>% 
  rename(state_fips = STATE,
         county_fips = COUNTY,
         state = STNAME,
         county = CTYNAME,
         popestimate2019 = POPESTIMATE2019)

# NYT reports cases for all five boroughs (which are their own counties) rolled into one,
# so let's compress these 5 rows in the all.counties dataset to 1. For merging purposes,
# we'll refer to all of New York as "New York County," which in reality is just Manhattan,
# but in our dataset will refer to all 5 boroughs
nyc.pop <- all.counties %>% 
  filter(state=="New York" & 
           county %in% c("New York County", "Kings County", "Queens County", "Richmond County", "Bronx County")) %>% 
  summarize(sum(popestimate2019)) %>% 
  .[[1]]

all.counties <- all.counties %>% 
  filter(!(state=="New York" & 
             county %in% c("New York County", "Kings County", "Queens County", "Richmond County", "Bronx County"))) %>% 
  add_row(state_fips="36", county_fips="061", state="New York", county="New York County", popestimate2019=nyc.pop) %>% 
  arrange(state_fips, county_fips)

# Read in state shelter in place orders
state.sip <- read_excel("data/raw/responses/all_actions_tracking_individual_sheets.xlsx", sheet = "shelter_in_place") %>% 
  select(state_code:sip_end_date) %>% 
  rename(state_sip_start_date = sip_start_date,
         state_sip_end_date = sip_end_date,
         state_fips = state_code,
         state = state_name)

# Join state-level SIP orders to county
all.merged <- all.counties %>% 
  left_join(state.sip, by=c("state_fips", "state"))

#write.csv(all.merged, "Intermediate/county_level_sip_to_update.csv", row.names=FALSE)

# Read in county shelter in place orders
responses.county <- read_csv("data/raw/responses/county_level_sip_updated.csv",
                             col_types = cols(state_code = col_character(), county_code = col_character()))

responses.county.fix <- responses.county %>%
  mutate(state_fips = str_pad(state_code, 2, pad = "0"),
         county_fips = str_pad(county_code, 3, pad = "0")) %>% 
  select(-c(citation1, citation2, citation3, citation4, citation5, state_code, county_code, state)) %>% 
  rename(state = state_name,
         county = county_name) %>% 
  # We're treating all of New York City as New York County, so drop NY counties for the other 4 boroughs
  # This is OK because all of NYC has the same start/end date for local SIP order
  filter(!(state=="New York" & 
             county %in% c("Kings County", "Queens County", "Richmond County", "Bronx County"))) %>% 
  mutate(popestimate2019 = ifelse(state=="New York" & county=="New York County", nyc.pop, popestimate2019))

# Generate daily time series from 1/27 - 5/31 for all counties
daily.county <- responses.county.fix %>% 
  distinct(state_fips, county_fips, state, county) %>% 
  group_by(state_fips, county_fips, state, county) %>% 
  do(data.frame(state=.$state, county=.$county, state_code=.$state_fips, county_fips=.$county_fips,
                date=seq(as.Date("01-27-20", format="%m-%d-%y"),
                         as.Date("05-31-20", format="%m-%d-%y"),
                         by="day")))

# Join the county-level SIP orders to daily time series
daily.merge <- daily.county %>% 
  left_join(responses.county.fix, by=c("state_fips", "county_fips", "state", "county"))

# Create daily indicator for shelter in place
indicators <- daily.merge %>%
  mutate(state_sip_start_date = as.Date(state_sip_start_date, "%m/%d/%Y"),
         state_sip_end_date = as.Date(state_sip_end_date, "%m/%d/%Y")) %>% 
  
  mutate(county_sip_start_date = as.Date(county_sip_start_date, "%m/%d/%Y"),
         county_sip_end_date = as.Date(county_sip_end_date, "%m/%d/%Y")) %>% 
  
  # Set vals for comparisons
  mutate(state_sip_start_date = ifelse(!is.na(state_sip_start_date), state_sip_start_date, Inf),
         county_sip_start_date = ifelse(!is.na(county_sip_start_date), county_sip_start_date, Inf),
         state_sip_end_date = ifelse(!is.na(state_sip_end_date), state_sip_end_date, 0),
         county_sip_end_date = ifelse(!is.na(county_sip_end_date), county_sip_end_date, 0)) %>% 
  
  mutate(
    state_sip = ifelse((state_sip_start_date <= date & state_sip_end_date >= date) |
                         (state_sip_start_date <= date & state_sip_end_date == 0) |
                         (is.infinite(state_sip_start_date) & state_sip_end_date >= date), 1, 0),
    
    county_sip = ifelse((county_sip_start_date <= date & county_sip_end_date >= date) |
                          (county_sip_start_date <= date & county_sip_end_date == 0) |
                          (is.infinite(county_sip_start_date) & county_sip_end_date >= date), 1, 0), 
    
    state_sip_start_date = ifelse(is.infinite(state_sip_start_date), NA, state_sip_start_date),
    county_sip_start_date = ifelse(is.infinite(county_sip_start_date), NA, county_sip_start_date),
    state_sip_end_date = ifelse(state_sip_end_date == 0, NA, state_sip_end_date),
    county_sip_end_date = ifelse(county_sip_end_date == 0, NA, county_sip_end_date)) %>% 
  
  mutate(state_sip_start_date = as_date(state_sip_start_date),
         state_sip_end_date = as_date(state_sip_end_date),
         county_sip_start_date = as_date(county_sip_start_date),
         county_sip_end_date = as_date(county_sip_end_date)) %>% 
  
  select(-c(state_code, popestimate2019))


# Create daily timeseries of sipos at county-level
daily.sipos <- indicators %>% 
  ungroup() %>% 
  mutate(county_fips = paste0(state_fips, county_fips),
         state = str_to_lower(state),
         county = str_to_lower(gsub(" County", "", county))) %>% 
  select(county_fips, state, county, everything()) %>% 
  select(-c(state_fips)) %>% 
  mutate(sip_binary = ifelse(state_sip==1 | county_sip==1, 1, 0))

write_csv(daily.sipos, "data/interim/responses/daily_sipo_timeseries.csv.gz")


# Create weekly timeseries of sipos at county-level
weekly.sipos <- daily.sipos %>% 
  group_by(county_fips) %>% 
  mutate(weeknum = lubridate::week(date),
         weekshift = dplyr::lead(weeknum, n=2L),
         weekshift = ifelse(is.na(weekshift), 22, weekshift)) %>% 
  group_by(county_fips, weekshift) %>% 
  mutate(daynum = row_number()) %>% 
  ungroup() %>% 
  mutate(week_start_date = ifelse(daynum==1, date, NA)) %>% 
  arrange(county_fips, date) %>% 
  group_by(county_fips) %>% 
  fill(week_start_date)

weekly.sipos.summary <- weekly.sipos %>% 
  arrange(county_fips, state, county, week_start_date) %>% 
  group_by(county_fips, state, county, week_start_date, 
           state_sip_start_date, state_sip_end_date, county_sip_start_date, county_sip_end_date) %>% 
  summarize(sip_days = sum(sip_binary, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(weekly_sipo_binary = ifelse(sip_days > 0, 1, 0),
         week_start_date = as_date(week_start_date))

write_csv(weekly.sipos.summary, "data/interim/responses/weekly_sipo_timeseries.csv.gz")





###################################################
# Process gridmet weather data at county level
###################################################

rm(list = ls())
library(tidyverse)
library(stringr)
library(data.table)
library(lubridate)

weather2020 <- data.table::fread("data/raw/weather/bayham/weather_county_2020-01-01_yesterday.csv.gz")

# Vars:
# precip = daily precipitation (mm)
# rmax, rmin = max/min daily relative humidity (%)
# srad = surface downwelling solar radiation (W/m^2)
# tmax, tmin = max/min daily temperature (degrees F)
# wind_speed = mean daily wind speed (mph)

daily.weather <- weather2020 %>% 
  mutate(county_fips = str_pad(as.character(county), 5, c("left"), "0"),
         date = as.Date(date, "%Y-%m-%d")) %>% 
  select(county_fips, date, precip, tmin, tmax) 

write_csv(daily.weather, "data/interim/weather/bayham/daily_weather_processed.csv.gz")

weekly.weather <- daily.weather %>% 
  mutate(week = lubridate::week(date),
         # Adjust week periods to align with safegraph data week periods: starting on 1/6, etc.
         weeknum = dplyr::lead(week, n=2L)) %>% 
  select(-c(week)) %>%
  group_by(county_fips, weeknum) %>% 
  mutate(daynum = row_number()) %>% 
  ungroup() %>% 
  mutate(week_start_date = ifelse(daynum==1, date, NA)) %>% 
  group_by(county_fips, weeknum) %>% 
  tidyr::fill(week_start_date) %>% 
  ungroup() %>% 
  mutate(week_start_date = as_date(week_start_date))

weekly.weather.summary <- weekly.weather %>% 
  filter(week_start_date >= as.Date("2020-01-27", "%Y-%m-%d")) %>% 
  group_by(county_fips, week_start_date) %>% 
  summarize(mean_precip = mean(precip, na.rm=TRUE),
            mean_tmin = mean(tmin, na.rm=TRUE),
            mean_tmax = mean(tmax, na.rm=TRUE)) %>% 
  ungroup()

write_csv(weekly.weather.summary, "data/interim/weather/bayham/weekly_weather_processed.csv.gz")