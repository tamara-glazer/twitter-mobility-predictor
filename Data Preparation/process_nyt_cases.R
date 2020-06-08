###################################################
# process_nyt_cases.R
# Build dataset of case and death counts 
###################################################

rm(list=ls())

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(tidycensus)
Sys.getenv("CENSUS_API_KEY")

# Read in county size
size <- read_xls("../Data/Raw/LND01.xls") %>% 
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
nyc.pop <- read_csv("../Data/Raw/co-est2019-alldata.csv") %>% 
  filter(STNAME=="New York" & CTYNAME %in% c("New York County", "Kings County", "Queens County", "Richmond County", "Bronx County")) %>% 
  select(POPESTIMATE2019) %>% 
  .[[1]] %>% 
  sum()

# Read in county shelter in place orders
responses.county <- read_csv("../Data/Raw/county_level_sip_updated.csv",
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
nyt <- read_csv("../Data/Raw/nyt-us-counties.csv") %>% 
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
# write_csv(cases.daily, "../Data/Processed/nyt_cases_deaths_daily.csv.gz")





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
# write_csv(cases.weekly.summary, "../Data/Processed/nyt_cases_deaths_weekly.csv.gz")
