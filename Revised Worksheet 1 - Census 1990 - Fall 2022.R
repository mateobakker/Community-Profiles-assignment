library(tidyverse)
library(tidycensus)
library(rvest)
library(jsonlite)
library(foreign)

#### start with these ####
# 1990 variables
# data dictionary is here: "https://www2.census.gov/census_1990/CD90_3A_11/document/tbl_mtx.asc", sep = "\t")
temp <- tempfile()
download.file("https://www2.census.gov/census_1990/CD90_3A_57/tables.dbf", temp)
variables_and_location <-
  read.dbf(temp) %>%
  as_tibble %>%
    mutate(across(everything(), as.character)) %>%
    arrange(TABLE) %>%
    #filter(TABLE == "P070") %>%
    mutate(universe = if_else(str_detect(TEXT, "Universe: "), TEXT, NA_character_),
           table_name = ifelse(str_detect(TEXT, "[A-Z][A-Z]"), TEXT, NA_character_)) %>%
    fill(table_name, .direction = "down") %>%
    fill(universe, .direction = "downup") %>%
    filter(!str_detect(TEXT, "[A-Z][A-Z]|Universe:")) %>%
    add_count(TABLE, FIELD, TEXT) %>%
    mutate(quali1 = if_else(n == 1 & str_detect(TEXT, ":"), TEXT, NA_character_),
           quali2 = if_else(n == 2 & str_detect(TEXT, ":"), TEXT, NA_character_),
           quali3 = if_else(n == 2 & !str_detect(lag(TEXT), ":"), TEXT, NA_character_)) %>%
  group_by(table_name) %>%
    fill(quali1, .direction = "down") %>%
  ungroup() %>%
    mutate(quali2 = if_else(is.na(quali3), quali2, NA_character_)) %>%
    mutate(label = if_else(!is.na(FIELD) & !is.na(quali1), paste0(quali1, TEXT), TEXT)) %>%
    select(TABLE, table_name, SEGMENT, variable = FIELD, label, universe) %>%
    filter(!is.na(variable))

county_names <- tidycensus::fips_codes %>% 
  as_tibble() %>% 
  mutate(GEOID = paste0(state_code, county_code),
         county = paste0(county, ", ", state)) %>% 
  select(GEOID, county) 

place_names <- get_acs("place",
                       state = "MD",
                       variables = "B01001_001") %>% 
  select(GEOID, NAME)

soc131_b_counties_fa22 <- county_names %>% 
  filter(county %in% c("Howard County, MD", "Montgomery County, MD", "Prince George's County, MD", "Alexandria city, VA", "Fairfax County, VA", "Prince William County, VA")) %>%  
  select(GEOID, county)

soc131_c_jurisdictions_fa22 <- 
  county_names %>% 
  filter(county %in% c("Alexandria city, VA", "Falls Church city, VA", "Fairfax city, VA", "Manassas Park city, VA")) %>% 
  bind_rows(place_names %>% 
              filter(str_detect(NAME, "Aspen Hill|^Bethesda|Langley|Fort Washington")) %>% 
              rename(county = NAME))

## I've decided to use some revised indicators in my worksheets
## This are modified versions of the SES indicators used by the NDI package in R
## That package is designed to easily measure/map neighborhood deprivation
## I don't really like that that deficit based model

## But I think it'll make for a good final individual essay from each student
## 1. I'll create a map of the NDI index for the entire DMV
## 2. And ask them to consider whether or not this changes there view about whether the region is segregated or not
## 3. Does the scale at which we look at the data matter?
## 4. Is it better to look at neighborhoods rather than entire counties?
## Etc.


powell_wiley <- read_html("https://cran.r-project.org/web/packages/ndi/vignettes/vignette.html") %>%
  html_table() %>%
  .[[2]] %>%
  arrange(`SES dimension`)


## 1. Demographics: race/ethnicity, immigration status, occupation/management, female headed hhs with children
## 2. Education, income, wealth: hs and grad, hh income, interest income, housing value, % owner occupied, unemployed


####Worksheet 1####
get_worksheet1 <- function(unit) {
  if(!unit %in% c("dmv", "counties", "places")) {stop("Only units are 'dmv', 'places', and 'counties'", call. = FALSE)}

  if(unit == "dmv") {

## hispanic by race (P12) is in STF301
download.file("https://www2.census.gov/census_1990/cd903c01/stf301us.dbf", temp)
race <- read.dbf(temp) %>%
  filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
         SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
         GEOCOMP == "00") %>%
  select(MSACMSA, starts_with("P012")) %>%
  pivot_longer(-MSACMSA) %>%
  group_by(MSACMSA, name) %>%
  summarise(value = sum(value)) %>%
  mutate(denom = sum(value)) %>% 
  ungroup() %>%
  left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
  mutate(label = if_else(str_detect(name, "6|7|8|9|10"), "Hispanic or Latino (of any race)", label),
         label = str_remove(label, ".*:")) %>%
  group_by(MSACMSA, label, universe) %>%
  summarise(value = sum(value),
            denom = mean(denom),
            perc = value / denom * 100) %>%
  ungroup() %>%
  select(MSACMSA, label, value, denom, perc, universe)


## citizenship (by age) (P37) is in STF309
download.file("https://www2.census.gov/census_1990/cd903c01/stf309us.dbf", temp)
cit <- read.dbf(temp) %>%
  filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
         SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
         GEOCOMP == "00") %>%
  select(MSACMSA, starts_with("P037")) %>%
  pivot_longer(-MSACMSA) %>%
  group_by(MSACMSA, name) %>%
  summarise(value = sum(value)) %>%
  mutate(denom = sum(value)) %>% 
  ungroup() %>%
  left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
  mutate(label = str_remove(label, ".*:"),
         denom = sum(value)) %>%
  group_by(MSACMSA, label, universe) %>%
  summarise(value = sum(value),
            denom = mean(denom),
            perc = value / denom * 100) %>%
  ungroup() %>%
  select(MSACMSA, label, value, denom, perc, universe)

## management data (table P78) is in STF314
download.file("https://www2.census.gov/census_1990/cd903c01/stf314us.dbf", temp)
mgt <- read.dbf(temp) %>%
  filter(MSACMSA == "8840",
         SUMLEV == "300",
         GEOCOMP == "00") %>%
  select(MSACMSA, contains("P078")) %>%
  pivot_longer(-MSACMSA) %>%
  group_by(MSACMSA, name) %>%
  summarise(value = sum(value)) %>%
  mutate(denom = sum(value)) %>% 
  ungroup() %>%
  left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
  mutate(label = str_remove(label, " \\(.*\\)")) %>%
  filter(str_detect(label, "^Manag")) %>%
  group_by(MSACMSA, label, universe) %>%
  summarise(value = sum(value),
            denom = mean(denom),
            perc = value / denom * 100) %>%
  ungroup %>%
  select(MSACMSA, label, value, denom, perc, universe)

## educational attainment (table P57) is in STF310
download.file("https://www2.census.gov/census_1990/cd903c01/stf310us.dbf", temp)
ed <- read.dbf(temp) %>%
  filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
         SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
         GEOCOMP == "00") %>%
  select(MSACMSA, starts_with("P057")) %>%
  pivot_longer(-MSACMSA) %>%
  group_by(MSACMSA, name) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(denom = sum(value),
         perc = value / denom * 100) %>% 
  left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
  filter(str_detect(label, "High sch|Graduate")) %>%
  select(MSACMSA, label, value, denom, perc, universe)

## total population (P003) is in STF301
download.file("https://www2.census.gov/census_1990/cd903c01/stf301us.dbf", temp)
totpop <- read.dbf(temp) %>%
  filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
         SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
         GEOCOMP == "00") %>%
  select(MSACMSA, "P0030001") %>%
  pivot_longer(-MSACMSA) %>%
  left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
  select(MSACMSA, label, value, universe)


## Unemployed (table P70) is in SFT312
download.file("https://www2.census.gov/census_1990/cd903c01/stf312us.dbf", temp)
sex_occupation <- read.dbf(temp) %>%
  filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
         SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
         GEOCOMP == "00") %>%
  select(MSACMSA, starts_with("P070")) %>%
  pivot_longer(-MSACMSA) %>%
  left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
  filter(!str_detect(label, "Not in labor force")) %>%
  mutate(total = sum(value))
unemployed <- sex_occupation %>%
  filter(str_detect(label, "Unemployed")) %>%
  mutate(label = str_remove(label, ".*:")) %>%
  group_by(MSACMSA, label, universe) %>%
    summarize(value = sum(value),
            denom = mean(total),
            perc = value / denom * 100) %>%
  select(MSACMSA, label, value, denom, perc, universe) %>%
  mutate(universe = paste(universe, "in labor force"))
employed <- sex_occupation %>%
  filter(str_detect(label, "Employed")) %>%
  mutate(label = str_remove(label, ".*:")) %>%
  group_by(MSACMSA, label, universe) %>%
  summarize(value = sum(value),
            denom = mean(total),
            perc = value / denom * 100) %>%
  ungroup() %>% 
  select(MSACMSA, label, value, denom, perc, universe) %>%
  mutate(universe = paste(universe, "in labor force")) 

bind_rows(totpop, race, cit, ed, mgt, employed, unemployed)
  }
  
  if(unit == "places") {
    map2_dfr("md", 11, ~{
      ## hispanic by race (P12) is in STF301
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf301{.x}.dbf"), temp)
      race <- read.dbf(temp) %>%
        filter(SUMLEV == "160") %>% 
        select(1:2, 5, starts_with("P012")) %>%
        pivot_longer(-c(1:3)) %>%
        left_join(variables_and_location %>% select(name = variable, label, universe)) %>% 
        mutate(label = if_else(str_detect(label, "^Hisp"), "Latinx", label),
               label = str_remove(label, ".*:")) %>% 
        group_by(STATEFP, PLACEFP, label, universe) %>%
        summarise(value = sum(value)) %>% 
        ungroup() %>% 
        group_by(STATEFP, PLACEFP, universe) %>% 
        mutate(denom = sum(value)) %>% 
        ungroup() %>% 
        mutate(perc = value / denom * 100,
               GEOID = paste0(STATEFP, PLACEFP)) %>% 
        select(GEOID, label, value, denom, perc, universe)
      
      ## citizenship (by age) (P37) is in STF309
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf309{.x}.dbf"), temp)
      cit <- read.dbf(temp) %>%
        filter(SUMLEV == "160") %>%
        select(1:2, 5, starts_with("P037")) %>%
        pivot_longer(-c(1:3)) %>%
        group_by(STATEFP, PLACEFP) %>% 
        mutate(denom = sum(value)) %>% 
        ungroup %>% 
        left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
        mutate(label = str_remove(label, ".*:")) %>%
        group_by(STATEFP, PLACEFP, label, universe) %>%
        summarise(value = sum(value),
                  denom = mean(denom),
                  perc = value / denom * 100) %>%
        ungroup() %>%
        mutate(GEOID = paste0(STATEFP, PLACEFP)) %>% 
        select(GEOID, label, value, denom, perc, universe)
      
      ## management data (table P78) is in STF314
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf314{.x}.dbf"), temp)
      mgt <- read.dbf(temp) %>%
        filter(SUMLEV == "160") %>%
        select(1:2, 5, starts_with("P078")) %>%
        pivot_longer(-c(1:3)) %>%
        group_by(STATEFP, PLACEFP, name) %>%
        summarise(value = sum(value)) %>%
        mutate(denom = sum(value)) %>% 
        ungroup() %>%
        left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
        mutate(label = str_remove(label, " \\(.*\\)")) %>%
        filter(str_detect(label, "^Manag")) %>%
        group_by(STATEFP, PLACEFP, label, universe) %>%
        summarise(value = sum(value),
                  denom = mean(denom),
                  perc = value / denom * 100) %>%
        ungroup %>%
        mutate(GEOID = paste0(STATEFP, PLACEFP)) %>% 
        select(GEOID, label, value, denom, perc, universe)
      
      ## educational attainment (table P57) is in STF310
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf310{.x}.dbf"), temp)
      ed <- read.dbf(temp) %>%
        filter(SUMLEV == "160") %>%
        select(1:2,5, starts_with("P057")) %>%
        pivot_longer(-c(1:3)) %>%
        group_by(STATEFP, PLACEFP, name) %>%
        summarise(value = sum(value)) %>%
        mutate(denom = sum(value)) %>% 
        ungroup() %>%
        left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
        mutate(perc = value / denom * 100) %>%
        filter(str_detect(label, "High sch|Graduate")) %>%
        mutate(GEOID = paste0(STATEFP, PLACEFP)) %>% 
        select(GEOID, label, value, denom, perc, universe)
      
      ## total population (P0030) is in STF301
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf301{.x}.dbf"), temp)
      totpop <- read.dbf(temp) %>%
        filter(SUMLEV == "160") %>%
        select(1:2,5, starts_with("P0030")) %>%
        pivot_longer(-c(1:3)) %>%
        left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
        mutate(GEOID = paste0(STATEFP, PLACEFP)) %>% 
        select(GEOID, label, value, universe)
      
      ## Unemployed (table P70) is in SFT312
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf312{.x}.dbf"), temp)
      sex_occupation <- read.dbf(temp) %>%
        filter(SUMLEV == "160") %>%
        select(1:2,5, starts_with("P070")) %>%
        pivot_longer(-c(1:3)) %>%
        left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
        filter(!str_detect(label, "Not in labor force")) %>%
        group_by(STATEFP, PLACEFP) %>% 
        mutate(total = sum(value)) %>% 
        ungroup()
      unemployed <- sex_occupation %>%
        filter(str_detect(label, "Unemployed")) %>%
        mutate(label = str_remove(label, ".*:")) %>%
        group_by(STATEFP, PLACEFP, label, universe) %>%
        summarize(value = sum(value),
                  denom = mean(total),
                  perc = value / denom * 100) %>%
        ungroup() %>% 
        mutate(universe = paste(universe, "in labor force"),
               GEOID = paste0(STATEFP, PLACEFP)) %>% 
        select(GEOID, label, value, denom, perc, universe) 
      employed <- sex_occupation %>%
        filter(str_detect(label, "Employed")) %>%
        mutate(label = str_remove(label, ".*:")) %>%
        group_by(STATEFP, PLACEFP, label, universe) %>%
        summarize(value = sum(value),
                  denom = mean(total),
                  perc = value / denom * 100) %>%
        ungroup() %>% 
        mutate(universe = paste(universe, "in labor force"),
               GEOID = paste0(STATEFP, PLACEFP)) %>% 
        select(GEOID, label, value, denom, perc, universe) 
      bind_rows(totpop, race, cit, ed, mgt, employed, unemployed) %>% 
        left_join(place_names) %>% 
        select(7, 2:6, GEOID)
    })
  }

else {
  map2_dfr(c("dc", "md", "va"), c(11, 11, 57), ~{
    ## hispanic by race (P12) is in STF301
    download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf301{.x}.dbf"), temp)
    race <- read.dbf(temp) %>%
      filter(SUMLEV == "050") %>%
      select(1:3, starts_with("P012")) %>%
      pivot_longer(-c(1:3)) %>%
      left_join(variables_and_location %>% select(name = variable, label, universe)) %>% 
      mutate(label = if_else(str_detect(label, "^Hisp"), "Latinx", label),
             label = str_remove(label, ".*:")) %>% 
      group_by(STATEFP, CNTY, label, universe) %>%
      summarise(value = sum(value)) %>% 
      ungroup() %>% 
      group_by(STATEFP, CNTY, universe) %>% 
      mutate(denom = sum(value)) %>% 
      ungroup() %>% 
      mutate(perc = value / denom * 100,
             GEOID = paste0(STATEFP, CNTY)) %>% 
      select(GEOID, label, value, denom, perc, universe)

    ## citizenship (by age) (P37) is in STF309
    download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf309{.x}.dbf"), temp)
    cit <- read.dbf(temp) %>%
      filter(SUMLEV == "050") %>%
      select(1:3, starts_with("P037")) %>%
      pivot_longer(-c(1:3)) %>%
      group_by(STATEFP, CNTY) %>% 
      mutate(denom = sum(value)) %>% 
      ungroup %>% 
      left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
      mutate(label = str_remove(label, ".*:")) %>%
      group_by(STATEFP, CNTY, label, universe) %>%
      summarise(value = sum(value),
                denom = mean(denom),
                perc = value / denom * 100) %>%
      ungroup() %>%
      mutate(GEOID = paste0(STATEFP, CNTY)) %>% 
      select(GEOID, label, value, denom, perc, universe)

    ## management data (table P78) is in STF314
    download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf314{.x}.dbf"), temp)
    mgt <- read.dbf(temp) %>%
      filter(SUMLEV == "050") %>%
      select(1:3, starts_with("P078")) %>%
      pivot_longer(-c(1:3)) %>%
      group_by(STATEFP, CNTY, name) %>%
      summarise(value = sum(value)) %>%
      mutate(denom = sum(value)) %>% 
      ungroup() %>%
      left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
      mutate(label = str_remove(label, " \\(.*\\)")) %>%
      filter(str_detect(label, "^Manag")) %>%
      group_by(STATEFP, CNTY, label, universe) %>%
      summarise(value = sum(value),
                denom = mean(denom),
                perc = value / denom * 100) %>%
      ungroup %>%
      mutate(GEOID = paste0(STATEFP, CNTY)) %>% 
      select(GEOID, label, value, denom, perc, universe)

    ## educational attainment (table P57) is in STF310
    download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf310{.x}.dbf"), temp)
    ed <- read.dbf(temp) %>%
      filter(SUMLEV == "050") %>%
      select(1:3, starts_with("P057")) %>%
      pivot_longer(-c(1:3)) %>%
      group_by(STATEFP, CNTY, name) %>%
      summarise(value = sum(value)) %>%
      mutate(denom = sum(value)) %>% 
      ungroup() %>%
      left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
      mutate(perc = value / denom * 100) %>%
      filter(str_detect(label, "High sch|Graduate")) %>%
      mutate(GEOID = paste0(STATEFP, CNTY)) %>% 
      select(GEOID, label, value, denom, perc, universe)
    
    ## total population (P0030) is in STF301
    download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf301{.x}.dbf"), temp)
    totpop <- read.dbf(temp) %>%
      filter(SUMLEV == "050") %>%
      select(1:3, starts_with("P0030")) %>%
      pivot_longer(-c(1:3)) %>%
      left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
      mutate(GEOID = paste0(STATEFP, CNTY)) %>% 
      select(GEOID, label, value, universe)

    ## Unemployed (table P70) is in SFT312
    download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf312{.x}.dbf"), temp)
    sex_occupation <- read.dbf(temp) %>%
      filter(SUMLEV == "050") %>%
      select(1:3, starts_with("P070")) %>%
      pivot_longer(-c(1:3)) %>%
      left_join(variables_and_location %>% select(variable, label, universe), by = c("name" = "variable")) %>%
      filter(!str_detect(label, "Not in labor force")) %>%
      group_by(STATEFP, CNTY) %>% 
      mutate(total = sum(value)) %>% 
      ungroup()
    unemployed <- sex_occupation %>%
      filter(str_detect(label, "Unemployed")) %>%
      mutate(label = str_remove(label, ".*:")) %>%
      group_by(STATEFP, CNTY, label, universe) %>%
      summarize(value = sum(value),
                denom = mean(total),
                perc = value / denom * 100) %>%
      ungroup() %>% 
      mutate(universe = paste(universe, "in labor force"),
             GEOID = paste0(STATEFP, CNTY)) %>% 
      select(GEOID, label, value, denom, perc, universe) 
    employed <- sex_occupation %>%
      filter(str_detect(label, "Employed")) %>%
      mutate(label = str_remove(label, ".*:")) %>%
      group_by(STATEFP, CNTY, label, universe) %>%
      summarize(value = sum(value),
                denom = mean(total),
                perc = value / denom * 100) %>%
      ungroup() %>% 
      mutate(universe = paste(universe, "in labor force"),
             GEOID = paste0(STATEFP, CNTY)) %>% 
      select(GEOID, label, value, denom, perc, universe) 

    bind_rows(totpop, race, cit, ed, mgt, employed, unemployed) %>% 
      left_join(county_names) %>% 
      select(7, 2:6, GEOID)
  })
}
}

get_worksheet1("counties") %>% 
  write_csv("Data/1990 Census - New Worksheet 1 - Counties - Fall 2022.csv")

get_worksheet1("dmv") %>% 
  write_csv("Data/1990 Census - New Worksheet 1 - DMV - Fall 2022.csv")

soc131_c <- get_worksheet1("counties") %>% 
  bind_rows(get_worksheet1("places")) %>% 
  left_join(soc131_c_jurisdictions_fa22, .,  by = "GEOID") %>% 
  select(county = county.x, label, value, universe, denom, perc, GEOID) %>% 
  write_csv("Data/1990 Census - New Worksheet 1 - Counties and Places - Fall 2022 - 131-C.csv")

soc131_b <- read_csv("Data/1990 Census - New Worksheet 1 - Counties - Fall 2022.csv")

dmv_worksheet1 <- read_csv("Data/1990 Census - New Worksheet 1 - DMV - Fall 2022.csv")
view(dmv_worksheet1)
soc131_b %>% 
  filter(GEOID %in% soc131_b_counties_fa22$GEOID) %>% 
  #left_join(soc131_b_counties_fa22) %>% 
  distinct() %>% 
  arrange(GEOID) %>% 
  select(GEOID, county, label, value, denom, perc) %>% 
  view
