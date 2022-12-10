## I've decided to use some revised indicators in my worksheets
## This are modified versions of the SES indicators used by the NDI package in R
## That package is designed to easily measure/map neighborhood deprivation
## I don't really like that that deficit based model
## But I think it'll make for a good final individual essay from each student
## I'll create a map of the NDI index for the entire DMV
## And ask them to consider whether or not this changes there view about whether the region is segregated or not
## Does the scale at which we look at the data matter? 
## Is it better to look at neighborhoods rather than entire counties? 
## Etc. 

library(tidyverse)
library(rvest)
library(jsonlite)
library(foreign)

####Worksheet 2####
## OJO: Run inputs for Revised Worksheet 2.R script first

get_worksheet2 <- function(unit) {
  require("foreign")
  temp <- tempfile()
  #if(!unit %in% c("dmv", "places", "counties")) {stop("Only units are 'dmv' and 'counties'", call. = FALSE)}
  
  if(unit == "dmv") { 
    download.file("https://www2.census.gov/census_1990/cd903c01/stf327us.dbf", temp) 
    dmv_owner_occupied <- read.dbf(temp) %>% 
      filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
             SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
             GEOCOMP == "00") %>% 
      select(MSACMSA, starts_with("H008000")) %>% 
      pivot_longer(-MSACMSA) %>% 
      mutate(denom = sum(value),
             denom_type = "all_occupied_housing_units",
             perc = value / sum(value) * 100) %>% 
      mutate(variable = "Median housing value",
             variable = if_else(str_detect(name, "01"), "owner occupied", "renter occupied")) %>% 
      filter(variable == "owner occupied") %>% 
      select(MSACMSA, variable, value, denom, perc, denom_type)
    
    ## median housing value (H61A) is in STF333 (Households)
    download.file("https://www2.census.gov/census_1990/cd903c01/stf333us.dbf", temp) 
    dmv_median_value <- read.dbf(temp) %>% 
      filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
             SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
             GEOCOMP == "00") %>% 
      select(MSACMSA, value = contains("61A")) %>% 
      mutate(variable = "Median housing value") %>% 
      select(MSACMSA, variable, value)
    
    ## median household income (P080A001) is in ST314 (Households)
    download.file("https://www2.census.gov/census_1990/cd903c01/stf314us.dbf", temp) 
    dmv_median_hh_inc <- read.dbf(temp) %>% 
      filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
             SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
             GEOCOMP == "00") %>% 
      select(MSACMSA, value = contains("P080A001")) %>% 
      mutate(variable = "median hh income") %>% 
      select(MSACMSA, variable, value)
    
    ## interest income (table P93) is in STF321 (Households) "P0930001"
    download.file("https://www2.census.gov/census_1990/cd903c01/stf321us.dbf", temp) 
    dmv_rcv_interest_inc <- read.dbf(temp) %>% 
      filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
             SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
             GEOCOMP == "00") %>% 
      select(MSACMSA, starts_with("P093000")) %>% 
      pivot_longer(-MSACMSA) %>% 
      mutate(denom = sum(value),
             denom_type = "all_households",
             perc = value / sum(value) * 100) %>% 
      filter(name == "P0930001") %>% 
      mutate(variable = "hh with interest, dividend, or net rental income") %>% 
      select(MSACMSA, variable, value, denom, perc, denom_type)
    
    ## female-headed hhs (P19) is in STF306 (Households)
    download.file("https://www2.census.gov/census_1990/cd903c01/stf306us.dbf", temp) 
    dmv_female_headed <- read.dbf(temp) %>% 
      filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
             SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
             GEOCOMP == "00") %>% 
      select(MSACMSA, starts_with("P019000")) %>% 
      pivot_longer(-MSACMSA) %>% 
      filter(name != "P0190007") %>% 
      mutate(denom = sum(value),
             denom_type = "all_family_households",
             perc = value / sum(value) * 100,
             variable = if_else(name == "P0190005", "single female-headed hh with children", "all other households")) %>% 
      filter(variable == "single female-headed hh with children") %>% 
      select(MSACMSA, variable, value, denom, perc, denom_type)
    
    all <- bind_rows(dmv_owner_occupied, dmv_median_value, dmv_median_hh_inc, dmv_rcv_interest_inc, dmv_female_headed)
    return(all)
    }
  
  if(unit == "places") { 
    map2_dfr("md", 11, ~{
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf327{.x}.dbf"), temp) 
      dmv_owner_occupied <- read.dbf(temp) %>% 
        filter(SUMLEV == "160") %>% 
        select(1:2,5, starts_with("H008000")) %>% 
        pivot_longer(-c(1:3)) %>% 
        group_by(STATEFP, PLACEFP) %>% 
        mutate(denom = sum(value),
               denom_type = "all_occupied_housing_units",
               perc = value / sum(value) * 100) %>% 
        mutate(variable = "Median housing value",
               variable = if_else(str_detect(name, "01"), "owner occupied", "renter occupied")) %>% 
        filter(variable == "owner occupied") %>% 
        select(STATEFP, PLACEFP, variable, value, denom, perc, denom_type) 
      
      ## median housing value (H61A) is in STF333 (Households)
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf333{.x}.dbf"), temp) 
      dmv_median_value <- read.dbf(temp) %>% 
        filter(SUMLEV == "160") %>% 
        select(1:2,5, contains("61A")) %>% 
        pivot_longer(-c(1:3)) %>% 
        group_by(STATEFP, PLACEFP) %>% 
        mutate(variable = "Median housing value") %>% 
        select(STATEFP, PLACEFP, variable, value)
      
      ## median household income (P080A001) is in ST314 (Households)
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf314{.x}.dbf"), temp) 
      dmv_median_hh_inc <- read.dbf(temp) %>% 
        filter(SUMLEV == "160") %>% 
        select(1:2,5, starts_with("P080A001")) %>% 
        pivot_longer(-c(1:3)) %>% 
        group_by(STATEFP, PLACEFP) %>% 
        mutate(variable = "median hh income") %>% 
        select(STATEFP, PLACEFP, variable, value)
      
      ## interest income (table P93) is in STF321 (Households) "P0930001"
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf321{.x}.dbf"), temp) 
      dmv_rcv_interest_inc <- read.dbf(temp) %>% 
        filter(SUMLEV == "160") %>% 
        select(1:2,5, starts_with("P09300")) %>% 
        pivot_longer(-c(1:3)) %>% 
        group_by(STATEFP, PLACEFP) %>% 
        mutate(denom = sum(value),
               denom_type = "all_households",
               perc = value / sum(value) * 100) %>% 
        filter(name == "P0930001") %>% 
        mutate(variable = "hh with interest, dividend, or net rental income") %>% 
        select(STATEFP, PLACEFP, variable, value, denom, perc, denom_type)
      
      ## female-headed hhs (P19) is in STF306 (Households)
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf306{.x}.dbf"), temp) 
      dmv_female_headed <- read.dbf(temp) %>% 
        filter(SUMLEV == "160") %>% 
        select(1:2,5, starts_with("P019000")) %>% 
        pivot_longer(-c(1:3)) %>% 
        filter(name != "P0190007") %>% 
        group_by(STATEFP, PLACEFP) %>% 
        mutate(denom = sum(value),
               denom_type = "all_family_households",
               perc = value / sum(value) * 100,
               variable = if_else(name == "P0190005", "single female-headed hh with children", "all other households")) %>% 
        filter(variable == "single female-headed hh with children") %>% 
        ungroup() %>% 
        select(STATEFP, PLACEFP, variable, value, denom, perc, denom_type)
      bind_rows(dmv_owner_occupied, dmv_median_value, dmv_median_hh_inc, dmv_rcv_interest_inc, dmv_female_headed)} ) }
    
    else {
    map2_dfr(c("dc", "md", "va"), c(11, 11, 57), ~{
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf327{.x}.dbf"), temp) 
      dmv_owner_occupied <- read.dbf(temp) %>% 
        filter(SUMLEV == "050") %>% 
        select(1:3, starts_with("H008000")) %>% 
        pivot_longer(-c(1:3)) %>% 
        group_by(STATEFP, CNTY) %>% 
        mutate(denom = sum(value),
               denom_type = "all_occupied_housing_units",
               perc = value / sum(value) * 100) %>% 
        mutate(variable = "Median housing value",
               variable = if_else(str_detect(name, "01"), "owner occupied", "renter occupied")) %>% 
        filter(variable == "owner occupied") %>% 
        select(STATEFP, CNTY, variable, value, denom, perc, denom_type)
      
      ## median housing value (H61A) is in STF333 (Households)
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf333{.x}.dbf"), temp) 
      dmv_median_value <- read.dbf(temp) %>% 
        filter(SUMLEV == "050") %>% 
        select(1:3, contains("61A")) %>% 
        pivot_longer(-c(1:3)) %>% 
        group_by(STATEFP, CNTY) %>% 
        mutate(variable = "Median housing value") %>% 
        select(STATEFP, CNTY, variable, value)
      
      ## median household income (P080A001) is in ST314 (Households)
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf314{.x}.dbf"), temp) 
      dmv_median_hh_inc <- read.dbf(temp) %>% 
        filter(SUMLEV == "050") %>% 
        select(1:3, starts_with("P080A001")) %>% 
        pivot_longer(-c(1:3)) %>% 
        group_by(STATEFP, CNTY) %>% 
        mutate(variable = "median hh income") %>% 
        select(STATEFP, CNTY, variable, value)
      
      ## interest income (table P93) is in STF321 (Households) "P0930001"
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf321{.x}.dbf"), temp) 
      dmv_rcv_interest_inc <- read.dbf(temp) %>% 
        filter(SUMLEV == "050") %>% 
        select(1:3, starts_with("P09300")) %>% 
        pivot_longer(-c(1:3)) %>% 
        group_by(STATEFP, CNTY) %>% 
        mutate(denom = sum(value),
               denom_type = "all_households",
               perc = value / sum(value) * 100) %>% 
        filter(name == "P0930001") %>% 
        mutate(variable = "hh with interest, dividend, or net rental income") %>% 
        select(STATEFP, CNTY, variable, value, denom, perc, denom_type)
      
      ## female-headed hhs (P19) is in STF306 (Households)
      download.file(glue::glue("https://www2.census.gov/census_1990/CD90_3A_{.y}/stf306{.x}.dbf"), temp) 
      dmv_female_headed <- read.dbf(temp) %>% 
        filter(SUMLEV == "050") %>% 
        select(1:3, starts_with("P019000")) %>% 
        pivot_longer(-c(1:3)) %>% 
        filter(name != "P0190007") %>% 
        group_by(STATEFP, CNTY) %>% 
        mutate(denom = sum(value),
               denom_type = "all_family_households",
               perc = value / sum(value) * 100,
               variable = if_else(name == "P0190005", "single female-headed hh with children", "all other households")) %>% 
        filter(variable == "single female-headed hh with children") %>% 
        ungroup() %>% 
        select(STATEFP, CNTY, variable, value, denom, perc, denom_type)
      bind_rows(dmv_owner_occupied, dmv_median_value, dmv_median_hh_inc, dmv_rcv_interest_inc, dmv_female_headed) %>% 
        ungroup %>% 
        mutate(GEOID = paste0(STATEFP, CNTY)) %>% 
        select(GEOID, 3:7)
    })
  }
  
}

w2_1990_places <- get_worksheet2("places") %>% 
  ungroup %>% 
  mutate(year = 1990) %>% 
  rename(label = variable) %>% 
  left_join(cpi) %>% 
  mutate(label = case_when(
           label == "owner occupied" ~ "Owner-occupied",
           label == "Median housing value" ~ "Median (dollars)",
           label == "median hh income" ~ "Median household income in the past 12 months",
           label == "hh with interest, dividend, or net rental income" ~ "With interest, dividends, or net rental income",
           label == "single female-headed hh with children" ~ "single-female-headed with young children"
         ),
         adjusted_value = if_else(str_detect(label, "^Median"), round(value * inflation_factor), NA_real_),
         year = "1990") %>% 
  mutate(GEOID = paste0(STATEFP, PLACEFP)) %>% 
  group_by(GEOID) %>% 
  mutate(total_household = denom[label == "Owner-occupied"],
         perc = if_else(!str_detect(label, "^Median"), value / total_household * 100, NA_real_)) %>% 
  ungroup() %>% 
  left_join(names_geoids) %>% 
  mutate(label = factor(label, levels = c("Median household income in the past 12 months", "With interest, dividends, or net rental income", "Owner-occupied", "Median (dollars)", "single-female-headed with young children" ))) %>% 
  arrange(NAME, GEOID, label) %>% 
  select(GEOID, NAME, label, year, value, adjusted_value, total = denom, perc, denom_type) 

w2_1990_counties <- get_worksheet2("counties") %>% 
  ungroup %>% 
  mutate(year = 1990) %>% 
  rename(label = variable) %>% 
  left_join(cpi) %>% 
  mutate(label = case_when(
           label == "owner occupied" ~ "Owner-occupied",
           label == "Median housing value" ~ "Median (dollars)",
           label == "median hh income" ~ "Median household income in the past 12 months",
           label == "hh with interest, dividend, or net rental income" ~ "With interest, dividends, or net rental income",
           label == "single female-headed hh with children" ~ "single-female-headed with young children"
         ),
         adjusted_value = if_else(str_detect(label, "^Median"), round(value * inflation_factor), NA_real_),
         year = "1990") %>% 
  group_by(GEOID) %>% 
  mutate(total_household = denom[label == "Owner-occupied"],
         perc = if_else(!str_detect(label, "^Median"), value / total_household * 100, NA_real_)) %>% 
  ungroup() %>% 
  left_join(names_geoids) %>% 
  mutate(label = factor(label, levels = c("Median household income in the past 12 months", "With interest, dividends, or net rental income", "Owner-occupied", "Median (dollars)", "single-female-headed with young children" ))) %>% 
  arrange(NAME, GEOID, label) %>% 
  select(GEOID, NAME, label, year, value, adjusted_value, total = denom, perc, denom_type) 



w2_counties_and_places <- bind_rows(w2_1990_counties, w2_1990_places)
write_csv(w2_counties_and_places, "Data/1990 Census - New Worksheet 2 - Counties and Places - Fall 2022.csv")
w2_1990_sectionB <- w2_counties_and_places %>% 
  filter(GEOID %in% c("51510", "24027", "51059", "51153", "24031", "24033")) 
w2_1990_sectionC <- w2_counties_and_places %>% 
  filter(GEOID %in% c("51510", "51600", "51610", "51685", "2402825", "2429525", "2445525")) 

w2_1990_dmv <- get_worksheet2("dmv") %>% 
  mutate(year = 1990) %>% 
  rename(label = variable,
         GEOID = MSACMSA) %>% 
  left_join(cpi) %>% 
  mutate(label = case_when(
    label == "owner occupied" ~ "Owner-occupied",
    label == "Median housing value" ~ "Median (dollars)",
    label == "median hh income" ~ "Median household income in the past 12 months",
    label == "hh with interest, dividend, or net rental income" ~ "With interest, dividends, or net rental income",
    label == "single female-headed hh with children" ~ "single-female-headed with young children"
  ),
  adjusted_value = if_else(str_detect(label, "^Median"), round(value * inflation_factor), NA_real_),
  year = "1990") %>% 
  group_by(GEOID) %>% 
  mutate(total_household = denom[label == "Owner-occupied"],
         perc = if_else(!str_detect(label, "^Median"), value / total_household * 100, NA_real_),
         NAME = "DMV Metro area (1990)") %>% 
  ungroup() %>% 
  mutate(label = factor(label, levels = c("Median household income in the past 12 months", "With interest, dividends, or net rental income", "Owner-occupied", "Median (dollars)", "single-female-headed with young children" ))) %>% 
  arrange(NAME, GEOID, label) %>% 
  select(GEOID, NAME, label, year, value, adjusted_value, total = denom, perc, denom_type) %>% 
  write_csv("Data/1990 Census - New Worksheet 2 - DMV - Fall 2022.csv")
  
####NDI tutorial####
powell_wiley <- read_html("https://cran.r-project.org/web/packages/ndi/vignettes/vignette.html") %>% 
  html_table() %>% 
  .[[2]] %>% 
  arrange(`SES dimension`)

####Earlier stuff not needed####
## 1990 variables
## data dictionary is here: "https://www2.census.gov/census_1990/CD90_3A_11/document/tbl_mtx.asc", sep = "\t") 

## 1. Demographics: race/ethnicity, immigration status, occupation/management, female headed hhs with children
## 2. Education, income, wealth: hs and grad, hh income, interest income, housing value, % owner occupied, unemployed 


## owner occupied (table H8) is in STF327 (Households)
## median household income (P080A001) is in ST314 (Households)
## interest income (table P93) is in STF321 (Households) "P0930001"
## female-headed hhs (P19) is in STF306 (Households)
## median housing value (H61A) is in STF333 (Households)


hs <- "P0570003"
grad <- "P0570007"
owner <- "H0080001"
mgt <- c("P0780001", "P0780002")
interest_income <- "P0930001"
female_hhs_children <- "P0190005"

####Worksheet 1####
## total population (P003) is in STF301

## hispanic by race (P12) is in STF301
download.file("https://www2.census.gov/census_1990/cd903c01/stf301us.dbf", temp)
dmv_race <- read.dbf(temp) %>% 
  filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
         SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
         GEOCOMP == "00") %>% 
  select(MSACMSA, starts_with("P012")) %>% 
  pivot_longer(-MSACMSA) %>% 
  mutate(variable = case_when(
    name %in% c("P0120006", "P0120007", "P0120008", "P0120009", "P0120010") ~ "Hispanic or Latino (of any race)",
    name == "P0120003" ~ "All other races",
    name == "P0120001" ~ "White alone",
    name == "P0120002" ~ "Black or African American alone",
    name == "P0120004" ~ "Asian alone",
    name == "P0120005" ~ "All other races"
  )) %>% 
  group_by(MSACMSA, variable) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(total = sum(value))

## citizenship (by age) (P37) is in STF309
download.file("https://www2.census.gov/census_1990/cd903c01/stf309us.dbf", temp)
dmv_cit <- read.dbf(temp) %>% 
  filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
         SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
         GEOCOMP == "00") %>% 
  select(MSACMSA, starts_with("P037")) %>% 
  pivot_longer(-MSACMSA) %>% 
  mutate(variable = case_when(
    name %in% c("P0370001", "P0370004") ~ "Native-born",
    name %in% c("P0370002", "P0370005") ~ "Naturalized",
    TRUE ~ "Not a citizen"
  )) %>% 
  group_by(MSACMSA, variable) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(total = sum(value))

## management data (table P78) is in STF314
download.file("https://www2.census.gov/census_1990/cd903c01/stf314us.dbf", temp)
dmv_mgt <- read.dbf(temp) %>% 
  filter(MSACMSA == "8840", 
         SUMLEV == "300", 
         GEOCOMP == "00") %>% 
  select(MSACMSA, contains("P078")) %>% 
  pivot_longer(-MSACMSA) %>% 
  #filter(name %in% mgt) %>% 
  mutate(variable = case_when(
    str_detect(name, "0001|02") ~ "Management, business, science, and arts occupations",
    str_detect(name, "06|007|08") ~ "Service occupations",
    str_detect(name, "03|04|05") ~ "Sales and office occupations",
    TRUE ~ "Other occupations")
  ) %>% 
  group_by(MSACMSA, variable) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(total = sum(value), 
         perc = value / total * 100) %>% 
  filter(str_detect(variable, "^Manag"))

## educational attainment (table P57) is in STF310
download.file("https://www2.census.gov/census_1990/cd903c01/stf310us.dbf", temp)
dmv_ed <- read.dbf(temp) %>% 
  filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
         SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
         GEOCOMP == "00") %>% 
  select(MSACMSA, starts_with("P057")) %>% 
  pivot_longer(-MSACMSA) %>% 
  mutate(variable = case_when(
    name == "P0570003" ~ "High school degree",
    name == "P0570006" ~ "Bachelor's degree",
    name == "P0570007" ~ "Graduate degree",
    TRUE ~ "Other ed"
  )) %>% 
  group_by(MSACMSA, variable) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(total = sum(value))

## Total population
download.file("https://www2.census.gov/census_1990/cd903c01/stf301us.dbf", temp)
dmv_totpop <- read.dbf(temp) %>% 
  filter(MSACMSA == "8840", ## this is DC https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/1990/historical-delineation-files/90mfips.txt
         SUMLEV == "300", ## these definitions found here: https://www2.census.gov/census_1990/CD90_3A_11/document/iden_ftn.asc
         GEOCOMP == "00") %>% 
  select(MSACMSA, "P0030001") %>% 
  pivot_longer(-MSACMSA) %>% 
  mutate(variable = "Total population",
         total = value) %>% 
  select(-name)