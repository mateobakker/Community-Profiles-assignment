library(tidyverse)
library(tidycensus)

#### Revised December 11, 2022
get_w2_2000 <- function(state, county, geography) {
  require(tidyverse)
  require(tidycensus)
  if(geography == "place") {
    owner_occ <- get_decennial(
      geography = geography,
      state = state,
      county = NULL,
      year = 2000,
      variable = "H007002",
      summary_var = "H007001",
      sumfile = "sf3"
    ) %>%
      left_join(load_variables(2000, "sf3", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!"),
             perc = value / summary_value * 100) %>%
      rename(total = summary_value) %>%
      select(GEOID, NAME, label, value, total, perc)
    
    
    med_value <- get_decennial(
      geography = geography,
      state = state,
      county = NULL,
      year = 2000,
      variable = "H076001",
      sumfile = "sf3"
    ) %>%
      left_join(load_variables(2000, "sf3", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!")) %>%
      select(GEOID, NAME, label, value)
    
    med_hh_inc <- get_decennial(
      geography = geography,
      state = state,
      county = NULL,
      year = 2000,
      variable = "P053001",
      sumfile = "sf3"
    ) %>%
      left_join(load_variables(2000, "sf3", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!")) %>%
      select(GEOID, NAME, label, value)
    
    int_income <- get_decennial(
      geography = geography,
      state = state,
      county = NULL,
      year = 2000,
      variable = "P061002",
      summary_var = "P061001",
      sumfile = "sf3"
    ) %>%
      left_join(load_variables(2000, "sf3", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!"),
             perc = value / summary_value * 100) %>%
      rename(total = summary_value) %>%
      select(GEOID, NAME, label, value, total, perc)
    
    single_mother_w_young <- 
      get_decennial(
        geography = geography,
        state = state,
        county = NULL,
        year = 2000,
        variable = "P015016",
        summary_var = "P015001",
        sumfile = "sf3"
      ) %>%
      left_join(load_variables(2000, "sf3", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!"),
             perc = value / summary_value * 100) %>%
      rename(total = summary_value) %>%
      select(GEOID, NAME, label, value, total, perc) 
    
    all_places <- bind_rows(single_mother_w_young,
              int_income,
              med_hh_inc,
              med_value,
              owner_occ) %>% 
      filter(NAME %in% county) %>% 
      mutate(label = case_when(
        label == "Owner occupied" ~ "Owner-occupied",
        label == "Median value" ~ "Median (dollars)",
        label == "Median household income in 1999" ~ "Median household income in the past 12 months",
        label == "With interest, dividends, or net rental income" ~ "With interest, dividends, or net rental income",
        label == "With own children under 18 years" ~ "single-female-headed with young children"
      )) %>% 
      mutate(year = 2000) %>% 
      left_join(cpi) %>% 
      mutate(adjusted_value = if_else(str_detect(label, "^Median"), value * inflation_factor, NA_real_)) %>% 
      mutate(year = "2000",
             NAME = str_squish(str_remove(NAME, "CDP.*|city.*|County.*"))) %>% 
      select(GEOID, NAME, year, label, value, adjusted_value, total, perc)
    
    return(all_places)
  }
  
  if (geography == "county") {
    owner_occ <- get_decennial(
      geography = geography,
      state = state,
      county = county,
      year = 2000,
      variable = "H007002",
      summary_var = "H007001",
      sumfile = "sf3"
    ) %>%
      left_join(load_variables(2000, "sf3", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!"),
             perc = value / summary_value * 100) %>%
      rename(total = summary_value) %>%
      select(GEOID, NAME, label, value, total, perc)
    
    
    med_value <- get_decennial(
      geography = geography,
      state = state,
      county = county,
      year = 2000,
      variable = "H076001",
      sumfile = "sf3"
    ) %>%
      left_join(load_variables(2000, "sf3", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!")) %>%
      select(GEOID, NAME, label, value)
    
    med_hh_inc <- get_decennial(
      geography = geography,
      state = state,
      county = county,
      year = 2000,
      variable = "P053001",
      sumfile = "sf3"
    ) %>%
      left_join(load_variables(2000, "sf3", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!")) %>%
      select(GEOID, NAME, label, value)
    
    int_income <- get_decennial(
      geography = geography,
      state = state,
      county = county,
      year = 2000,
      variable = "P061002",
      summary_var = "P061001",
      sumfile = "sf3"
    ) %>%
      left_join(load_variables(2000, "sf3", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!"),
             perc = value / summary_value * 100) %>%
      rename(total = summary_value) %>%
      select(GEOID, NAME, label, value, total, perc)
    
    single_mother_w_young <- 
      get_decennial(
        geography = geography,
        state = state,
        county = county,
        year = 2000,
        variable = "P015016",
        summary_var = "P015001",
        sumfile = "sf3"
      ) %>%
      left_join(load_variables(2000, "sf3", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!"),
             perc = value / summary_value * 100) %>%
      rename(total = summary_value) %>%
      select(GEOID, NAME, label, value, total, perc) 
    
    all_counties <- bind_rows(single_mother_w_young,
              int_income,
              med_hh_inc,
              med_value,
              owner_occ) %>% 
      mutate(label = case_when(
        label == "Owner occupied" ~ "Owner-occupied",
        label == "Median value" ~ "Median (dollars)",
        label == "Median household income in 1999" ~ "Median household income in the past 12 months",
        label == "With interest, dividends, or net rental income" ~ "With interest, dividends, or net rental income",
        label == "With own children under 18 years" ~ "single-female-headed with young children"
      )) %>% 
      mutate(year = 2000) %>% 
      left_join(cpi) %>% 
      mutate(adjusted_value = if_else(str_detect(label, "^Median"), value * inflation_factor, NA_real_)) %>% 
      mutate(year = "2000",
             NAME = str_squish(str_remove(NAME, "CDP.*|city.*|County.*"))) %>% 
      select(GEOID, NAME, year, label, value, adjusted_value, total, perc)
    return(all_counties)
  }
    
    else {
      # Median value
      variables = "H076001"
      url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={variables},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
      med_value <- jsonlite::fromJSON(url) %>%
        as_tibble() %>%
        set_names(.[1,]) %>%
        slice(-1) %>%
        rename(value = {{variables}}) %>%
        mutate(name = variables) %>%
        left_join(load_variables(2000, "sf3", T)) %>%
        mutate(label = str_remove(label, ".*!")) %>%
        select(GEOID = 4, NAME, label, value) %>%
        mutate(value = parse_number(value)) %>%
        filter(GEOID == "8840") %>%
        separate(NAME, c("NAME", "drop"), sep = ";") %>%
        select(-drop)

      ## Owner-occupied units
      variables = "H007002"
      summary_var = "H007001"
      cens_vars = str_c(summary_var, variables, sep = ",")
      url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={cens_vars},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
      owner_occ <- jsonlite::fromJSON(url) %>%
        as_tibble() %>%
        set_names(.[1,]) %>%
        slice(-1) %>%
        rename(summary_val = {{summary_var}}) %>%
        pivot_longer(variables) %>%
        left_join(load_variables(2000, "sf3", T)) %>%
        mutate(label = str_remove(label, ".*!")) %>%
        select(GEOID = 4, NAME, label, value, summary_val) %>%
        mutate(across(value:summary_val, parse_number)) %>%
        rename(total = summary_val) %>%
        select(GEOID, NAME, label, value, total) %>%
        mutate(perc = value / total * 100) %>%
        filter(GEOID == "8840") %>%
        group_by(GEOID, NAME, label) %>%
        summarise(value = sum(value),
                  total = mean(total)) %>%
        mutate(perc = value / total * 100) %>%
        separate(NAME, c("NAME", "drop"), sep = ";") %>%
        select(-drop) %>%
        ungroup()

      ## Median hh income
      variables <- "P053001"
      url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={variables},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
      med_hh_inc <- jsonlite::fromJSON(url) %>%
        as_tibble() %>%
        set_names(.[1,]) %>%
        slice(-1) %>%
        rename(value = {{variables}}) %>%
        mutate(name = variables) %>%
        left_join(load_variables(2000, "sf3", T)) %>%
        mutate(label = str_remove(label, ".*!")) %>%
        select(GEOID = 4, NAME, label, value) %>%
        mutate(value = parse_number(value)) %>%
        filter(GEOID == "8840") %>%
        separate(NAME, c("NAME", "drop"), sep = ";") %>%
        select(-drop)

      ## Has interest, dividend, or net rental income
      variables = "P061002"
      summary_var = "P061001"
      cens_vars = str_c(summary_var, variables, sep = ",")
      url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={cens_vars},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
      int_income <- jsonlite::fromJSON(url) %>%
        as_tibble() %>%
        set_names(.[1,]) %>%
        slice(-1) %>%
        rename(summary_val = {{summary_var}}) %>%
        pivot_longer(variables) %>%
        left_join(load_variables(2000, "sf3", T)) %>%
        mutate(label = str_remove(label, ".*!")) %>%
        select(GEOID = 4, NAME, label, value, summary_val) %>%
        mutate(across(value:summary_val, parse_number)) %>%
        rename(total = summary_val) %>%
        select(GEOID, NAME, label, value, total) %>%
        mutate(perc = value / total * 100) %>%
        filter(GEOID == "8840") %>%
        group_by(GEOID, NAME, label) %>%
        summarise(value = sum(value),
                  total = mean(total)) %>%
        mutate(perc = value / total * 100) %>%
        separate(NAME, c("NAME", "drop"), sep = ";") %>%
        select(-drop) %>%
        ungroup()

      ## Single-female headed with children
      variables = "P015016"
      summary_var = "P015001"
      cens_vars = str_c(summary_var, variables, sep = ",")
      url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={cens_vars},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
      single_mother_w_young <- jsonlite::fromJSON(url) %>%
        as_tibble() %>%
        set_names(.[1,]) %>%
        slice(-1) %>%
        rename(summary_val = {{summary_var}}) %>%
        pivot_longer(variables) %>%
        left_join(load_variables(2000, "sf3", T)) %>%
        mutate(label = str_remove(label, ".*!")) %>%
        select(GEOID = 4, NAME, label, value, summary_val) %>%
        mutate(across(value:summary_val, parse_number)) %>%
        rename(total = summary_val) %>%
        select(GEOID, NAME, label, value, total) %>%
        mutate(perc = value / total * 100) %>%
        filter(GEOID == "8840") %>%
        group_by(GEOID, NAME, label) %>%
        summarise(value = sum(value),
                  total = mean(total)) %>%
        mutate(perc = value / total * 100) %>%
        separate(NAME, c("NAME", "drop"), sep = ";") %>%
        select(-drop) %>%
        ungroup()

      all_dmv <- bind_rows(owner_occ, med_value, med_hh_inc, int_income, single_mother_w_young) %>%
        mutate(label = case_when(
          label == "Owner occupied" ~ "Owner-occupied",
          label == "Median value" ~ "Median (dollars)",
          label == "Median household income in 1999" ~ "Median household income in the past 12 months",
          label == "With interest, dividends, or net rental income" ~ "With interest, dividends, or net rental income",
          label == "With own children under 18 years" ~ "single-female-headed with young children"
        )) %>% 
        mutate(year = 2000) %>% 
        left_join(cpi) %>% 
        mutate(adjusted_value = if_else(str_detect(label, "^Median"), value * inflation_factor, NA_real_)) %>% 
        mutate(year = "2000",
               NAME = "DMV Metro area") %>% 
        select(GEOID, NAME, year, label, value, adjusted_value, total, perc)
      return(all_dmv)
    }
  
}

### OJO: Run inputs for Revised Worksheet 2.R script for needed counties_places tibble
w2_2000_counties_and_places <- pmap_dfr(list(counties_places$state, 
                                  counties_places$county, 
                                  counties_places$geography), 
                             get_w2_2000) %>% 
  write_csv("Data/2000 Census - New Worksheet 2 - Counties and Places - Fall 2022.csv")

w2_2000_dmv <- get_w2_2000(NULL, NULL, "dmv") %>% 
  write_csv("Data/2000 Census - New Worksheet 2 - DMV - Fall 2022.csv")


#### DMV metro ####
## This is for the DMV as a whole
## Median value
# variables = "H076001"
# url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={variables},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
# med_value <- jsonlite::fromJSON(url) %>%
#   as_tibble() %>%
#   set_names(.[1,]) %>%
#   slice(-1) %>%
#   rename(value = {{variables}}) %>% 
#   mutate(name = variables) %>% 
#   left_join(load_variables(2000, "sf3", T)) %>%
#   mutate(label = str_remove(label, ".*!")) %>% 
#   select(GEOID = 4, NAME, label, value) %>%
#   mutate(value = parse_number(value)) %>%
#   filter(GEOID == "8840") %>% 
#   separate(NAME, c("NAME", "drop"), sep = ";") %>%
#   select(-drop) 
# 
# ## Owner-occupied units
# variables = "H007002"
# summary_var = "H007001"
# cens_vars = str_c(summary_var, variables, sep = ",")
# url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={cens_vars},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
# owner_occ <- jsonlite::fromJSON(url) %>%
#   as_tibble() %>%
#   set_names(.[1,]) %>%
#   slice(-1) %>%
#   rename(summary_val = {{summary_var}}) %>%
#   pivot_longer(variables) %>%
#   left_join(load_variables(2000, "sf3", T)) %>%
#   mutate(label = str_remove(label, ".*!")) %>%
#   select(GEOID = 4, NAME, label, value, summary_val) %>%
#   mutate(across(value:summary_val, parse_number)) %>%
#   rename(total = summary_val) %>%
#   select(GEOID, NAME, label, value, total) %>%
#   mutate(perc = value / total * 100) %>%
#   filter(GEOID == "8840") %>%
#   group_by(GEOID, NAME, label) %>%
#   summarise(value = sum(value),
#             total = mean(total)) %>%
#   mutate(perc = value / total * 100) %>% 
#   separate(NAME, c("NAME", "drop"), sep = ";") %>%
#   select(-drop) %>% 
#   ungroup()
# 
# ## Median hh income
# variables <- "P053001"
# url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={variables},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
# med_hh_inc <- jsonlite::fromJSON(url) %>%
#   as_tibble() %>%
#   set_names(.[1,]) %>%
#   slice(-1) %>%
#   rename(value = {{variables}}) %>% 
#   mutate(name = variables) %>% 
#   left_join(load_variables(2000, "sf3", T)) %>%
#   mutate(label = str_remove(label, ".*!")) %>% 
#   select(GEOID = 4, NAME, label, value) %>%
#   mutate(value = parse_number(value)) %>%
#   filter(GEOID == "8840") %>% 
#   separate(NAME, c("NAME", "drop"), sep = ";") %>%
#   select(-drop) 
# 
# ## Has interest, dividend, or net rental income
# variables = "P061002"
# summary_var = "P061001"
# cens_vars = str_c(summary_var, variables, sep = ",")
# url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={cens_vars},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
# int_income <- jsonlite::fromJSON(url) %>%
#   as_tibble() %>%
#   set_names(.[1,]) %>%
#   slice(-1) %>%
#   rename(summary_val = {{summary_var}}) %>%
#   pivot_longer(variables) %>%
#   left_join(load_variables(2000, "sf3", T)) %>%
#   mutate(label = str_remove(label, ".*!")) %>%
#   select(GEOID = 4, NAME, label, value, summary_val) %>%
#   mutate(across(value:summary_val, parse_number)) %>%
#   rename(total = summary_val) %>%
#   select(GEOID, NAME, label, value, total) %>%
#   mutate(perc = value / total * 100) %>%
#   filter(GEOID == "8840") %>%
#   group_by(GEOID, NAME, label) %>%
#   summarise(value = sum(value),
#             total = mean(total)) %>%
#   mutate(perc = value / total * 100) %>% 
#   separate(NAME, c("NAME", "drop"), sep = ";") %>%
#   select(-drop) %>% 
#   ungroup()
# 
# ## Single-female headed with children
# variables = "P015016"
# summary_var = "P015001"
# cens_vars = str_c(summary_var, variables, sep = ",")
# url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={cens_vars},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
# single_mother_w_young <- jsonlite::fromJSON(url) %>%
#   as_tibble() %>%
#   set_names(.[1,]) %>%
#   slice(-1) %>%
#   rename(summary_val = {{summary_var}}) %>%
#   pivot_longer(variables) %>%
#   left_join(load_variables(2000, "sf3", T)) %>%
#   mutate(label = str_remove(label, ".*!")) %>%
#   select(GEOID = 4, NAME, label, value, summary_val) %>%
#   mutate(across(value:summary_val, parse_number)) %>%
#   rename(total = summary_val) %>%
#   select(GEOID, NAME, label, value, total) %>%
#   mutate(perc = value / total * 100) %>%
#   filter(GEOID == "8840") %>%
#   group_by(GEOID, NAME, label) %>%
#   summarise(value = sum(value),
#             total = mean(total)) %>%
#   mutate(perc = value / total * 100) %>% 
#   separate(NAME, c("NAME", "drop"), sep = ";") %>%
#   select(-drop) %>% 
#   ungroup()
# 
# all_dmv <- bind_rows(owner_occ, med_value, med_hh_inc, int_income, single_mother_w_young)
# worksheet2_2000_dmv %>% 
#   write_csv("Data/2000 Census - New Worksheet 2 - DMV - Fall 2022.csv")

temp <- tempfile()
download.file("https://www.bls.gov/cpi/research-series/r-cpi-u-rs-allitems.xlsx", temp)
# Formula for calculating inflation example: $1.00 * (1980 CPI/ 2014 CPI) = 1980 price

cpi <- readxl::read_excel(temp, skip = 5) %>% 
  select(year = YEAR, avg = AVG) %>% 
  filter(year %in% c(1990, 2000, 2010, 2020)) %>% 
  mutate(base = avg[year == 2020],
         inflation_factor = base/avg) %>% 
  select(year, inflation_factor) 



sectionB_w2_2000 <- read_csv("Data/2000 Census - New Worksheet 2 - Counties and Places - Fall 2022.csv") %>% 
  filter(GEOID %in% c("51510", "24027", "51059", "51153", "24031", "24033")) 

sectionC_w2_2000 <-  read_csv("Data/2000 Census - New Worksheet 2 - Counties and Places - Fall 2022.csv") %>% 
  filter(GEOID %in% c("51510", "51600", "51610", "51685", "2402825", "2429525", "2445525")) 

 


  arrange(year, label)
  mutate(adjusted_value = )
  