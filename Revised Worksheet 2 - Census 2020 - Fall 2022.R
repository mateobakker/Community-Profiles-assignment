library(tidyverse)
library(tidycensus)

get_worksheet2_2020 <- function(unit) {
  if (!unit %in% c("dmv", "counties", "place")) {stop("unit should be either 'dmv', 'place', or 'counties'")}
  if(unit == "dmv") {
    int_inc_20 <-
  get_acs("metropolitan statistical area/micropolitan statistical area",
          variables = "B19054_002",
          summary_var = "B19054_001",
          year = 2020) %>%
  mutate(perc = estimate / summary_est * 100) %>%
  filter(GEOID == "47900") %>%
  left_join(load_variables(2020, "acs5", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate, summary_est, perc) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))

single_mother_w_young_20 <- get_acs(
    "metropolitan statistical area/micropolitan statistical area",
    variables = "B11004_016",
    summary_var = "B11004_001",
    year = 2020
  ) %>%
    filter(GEOID == "47900") %>%
    left_join(load_variables(2020, "acs5", T), by = c("variable" = "name")) %>%
    mutate(label = str_remove(label, ".*!"),
           NAME = str_extract(NAME, ".*,")) %>%
    group_by(GEOID, NAME) %>%
    summarise(estimate = sum(estimate),
              summary_est = mean(summary_est)) %>%
    ungroup() %>%
    mutate(perc = estimate / summary_est * 100,
           label = "single-female-headed with young children") %>%
    select(GEOID, NAME, label, estimate, summary_est, perc)

owner_occ_20 <- #DP04_0045
  get_acs("metropolitan statistical area/micropolitan statistical area",
          variables = "DP04_0046",
          summary_var = "DP04_0045",
          year = 2020) %>%
  mutate(perc = estimate / summary_est * 100) %>%
  filter(GEOID == "47900") %>%
  left_join(load_variables(2020, "acs5/profile", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate, summary_est, perc) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))

med_value_20 <- #DP04_0088
  get_acs("metropolitan statistical area/micropolitan statistical area",
          variables = "DP04_0089",
          year = 2020) %>%
  filter(GEOID == "47900") %>%
  left_join(load_variables(2020, "acs5/profile", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))

med_hh_inc_20 <- #B19013_001
  get_acs("metropolitan statistical area/micropolitan statistical area",
          variables = "B19013_001",
          year = 2020) %>%
  filter(GEOID == "47900") %>%
  left_join(load_variables(2020, "acs5", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))
bind_rows(med_hh_inc_20, int_inc_20, owner_occ_20, med_value_20, single_mother_w_young_20)
  }
  
  if(unit == "place") {
    int_inc_20 <-
      get_acs("place",
              state = "MD",
              variables = "B19054_002",
              summary_var = "B19054_001",
              year = 2020) %>%
      mutate(perc = estimate / summary_est * 100) %>%
      left_join(load_variables(2020, "acs5", T), by = c("variable" = "name")) %>%
      select(GEOID, NAME, label, estimate, summary_est, perc) %>%
      mutate(label = str_remove(label, ".*!"),
             NAME = str_extract(NAME, ".*,"))
    
    single_mother_w_young_20 <- get_acs(
      "place",
      state = "MD",
      variables = "B11004_016",
      summary_var = "B11004_001",
      year = 2020
    ) %>%
      left_join(load_variables(2020, "acs5", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!"),
             NAME = str_extract(NAME, ".*,")) %>%
      group_by(GEOID, NAME) %>%
      summarise(estimate = sum(estimate),
                summary_est = mean(summary_est)) %>%
      ungroup() %>%
      mutate(perc = estimate / summary_est * 100,
             label = "single-female-headed with young children") %>%
      select(GEOID, NAME, label, estimate, summary_est, perc)
    
    owner_occ_20 <- #DP04_0045
      get_acs("place",
              state = "MD",
              variables = "DP04_0046",
              summary_var = "DP04_0045",
              year = 2020) %>%
      mutate(perc = estimate / summary_est * 100) %>%
      left_join(load_variables(2020, "acs5/profile", T), by = c("variable" = "name")) %>%
      select(GEOID, NAME, label, estimate, summary_est, perc) %>%
      mutate(label = str_remove(label, ".*!"),
             NAME = str_extract(NAME, ".*,"))
    
    med_value_20 <- #DP04_0088
      get_acs("place",
              state = "MD",
              variables = "DP04_0089",
              year = 2020) %>%
      left_join(load_variables(2020, "acs5/profile", T), by = c("variable" = "name")) %>%
      select(GEOID, NAME, label, estimate) %>%
      mutate(label = str_remove(label, ".*!"),
             NAME = str_extract(NAME, ".*,"))
    
    med_hh_inc_20 <- #B19013_001
      get_acs("place",
              state = "MD",
              variables = "B19013_001",
              year = 2020) %>%
      left_join(load_variables(2020, "acs5", T), by = c("variable" = "name")) %>%
      select(GEOID, NAME, label, estimate) %>%
      mutate(label = str_remove(label, ".*!"),
             NAME = str_extract(NAME, ".*,"))
    
    bind_rows(med_hh_inc_20, int_inc_20, owner_occ_20, med_value_20, single_mother_w_young_20) %>%
      arrange(GEOID)
  }
  else {
    int_inc_20 <-
  get_acs("county",
          state = c("DC", "MD", "VA"),
          variables = "B19054_002",
          summary_var = "B19054_001",
          year = 2020) %>%
  mutate(perc = estimate / summary_est * 100) %>%
  left_join(load_variables(2020, "acs5", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate, summary_est, perc) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))

single_mother_w_young_20 <- get_acs(
  "county",
  state = c("DC", "MD", "VA"),
  variables = "B11004_016",
  summary_var = "B11004_001",
  year = 2020
) %>%
  left_join(load_variables(2020, "acs5", T), by = c("variable" = "name")) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,")) %>%
  group_by(GEOID, NAME) %>%
  summarise(estimate = sum(estimate),
            summary_est = mean(summary_est)) %>%
  ungroup() %>%
  mutate(perc = estimate / summary_est * 100,
         label = "single-female-headed with young children") %>%
  select(GEOID, NAME, label, estimate, summary_est, perc)

owner_occ_20 <- #DP04_0045
  get_acs("county",
          state = c("DC", "MD", "VA"),
          variables = "DP04_0046",
          summary_var = "DP04_0045",
          year = 2020) %>%
  mutate(perc = estimate / summary_est * 100) %>%
  left_join(load_variables(2020, "acs5/profile", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate, summary_est, perc) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))

med_value_20 <- #DP04_0088
  get_acs("county",
          state = c("DC", "MD", "VA"),
          variables = "DP04_0089",
          year = 2020) %>%
  left_join(load_variables(2020, "acs5/profile", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))

med_hh_inc_20 <- #B19013_001
  get_acs("county",
          state = c("DC", "MD", "VA"),
          variables = "B19013_001",
          year = 2020) %>%
  left_join(load_variables(2020, "acs5", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))

bind_rows(med_hh_inc_20, int_inc_20, owner_occ_20, med_value_20, single_mother_w_young_20) %>%
  arrange(GEOID)
  }

}

# w2_2020_dmv <- get_worksheet2_2020("dmv") %>%
#   write_csv("Data - New Worksheet 2 - 2020 ACS - DMV.csv")
# w2_2020_counties <- get_worksheet2_2020("counties") %>%
#   write_csv("Data - New Worksheet 2 - 2020 ACS - Counties.csv")
# w2_2020_places <- get_worksheet2_2020("place") %>%
#   write_csv("Data - New Worksheet 2 - 2020 - places.csv")
# 
# w2_2020_dmv <- read_csv("Data - New Worksheet 2 - 2020 ACS - DMV.csv")
# w2_2020_counties <- read_csv("Data - New Worksheet 2 - 2020 ACS - Counties.csv")
# w2_2020_places <- read_csv("Data - New Worksheet 2 - 2020 - places.csv")


w2_2020_counties_and_places <- bind_rows(w2_2020_counties, w2_2020_places)

w2_2020_sectionB <- w2_2020_counties_and_places %>% 
  filter(GEOID %in% c("51510", "24027", "51059", "51153", "24031", "24033")) 

w2_2020_sectionC <- w2_2020_counties_and_places %>% 
  filter(GEOID %in% c("51510", "51600", "51610", "51685", "2402825", "2429525", "2445525")) 

rm(w2_2020_counties, w2_2020_counties_and_places, w2_2020_places)

bind_rows(w2_2020_dmv %>% filter(GEOID == "11001"), w2_2020_sectionC) %>% 
  mutate(reportable_value = if_else(is.na(perc), estimate, perc),
         year = 2020L) %>% 
  select(year, NAME, label, reportable_value, summary_est) 
  
