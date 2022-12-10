library(tidyverse)
get_worksheet2_2010 <- function(unit) {
  require("tidycensus")
  require("tidyverse")
  
  if (unit == "dmv")
    {
    int_inc_10 <-
      get_acs("metropolitan statistical area/micropolitan statistical area",
              variables = "B19054_002",
              summary_var = "B19054_001",
              year = 2010) %>%
      mutate(perc = estimate / summary_est * 100) %>%
      filter(GEOID == "47900") %>%
      left_join(load_variables(2010, "acs5", T), by = c("variable" = "name")) %>%
      select(GEOID, NAME, label, estimate, summary_est, perc) %>%
      mutate(label = str_remove(label, ".*!"),
             NAME = str_extract(NAME, ".*,"))
    
    single_mother_w_young_10  <- get_acs(
      "metropolitan statistical area/micropolitan statistical area",
      variables = "B11004_016", 
      summary_var = "B11004_001",
      year = 2010
    ) %>%
      filter(GEOID == "47900") %>%
      left_join(load_variables(2010, "acs5", T), by = c("variable" = "name")) %>%
      mutate(label = str_remove(label, ".*!"),
             NAME = str_extract(NAME, ".*,")) %>%
      group_by(GEOID, NAME) %>%
      summarise(estimate = sum(estimate),
                summary_est = mean(summary_est)) %>%
      ungroup() %>%
      mutate(perc = estimate / summary_est * 100,
             label = "single-female-headed with young children") %>%
      select(GEOID, NAME, label, estimate, summary_est, perc)
    
    owner_occ_10 <- #DP04_0045
      get_acs("metropolitan statistical area/micropolitan statistical area",
              variables = "DP04_0045",
              summary_var = "DP04_0044",
              year = 2010) %>%
      mutate(perc = estimate / summary_est * 100) %>%
      filter(GEOID == "47900") %>%
      left_join(load_variables(2010, "acs5/profile", T), by = c("variable" = "name")) %>%
      select(GEOID, NAME, label, estimate, summary_est, perc) %>%
      mutate(label = str_remove(label, ".*!"),
             NAME = str_extract(NAME, ".*,"))
    
    med_value_10 <- #DP04_0088
      get_acs("metropolitan statistical area/micropolitan statistical area",
              variables = "DP04_0088",
              year = 2010) %>%
      filter(GEOID == "47900") %>%
      left_join(load_variables(2010, "acs5/profile", T), by = c("variable" = "name")) %>%
      select(GEOID, NAME, label, estimate) %>%
      mutate(label = str_remove(label, ".*!"),
             NAME = str_extract(NAME, ".*,"))
    
    med_hh_inc_10 <- #B19013_001
      get_acs("metropolitan statistical area/micropolitan statistical area",
              variables = "B19013_001",
              year = 2010) %>%
      filter(GEOID == "47900") %>%
      left_join(load_variables(2010, "acs5", T), by = c("variable" = "name")) %>%
      select(GEOID, NAME, label, estimate) %>%
      mutate(label = str_remove(label, ".*!"),
             NAME = str_extract(NAME, ".*,"))
    
    all_dmv <- bind_rows(med_hh_inc_10, int_inc_10, owner_occ_10, med_value_10, single_mother_w_young_10) %>% 
      mutate(adjusted_estimate = if_else(str_detect(label, "^Median"), estimate * 1.19, NA_real_),
             NAME = "DMV Metro area",
             year = 2010)
    return(all_dmv)
  }
  if (unit == "place") 
    {
  int_inc_10 <-
    get_acs("place",
            state = "MD",
            variables = "B19054_002",
            summary_var = "B19054_001",
            year = 2010) %>%
    mutate(perc = estimate / summary_est * 100) %>%
    left_join(load_variables(2010, "acs5", T), by = c("variable" = "name")) %>%
    select(GEOID, NAME, label, estimate, summary_est, perc) %>%
    mutate(label = str_remove(label, ".*!"),
           NAME = str_extract(NAME, ".*,"),
           year = 2010)

  single_mother_w_young_10  <- get_acs(
    "place",
    state = "MD",
    variables = "B11004_016",
    summary_var = "B11004_001",
    year = 2010
  ) %>%
    left_join(load_variables(2010, "acs5", T), by = c("variable" = "name")) %>%
    mutate(label = str_remove(label, ".*!"),
           NAME = str_extract(NAME, ".*,")) %>%
    group_by(GEOID, NAME) %>%
    summarise(estimate = sum(estimate),
              summary_est = mean(summary_est)) %>%
    ungroup() %>%
    mutate(perc = estimate / summary_est * 100,
           label = "single-female-headed with young children") %>%
    select(GEOID, NAME, label, estimate, summary_est, perc)

  owner_occ_10 <- #DP04_0045
    get_acs("place",
            state = "MD",
            variables = "DP04_0045",
            summary_var = "DP04_0044",
            year = 2010) %>%
    mutate(perc = estimate / summary_est * 100) %>%
    left_join(load_variables(2010, "acs5/profile", T), by = c("variable" = "name")) %>%
    select(GEOID, NAME, label, estimate, summary_est, perc) %>%
    mutate(label = str_remove(label, ".*!"),
           NAME = str_extract(NAME, ".*,"))

  med_value_10 <- #DP04_0088
    get_acs("place",
            state = "MD",
            variables = "DP04_0088",
            year = 2010) %>%
    left_join(load_variables(2010, "acs5/profile", T), by = c("variable" = "name")) %>%
    select(GEOID, NAME, label, estimate) %>%
    mutate(label = str_remove(label, ".*!"),
           NAME = str_extract(NAME, ".*,"))

  med_hh_inc_10 <- #B19013_001
    get_acs("place",
            state = "MD",
            variables = "B19013_001",
            year = 2010) %>%
    left_join(load_variables(2010, "acs5", T), by = c("variable" = "name")) %>%
    select(GEOID, NAME, label, estimate) %>%
    mutate(label = str_remove(label, ".*!"),
           NAME = str_extract(NAME, ".*,"))
  bind_rows(med_hh_inc_10, int_inc_10, owner_occ_10, med_value_10, single_mother_w_young_10)%>% 
    mutate(adjusted_estimate = if_else(str_detect(label, "^Median"), estimate * 1.19, NA_real_),
           NAME = str_squish(str_remove(NAME, "CDP.*|city.*|County.*")),
           year = 2010)
  
  }
  else {
int_inc_10 <-
  get_acs("county",
          state = c("DC", "MD", "VA"),
          variables = "B19054_002",
          summary_var = "B19054_001",
          year = 2010) %>%
  mutate(perc = estimate / summary_est * 100) %>%
  left_join(load_variables(2010, "acs5", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate, summary_est, perc) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))

single_mother_w_young_10  <- get_acs(
  "county",
  state = c("DC", "MD", "VA"),
  variables = "B11004_016",
  summary_var = "B11004_001",
  year = 2010
) %>%
  left_join(load_variables(2010, "acs5", T), by = c("variable" = "name")) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,")) %>%
  group_by(GEOID, NAME) %>%
  summarise(estimate = sum(estimate),
            summary_est = mean(summary_est)) %>%
  ungroup() %>%
  mutate(perc = estimate / summary_est * 100,
         label = "single-female-headed with young children") %>%
  select(GEOID, NAME, label, estimate, summary_est, perc)

owner_occ_10 <- #DP04_0045
  get_acs("county",
          state = c("DC", "MD", "VA"),
          variables = "DP04_0045",
          summary_var = "DP04_0044",
          year = 2010) %>%
  mutate(perc = estimate / summary_est * 100) %>%
  left_join(load_variables(2010, "acs5/profile", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate, summary_est, perc) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))

med_value_10 <- #DP04_0088
  get_acs("county",
          state = c("DC", "MD", "VA"),
          variables = "DP04_0088",
          year = 2010) %>%
  left_join(load_variables(2010, "acs5/profile", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))

med_hh_inc_10 <- #B19013_001
  get_acs("county",
          state = c("DC", "MD", "VA"),
          variables = "B19013_001",
          year = 2010) %>%
  left_join(load_variables(2010, "acs5", T), by = c("variable" = "name")) %>%
  select(GEOID, NAME, label, estimate) %>%
  mutate(label = str_remove(label, ".*!"),
         NAME = str_extract(NAME, ".*,"))

all_counties <- bind_rows(med_hh_inc_10, int_inc_10, owner_occ_10, med_value_10, single_mother_w_young_10) %>%
  arrange(GEOID) %>% 
  mutate(adjusted_estimate = if_else(str_detect(label, "^Median"), estimate * 1.19, NA_real_),
         NAME = str_squish(str_remove(NAME, "CDP.*|city.*|County.*")),
         year = 2010)
return(all_counties)
}
  }

#w2_2010_dmv <-  get_worksheet2_2010("dmv")
# w2_2010_counties <- get_worksheet2_2010("counties")
# w2_2010_places <- get_worksheet2_2010("place")
# 
# write_csv(w2_2010_dmv, "Data/2010 Census - New Worksheet 2 - DMV.csv")
# write_csv(w2_2010_counties, "Data/2010 Census - New Worksheet 2 - Counties.csv")
# write_csv(w2_2010_places, "Data/2010 Census - New Worksheet 2 - Places.csv")

w2_2010_dmv <- read_csv("Data/2010 Census - New Worksheet 2 - DMV.csv")
w2_2010_counties <- read_csv( "Data/2010 Census - New Worksheet 2 - Counties.csv")
w2_2010_places <- read_csv("Data/2010 Census - New Worksheet 2 - Places.csv")
w2_2010_counties_and_places <- bind_rows(w2_2010_counties, w2_2010_places)

w2_2010_sectionB <- w2_2010_counties_and_places %>% 
  filter(GEOID %in% c("51510", "24027", "51059", "51153", "24031", "24033")) 
w2_2010_sectionC <- w2_2010_counties_and_places %>% 
  filter(GEOID %in% c("51510", "51600", "51610", "51685", "2402825", "2429525", "2445525")) 

rm(w2_2010_counties, w2_2010_places, w2_2010_counties_and_places)

bind_rows(w2_2010_dmv %>% filter(GEOID == "11001"), w2_2010_sectionC) %>% 
  mutate(reportable_value = if_else(is.na(perc), estimate, perc),
         year = 2010L) %>% 
  select(year, NAME, label, reportable_value, summary_est) 

