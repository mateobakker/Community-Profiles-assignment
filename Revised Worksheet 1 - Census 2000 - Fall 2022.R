library(tidyverse)
library(tidycensus)
 vars00 <- load_variables(2000, "sf3", T)

## Places
 # all_race <- get_decennial("place",
 #                           state =  "MD",
 #                           year = 2000,
 #                           variable = c("P007003", "P007004", "P007006", "P007009", "P007010"),
 #                           summary_var = "P007001",
 #                           sumfile = "sf3") %>%
 #   left_join(vars00, by = c("variable" = "name")) %>%
 #   mutate(label = str_remove(label, ".*!"),
 #          perc = value / summary_value * 100) %>%
 #   rename(total = summary_value) %>%
 #   select(GEOID, NAME, label, value, total, perc)
 # 
 # all_cit <- get_decennial("place",
 #                          state =  "MD",
 #                          year = 2000,
 #                          variables = c("P021002", "P021014", "P021015"),
 #                          summary_var = "P021001",
 #                          sumfile = "sf3") %>%
 #   left_join(vars00, by = c("variable" = "name")) %>%
 #   mutate(label = str_remove(label, ".*!")) %>%
 #   select(GEOID, NAME, label, value, total = summary_value) %>%
 #   mutate(perc = value / total * 100)
 # 
 # all_occ <- get_decennial("place",
 #                          state = "MD",
 #                          year = 2000,
 #                          variables = c("P050003", "P050050"),
 #                          summary_var = "P050001",
 #                          sumfile = "sf3") %>%
 #   left_join(vars00, by = c("variable" = "name")) %>%
 #   mutate(label = str_remove(label, ".*!")) %>%
 #   group_by(GEOID, NAME, label) %>%
 #   summarise(value = sum(value),
 #             total = mean(summary_value)) %>%
 #   mutate(perc = value / total * 100)
 # 
 # all_ed <- get_decennial("place",
 #                         state = "MD",
 #                         year = 2000,
 #                         variables =  c("P037011", "P037016", "P037017", "P037018","P037028", "P037033", "P037034", "P037035"),
 #                         summary_var = "P037001",
 #                         sumfile = "sf3") %>%
 #   left_join(vars00, by = c("variable" = "name")) %>%
 #   mutate(label = str_remove(label, ".*!")) %>%
 #   select(GEOID, NAME, label, value, summary_value) %>%
 #   filter(str_detect(label, "High|Master|Profess|Docto")) %>%
 #   mutate(label = if_else(str_detect(label, "High school"), "High school graduate", "Professional or graduate degree recipient")) %>%
 #   group_by(GEOID, NAME, label) %>%
 #   summarise(value = sum(value),
 #             total = mean(summary_value)) %>%
 #   mutate(perc = value / total * 100)
 # ## unemployed
 # all_unemp <- get_decennial("place",
 #               state = "MD",
 #               table = "P043",
 #               year = 2000,
 #               sumfile = "sf3") %>%
 #   left_join(vars00, by = c("variable" = "name")) %>%
 #   mutate(label = str_remove(label, ".*!")) %>%
 #   group_by(GEOID, NAME, label) %>%
 #   summarise(value = sum(value)) %>%
 #   filter(label %in% c("Employed", "Unemployed", "In labor force"))
 # worksheet1_2000_places <- bind_rows(all_race, all_cit, all_ed, all_occ, all_unemp)
 # rm(all_race, all_cit, all_ed, all_occ, all_unemp)
## Counties
# vars00 %>% filter(str_detect(name, "P0430")) %>% view
# all_race <- get_decennial("county",
#                           state =  c("DC", "MD", "VA"),
#                           year = 2000,
#                           variable = c("P007003", "P007004", "P007006", "P007009", "P007010"),
#                           summary_var = "P007001",
#                           sumfile = "sf3") %>%
#   left_join(vars00, by = c("variable" = "name")) %>%
#   mutate(label = str_remove(label, ".*!"),
#          perc = value / summary_value * 100) %>%
#   rename(total = summary_value) %>%
#   select(GEOID, NAME, label, value, total, perc)
# 
# all_cit <- get_decennial("county",
#                          state =  c("DC", "MD", "VA"),
#                          year = 2000,
#                          variables = c("P021002", "P021014", "P021015"),
#                          summary_var = "P021001",
#                          sumfile = "sf3") %>%
#   left_join(vars00, by = c("variable" = "name")) %>%
#   mutate(label = str_remove(label, ".*!")) %>%
#   select(GEOID, NAME, label, value, total = summary_value) %>%
#   mutate(perc = value / total * 100)
# 
# all_occ <- get_decennial("county",
#                          state =  c("DC", "MD", "VA"),
#                          year = 2000,
#                          variables = c("P050003", "P050050"),
#                          summary_var = "P050001",
#                          sumfile = "sf3") %>%
#   left_join(vars00, by = c("variable" = "name")) %>%
#   mutate(label = str_remove(label, ".*!")) %>%
#   group_by(GEOID, NAME, label) %>%
#   summarise(value = sum(value),
#             total = mean(summary_value)) %>%
#   mutate(perc = value / total * 100)
# 
# all_ed <- get_decennial("county",
#                         state = c("DC", "MD", "VA"),
#                         year = 2000,
#                         variables =  c("P037011", "P037016", "P037017", "P037018","P037028", "P037033", "P037034", "P037035"),
#                         summary_var = "P037001",
#                         sumfile = "sf3") %>%
#   left_join(vars00, by = c("variable" = "name")) %>%
#   mutate(label = str_remove(label, ".*!")) %>%
#   select(GEOID, NAME, label, value, summary_value) %>%
#   filter(str_detect(label, "High|Master|Profess|Docto")) %>%
#   mutate(label = if_else(str_detect(label, "High school"), "High school graduate", "Professional or graduate degree recipient")) %>%
#   group_by(GEOID, NAME, label) %>%
#   summarise(value = sum(value),
#             total = mean(summary_value)) %>%
#   mutate(perc = value / total * 100)
# ## unemployed
# all_unemp <- get_decennial("county",
#               state = c("DC", "MD", "VA"),
#               table = "P043",
#               year = 2000,
#               sumfile = "sf3") %>%
#   left_join(vars00, by = c("variable" = "name")) %>%
#   mutate(label = str_remove(label, ".*!")) %>%
#   group_by(GEOID, NAME, label) %>%
#   summarise(value = sum(value)) %>%
#   filter(label %in% c("Employed", "Unemployed", "In labor force"))
# worksheet1_2000_counties <- bind_rows(all_race, all_cit, all_ed, all_occ, all_unemp)
# rm(all_race, all_cit, all_ed, all_occ, all_unemp)
# 
## DMV
# ## race
# variables = c("P007003", "P007004", "P007006", "P007009", "P007010")
# summary_var = "P007001"
# variables2 = str_c(variables, collapse = ",")
# cens_vars = str_c(summary_var, variables2, sep = ",")
# url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={cens_vars},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
# all_race <- jsonlite::fromJSON(url) %>%
#   as_tibble() %>%
#   set_names(.[1,]) %>%
#   slice(-1) %>%
#   rename(summary_val = {{summary_var}}) %>%
#   pivot_longer(variables) %>%
#   left_join(vars00) %>%
#   mutate(label = str_remove(label, ".*!")) %>%
#   select(GEOID = 4, NAME, label, value, summary_val) %>%
#   mutate(across(value:summary_val, parse_number)) %>%
#   rename(total = summary_val) %>%
#   select(GEOID, NAME, label, value, total) %>%
#   mutate(perc = value / total * 100) %>%
#   filter(GEOID == "8840")
# 
# # cit
# variables = c("P021002", "P021014", "P021015")
# summary_var = "P021001"
# variables2 = str_c(variables, collapse = ",")
# cens_vars = str_c(summary_var, variables2, sep = ",")
# url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={cens_vars},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
# all_cit <- jsonlite::fromJSON(url) %>%
#   as_tibble() %>%
#   set_names(.[1,]) %>%
#   slice(-1) %>%
#   rename(summary_val = {{summary_var}}) %>%
#   pivot_longer(variables) %>%
#   left_join(vars00) %>%
#   mutate(label = str_remove(label, ".*!")) %>%
#   select(GEOID = 4, NAME, label, value, summary_val) %>%
#   mutate(across(value:summary_val, parse_number)) %>%
#   rename(total = summary_val) %>%
#   select(GEOID, NAME, label, value, total) %>%
#   mutate(perc = value / total * 100) %>%
#   filter(GEOID == "8840")
# 
# ## ed
# variables =  c("P037011", "P037016", "P037017", "P037018","P037028", "P037033", "P037034", "P037035")
# summary_var = "P037001"
# variables2 = str_c(variables, collapse = ",")
# cens_vars = str_c(summary_var, variables2, sep = ",")
# url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={cens_vars},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
# all_ed <- jsonlite::fromJSON(url) %>%
#   as_tibble() %>%
#   set_names(.[1,]) %>%
#   slice(-1) %>%
#   rename(summary_val = {{summary_var}}) %>%
#   pivot_longer(variables) %>%
#   left_join(vars00) %>%
#   mutate(label = str_remove(label, ".*!")) %>%
#   select(GEOID = 4, NAME, label, value, summary_val) %>%
#   mutate(across(value:summary_val, parse_number)) %>%
#   rename(total = summary_val) %>%
#   select(GEOID, NAME, label, value, total) %>%
#   mutate(perc = value / total * 100,
#          label = if_else(str_detect(label, "High school"), "High school graduate", "Professional or graduate degree recipient")) %>%
#   filter(GEOID == "8840") %>%
#   group_by(GEOID, NAME, label) %>%
#   summarise(value = sum(value),
#             total = mean(total)) %>%
#   mutate(perc = value / total * 100)
# 
# ## occ
# variables = c("P050003", "P050050")
# summary_var = "P050001"
# variables2 = str_c(variables, collapse = ",")
# cens_vars = str_c(summary_var, variables2, sep = ",")
# url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={cens_vars},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
# all_occ <- jsonlite::fromJSON(url) %>%
#   as_tibble() %>%
#   set_names(.[1,]) %>%
#   slice(-1) %>%
#   rename(summary_val = {{summary_var}}) %>%
#   pivot_longer(variables) %>%
#   left_join(vars00) %>%
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
#   mutate(perc = value / total * 100)
# ## unemployed
# variables = c("P043003", "P043007","P043010", "P043014")
# cens_vars = str_c(variables, collapse = ",")
# url <- glue::glue("https://api.census.gov/data/2000/dec/sf3?get={cens_vars},NAME&for=primary%20metropolitan%20statistical%20area:*&in=consolidated%20metropolitan%20statistical%20area:8872")
# all_unemp <- jsonlite::fromJSON(url) %>%
#   as_tibble() %>%
#   set_names(.[1,]) %>%
#   slice(-1) %>%
#   pivot_longer({{variables}}) %>%
#   left_join(vars00) %>%
#   mutate(label = str_remove(label, ".*!")) %>%
#   rename(GEOID = 3) %>%
#   group_by(GEOID, NAME, label) %>%
#   summarize(value = sum(parse_number(value) )) %>%
#   mutate(total = value[label == "In labor force"],
#          perc = value / total * 100) %>%
#   filter(label == "Unemployed",
#          GEOID == "8840")
# worksheet1_2000_dmv <- bind_rows(all_race, all_cit, all_ed, all_occ, all_unemp) %>%
#   separate(NAME, c("NAME", "drop"), sep = ";") %>%
#   select(-drop)
# rm(all_race, all_cit, all_ed, all_occ)
# 
# worksheet1_2000_dmv %>%
#   write_csv("Data/2000 Census - New Worksheet 1 - DMV - Fall 2022.csv")
# worksheet1_2000_counties %>%
#   write_csv("Data/2000 Census - New Worksheet 1 - Counties - Fall 2022.csv")

w1_2000_places <- worksheet1_2000_places %>% 
  filter(str_detect(NAME, "^Aspen|^Langley|^Fort Washington"))
w1_2000_counties <- read_csv("Data/2000 Census - New Worksheet 1 - Counties - Fall 2022.csv")
w1_2000_dmv <- read_csv("Data/2000 Census - New Worksheet 1 - DMV - Fall 2022.csv")

w1_soc131_c_2000 <- 
  w1_2000_places %>% 
  bind_rows(w1_2000_counties %>% 
              filter(str_detect(NAME, "Alexandria|Fairfax city|Falls Church|Manassas Park")) %>% 
              mutate(GEOID = as.character(GEOID))) %>% 
  arrange(GEOID)
# soc131_b_counties_fa22 ## created in the 1990 version

w1_2000_counties %>% 
  arrange(GEOID) %>% 
  filter(GEOID %in% soc131_b_counties_fa22$GEOID) %>% 
  arrange(GEOID) %>% 
  view
