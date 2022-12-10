library(tidyverse)
# get_worksheet1_2020 <- function(unit) {
#   
#   if(!unit %in% c("dmv", "counties")) {stop("must be either 'dmv' or 'counties'")}
#   
#   if(unit == "dmv") 
#   {race_20 <- get_acs("metropolitan statistical area/micropolitan statistical area",
#                       variables = c("B03002_003", "B03002_004", "B03002_006", "B03002_009", "B03002_012"),
#                       summary_var = "B03002_001",
#                       year = 2020) %>%
#     mutate(perc = estimate / summary_est * 100) %>%
#     filter(GEOID == "47900") %>%
#     left_join(vars20, by = c("variable" = "name")) %>%
#     select(GEOID, NAME, label, estimate, summary_est, perc) %>%
#     mutate(label = str_remove(label, ".*!"),
#            NAME = str_extract(NAME, ".*,"))
#   
#   cit_20 <- #"B05001_002", "B05001_005", "B05001_006"
#     get_acs("metropolitan statistical area/micropolitan statistical area",
#             variables = c("B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006"),
#             summary_var = "B05001_001",
#             year = 2020) %>%
#     left_join(vars20, by = c("variable" = "name")) %>%
#     mutate(label = if_else(str_detect(label, "born"), "Native-born U.S. citizen", label)) %>%
#     filter(GEOID == "47900") %>%
#     group_by(GEOID, NAME, label) %>%
#     summarise(estimate = sum(estimate),
#               summary_est = mean(summary_est)) %>% 
#     mutate(perc = estimate / summary_est * 100) %>% 
#     mutate(label = str_remove(label, ".*!"),
#            NAME = str_extract(NAME, ".*,")) 
#   
#   ed_20 <- #"B06009_003", "B06009_006"
#     get_acs("metropolitan statistical area/micropolitan statistical area",
#             variables = c("B06009_003", "B06009_006"),
#             summary_var = "B06009_001",
#             year = 2020) %>%
#     left_join(vars20, by = c("variable" = "name")) %>% 
#     mutate(perc = estimate / summary_est *100) %>% 
#     filter(GEOID == "47900") %>%
#     mutate(label = str_remove(label, ".*!"),
#            NAME = str_extract(NAME, ".*,")) %>% 
#     select(GEOID, NAME, label, estimate, summary_est, perc)
#   
#   unemp_20 <- #"DP03_0009"
#     get_acs("metropolitan statistical area/micropolitan statistical area",
#             variables = "DP03_0005",
#             summary_var = "DP03_0002",
#             year = 2020) %>%
#     left_join(vars20_prof, by = c("variable" = "name")) %>% 
#     mutate(perc = estimate / summary_est *100) %>% 
#     filter(GEOID == "47900") %>%
#     mutate(label = str_remove(label, ".*!"),
#            NAME = str_extract(NAME, ".*,")) %>% 
#     select(GEOID, NAME, label, estimate, summary_est, perc)
#   
#   occ_20 <- #"DP03_0027"
#     get_acs("metropolitan statistical area/micropolitan statistical area",
#             variables = "DP03_0027",
#             summary_var = "DP03_0026",
#             year = 2020) %>%
#     left_join(vars20_prof, by = c("variable" = "name")) %>% 
#     mutate(perc = estimate / summary_est *100) %>% 
#     filter(GEOID == "47900") %>%
#     mutate(label = str_remove(label, ".*!"),
#            NAME = str_extract(NAME, ".*,")) %>% 
#     select(GEOID, NAME, label, estimate, summary_est, perc)
#   
#   bind_rows(race_20, cit_20, ed_20, occ_20, unemp_20)}
#   else {
#     race_20 <- get_acs("county",
#                        state = c("DC", "MD", "VA"),variables = c("B03002_003", "B03002_004", "B03002_006", "B03002_009", "B03002_012"),
#                        summary_var = "B03002_001",
#                        year = 2020) %>%
#       mutate(perc = estimate / summary_est * 100) %>%
#       left_join(vars20, by = c("variable" = "name")) %>%
#       select(GEOID, NAME, label, estimate, summary_est, perc) %>%
#       mutate(label = str_remove(label, ".*!"),
#              NAME = str_extract(NAME, ".*,"))
#     
#     cit_20 <- #"B05001_002", "B05001_005", "B05001_006"
#       get_acs("county",
#               state = c("DC", "MD", "VA"),
#               variables = c("B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006"),
#               summary_var = "B05001_001",
#               year = 2020) %>%
#       left_join(vars20, by = c("variable" = "name")) %>%
#       mutate(label = if_else(str_detect(label, "born"), "Native-born U.S. citizen", label)) %>%
#       group_by(GEOID, NAME, label) %>%
#       summarise(estimate = sum(estimate),
#                 summary_est = mean(summary_est)) %>% 
#       mutate(perc = estimate / summary_est * 100) %>% 
#       mutate(label = str_remove(label, ".*!"),
#              NAME = str_extract(NAME, ".*,")) 
#     
#     ed_20 <- #"B06009_003", "B06009_006"
#       get_acs("county",
#               state = c("DC", "MD", "VA"),
#               variables = c("B06009_003", "B06009_006"),
#               summary_var = "B06009_001",
#               year = 2020) %>%
#       left_join(vars20, by = c("variable" = "name")) %>% 
#       mutate(perc = estimate / summary_est *100) %>% 
#       mutate(label = str_remove(label, ".*!"),
#              NAME = str_extract(NAME, ".*,")) %>% 
#       select(GEOID, NAME, label, estimate, summary_est, perc)
#     
#     unemp_20 <- #"DP03_0009"
#       get_acs("county",
#               state = c("DC", "MD", "VA"),
#               variables = "DP03_0005",
#               summary_var = "DP03_0002",
#               year = 2020) %>%
#       left_join(vars20_prof, by = c("variable" = "name")) %>% 
#       mutate(perc = estimate / summary_est *100) %>% 
#       mutate(label = str_remove(label, ".*!"),
#              NAME = str_extract(NAME, ".*,")) %>% 
#       select(GEOID, NAME, label, estimate, summary_est, perc)
#     
#     occ_20 <- #"DP03_0027"
#       get_acs("county",
#               state = c("DC", "MD", "VA"),
#               variables = "DP03_0027",
#               summary_var = "DP03_0026",
#               year = 2020) %>%
#       left_join(vars20_prof, by = c("variable" = "name")) %>% 
#       mutate(perc = estimate / summary_est *100) %>% 
#       mutate(label = str_remove(label, ".*!"),
#              NAME = str_extract(NAME, ".*,")) %>% 
#       select(GEOID, NAME, label, estimate, summary_est, perc)
#     
#     bind_rows(race_20, cit_20, ed_20, occ_20, unemp_20) %>% 
#       arrange(GEOID)
#   }
#   
# }
# 
# w1_2020_dmv <- get_worksheet1_2020("dmv") %>% 
#   write_csv("Data/2020 Census - New Worksheet 1 - DMV.csv")
# 
# w1_2020_counties <- get_worksheet1_2020("counties") %>% 
#   write_csv("Data/2020 Census - New Worksheet 1 - Counties.csv")

w1_2020_dmv <- read_csv("Data/2020 Census - New Worksheet 1 - DMV.csv")
w1_2020_counties <- read_csv("Data/2020 Census - New Worksheet 1 - Counties.csv")
