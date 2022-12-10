library(tidyverse)
# get_worksheet1_2010 <- function(unit) {
#   
#   if(!unit %in% c("dmv", "counties")) {stop("must be either 'dmv' or 'counties'")}
#   
#   if(unit == "dmv") 
#   {race_10 <- get_acs("metropolitan statistical area/micropolitan statistical area",
#         variables = c("B03002_003", "B03002_004", "B03002_006", "B03002_009", "B03002_012"),
#         summary_var = "B03002_001",
#         year = 2010) %>%
#   mutate(perc = estimate / summary_est * 100) %>%
#   filter(GEOID == "47900") %>%
#   left_join(vars10, by = c("variable" = "name")) %>%
#   select(GEOID, NAME, label, estimate, summary_est, perc) %>%
#   mutate(label = str_remove(label, ".*!"),
#          NAME = str_extract(NAME, ".*,"))
# 
# cit_10 <- #"B05001_002", "B05001_005", "B05001_006"
#   get_acs("metropolitan statistical area/micropolitan statistical area",
#           variables = c("B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006"),
#           summary_var = "B05001_001",
#           year = 2010) %>%
#   left_join(vars10, by = c("variable" = "name")) %>%
#   mutate(label = if_else(str_detect(label, "born"), "Native-born U.S. citizen", label)) %>%
#   filter(GEOID == "47900") %>%
#   group_by(GEOID, NAME, label) %>%
#   summarise(estimate = sum(estimate),
#             summary_est = mean(summary_est)) %>% 
#   mutate(perc = estimate / summary_est * 100) %>% 
#   mutate(label = str_remove(label, ".*!"),
#          NAME = str_extract(NAME, ".*,")) 
# 
# ed_10 <- #"B06009_003", "B06009_006"
#   get_acs("metropolitan statistical area/micropolitan statistical area",
#           variables = c("B06009_003", "B06009_006"),
#           summary_var = "B06009_001",
#           year = 2010) %>%
#   left_join(vars10, by = c("variable" = "name")) %>% 
#   mutate(perc = estimate / summary_est *100) %>% 
#   filter(GEOID == "47900") %>%
#   mutate(label = str_remove(label, ".*!"),
#          NAME = str_extract(NAME, ".*,")) %>% 
#   select(GEOID, NAME, label, estimate, summary_est, perc)
# 
# unemp_10 <- #"DP03_0009"
#   get_acs("metropolitan statistical area/micropolitan statistical area",
#           variables = "DP03_0005",
#           summary_var = "DP03_0002",
#           year = 2010) %>%
#   left_join(vars10_prof, by = c("variable" = "name")) %>% 
#   mutate(perc = estimate / summary_est *100) %>% 
#   filter(GEOID == "47900") %>%
#   mutate(label = str_remove(label, ".*!"),
#          NAME = str_extract(NAME, ".*,")) %>% 
#   select(GEOID, NAME, label, estimate, summary_est, perc)
# 
# occ_10 <- #"DP03_0027"
#   get_acs("metropolitan statistical area/micropolitan statistical area",
#           variables = "DP03_0027",
#           summary_var = "DP03_0026",
#           year = 2010) %>%
#   left_join(vars10_prof, by = c("variable" = "name")) %>% 
#   mutate(perc = estimate / summary_est *100) %>% 
#   filter(GEOID == "47900") %>%
#   mutate(label = str_remove(label, ".*!"),
#          NAME = str_extract(NAME, ".*,")) %>% 
#   select(GEOID, NAME, label, estimate, summary_est, perc)
# 
# bind_rows(race_10, cit_10, ed_10, occ_10, unemp_10)}
#   else {
#     race_10 <- get_acs("county",
#                        state = c("DC", "MD", "VA"),variables = c("B03002_003", "B03002_004", "B03002_006", "B03002_009", "B03002_012"),
#                        summary_var = "B03002_001",
#                        year = 2010) %>%
#       mutate(perc = estimate / summary_est * 100) %>%
#       left_join(vars10, by = c("variable" = "name")) %>%
#       select(GEOID, NAME, label, estimate, summary_est, perc) %>%
#       mutate(label = str_remove(label, ".*!"),
#              NAME = str_extract(NAME, ".*,"))
#     
#     cit_10 <- #"B05001_002", "B05001_005", "B05001_006"
#       get_acs("county",
#               state = c("DC", "MD", "VA"),
#               variables = c("B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006"),
#               summary_var = "B05001_001",
#               year = 2010) %>%
#       left_join(vars10, by = c("variable" = "name")) %>%
#       mutate(label = if_else(str_detect(label, "born"), "Native-born U.S. citizen", label)) %>%
#       group_by(GEOID, NAME, label) %>%
#       summarise(estimate = sum(estimate),
#                 summary_est = mean(summary_est)) %>% 
#       mutate(perc = estimate / summary_est * 100) %>% 
#       mutate(label = str_remove(label, ".*!"),
#              NAME = str_extract(NAME, ".*,")) 
#     
#     ed_10 <- #"B06009_003", "B06009_006"
#       get_acs("county",
#               state = c("DC", "MD", "VA"),
#               variables = c("B06009_003", "B06009_006"),
#               summary_var = "B06009_001",
#               year = 2010) %>%
#       left_join(vars10, by = c("variable" = "name")) %>% 
#       mutate(perc = estimate / summary_est *100) %>% 
#       mutate(label = str_remove(label, ".*!"),
#              NAME = str_extract(NAME, ".*,")) %>% 
#       select(GEOID, NAME, label, estimate, summary_est, perc)
#     
#     unemp_10 <- #"DP03_0009"
#       get_acs("county",
#               state = c("DC", "MD", "VA"),
#               variables = "DP03_0005",
#               summary_var = "DP03_0002",
#               year = 2010) %>%
#       left_join(vars10_prof, by = c("variable" = "name")) %>% 
#       mutate(perc = estimate / summary_est *100) %>% 
#       mutate(label = str_remove(label, ".*!"),
#              NAME = str_extract(NAME, ".*,")) %>% 
#       select(GEOID, NAME, label, estimate, summary_est, perc)
#     
#     occ_10 <- #"DP03_0027"
#       get_acs("county",
#               state = c("DC", "MD", "VA"),
#               variables = "DP03_0027",
#               summary_var = "DP03_0026",
#               year = 2010) %>%
#       left_join(vars10_prof, by = c("variable" = "name")) %>% 
#       mutate(perc = estimate / summary_est *100) %>% 
#       mutate(label = str_remove(label, ".*!"),
#              NAME = str_extract(NAME, ".*,")) %>% 
#       select(GEOID, NAME, label, estimate, summary_est, perc)
#     
#     bind_rows(race_10, cit_10, ed_10, occ_10, unemp_10) %>% 
#       arrange(GEOID)
#   }
#   
# }
# 
# w1_2010_dmv <- get_worksheet1_2010("dmv") %>% 
#   write_csv("Data/2010 Census - New Worksheet 1 - DMV.csv")
# w1_2010_counties <- get_worksheet1_2010("counties") %>% 
#   write_csv("Data/2010 Census - New Worksheet 1 - Counties.csv")

w1_2010_dmv <- read_csv("Data/2010 Census - New Worksheet 1 - DMV.csv")
w1_2010_counties <- read_csv("Data/2010 Census - New Worksheet 1 - Counties.csv")

