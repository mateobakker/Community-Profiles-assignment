library(tidyverse)
library(ndi)
library(tmap)
ttm()

### Modified the ndi::messer function to allow DC Metro analysis

vars <- c(PctMenMgmtBusScArti_num1 = "C24030_018", PctMenMgmtBusScArti_num2 = "C24030_019", 
          PctMenMgmtBusScArti_den = "C24030_002", PctCrwdHH_num1 = "B25014_005", 
          PctCrwdHH_num2 = "B25014_006", PctCrwdHH_num3 = "B25014_007", 
          PctCrwdHH_num4 = "B25014_011", PctCrwdHH_num5 = "B25014_012", 
          PctCrwdHH_num6 = "B25014_013", PctCrwdHH_den = "B25014_001", 
          PctHHPov_num = "B17017_002", PctHHPov_den = "B17017_001", 
          PctFemHeadKids_num1 = "B25115_012", PctFemHeadKids_num2 = "B25115_025", 
          PctFemHeadKids_den = "B25115_001", PctPubAsst_num = "B19058_002", 
          PctPubAsst_den = "B19058_001", PctHHUnder30K_num1 = "B19001_002", 
          PctHHUnder30K_num2 = "B19001_003", PctHHUnder30K_num3 = "B19001_004", 
          PctHHUnder30K_num4 = "B19001_005", PctHHUnder30K_num5 = "B19001_006", 
          PctHHUnder30K_den = "B19001_001", PctEducLessThanHS_num = "B06009_002", 
          PctEducLessThanHS_den = "B06009_001", PctUnemp_num = "B23025_005", 
          PctUnemp_den = "B23025_003")
dmv_counties <- tribble(
  ~county, ~state,
  "Arlington", "VA",
  "Alexandria", "VA",
  "Falls Church", "VA",
  "Fairfax city", "VA",
  "Fairfax County", "VA",
  "Loudoun County", "VA",
  "Manassas city", "VA",
  "Manassas Park city", "VA",
  "Prince William", "VA",
  "Montgomery", "MD",
  "Prince George", "MD",
  "Charles", "MD",
  "Howard", "MD",
  "District of Columbia", "DC"
) 
ndi_vars <- map2_dfr(dmv_counties$county,dmv_counties$state, ~tidycensus::get_acs(
  geography = "tract",
  county = .x,
  state = .y,
  year = 2020, 
  output = "wide", 
  variables = vars)) %>% 
  separate(NAME, into = c("tract","county", "state"), sep = ",") %>% 
  mutate(tract = gsub("[^0-9\\.]", "", tract))

ndi_vars <- ndi_vars %>% dplyr::mutate(OCC = (PctMenMgmtBusScArti_num1E + 
                                                PctMenMgmtBusScArti_num2E)/PctMenMgmtBusScArti_denE, 
                                       CWD = (PctCrwdHH_num1E + PctCrwdHH_num2E + PctCrwdHH_num3E + 
                                                PctCrwdHH_num4E + PctCrwdHH_num5E + PctCrwdHH_num6E)/PctCrwdHH_denE, 
                                       POV = PctHHPov_numE/PctHHPov_denE, FHH = (PctFemHeadKids_num1E + 
                                                                                   PctFemHeadKids_num2E)/PctFemHeadKids_denE, PUB = PctPubAsst_numE/PctPubAsst_denE, 
                                       U30 = (PctHHUnder30K_num1E + PctHHUnder30K_num2E + 
                                                PctHHUnder30K_num3E + PctHHUnder30K_num4E + PctHHUnder30K_num5E)/PctHHUnder30K_denE, 
                                       EDU = PctEducLessThanHS_numE/PctEducLessThanHS_denE, 
                                       EMP = PctUnemp_numE/PctUnemp_denE, county = stringr::str_trim(county))

ndi_vars_pca <- ndi_vars %>% dplyr::select(OCC, CWD, POV, 
                                           FHH, PUB, U30, EDU, EMP)
ndi_vars_pca <- do.call(data.frame, lapply(ndi_vars_pca, 
                                           function(x) replace(x, is.infinite(x), 0)))
pca <- psych::principal(ndi_vars_pca, nfactors = 1, n.obs = nrow(ndi_vars_pca), 
                        covar = FALSE, scores = TRUE, missing = F)
missingYN <- ndi_vars_pca %>% dplyr::select(OCC, CWD, POV, 
                                            FHH, PUB, U30, EDU, EMP) %>% tidyr::gather(key = "variable", 
                                                                                       value = "val") %>% dplyr::mutate(missing = is.na(val)) %>% 
  dplyr::group_by(variable) %>% dplyr::mutate(total = n()) %>% 
  dplyr::group_by(variable, total, missing) %>% dplyr::count() %>% 
  dplyr::mutate(percent = round(n/total * 100, 2), percent = paste0(percent, 
                                                                    " %")) %>% dplyr::filter(missing == TRUE)
if (quiet == FALSE) {
  if (nrow(missingYN) != 0) {
    message("Warning: Missing census data")
  }
  else {
    returnValue(missingYN)
  }
  if (pca$Vaccounted[2] < 0.5) {
    message("Warning: The proportion of variance explained by PC1 is less than 0.50.")
  }
}

NDIQuart <- data.frame(PC1 = pca$scores) %>% dplyr::mutate(NDI = PC1/pca$value[1]^2, 
                                                           NDIQuart = cut(NDI, breaks = stats::quantile(NDI, probs = c(0, 
                                                                                                                       0.25, 0.5, 0.75, 1), na.rm = TRUE), labels = c("1-Least deprivation", 
                                                                                                                                                                      "2-BelowAvg deprivation", "3-AboveAvg deprivation", 
                                                                                                                                                                      "4-Most deprivation"), include.lowest = TRUE), NDIQuart = factor(replace(as.character(NDIQuart), 
                                                                                                                                                                                                                                               is.na(NDIQuart), "9-NDI not avail"), c(levels(NDIQuart), 
                                                                                                                                                                                                                                                                                      "9-NDI not avail"))) %>% dplyr::select(NDI, NDIQuart)
ndi <- cbind(ndi_vars, NDIQuart) %>% dplyr::mutate(OCC = round(OCC, 
                                                               digits = 3), CWD = round(CWD, digits = 3), POV = round(POV, 
                                                                                                                      digits = 3), FHH = round(FHH, digits = 3), PUB = round(PUB, 
                                                                                                                                                                             
                                                                                                                                                                             digits = 3), U30 = round(U30, digits = 3), EDU = round(EDU, 
                                                                                                                                                                                                                                    digits = 3), EMP = round(EMP, digits = 3), NDI = round(NDI, 
                                                                                                                                                                                                                                                                                           digits = 4)) %>% 
  as_tibble() %>% 
  select(GEOID, state, county, tract, 
         NDI, NDIQuart, OCC, CWD, POV, FHH, PUB, U30, EDU, 
         EMP)

#quiet <- FALSE
#load_variables(2020, "acs5", T)

dmv_tracts_geo <- 
  map2_dfr(dmv_counties$county,dmv_counties$state, ~tidycensus::get_acs(
    geography = "tract",
    county = .x,
    state = .y,
    #year = 2020, 
    variables = "B01001_001",
    geometry = T)) %>% 
  select(GEOID, geometry)

dmv_counties_geo <- 
  map2_dfr(dmv_counties$county,dmv_counties$state, ~tidycensus::get_acs(
    geography = "county",
    county = .x,
    state = .y,
    #year = 2020, 
    variables = "B01001_001",
    geometry = T)) %>% 
  select(GEOID, geometry)


ndi_geo <- ndi %>% 
  filter(NDIQuart != "9-NDI not avail") %>% 
  rename(NDI_original = NDI) %>% 
  mutate(NDI = factor(NDIQuart, labels = c("1-Least deprivation", "2-BelowAvg deprivation", "3-AboveAvg deprivation", "4-Most deprivation"))) %>% 
  left_join(dmv_tracts_geo) %>% 
  sf::st_sf() 
?as.factor

ndi_map <- tm_shape(ndi_geo) +
  tm_fill("NDI", 
          palette = "plasma", 
          alpha = .4,
          popup.vars = c("neighborhood deprivation index score" = "NDI_original",
                         "neighborhood deprivation index category" = "NDI",
                         "proportion of men in management, science, and arts occupation" = "OCC", 
                         "proportion of overcrowded housing" = "CWD", 
                         "proportion of households in poverty" = "POV", 
                         "proportion of female-headed households with dependents" = "FHH", 
                         "proportion of households receiving public assistance" = "PUB", 
                         "proportion of household earning less than $30k/year" = "U30", 
                         "proportion of individuals with less than HS education" = "EDU", 
                         "proportion of individuals unemployed" = "EMP")) +
  tm_basemap("OpenStreetMap") +
  tm_shape(dmv_counties_geo) +
  tm_borders(col = "black") +
  tm_view(set.view = 9)

ndi_map
tmap_save(ndi_map, "Data/NDI map.html")
?tm_fill
