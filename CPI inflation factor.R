library(tidyverse)
library(readxl)

temp <- tempfile()
download.file("https://www.bls.gov/cpi/research-series/r-cpi-u-rs-allitems.xlsx", temp)
# Formula for calculating inflation example: $1.00 * (1980 CPI/ 2014 CPI) = 1980 price
cpi <- readxl::read_excel(temp, skip = 5) %>% 
  select(year = YEAR, avg = AVG) %>% 
  filter(year %in% c(1990, 2000, 2010, 2020)) %>% 
  mutate(base = avg[year == 2020],
         inflation_factor = base/avg) %>% 
  select(year, inflation_factor) 