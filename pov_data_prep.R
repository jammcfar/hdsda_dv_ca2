### Title: Povcar data prep
### Author: x19175329@student.ncirl.ie
### Desc: r file for creating the pov data

library(tidyverse)
library(povcalnetR)
library(wbstats)

## import poverty data
pov_dat <- povcalnet(
  fill_gaps = T,
  year = c(1990:2018)
)
povcal_extra <- povcalnet_info() %>% as_tibble()
pov_extra_trim <- povcal_extra %>% select(country_code, wb_region) ## can also get more detailed region

pov_dat <-
  pov_dat %>%
  left_join(pov_extra_trim, by = c(countrycode = "country_code"))

## and remove some stuff
pov_dat <- pov_dat %>%
  select(
    -isinterpolated,
    -usemicrodata,
    -povertyline, ## might need to undo this at some stage
    -mean,
    -mld,
    -polarization, ## dunno what this is
    -contains("decile"),
    -datayear,
    -datatype,
    -regioncode,
    -coveragetype,
    -povertygapsq
  ) %>%
  rename(purchase_power_parity = ppp)

## code ripped from https://github.com/nset-ornl/wbstats
my_indicators <- c(
  life_exp = "SP.DYN.LE00.IN",
  gdp_capita = "NY.GDP.PCAP.CD",
  pop = "SP.POP.TOTL"
)

wb_dat <- wb_data(my_indicators, start_date = 1990, end_date = 2018)

## changed left join to right join
dat_j <-
  pov_dat %>%
  right_join(wb_dat, by = c("countrycode" = "iso3c", "year" = "date"))

## get coarse map data
rwm_low <- fortify(rworldmap::countriesCoarseLessIslands) %>% as_tibble()

rwm_names <- rworldmap::countrySynonyms %>%
  as_tibble() %>%
  pivot_longer(name1:name8) %>%
  filter(value != "")

rwm_tidy <-
  rwm_low %>%
  left_join(rwm_names, by = c("id" = "value")) %>%
  mutate(ISO3 = str_to_upper(ISO3)) %>%
  select(long, lat, id, group, ISO3)

dat_map <-
  rwm_tidy %>%
  left_join(dat_j, by = c("ISO3" = "countrycode")) %>%
  filter(id != "Antarctica")

dat_map_rounded <-
  dat_map %>%
  select(-country, -iso2c, -pop, -countryname, -median) %>%
  mutate(
    headcount = round(headcount, 4),
    gini = round(gini, 3),
    watts = round(watts, 3),
    life_exp = round(life_exp, 3),
    purchase_power_parity = round(purchase_power_parity, 3),
    ## median = round(median, 3),
    gdp_capita = round(gdp_capita, 3),
    povertygap = round(povertygap, 3)
  )

dat_map_rounded %>%
  group_by(id, year) %>%
  slice_head(n = 10) %>%
  filter(is.na(wb_region)) %>%
  ungroup() %>%
  select(id) %>%
  unique() %>%
  as_vector()

# fix the missing data problems
dat_map_named <-
  dat_map_rounded %>%
  rename(
    Country = id,
    "Purchase\npower parity" = purchase_power_parity,
    "% below\npoverty line" = headcount,
    "Mean distance\nbelow poverty line" = povertygap,
    "Watt's index" = watts,
    "Gini index" = gini,
    "Population (MM)" = population,
    "Region" = wb_region,
    "GDP per\ncapita" = gdp_capita,
    "Life\nexpectancy" = life_exp
  )

write_csv(dat_map_rounded, "test_data_small.csv")

## experiments

map_isos <- sort(unique(rwm_tidy$ISO3))
wb_isos <- sort(unique(dat_j$countrycode))

wb_miss <- setdiff(map_isos, wb_isos) ## what is in map thats not in wb
map_miss <- setdiff(wb_isos, map_isos)

rwm_tidy %>%
  filter(ISO3 %in% wb_miss) %>%
  select(id, ISO3) %>%
  unique() %>%
  arrange(id)

dat_j %>%
  filter(countrycode %in% map_miss) %>%
  select(countryname, countrycode) %>%
  unique() %>%
  arrange(countrycode)

library(countrycode)

codelist
countrycode(
  sourcevar = wb_isos,
  origin = "wb",
  destination = "iso3c"
)

countrycode(
  sourcevar = pov_extra_trim$country_code,
  origin = "wb",
  destination = "iso3c"
)

guess_field(pov_extra_trim$country_code, min_similarity = 80)

guess_field(rwm_tidy$ISO3, min_similarity = 80)

guess_field(dat_j$countrycode, min_similarity = 80)
