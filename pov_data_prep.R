### Title: Povcar data prep
### Author: x19175329@student.ncirl.ie
### Desc: r file for creating the pov data


# convenience function to install the packages if they are not installed
package_installer <- function(x) {
  # find packages from vector which are not installed and save them
  missing_pkg <- which(!package_list %in% installed.packages()[, 1])
  # if there are any missing ones then install them, else print a message
  if (length(missing_pkg) > 0) {
    install.packages(package_list[missing_pkg])
  } else {
    print("All packages already installed!")
  }
}

# vector of required package names, then load
package_list <- c(
  "tidyverse",
  "povcalnetR",
  "wbstats",
  "countrycode",
  "rworldmap",
  "ggpubr"
)

lapply(package_list, library, character.only = T)

## import poverty data
pov_dat <- povcalnet(
  fill_gaps = T,
  year = c(1990:2018)
)

## duplicates for covertype exist. Prioritise N and A.
pov_dat_slice <-
  pov_dat %>%
  group_by(countrycode, year) %>%
  arrange(countrycode, year, coveragetype) %>%
  slice_head(n = 1)

## get more detailed region
povcal_extra <- povcalnet_info() %>% as_tibble()
pov_extra_trim <- povcal_extra %>%
  select(country_code, wb_region) %>%
  unique()

pov_dat_se <-
  pov_dat_slice %>%
  left_join(pov_extra_trim, by = c(countrycode = "country_code"))

## and remove some stuff
pov_dat_se <- pov_dat_se %>%
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

## code help from https://github.com/nset-ornl/wbstats
my_indicators <- c(
  life_exp = "SP.DYN.LE00.IN",
  gdp_capita = "NY.GDP.PCAP.CD",
  pop = "SP.POP.TOTL"
)

wb_dat <- wb_data(my_indicators, start_date = 1990, end_date = 2018)

## convert to iso3 on both datasets and repair things
pov_dat_se$countrycode <-
  countrycode(
    sourcevar = pov_dat_se$countrycode,
    origin = "wb",
    destination = "iso3c"
  )

pov_dat_se <-
  pov_dat_se %>%
  mutate(countrycode = case_when(
    countryname == "Kosovo" ~ "XKX",
    TRUE ~ countrycode
  ))

wb_dat$iso3c <-
  countrycode(
    sourcevar = wb_dat$iso3c,
    origin = "wb",
    destination = "iso3c"
  )

wb_dat <-
  wb_dat %>%
  mutate(iso3c = case_when(
    country == "Kosovo" ~ "XKX",
    country == "Channel Islands" ~ "CHI",
    TRUE ~ iso3c
  ))

## changed left join to right join
dat_j <-
  pov_dat_se %>%
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

# fix these disputed territories or having no iso codes
rwm_tidy_filled <-
  rwm_tidy %>%
  mutate(ISO3 = case_when(
    id == "Falkland Islands" ~ "GBR",
    id == "Northern Cyprus" ~ "CYP",
    id == "Somaliland" ~ "SOM",
    id == "Republic of Serbia" ~ "SRB",
    id == "Kosovo" ~ "XKX",
    TRUE ~ ISO3
  ))

# convert the wb codes to ISO3
dat_j$countrycode <-
  countrycode(
    sourcevar = dat_j$countrycode,
    origin = "wb",
    destination = "iso3c"
  )

dat_j_fix <-
  dat_j %>%
  mutate(countrycode = case_when(
    country == "Kosovo" ~ "XKX",
    country == "Channel Islands" ~ "CHI",
    TRUE ~ countrycode
  ))

dat_map <-
  rwm_tidy %>%
  left_join(dat_j, by = c("ISO3" = "countrycode")) %>%
  filter(!(ISO3 %in% c("ATA", "ATF")))

dat_map_rounded <-
  dat_map %>%
  select(-country, -iso2c, -pop, -countryname, -median, -povertygap) %>%
  mutate(
    headcount = round(headcount, 4),
    gini = round(gini, 3),
    watts = round(watts, 3),
    life_exp = round(life_exp, 3),
    purchase_power_parity = round(purchase_power_parity, 3),
    gdp_capita = round(gdp_capita, 3)
  )


# fix the missing data problems
dat_map_named <-
  dat_map_rounded %>%
  rename(
    Country = id,
    Year = year,
    "ppp" = purchase_power_parity,
    "per_pov_line" = headcount,
    "watts" = watts,
    "gini" = gini,
    "pop_mm" = population,
    "Region" = wb_region,
    "gdp" = gdp_capita,
    "life_exp" = life_exp
  )

## a bunch of countries are missing region. Add in manually
dat_map_named <-
  dat_map_named %>%
  mutate(Region = case_when(
    Country == "Afghanistan" ~ "ECA",
    Country == "United Arab Emirates" ~ "MNA",
    Country == "The Bahamas" ~ "LAC",
    Country == "Brunei" ~ "EAP",
    Country == "Cuba" ~ "LAC",
    Country == "Northern Cyprus" ~ "ECA",
    Country == "Eritrea" ~ "SSA",
    Country == "Falkland Islands" ~ "LAC",
    Country == "Equatorial Guinea" ~ "SSA",
    Country == "Greenland" ~ "ECA",
    Country == "Cambodia" ~ "EAP",
    Country == "Kuwait" ~ "MNA",
    Country == "Libya" ~ "MNA",
    Country == "New Caledonia" ~ "EAP",
    Country == "New Zealand" ~ "EAP",
    Country == "Oman" ~ "MNA",
    Country == "Puerto Rico" ~ "LAC",
    Country == "North Korea" ~ "EAP",
    Country == "Qatar" ~ "MNA",
    Country == "Saudi Arabia" ~ "MNA",
    Country == "South Sudan" ~ "SSA",
    Country == "Somaliland" ~ "SSA",
    Country == "Somalia" ~ "SSA",
    Country == "Republic of Serbia" ~ "ECA",
    TRUE ~ Region
  ))


write_csv(dat_map_named, "test_data_small.csv")

## get summary statistics for raw data

dimFun <- function(x, origin, purpose, updated) {
  c(
    "Source" = origin,
    "Description" = purpose,
    "Updated" = updated,
    "Columns" = ncol(x),
    "Rows" = nrow(x),
    "NAs" = sum(colSums(is.na(x)))
  )
}

data_descs <-
  bind_rows(
    dimFun(pov_dat, "povcalNet API", "Poverty data", "2020"),
    dimFun(povcal_extra, "povcalNet API", "Extra labels for poverty data", "2020"),
    dimFun(wb_dat, "World Bank API", "World bank data from main database", "2020"),
    dimFun(rwm_low, "rworldmap", "Coarse world map polygon data", "2016"),
    dimFun(rworldmap::countrySynonyms, "rworldmap", "Extra labels for map data", "2016")
  )

cs_plot <- ggtexttable(x = data_descs, theme = ttheme(base_style = "light"))

ggsave("raw_data_stats.png", cs_plot, units = "cm", height = 5, width = 18)

## get statistics of final dataset
final_dims <- funModeling::status(dat_map_named)

final_dims$q_zeros <- NULL
final_dims$q_na <- NULL
final_dims$q_inf <- NULL

final_dims_plot <-
  final_dims %>%
  mutate(
    p_zeros = round(p_zeros * 100, 2),
    p_na = round(p_na * 100, 2)
  ) %>%
  rename(
    Variable = variable,
    Type = type,
    `N unique` = unique,
    `% zeros` = p_zeros,
    `% NAs` = p_na,
    `% Inf` = p_inf,
  ) %>%
  select(1, 5, 6, 2, 3, 4) %>%
  ggtexttable(theme = ttheme(base_style = "light"))

ggsave("data_proc_plot.png", final_dims_plot, units = "cm", height = 11, width = 14)
