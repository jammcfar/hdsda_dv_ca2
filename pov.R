### Title: povcar
### Author: x19175329@student.ncirl.ie
### Desc: r file

library(tidyverse)
library(povcalnetR)
library(shiny)
library(shinydashboard)
library(plotly)
library(wbstats)
library(rmapshaper)
library(sp)
library(geojsonio)


## import poverty data
pov_dat <- povcalnet(
  fill_gaps = T,
  year = c(1980:2019)
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

wb_dat <- wb_data(my_indicators, start_date = 1980, end_date = 2019)

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


rwm_tidy %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(colour = "grey80", size = 0.5)

dat_map <-
  rwm_tidy %>%
  left_join(dat_j, by = c("ISO3" = "countrycode")) %>%
  filter(id != "Antarctica")

## this is all redundant now I think
## deal with the ones who wouldnt gather data and have no years
# no_dat_collect <-
# dat_map %>%
# filter(is.na(countryname), id != "Antarctica")
#
# years_available <- na.omit(unique(dat_map$year))
#
# nrow(no_dat_collect)
#
# for (i in 1:length(years_available)) {
# no_dat_collect <-
# no_dat_collect %>%
# add_column(
# x = years_available[i],
# .name_repair = "unique"
# )
# }
# no_dat_fix <-
# no_dat_collect %>%
# pivot_longer(cols = x...21:x...58) %>%
# select(-year, -name) %>%
# rename(year = "value")
#
# dat_map_fix <-
# dat_map %>%
# filter(!is.na(countryname)) %>%
##  bind_rows(no_dat_fix) %>%
# select(-countryname, -iso2c)
#
### no fill in for countries
# dat_map_fix <-
# dat_map_fix %>%
# group_by(group) %>%
# complete(year = 1981:2019, nesting(long, lat))
#
# dat_map_fix %>% filter(ISO3 == "SSD")
# expand(dat_map_fix, nesting(group), year = 1981:2019)

## join up map data
# dat_map <-
# world_reduce %>%
# inner_join(dat_j, by = c("region" = "countryname"))

write_csv(dat_map, "test_data_small.csv")

## variables
## year
## country
## world region
## inequality index
## incomes
## consumption
## population (millions)
## headcount (% below the poverty line)
## poverty gap (mean distance below poverty line)
## gini inequality measure
## watts poverty measure

## this is test stuff
pov_data <- read.csv("test_data_small.csv") %>% as_tibble()

pov_data_go <- pov_data %>% filter(year == 2000)

map_plot <-
  pov_data_go %>%
  ggplot(aes(x = long, y = lat, group = group, text = country, fill = headcount)) +
  geom_polygon(colour = "grey80", size = 0.1) +
  scale_fill_continuous(na.value = "grey70")
map_plot


# note curve number is position in list -1
str_extract(plotly_test$x$data[[10]]$text, "[^<]+")


countries_vector <- vector("character", nrow(d))

for (i in 1:nrow(d)) {
  foo_curve <- d$curveNumber[i] + 1
  foo_country <- str_extract(plotly_test$x$data[[foo_curve]]$text, "[^<]+")
  countries_vector[i] <- foo_country
}

pov_dat_hist <- pov_dat_go %>% filter(pov_data_go %>% countries_vector())

hist(pov_dat_hist)

## shiny time

## Sidebar content
ui <- dashboardPage(
  # disabling the header is an option
  dashboardHeader(
    title = "WB Poverty dashboard",
    titleWidth = 450
  ),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    "Why not choose\nsome options",
    br(),
    sliderInput("slider", "Year:",
      min = 1990,
      max = 2019,
      value = 1990,
      animate = T
    ),
    # dateRangeInput("daterange", "Select years:",
    # startview = "decade",
    # start = "1967",
    # end = "2019",
    # min = "1967",
    # max = "2019"
    # ),
    textInput("text", "Text input:")
  )),
  dashboardBody(
    # Boxes need to be put in a row (or column) tabItems(
    # First tab content
    tabItem(
      tabName = "dashboard",
      fluidRow(
        box(
          title = "Histogram",
          status = "primary",
          solidHeader = TRUE,
          background = "light-blue",
          plotlyOutput("plot1", height = 600)
        )
      ),
      box(
        title = "Line graph",
        solidHeader = T,
        plotOutput("plot2", height = 300)
      ),
      box(
        title = "debug",
        verbatimTextOutput("clicking")
      )
    ),

    # Second tab content
    tabItem(
      tabName = "widgets",
      h2("Widgets tab content")
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  ## read in the data
  pov_data <- read.csv("test_data_small.csv") %>% as_tibble()

  output$plot1 <- renderPlotly({
    pov_data_go <- pov_data %>% filter(year == input$slider)

    map_plot <-
      pov_data_go %>%
      ggplot(aes(x = long, y = lat, group = group, text = id, fill = headcount)) +
      geom_polygon(colour = "grey80", size = 0.1)


    map_plotly <-
      map_plot %>%
      ggplotly() %>%
      event_register("plotly_click")

    map_plotly
  })

  output$plot2 <- renderPlot({
    d <- event_data("plotly_click")

    # if (is.null(d)) {
    # hist(pov_data$mean)
    # } else {
    # (hist(d$mean))
    # }
    ## repeating this
    pov_data_go_2 <- pov_data %>% filter(year == input$slider)

    map_plot_2 <-
      pov_data_go_2 %>%
      ggplot(aes(x = long, y = lat, group = group, text = id, fill = headcount)) +
      geom_polygon(colour = "grey80", size = 0.1)

    map_plotly_2 <-
      map_plot_2 %>%
      ggplotly()

    countries_vector <- c()

    for (i in 1:length(d)) {
      foo_curve <- d$curveNumber[i] + 1
      foo_country <- str_extract(map_plotly_2$x$data[[foo_curve]]$text, "[^<]+")
      countries_vector[i] <- foo_country
    }

    pov_data_hist <- pov_data_go_2 %>% filter(id %in% countries_vector)

    hist(pov_data_hist$headcount)
  })

  output$clicking <- renderPrint({
    d <- event_data("plotly_click")
    d

    ## hist(df_select$mean)
  })
  # output$table1 <- renderTable({
  # d <- event_data("plotly_click")
  # if (is.null(d)) "Click events appear here (double-click to clear)" else d
  # })

  ## output$table1

  ## output$plot2 <- renderPlot({
}


shinyApp(ui, server)
