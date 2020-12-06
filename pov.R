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

dat_j <-
  pov_dat %>%
  left_join(wb_dat, by = c("countrycode" = "iso3c", "year" = "date"))

## naniar::vis_miss(dat_j)


## join ggplot2 stuff
dat_world <- ggplot2::map_data("world")

## alot match, but many dont. Will have to go in manually
dat_world <- dat_world %>%
  as_tibble() %>%
  mutate(subregion = NA) %>%
  filter(region != "Antarctica") %>%
  arrange(region)

## map the map smaller
map_json <- geojson_json(dat_world, geometry = "polygon", group = "group")

map_sp <- geojson_sp(map_json)

map_simp <- ms_simplify(map_sp, keep = 0.05, keep_shapes = T)

coord_bits <- as_tibble(raster::geom(map_simp))

world_reduce <-
  as_tibble(map_simp) %>%
  mutate(
    group = as.numeric(trimws(group)),
    order = as.numeric(trimws(order))
  ) %>%
  group_by(region) %>%
  mutate(region_id = cur_group_id()) %>%
  right_join(coord_bits, by = c(region_id = "object")) %>%
  select(
    long = x,
    lat = y,
    group,
    order,
    region,
    subregion
  )

## join up map data
dat_map <-
  world_reduce %>%
  inner_join(dat_j, by = c("region" = "countryname"))

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
pov_data <- read_csv("test_data.csv")


pov_data_go <- pov_data %>% filter(year == 2000)

map_plot <-
  pov_data_go %>%
  ggplot(aes(x = long, y = lat, group = group, text = country, fill = mean)) +
  geom_polygon(colour = "grey80", size = 0.5)

plotly_test <- map_plot %>%
  ggplotly() %>%
  event_register("plotly_click")

plotly_test

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
  pov_data <- read_csv("test_data_small.csv")

  output$plot1 <- renderPlotly({
    pov_data_go <- pov_data %>% filter(year == input$slider)

    map_plot <-
      pov_data_go %>%
      ggplot(aes(x = long, y = lat, group = group, text = country, fill = headcount)) +
      geom_polygon(colour = "grey80", size = 0.5)


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
      ggplot(aes(x = long, y = lat, group = group, text = country, fill = headcount)) +
      geom_polygon(colour = "grey80", size = 0.5)

    map_plotly_2 <-
      map_plot_2 %>%
      ggplotly()

    countries_vector <- c()

    for (i in 1:length(d)) {
      foo_curve <- d$curveNumber[i] + 1
      foo_country <- str_extract(map_plotly_2$x$data[[foo_curve]]$text, "[^<]+")
      countries_vector[i] <- foo_country
    }

    pov_data_hist <- pov_data_go_2 %>% filter(region %in% countries_vector)

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
