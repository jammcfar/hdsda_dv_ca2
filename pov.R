### Title: povcar
### Author: x19175329@student.ncirl.ie
### Desc: r file

library(tidyverse)
library(povcalnetR)
library(shiny)
library(shinydashboard)
library(plotly)
library(wbstats)

pov_dat <- povcalnet()

## code ripped from https://github.com/nset-ornl/wbstats
my_indicators <- c(
  life_exp = "SP.DYN.LE00.IN",
  gdp_capita = "NY.GDP.PCAP.CD",
  pop = "SP.POP.TOTL"
)

wb_dat <- wb_data(my_indicators, start_date = 1967, end_date = 2020)

dat_j <-
  pov_dat %>%
  left_join(wb_dat, by = c("countrycode" = "iso3c", "year" = "date"))

## join ggplot2 stuff
dat_world <- ggplot2::map_data("world")

## alot match, but many dont. Will have to go in manually
dat_world <- dat_world %>% as_tibble()

## join up map data

dat_map <-
  dat_world %>%
  inner_join(dat_j, by = c("region" = "countryname"))

dat_map_trimmed <-
  dat_map %>%
  select(-contains("decile"))

## test plot
map_plot <-
  dat_map_trimmed %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean), colour = "grey80", size = 0.5)

ggplotly(map_plot)


write_csv(data_map_trimmed, "test_data.csv")

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
    sliderInput("slider", "Number of observations:", 1, 100, 50),
    dateRangeInput("daterange", "Select years:",
      startview = "decade",
      start = "1967",
      end = "2019",
      min = "1967",
      max = "2019"
    ),
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
          background = "grey70",
          plotOutput("plot1", height = 250)
        )
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

  pov_data <- read_csv("test_data.csv")

  output$plot1 <- renderPlot({
    data <- pov_data$mean[seq_len(input$slider)]
    hist(data)
  })
}


shinyApp(ui, server)
