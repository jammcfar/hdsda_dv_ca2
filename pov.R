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
  select(-contains("decile")) %>%
  filter(year > 1990)

# these might help getting lower res map stuff
## coarse_coords <- rworldmap::getMap(resolution = "coarse")
## rnat_data <- rnaturalearth::ne_countries(scale = "small", type = "countries")


## test plot
map_plot <-
  dat_map_trimmed %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean), colour = "grey80", size = 0.5)

ggplotly(map_plot)

write_csv(dat_map_trimmed, "test_data.csv")

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

  pov_data <- read_csv("test_data.csv")

  output$plot1 <- renderPlotly({

    pov_data_go <- pov_data %>% filter(year == input$slider)

    map_plot <-
      pov_data_go %>%
      ggplot() +
      geom_polygon(aes(x = long, y = lat, group = group, fill = mean), colour = "grey80", size = 0.5)

    map_plot %>% ggplotly() %>% event_register("plotly_click")
  })

  output$plot2 <- renderPlot({
    d <- event_data("plotly_click")

    if (is.null(d)) {
      hist(pov_data$mean)
    } else {
      (hist(d$mean))
    }
  })

  output$clicking <- renderPrint({
   d <- event_data("plotly_click")

df_select <- pov_data[pov_data$ %in% d$customdata,]

hist(df_select$mean)

  })
  # output$table1 <- renderTable({
  # d <- event_data("plotly_click")
  # if (is.null(d)) "Click events appear here (double-click to clear)" else d
  # })

  ## output$table1

  ## output$plot2 <- renderPlot({
}


shinyApp(ui, server)



## this one workss
ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("event")
)
server <- function(input, output, session) {

  #df sample using dput
  df <- structure(list(col_group = structure(1:8, .Label = c("1", "2",
                                                             "3", "4", "5", "6", "7", "8"), class = "factor"), color = structure(c(4L,
                                                                                                                                   6L, 7L, 8L, 5L, 3L, 2L, 1L), .Label = c("#1A9850", "#66BD63",
                                                                                                                                                                           "#A6D96A", "#D73027", "#D9EF8B", "#F46D43", "#FDAE61", "#FEE08B"
                                                                                                                                   ), class = "factor"), geometry = structure(list(structure(c(4.52343199617246,
                                                                                                                                                                                               4.52324233358547, 4.52267342343957, 4.52224662532908, 4.52210428346744,
                                                                                                                                                                                               4.52200925015845, 4.52192088448668, 4.52180475361204, 4.52172945325391,
                                                                                                                                                                                               4.52168196905882, 4.5215535740952, 4.52095980475523, 4.52076420632298,
                                                                                                                                                                                               4.52062274547478, 51.9453195440486, 51.9453722803981, 51.945526317454,
                                                                                                                                                                                               51.9456480852256, 51.9456928353329, 51.9457296098645, 51.9457705951341,
                                                                                                                                                                                               51.9458530114811, 51.945914910501, 51.9459312169901, 51.9459510896027,
                                                                                                                                                                                               51.9459966849614, 51.9460077291392, 51.9460066867221), .Dim = c(14L,
                                                                                                                                                                                                                                                               2L), class = c("XY", "LINESTRING", "sfg")), structure(c(4.4964540696277,
                                                                                                                                                                                                                                                                                                                       4.49696710232736, 51.9086692611627, 51.9084484303039), .Dim = c(2L,
                                                                                                                                                                                                                                                                                                                                                                                       2L), class = c("XY", "LINESTRING", "sfg")), structure(c(4.13635479859532,
                                                                                                                                                                                                                                                                                                                                                                                                                                               4.13644010080625, 51.975098751212, 51.9751715711302), .Dim = c(2L,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              2L), class = c("XY", "LINESTRING", "sfg")), structure(c(4.47801457239531,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      4.47834576882542, 51.9300740588744, 51.9304218318716), .Dim = c(2L,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      2L), class = c("XY", "LINESTRING", "sfg")), structure(c(4.45974369011875,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              4.46029492493512, 4.46033964902157, 51.9290774018138, 51.9284345596986,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              51.9283809798498), .Dim = 3:2, class = c("XY", "LINESTRING",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "sfg")), structure(c(4.43886518844695, 4.43891013390463, 4.43910559159559,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            4.43913561800293, 51.93455577157, 51.9344932127658, 51.9341891712133,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            51.9341444695365), .Dim = c(4L, 2L), class = c("XY", "LINESTRING",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           "sfg")), structure(c(4.54844667292407, 4.55002805772657, 51.9658870347267,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                51.9661409927825), .Dim = c(2L, 2L), class = c("XY", "LINESTRING",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "sfg")), structure(c(4.47522875290306, 4.47598748813623, 51.9347985281278,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    51.9353886781931), .Dim = c(2L, 2L), class = c("XY", "LINESTRING",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "sfg"))), class = c("sfc_LINESTRING", "sfc"), precision = 0, bbox = structure(c(xmin = 4.13635479859532,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ymin = 51.9084484303039, xmax = 4.55002805772657, ymax = 51.9751715711302
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ), class = "bbox"), crs = structure(list(epsg = 4326L, proj4string = "+proj=longlat +datum=WGS84 +no_defs"), class = "crs"), n_empty = 0L),
                       key = c("1", "8000", "10000", "12000", "14000", "16000",
                               "18000", "22000")), row.names = c(1L, 8000L, 10000L, 12000L,
                                                                 14000L, 16000L, 18000L, 22000L), class = c("sf", "data.frame"
                                                                 ), sf_column = "geometry", agr = structure(c(col_group = NA_integer_,
                                                                                                              color = NA_integer_, key = NA_integer_), .Label = c("constant",
                                                                                                                                                                  "aggregate", "identity"), class = "factor"))
  #plotly
  output$plot <- renderPlotly({
    plot_geo(df, type = "scattergl", mode = "lines",
             customdata = ~key,
             color = ~col_group, colors = rev(RColorBrewer::brewer.pal(8, name = "RdYlGn"))
    )
  })

  #event_data
  output$event <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else {
      df_select <- df[df$key %in% d$customdata,]
      print(df_select)
    }
  })

}
shinyApp(ui, server)




## test stuff
ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("click")
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    us <- map_data("state")
    p <- ggplot(us, aes(x = long, y = lat, group = region, key = region)) +
      geom_polygon()
    ggplotly(p)
  })

  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
}

shinyApp(ui, server, options = list(display.mode = "showcase"))


df <- structure(list(col_group = structure(1:8, .Label = c("1", "2",
                                                             "3", "4", "5", "6", "7", "8"), class = "factor"), color = structure(c(4L,
                                                                                                                                   6L, 7L, 8L, 5L, 3L, 2L, 1L), .Label = c("#1A9850", "#66BD63",
                                                                                                                                                                           "#A6D96A", "#D73027", "#D9EF8B", "#F46D43", "#FDAE61", "#FEE08B"
                                                                                                                                   ), class = "factor"), geometry = structure(list(structure(c(4.52343199617246,
                                                                                                                                                                                               4.52324233358547, 4.52267342343957, 4.52224662532908, 4.52210428346744,
                                                                                                                                                                                               4.52200925015845, 4.52192088448668, 4.52180475361204, 4.52172945325391,
                                                                                                                                                                                               4.52168196905882, 4.5215535740952, 4.52095980475523, 4.52076420632298,
                                                                                                                                                                                               4.52062274547478, 51.9453195440486, 51.9453722803981, 51.945526317454,
                                                                                                                                                                                               51.9456480852256, 51.9456928353329, 51.9457296098645, 51.9457705951341,
                                                                                                                                                                                               51.9458530114811, 51.945914910501, 51.9459312169901, 51.9459510896027,
                                                                                                                                                                                               51.9459966849614, 51.9460077291392, 51.9460066867221), .Dim = c(14L,
                                                                                                                                                                                                                                                               2L), class = c("XY", "LINESTRING", "sfg")), structure(c(4.4964540696277,
                                                                                                                                                                                                                                                                                                                       4.49696710232736, 51.9086692611627, 51.9084484303039), .Dim = c(2L,
                                                                                                                                                                                                                                                                                                                                                                                       2L), class = c("XY", "LINESTRING", "sfg")), structure(c(4.13635479859532,
                                                                                                                                                                                                                                                                                                                                                                                                                                               4.13644010080625, 51.975098751212, 51.9751715711302), .Dim = c(2L,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              2L), class = c("XY", "LINESTRING", "sfg")), structure(c(4.47801457239531,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      4.47834576882542, 51.9300740588744, 51.9304218318716), .Dim = c(2L,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      2L), class = c("XY", "LINESTRING", "sfg")), structure(c(4.45974369011875,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              4.46029492493512, 4.46033964902157, 51.9290774018138, 51.9284345596986,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              51.9283809798498), .Dim = 3:2, class = c("XY", "LINESTRING",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "sfg")), structure(c(4.43886518844695, 4.43891013390463, 4.43910559159559,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            4.43913561800293, 51.93455577157, 51.9344932127658, 51.9341891712133,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            51.9341444695365), .Dim = c(4L, 2L), class = c("XY", "LINESTRING",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           "sfg")), structure(c(4.54844667292407, 4.55002805772657, 51.9658870347267,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                51.9661409927825), .Dim = c(2L, 2L), class = c("XY", "LINESTRING",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "sfg")), structure(c(4.47522875290306, 4.47598748813623, 51.9347985281278,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    51.9353886781931), .Dim = c(2L, 2L), class = c("XY", "LINESTRING",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "sfg"))), class = c("sfc_LINESTRING", "sfc"), precision = 0, bbox = structure(c(xmin = 4.13635479859532,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ymin = 51.9084484303039, xmax = 4.55002805772657, ymax = 51.9751715711302
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ), class = "bbox"), crs = structure(list(epsg = 4326L, proj4string = "+proj=longlat +datum=WGS84 +no_defs"), class = "crs"), n_empty = 0L),
                       key = c("1", "8000", "10000", "12000", "14000", "16000",
                               "18000", "22000")), row.names = c(1L, 8000L, 10000L, 12000L,
                                                                 14000L, 16000L, 18000L, 22000L), class = c("sf", "data.frame"
                                                                 ), sf_column = "geometry", agr = structure(c(col_group = NA_integer_,
                                                                                                              color = NA_integer_, key = NA_integer_), .Label = c("constant",

                                                                                                                                                                  "aggregate", "identity"), class = "factor"))
