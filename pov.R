### Title: povcar
### Author: x19175329@student.ncirl.ie
### Desc: r file

library(tidyverse)
library(povcalnetR)
library(shiny)
library(shinydashboard)
library(plotly)
library(wbstats)

## import poverty data
pov_dat <- povcalnet(
  fill_gaps = T,
  year = c(1990:2019)
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

wb_dat <- wb_data(my_indicators, start_date = 1990, end_date = 2019)

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
  geom_polygon(colour = "grey80", size = 1) +
  scale_fill_continuous(na.value = "grey70")

## plotly highlight testing
map_plotly <- ggplotly(map_plot)

s <- attrs_selected(
  showlegend = TRUE,
  mode = "lines+markers",
  marker = list(symbol = "x")
)

highlight(map_plotly,
  on = "plotly_click",
  color = toRGB("red"),
  selected = s
)

## line chart
pov_data %>%
  filter(id == "Ireland") %>%
  ggplot(aes(x = year, y = headcount, colour = id)) +
  geom_point() +
  geom_path() +
  theme_classic()

## gapminder
pov_data_go %>%
  group_by(id) %>%
  top_n(1) %>%
  ggplot(aes(x = gdp_capita, y = gini, colour = wb_region, size = population)) +
  geom_point()

# note curve number is position in list -1
str_extract(map_plotly$x$data[[10]]$text, "[^<]+")


countries_vector <- vector("character", nrow(d))

for (i in 1:nrow(d)) {
  foo_curve <- d$curveNumber[i] + 1
  foo_country <- str_extract(plotly_test$x$data[[foo_curve]]$text, "[^<]+")
  countries_vector[i] <- foo_country
}

pov_dat_hist <- pov_dat_go %>% filter(pov_data_go %>% countries_vector())

hist(pov_dat_hist)

## shiny time

ani_opts <-
  animationOptions(
    interval = 4000,
    playButton = "Play",
    pauseButton = "Pause"
  )

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
      animate = ani_opts
    ),
    # dateRangeInput("daterange", "Select years:",
    # startview = "decade",
    # start = "1967",
    # end = "2019",
    # min = "1967",
    # max = "2019"
    # ),
    ## textInput("text", "Text input:")
    selectInput(
      "var_primary", "Primary variable:",
      c(
        "Extreme poverty %" = "headcount",
        "Watt's Poverty Index" = "watts",
        "Gini Inequality Index" = "gini",
        "Life expectancy" = "life_exp",
        "GDP per capita" = "gdp_capita"
      )
    ),
    selectInput(
      "var_secondary", "Secondary variable",
      c(
        "Extreme poverty %" = "headcount",
        "Watt's Poverty Index" = "watts",
        "Gini Inequality Index" = "gini",
        "Life expectancy" = "life_exp",
        "GDP per capita" = "gdp_capita"
      )
    ),

    tableOutput("data")
  )),
  dashboardBody(
    # Boxes need to be put in a row (or column) tabItems(
    fluidRow(
      box(
        title = "World map",
        status = "primary",
        solidHeader = TRUE,
        background = "light-blue",
        plotlyOutput("plot1", height = 600)
      ),
      box(
        title = "Gapminder",
        solidHeader = TRUE,
        background = "light-blue",
        plotlyOutput("gmplot", height = 600)
      )
    ),
    fluidRow(
      box(
        title = "Country data",
        solidHeader = T
      ),
      box(
        title = "Line graph",
        solidHeader = T,
        plotlyOutput("plot2", height = 300)
      ),
      box(
        title = "debug",
        verbatimTextOutput("clicking")
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  ## read in the data
  pov_data <- read.csv("test_data_small.csv") %>% as_tibble()

  ## transform to non-map data
  pov_data_nomap <-
    pov_data %>%
    group_by(group, year) %>%
    slice_head(n = 1)

  ## create a reactive object to pass elsewhere
  map_r <- reactive({
    pov_data_go <- pov_data %>% filter(year == input$slider)

    map_plot <-
      pov_data_go %>%
      ggplot(aes(x = long, y = lat, group = group, text = id)) +
      geom_polygon(aes_string(fill = input$var_primary), colour = "grey80", size = 0.1)

    map_plotly <- ggplotly(map_plot)

    map_plotly
  })

  output$plot1 <- renderPlotly({
    # pov_data_go <- pov_data %>% filter(year == input$slider)
    #
    # map_plot <-
    # pov_data_go %>%
    # ggplot(aes(x = long, y = lat, group = group, text = id)) +
    # geom_polygon(aes_string(fill = input$var_primary), colour = "grey80", size = 0.1)
    #
    # map_plotly <- ggplotly(map_plot)
    map_r() %>% event_register("plotly_click")
    ## map_data

    # map_plotly <- plot_geo(pov_data_go)
    # map_plotly <- map_plotly %>% add_trace(
    # z = ~headcount, text = ~id, color = ~headcount, locations = ~ISO3, colors = "Purples"
    # )

    # highlight_key() # this might be the answer
    # highligh deosnt seem to work
    # highlight(map_plotly,
    # on = "plotly_click",
    # color = toRGB("red")
    # ) %>%
    # event_register("plotly_click")
  })

  # gapminder style visual
  output$gmplot <- renderPlotly({
    data_gm <- pov_data_nomap %>% filter(year == input$slider)

    wb_plot <-
      ggplot(data_gm, aes_string(
        x = input$var_primary,
        y = input$var_secondary,
        text = "id"
      )) +
      geom_point(aes(colour = wb_region, size = population))

    ggplotly(wb_plot, tooltip = "text") %>% event_unregister("plotly_click")
  })

  # line plot over year
  output$plot2 <- renderPlotly({
    d <- event_data("plotly_click")

    # pov_data_go_2 <- pov_data %>% filter(year == input$slider)
    #
    # map_plot_2 <-
    # pov_data_go_2 %>%
    # ggplot(aes(x = long, y = lat, group = group, text = id)) +
    # geom_polygon(aes_string(fill = input$var_primary))
    #
    # map_plotly_2 <-
    # map_plot_2 %>%
    # ggplotly()


    ## extract out the clicked country (I think)
    countries_vector <- c()

    for (i in 1:length(d)) {
      foo_curve <- d$curveNumber[i] + 1
      foo_country <- str_extract(map_r()$x$data[[foo_curve]]$text, "[^>]*$")
      countries_vector[i] <- foo_country
    }

    pov_data_line <- pov_data_nomap %>% filter(id %in% countries_vector)

    line_plot <-
      ggplot(
        pov_data_line,
        aes_string(y = input$var_primary)
      ) +
      geom_line(aes(x = year, colour = id))

    ggplotly(line_plot) %>% event_unregister("plotly_click")
  })

  output$clicking <- renderPrint({
    d <- event_data("plotly_click")
    txt <- input$var_primary

    paste(d, txt)

    ## hist(df_select$mean)
  })
  # output$table1 <- renderTable({
  # d <- event_data("plotly_click")
  # if (is.null(d)) "Click events appear here (double-click to clear)" else d
  # })

  ## output$table1

  ## output$plot2 <- renderPlot({
}

s_dash <- shinyApp(ui, server)
s_dash


## experiments
# fig <- plot_geo(pov_data_go)
# fig2 <- fig %>% add_trace(
# z = ~headcount, text = ~id, color = ~headcount, locations = ~ISO3, colors = "Purples"
# ) %>% partial_bundle()
# fig2
# df <- read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
