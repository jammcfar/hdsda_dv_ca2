### Title: povcar
### Author: x19175329@student.ncirl.ie
### Desc: r file
library(formattable)
library(tidyverse)
library(povcalnetR)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
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

dat_map_rounded <-
  dat_map %>%
  select(-country, -iso2c, -pop) %>%
  mutate(
    headcount = round(headcount, 4),
    gini = round(gini, 3),
    watts = round(watts, 3),
    life_exp = round(life_exp, 3),
    purchase_power_parity = round(purchase_power_parity, 3),
    ## median = round(median, 3),
    gdp_capita = round(gdp_capita, 3),
    povertygap = round(povertygap, 3)
  ) %>%
  filter(year != 2019)

write_csv(dat_map_rounded, "test_data_small.csv")

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

## shiny time========================================================

ani_opts <-
  animationOptions(
    interval = 4000,
    playButton = "Play",
    pauseButton = "Pause"
  )

d_header <- # disabling the header is an option
  dashboardHeader(
    title = "WB Poverty dashboard",
    titleWidth = 450
  )

d_sidebar <-
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    "Why not choose\nsome options",
    br(),
    sliderInput("slider", "Year:",
      min = 1990,
      max = 2018,
      value = 1990,
      animate = ani_opts
    ),
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
      ),
      selected = "gini"
    ),
    actionButton(
      "reset_butt",
      "Clear drilldown",
      icon = icon("trash-alt")
    ),
    tableOutput("data")
  ))

## Sidebar content
ui <- dashboardPage(
  d_header,
  d_sidebar,
  dashboardBody(
    # Boxes need to be put in a row (or column) tabItems(
    fluidRow(
      column(
        width = 5,
        box(
          valueBoxOutput("worldMedian"),
          valueBoxOutput("worldBottom"),
          valueBoxOutput("worldTop"),
          width = NULL
        ),
        box(
          title = "World map",
          status = "primary",
          solidHeader = TRUE,
          background = "light-blue",
          withSpinner(plotlyOutput("plot1")),
          width = NULL
        ),
        box(
          title = "Gapminder",
          solidHeader = TRUE,
          background = "light-blue",
          plotlyOutput("gmplot", height = 600),
          width = NULL
        )
      ),
      column(
        width = 5,
        box(
          title = "Selected countries: All years for primary variable",
          solidHeader = T,
          plotlyOutput("plot2", height = 300),
          width = NULL
        ),
        box(
          title = "Selected countries: all variables for selected year",
          solidHeader = T,
          tableOutput("clickTable"),
          width = NULL
        ),
        box(
          title = "debug",
          verbatimTextOutput("clicking"),
          width = NULL
        )
      ),
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  ## read in the data
  pov_data <- read.csv("test_data_small.csv") %>% as_tibble()

  ## transform to non-map data
  pov_data_nomap <-
    pov_data %>%
    group_by(id, year) %>%
    slice_head(n = 1)

  ## create dummy data for empty line plot
  dummy_data <- tibble(
    year = c(1990, 2004, 2018),
    y_var = c(0, 0.5, 1),
    note = c(NA, "Click countries on the map\nto activate drilldowns", NA)
  )

  ## create a reactive object to pass elsewhere
  map_r <- reactive({
    pov_data_go <- pov_data %>% filter(year == input$slider)

    map_plot <-
      pov_data_go %>%
      ggplot(aes(x = long, y = lat, group = group, text = id)) +
      geom_polygon(aes_string(fill = input$var_primary), colour = "grey80", size = 0.1)

    map_plotly <- ggplotly(map_plot, height = 400)

    map_plotly
  })

  # data for summary boxes; min, max and median
  sum_dat_r <- reactive({
    pov_dat_4_summs <- pov_data_nomap %>% filter(year == input$slider)

    foo_prim <- input$var_primary

    pov_dat_summs_sel <- pov_dat_4_summs[c("id", foo_prim)]

    colnames(pov_dat_summs_sel) <- c("id", "var2")

    ## this might have trouble with duplicates, be careful
    pov_min_max <-
      pov_dat_summs_sel %>%
      ungroup() %>%
      summarise(
        min_id = id[which.min(var2)],
        min_val = min(var2, na.rm = T),
        max_id = id[which.max(var2)],
        max_val = max(var2, na.rm = T),
        .groups = NULL
      )

    get_med <- median(pov_dat_summs_sel$var2, na.rm = T)

    pov_min_max$med <- get_med

    pov_min_max
    # pov_dat_summs_sel %>%
    # slice_min(order_by = var2, with_ties = F, n = 1) %>%
    # select(id) %>%
    # as_vector()
  })

  # output for the plot
  output$plot1 <- renderPlotly({
    map_r() %>% event_register("plotly_click")
  })

  ## valuebox output
  output$worldMedian <- renderValueBox({
    foo_prim <- input$var_primary

    foo_summ <- sum_dat_r()

    valueBox(
      paste(foo_summ[1, 5]), "Global median",
      icon = icon("globe-africa"), color = "purple"
    )
  })

  output$worldBottom <- renderValueBox({
    foo_prim <- input$var_primary

    foo_summ <- sum_dat_r()

    valueBox(
      paste(foo_summ[1, 2]), paste0("Lowest (", foo_summ[1, 1], ")"),
      icon = icon("arrow-alt-circle-down"), color = "purple"
    )
  })

  output$worldTop <- renderValueBox({
    foo_prim <- input$var_primary

    foo_summ <- sum_dat_r()

    valueBox(
      paste(foo_summ[1, 4]), paste0("Highest (", foo_summ[1, 3], ")"),
      icon = icon("arrow-alt-circle-up"), color = "purple"
    )
  })
  ## save the plotly clicks to a list
  click_vals <- reactiveValues(dList = NULL)

  ## and add the clicked countries to a list
  observe({
    if (length(event_data("plotly_click")) > 0 & length(isolate(click_vals$dList)) < 8) {
      d <- event_data("plotly_click")

      foo_curve <- d$curveNumber[1] + 1
      foo_country <- str_extract(map_r()$x$data[[foo_curve]]$text, "[^>]*$")

      # only add if not already present
      if (!(foo_country %in% click_vals$dList)) {
        click_vals$dList <- c(isolate(click_vals$dList), foo_country)
      }
    }
  })

  ## clear the drilldown if the year or metric is changed (There might be a way to preserve this though)
  observeEvent(input$slider, {
    click_vals$dList <- NULL
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
    if (length(click_vals$dList) > 0) {
      pov_data_line <- pov_data_nomap %>% filter(id %in% click_vals$dList)

      line_plot <-
        ggplot(
          pov_data_line,
          aes_string(y = input$var_primary)
        ) +
        geom_line(aes(x = year, colour = id)) +
        geom_vline(xintercept = input$slider, linetype = "longdash")

      ggplotly(line_plot) %>% event_unregister("plotly_click")
    } else {
      line_plot <-
        ggplot(dummy_data, aes(x = year, y = y_var)) +
        geom_text(aes(label = note))

      ggplotly(line_plot) %>% event_unregister("plotly_click")
    }
  })

  # table country data
  output$clickTable <- renderTable({
    pov_data_table <- pov_data_nomap %>% filter(
      id %in% click_vals$dList,
      year == input$slider
    )

    pov_data_table %>%
      select(id, headcount, watts, gini, gdp_capita, life_exp, population)
  })

  ## reset the drilldown
  observeEvent(input$reset_butt, {
    click_vals$dList <- NULL
  })

  observeEvent(input$var_primary, {
    click_vals$dList <- NULL
  })

  # this is for debugging
  output$clicking <- renderPrint({
    sum_dat_r()
  })

  ## close app when closing browser tab
  session$onSessionEnded(stopApp)
}

s_dash <- shinyApp(ui, server)
s_dash


## formattable experiments



## experiments
# fig <- plot_geo(pov_data_go)
# fig2 <- fig %>% add_trace(
# z = ~headcount, text = ~id, color = ~headcount, locations = ~ISO3, colors = "Purples"
# ) %>% partial_bundle()
# fig2
# df <- read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')

# test <- tibble(
# x = c("a", "aa", "a", "b", "b", "b", "c", "c", "c"),
# y = c(1, 1:8)
# )
#
# test %>%
# summarise(
# min = min(y),
# minName = x[which.max(y)]
# )
#
#
# test %>%
# summarise(
# min_id = x[which.min(y)],
# min_val = min(y, na.rm = T),
# max_id = x[which.max(y)],
# max = max(y, na.rm = T)
# )
unique(pov_dat$year)
pov_dat %>% filter(year == 1990) -> pov_data_go
colnames(pov_data_go)[1] <- "id"
foo_prim <- "headcount"

pov_dat_summs_sel <- pov_data_go[c("id", foo_prim)]

colnames(pov_dat_summs_sel) <- c("id", "var2")

## this might have trouble with duplicates, be careful
pov_min_max <-
  pov_dat_summs_sel %>%
  summarise(
    min_id = id[which.min(var2)],
    min_val = min(var2, na.rm = T),
    max_id = id[which.max(var2)],
    max = max(var2, na.rm = T)
  )

get_med <- median(pov_dat_summs_sel$var2, na.rm = T)

pov_min_max$med <- get_med

pov_dat_summs_sel %>%
  slice_min(order_by = var2, with_ties = F, n = 1) %>%
  select(id) %>%
  as_vector()
