### Title: Shiny dashboard
### Author: x19175329@student.ncirl.ie
### Desc: Builds the dashboard


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
  "formattable",
  "shiny",
  "shinycssloaders",
  "shinydashboard",
  "plotly",
  "shinyalert"
)

lapply(package_list, library, character.only = T)

## shiny dashboard========================================================

## set up the individual components
ani_opts <-
  animationOptions(
    interval = 4000,
    playButton = "Play",
    pauseButton = "Pause"
  )

d_header <- # disabling the header is an option
  dashboardHeader(
    title = "Exploring global poverty",
    titleWidth = 300
  )

## set up side panel. Enable shinyalert for help popup.
d_sidebar <-
  dashboardSidebar(
    useShinyalert(),
    # sidebarMenu(
    # menuItem("Controls", tabName = "dashboard", icon = icon("dashboard")),
    actionButton("help",
      "Info",
      icon = icon("info-circle")
    ),
    br(),
    sliderInput("slider", "Year:",
      min = 1990,
      max = 2018,
      value = 1990,
      sep = "",
      animate = ani_opts
    ),
    selectInput(
      "var_primary", "Primary variable:",
      c(
        "Precent below poverty line" = "per_pov_line",
        "Watt's Poverty Index" = "watts",
        "Gini Inequality Index" = "gini",
        "Life expectancy" = "life_exp",
        "GDP per capita" = "gdp",
        "Purchase power parity" = "ppp"
      )
    ),
    selectInput(
      "var_secondary", "Secondary variable",
      c(
        "Percent below poverty line" = "per_pov_line",
        "Watt's Poverty Index" = "watts",
        "Gini Inequality Index" = "gini",
        "Life expectancy" = "life_exp",
        "GDP per capita" = "gdp",
        "Purchase power parity" = "ppp"
      ),
      selected = "gini"
    ),
    br(),
    actionButton(
      "reset_butt",
      "Clear drilldown",
      icon = icon("trash-alt")
    )
    # )
  )

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
        tabBox(
          title = "",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", width = 12,
          tabPanel("World map", plotlyOutput("plot1")),
          tabPanel("\"Gapminder\"", plotlyOutput("gmplot"))
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
          formattableOutput("clickTable"),
          width = NULL
        ) # , # this is for debugging
        # box(
        # title = "debug",
        # verbatimTextOutput("clicking"),
        # width = NULL
        # )
      )
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)

  # define some custom colours from Sasha Trubetskoy
  cus_cols <- c("#e6194B", "#3cb44b", "#4363d8", "#f58231", "#42d4f4", "#f032e6", "#800000", "#000075")

  ## read in the data
  pov_data <- read_csv("test_data_small.csv", col_types = "ddcccddddddcdd") %>% as_tibble()

  ## transform to non-map data
  pov_data_nomap <-
    pov_data %>%
    group_by(Country, Year) %>%
    slice_head(n = 1)

  ## create dummy data for empty line plot
  dummy_data <- tibble(
    Year = c(1990, 2004, 2018),
    y_var = c(0, 0.5, 1),
    note = c(
      NA,
      "Click countries on the map to activate drilldowns.\nUp to 8 may be selected.\nNote that adjusting the slider or primary\nvariable will reset the drilldowns",
      NA
    )
  )

  ## create a vector to substitute labels on graphs
  metrics_sub_v <-
    c(
      "Percent below\npoverty line" = "per_pov_line",
      "Watt's index" = "watts",
      "Gini index" = "gini",
      "GDP per\ncapita" = "gdp",
      "Life\nexpectancy\n(years)" = "life_exp",
      "Population\n(MM)" = "pop_mm",
      "Purchase\npower parity" = "ppp"
    )

  ## create a reactive variable for plotly click
  ## this allows it to be cleared properly by reset button, etc.
  click_data <- reactiveValues(e = NULL)

  observe({
    click_data$e <- event_data("plotly_click")
  })

  ## create a reactive object to pass elsewhere
  map_r <- reactive({
    pov_data_go <- pov_data %>% filter(Year == input$slider)

    fill_var <- names(which(metrics_sub_v == input$var_primary))

    map_plot <-
      pov_data_go %>%
      ggplot(aes(x = long, y = lat, group = group, text = Country)) +
      geom_polygon(aes_string(fill = input$var_primary), colour = "grey90", size = 0.1) +
      theme_classic() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()
      ) +
      scale_fill_gradientn(
        name = paste(fill_var),
        colors = c("#ABCEE2", "#3C8DBC", "#193C50")
      )

    map_plotly <- ggplotly(map_plot, height = 400)

    map_plotly
  })

  # data for summary boxes; min, max and median
  sum_dat_r <- reactive({
    pov_dat_4_summs <- pov_data_nomap %>% filter(Year == input$slider)

    foo_prim <- input$var_primary

    pov_dat_summs_sel <- pov_dat_4_summs[c("Country", foo_prim)]

    colnames(pov_dat_summs_sel) <- c("Country", "var2")

    ## this might have trouble with duplicates, be careful
    pov_min_max <-
      pov_dat_summs_sel %>%
      ungroup() %>%
      summarise(
        min_id = Country[which.min(var2)],
        min_val = min(var2, na.rm = T),
        max_id = Country[which.max(var2)],
        max_val = max(var2, na.rm = T),
        .groups = NULL
      )

    get_med <- median(pov_dat_summs_sel$var2, na.rm = T)

    pov_min_max$med <- get_med

    pov_min_max
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
      icon = icon("globe-africa"), color = "blue"
    )
  })

  output$worldBottom <- renderValueBox({
    foo_prim <- input$var_primary

    foo_summ <- sum_dat_r()

    valueBox(
      paste(foo_summ[1, 2]), paste0("Lowest (", foo_summ[1, 1], ")"),
      icon = icon("arrow-alt-circle-down"), color = "blue"
    )
  })

  output$worldTop <- renderValueBox({
    foo_prim <- input$var_primary

    foo_summ <- sum_dat_r()

    valueBox(
      paste(foo_summ[1, 4]), paste0("Highest (", foo_summ[1, 3], ")"),
      icon = icon("arrow-alt-circle-up"), color = "blue"
    )
  })

  ## observe which tab is selected


  ## save the plotly clicks to a list
  click_vals <- reactiveValues(dList = NULL)

  ## and add the clicked countries to a list
  observe({
    if (length(click_data$e == 1) & length(isolate(click_vals$dList)) < 8 & !is.null(input$slider) & input$tabset1 == "World map") {
      d <- click_data$e

      foo_curve <- d$curveNumber[1] + 1
      foo_country <- str_extract(map_r()$x$data[[foo_curve]]$text, "[^>]*$")

      # only add if not already present
      if (!(foo_country %in% click_vals$dList)) {
        click_vals$dList <- c(isolate(click_vals$dList), foo_country)
      }
      # now if it is from the gapminder...
    } else if (length(click_data$e) > 1 & length(isolate(click_vals$dList)) < 8) {
      d <- click_data$e

      foo_curve <- d$curveNumber[1] + 1
      foo_pointnum <- d$pointNumber[1] + 1
      foo_country <- str_extract(gap_r()$x$data[[foo_curve]]$text[foo_pointnum], "[^>]*$")

      # only add if not already present
      if (!(foo_country %in% click_vals$dList)) {
        click_vals$dList <- c(isolate(click_vals$dList), foo_country)
      }
    }
  })
  # observe({
  # if (length(click_data$e == 1) & length(isolate(click_vals$dList)) < 8 & !is.null(input$slider)) {
  # d <- click_data$e
  #
  # if(input$tabset1 == "World map"){
  #
  # foo_curve <- d$curveNumber[1] + 1
  # foo_country <- str_extract(map_r()$x$data[[foo_curve]]$text, "[^>]*$")
  #
  # } else if(input$tabset1 == "  \"Gapminder\""){
  #
  # foo_curve <- d$curveNumber[1] + 1
  # foo_pointnum <- d$pointNumber[1] + 1
  # foo_country <- str_extract(gap_r()$x$data[[foo_curve]]$text[foo_pointnum], "[^>]*$")
  #
  # }
  #
  ## only add if not already present
  # if (!(foo_country %in% click_vals$dList)) {
  # click_vals$dList <- c(isolate(click_vals$dList), foo_country)
  # }
  #
  # }
  # })

  ## gapminder style visual, first create a reactive object
  gap_r <- reactive({
    data_gm <- pov_data_nomap %>% filter(Year == input$slider)

    x_var <- names(which(metrics_sub_v == input$var_primary))
    y_var <- names(which(metrics_sub_v == input$var_secondary))

    wb_plot <-
      ggplot(data_gm, aes_string(
        x = input$var_primary,
        y = input$var_secondary,
        text = "Country"
      )) +
      geom_point(aes(colour = Region, size = pop_mm), alpha = 0.8) +
      scale_colour_manual(values = cus_cols, na.translate = F) +
      theme_classic() +
      theme(legend.title = element_blank()) +
      labs(x = paste(x_var), y = paste(y_var))

    ggplotly(wb_plot, tooltip = c("text", "size", "colour"))
  })

  ## now the actual gapminder viz
  output$gmplot <- renderPlotly({
    gap_r()
  })

  # line plot over year
  output$plot2 <- renderPlotly({
    if (length(click_vals$dList) > 0) {
      pov_data_line <- pov_data_nomap %>% filter(Country %in% click_vals$dList)

      y_var_line <- names(which(metrics_sub_v == input$var_secondary))

      line_plot <-
        ggplot(
          pov_data_line,
          aes_string(y = input$var_primary)
        ) +
        geom_line(aes(x = Year, colour = Country)) +
        geom_vline(xintercept = input$slider, linetype = "longdash", colour = "grey30") +
        scale_colour_manual(values = cus_cols) +
        theme_classic() +
        theme(legend.title = element_blank()) +
        labs(y = paste(y_var_line))

      ggplotly(line_plot) %>% event_unregister("plotly_click")
    } else {
      line_plot <-
        ggplot(dummy_data, aes(x = Year, y = y_var)) +
        geom_text(aes(label = note)) +
        theme_classic() +
        theme(legend.title = element_blank())

      ggplotly(line_plot) %>% event_unregister("plotly_click")
    }
  })

  # table country data
  output$clickTable <- renderFormattable({
    foo_data_table <- pov_data_nomap %>% filter(
      Country %in% click_vals$dList,
      Year == input$slider
    )
    data_table_out <-
      foo_data_table %>%
      ungroup() %>%
      select(
        Country,
        "Percent below poverty line" = "per_pov_line",
        "Watt's index" = "watts",
        "Gini index" = "gini",
        "GDP per capita" = "gdp",
        "Life expectancy (years)" = "life_exp",
        "Population (MM)" = "pop_mm",
        "Purchase power parity" = "ppp"
      )

    formattable(
      data_table_out,
      align = c("l", "c", "c", "c", "c", "c", "c", "c"),
      list(
        "Percent below poverty line" = color_tile("#E3EEF5", "#73ADCF"),
        "Watt's index" = color_tile("#E3EEF5", "#73ADCF"),
        "Gini index" = color_tile("#E3EEF5", "#73ADCF"),
        "GDP per capita" = color_tile("#E3EEF5", "#73ADCF"),
        "Life expectancy (years)" = color_tile("#E3EEF5", "#73ADCF"),
        "Population (MM)" = color_tile("#E3EEF5", "#73ADCF"),
        "Purchase power parity" = color_tile("#E3EEF5", "#73ADCF")
      )
    )
  })

  ## reset the drilldown
  observeEvent(input$reset_butt, {
    click_vals$dList <- NULL
    click_data$e <- NULL
  })

  observeEvent(input$var_primary, {
    click_vals$dList <- NULL
    click_data$e <- NULL
  })

  ## clear the drilldown if the year or metric is changed (There might be a way to preserve this though)
  observeEvent(input$slider, {
    click_vals$dList <- NULL
    click_data$e <- NULL
  })

  ## shinyalert to display the help page
  observeEvent(input$help, {
    shinyalert(
      title = "Metric info",
      text = "Here are some simplified explanations of some of the metrics used in the dashboard.\n
Percent below poverty line: The percent of the population below the poverty line, set at $1.90 US.\n
Watt's Index: A distribution sensitive measure of poverty using logarithms. It adds extra weight to people in extreme poverty. Note it can behave strangely sometimes.\n
Gini Index: A measure of inequality. A value of 0 has all the wealth distributed equally, a value of 1 has all the wealth held by one person.\n
Purchase power parity: How much purchasing power individuals have with the local currency, compared to the US Dollar. In other words, it describes how much things would cost if being sold in the United States.\n
For more information and access to the raw data, visit \"http://iresearch.worldbank.org/PovcalNet/home.aspx\"",
      type = "info",
      size = "m",
      closeOnClickOutside = T
    )
  })

  # this is for debugging
  # output$clicking <- renderPrint({
  ## sum_dat_r()
  # click_check <- event_data("plotly_click")
  # paste(
  # click_data$e,
  # click_check,
  # input$tabset1
  # )
  # })

  ## close app when closing browser tab
  session$onSessionEnded(stopApp)
}

s_dash <- shinyApp(ui, server)
s_dash
