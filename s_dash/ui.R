### Title: Shiny dashboard
### Author: x19175329@student.ncirl.ie
### Desc: Builds the dashboard ui


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
      selected = "life_exp"
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
