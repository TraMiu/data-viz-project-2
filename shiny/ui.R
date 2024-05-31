library(shiny)
library(shinythemes)
library(colourpicker)
library(tidyr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(RColorBrewer)

# Define UI for application that draws a histogram
fluidPage(
  
  # Use a theme from shinythemes
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Group E Project"),
  
  # Dropdown to choose between static and real-time data
  selectInput("choice", "Choose Option", choices = c("Static Data", "Real Time Data"), width = 190),
  
  # Sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.choice == 'Static Data'",
        fileInput("file", "Choose CSV File", accept = c(".csv")),
        uiOutput("xvar_ui"),
        uiOutput("yvar_ui"),
        selectInput("xcol1", "X-Axis Variable (Plot 1)", ""),
        selectInput("ycol1", "Y-Axis Variable (Plot 1)", ""),
        checkboxInput("wantSecondChart", "Want to Compare?", value = FALSE),
        conditionalPanel(
          condition = "input.wantSecondChart",
          selectInput("xcol2", "X-Axis Variable (Plot 2)", ""),
          selectInput("ycol2", "Y-Axis Variable (Plot 2)", ""),
          checkboxInput("wantShowTable", "Want to Show Differences?", value = FALSE)
        ),
        colourInput("col", "Select Color", "black"),
        selectInput("plotType", "Choose Plot Type",
                    choices = list("Scatter Plot" = "scatter", 
                                   "Line Plot" = "line", 
                                   "Bar Plot" = "bar")),
        selectInput("colorPalette", "Choose Color Palette",
                    choices = c("Default" = "default",
                                "Protanopia" = "protanopia",
                                "Tritanopia" = "tritanopia")),
        checkboxInput("removeNA", "Remove NA Values", value = TRUE),
        sliderInput("pointSize", "Point Size", min = 1, max = 5, value = 3)
      ),
      conditionalPanel(
        condition = "input.choice == 'Real Time Data'",
        selectInput("plotType1", "Choose Plot Type",
                    choices = list("Scatter Plot" = "scatter", 
                                   "Line Plot" = "line", 
                                   "Box Plot" = "bar")),
        checkboxInput("wantShowSummary", "Show Data", value = FALSE),
        selectInput("colorPalette1", "Choose Color Palette",
                    choices = c("Default" = "default",
                                "Protanopia" = "protanopia",
                                "Tritanopia" = "tritanopia")),
        sliderInput("refreshRate", "Refresh After (seconds):", min = 1, max = 5, value = 3),
        textInput("searchTerm", "Search Data"),
        dateRangeInput("dateRange", "Select Date Range", start = Sys.Date() - 30, end = Sys.Date()),
        actionButton("snapshot_btn", "Take Snapshot", class = "btn-primary"),
        
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      conditionalPanel(
        condition = "input.choice != 'Real Time Data'",
        fluidRow(
          column(width = 6, plotOutput("plot1")),
          column(width = 6, 
                 conditionalPanel(
                   condition = "input.xcol2 != '' && input.ycol2 != ''",
                   plotOutput("plot2")
                 )
          )
        ),
        fluidRow(
          column(width = 12, align = "center",
                 conditionalPanel(
                   condition = "input.wantShowTable",
                   tableOutput("table"),
                   
                 )
          )
        )
      ),
      conditionalPanel(
        condition = "input.choice == 'Real Time Data'",
        column(width = 6,plotOutput("plot3")),
        column(width = 6,plotOutput("plot4")),
        fluidRow(
          column(width = 12, align = "center",
                 conditionalPanel(
                   condition = "input.wantShowSummary",
                   tableOutput("table1"),

                   dataTableOutput("searchResultsTable")
                 )
          )
        )
      )
    )
  )
)