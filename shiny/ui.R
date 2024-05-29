library(shiny)
library(colourpicker)
library(colourpicker)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Plot customization app"),
  
  selectInput("choice", "Choose Option", choices = c("Static Data", "Real Time Data"), width = 190),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.choice == 'Static Data'",
        fileInput("file", "Choose CSV File", accept = c(".csv")),
        uiOutput("xvar_ui"),
        uiOutput("yvar_ui"),
        selectInput("xcol1", "X-Axis variable (Plot 1)", ""),
        selectInput("ycol1", "Y-Axis variable (Plot 1)", ""),
        checkboxInput("wantSecondChart", "Want to compare?", value = FALSE),
        conditionalPanel(
          condition = "input.wantSecondChart",
          selectInput("xcol2", "X-Axis variable (Plot 2)", ""),
          selectInput("ycol2", "Y-Axis variable (Plot 2)", ""),
          checkboxInput("wantShowTable", "Want to show differences?", value = FALSE)
        ),
        colourInput("col", "Select color","black" ),
        selectInput("plotType", "Choose Plot Type",
                    choices = list("Scatter Plot" = "scatter", 
                                   "Line Plot" = "line", 
                                   "Bar Plot" = "bar")),
        selectInput("colorPalette", "Choose Color Palette",
                    choices = c("Default" = "default",
                                "Protanopia" = "protanopia",
                                "Tritanopia" = "tritanopia")),
        checkboxInput("removeNA", "Remove NA values", value = TRUE),
        sliderInput("pointSize", "Point Size", min = 1, max = 5, value = 3)
      ),
      conditionalPanel(
        condition = "input.choice == 'Real Time Data'",
        checkboxInput("frequencyChart", "Is it not frequency chart?", value = FALSE),
        conditionalPanel(
          condition = "input.frequencyChart",
          selectInput("plotType1", "Choose Plot Type",
                      choices = list("Scatter Plot" = "scatter", 
                                     "Line Plot" = "line", 
                                     "Bar Plot" = "bar")),
        ),
        checkboxInput("wantShowSummary", "Want to show summary?", value = FALSE),
        selectInput("colorPalette1", "Choose Color Palette",
                    choices = c("Default" = "default",
                                "Protanopia" = "protanopia",
                                "Tritanopia" = "tritanopia")),
        sliderInput("refreshRate", "Refresh After (second): ", min = 1, max = 5, value = 3),
        actionButton("stopButton", "Stop"),
      )
    ),
    
    # Show a plot of the generated distribution
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
                   tableOutput("table")
                 )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          conditionalPanel(
            condition = "input.choice == 'Real Time Data'",
            plotOutput('plot'),
            column(width = 12, align = "center",
                     condition = "input.wantShowSummary",
                     tableOutput("table1")
                   )
            )
          )
        )
      )
    )
  )
