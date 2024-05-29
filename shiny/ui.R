library(shiny)
library(colourpicker)
library(colourpicker)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Plot customization app"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      fileInput("file", "Choose CSV File", accept = c(".csv")),
      uiOutput("xvar_ui"),
      uiOutput("yvar_ui"),
      #selectInput("data", "Choose a database",
      #            choices=list("mtcars", "iris"), selected = "mtcars"),
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
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(width = 6, plotOutput("plot1")),
        column(width = 6, 
               conditionalPanel(
                 condition = "input.xcol2 != '' && input.ycol2 != ''",
                 plotOutput("plot2")
               )
      ),
      column(width = 12, align = "center",
             conditionalPanel(
               condition = "input.wantShowTable",
               tableOutput("table")
             )
      )
      
      )
    )
  )
)