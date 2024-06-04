library(shiny)
library(colourpicker)
library(tidyr)
library(ggplot2)
library(dplyr) 
library(magrittr)
library(RColorBrewer)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Shiny plot customization app"),
  
  tabsetPanel(
    id = "tabset",
    tabPanel("Data Preparation"),
    tabPanel("Parameter Selection"),
    tabPanel("Plot Customization"),
    tabPanel("Theme & Accessibility")
  ),
  
  selectInput("choice", "Data Type", choices = c("Static Data", "Real Time Data"), width = 190),
  textInput("plotTitle", "Enter Plot Title", value = "", width = 190),
  checkboxInput("wantPlotSubtitle", "Add subtitle?", value = FALSE),
  
  conditionalPanel(
    condition = "input.wantPlotSubtitle",
    textInput("plotSubtitle", "Enter Plot Subtitle", value = "", width = 190),
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Data preparation tab
      conditionalPanel(
        condition = "input.tabset == 'Data Preparation' && input.choice == 'Static Data'",
        checkboxInput("removeNA", "Remove NA values", value = TRUE),
        fileInput("file", "Import CSV File", accept = c(".csv")),
      ),
      
      
      # Parameter selection tab
      conditionalPanel(
        condition = "input.tabset == 'Parameter Selection' && input.choice == 'Static Data'",
        selectInput("xcol1", "X-Axis variable (Plot 1)", ""),
        selectInput("ycol1", "Y-Axis variable (Plot 1)", ""),
        
        
        checkboxInput("changeLabel1", "Change labels of plot 1?", value = FALSE),
        conditionalPanel(
          condition = "input.changeLabel1",
          textInput("plotX_1label", "Enter X label", value = ""),
          textInput("plotY_1label", "Enter Y label", value = ""),
        ),
        
        checkboxInput("wantSecondChart", "Want to compare?", value = FALSE),
        conditionalPanel(
          condition = "input.wantSecondChart",
          selectInput("xcol2", "X-Axis variable (Plot 2)", ""),
          selectInput("ycol2", "Y-Axis variable (Plot 2)", ""),
          
          checkboxInput("changeLabel2", "Change labels of plot 2?", value = FALSE),
          conditionalPanel(
            condition = "input.changeLabel2",
            textInput("plotX_2label", "Enter X label", value = ""),
            textInput("plotY_2label", "Enter Y label", value = ""),
          ),
          checkboxInput("wantShowTable", "Want to show differences?", value = FALSE)
        ),
        
      ),
      
      
      
      # Select Plot Type
      conditionalPanel(
        condition = "input.tabset == 'Plot Customization' && input.choice == 'Static Data'",
        selectInput("plotType", "Choose Plot Type",
                    choices = list("Scatter Plot" = "scatter", 
                                   "Line Plot" = "line", 
                                   "Bar Plot" = "bar")),
        
        conditionalPanel(
          condition = "input.plotType == 'bar'",
          checkboxInput("categorizeBar", "Want to categorize bars?", value = FALSE),
          conditionalPanel(
            condition = "input.categorizeBar",
            uiOutput("fillcol_ui")
          )
        ),
        
        conditionalPanel(
          condition = "input.plotType == 'line'",
          checkboxInput("categorizeLine", "Want to categorize lines?", value = FALSE),
          conditionalPanel(
            condition = "input.categorizeLine",
            uiOutput("colorcol_ui")
          )
        ),
        sliderInput("pointSize", "Point Size", min = 1, max = 5, value = 3),
      ),
      
      
      
      
      # Select theme
      conditionalPanel(
        condition = "input.tabset == 'Theme & Accessibility' && input.choice == 'Static Data'",
        colourInput("col", "Select color","black" ),
        selectInput("colorPalette", "Choose Color Palette",
                    choices = c("Default" = "default",
                                "Protanopia" = "protanopia",
                                "Tritanopia" = "tritanopia"), selected = "protanopia"),
        selectInput("themeInput", "Select a Theme", 
                    choices = c(
                      "Minimal" = "minimal", 
                      "Dark" = "dark", 
                      "Light" = "light",
                      "Grey" = "grey",
                      "Classic" = "classic", 
                      "Void" = "void",
                      "VinUniversity Theme*" = "vinuni")),
        checkboxInput("colorBlind", "Color blind friendly", value = FALSE)
      ),
      
      
      # UI for real time data
      conditionalPanel(
        condition = "input.choice == 'Real Time Data'",
        
        
        conditionalPanel(
          condition = "input.tabset == 'Plot Customization' && input.notFrequencyChart",
          selectInput("plotType1", "Choose Plot Type",
                      choices = list("Scatter Plot" = "scatter", 
                                     "Line Plot" = "line", 
                                     "Bar Plot" = "bar"))
        ),
        conditionalPanel(
          condition = "input.tabset == 'Theme & Accessibility'",
          selectInput("colorPalette1", "Choose Color Palette",
                      choices = c("Default" = "default",
                                  "Protanopia" = "protanopia",
                                  "Tritanopia" = "tritanopia"))
        ),
        selectInput("plotType2", "Choose Plot Type",
                    choices = list("Scatter Plot" = "scatter", 
                                   "Line Plot" = "line", 
                                   "Box Plot" = "bar")),
        textInput("searchTerm", "Search Data"),
        checkboxInput("wantShowSummary", "Show Data", value = FALSE),
        dateRangeInput("dateRange", "Select Date Range", start = Sys.Date() - 30, end = Sys.Date()),
        
        sliderInput("refreshRate", "Refresh After (second): ", min = 5, max = 10, value = 7),
        actionButton("snapshot_btn", "Take Snapshot", class = "btn-primary"),
        
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      # For static data without color blind and with second plot
      conditionalPanel(
        condition = "input.choice != 'Real Time Data'",
        uiOutput("dynamicPlot"),
        fluidRow(
          column(width = 12, align = "center",
                 conditionalPanel(
                   condition = "input.wantShowTable",
                   tableOutput("table")
                 )
          )
        )
      ),
      
      
      # For Real Time Data
      fluidRow(
        column(
          width = 12,
          conditionalPanel(
            condition = "input.choice == 'Real Time Data'",
            column(width = 8,plotOutput("plot3")),
            column(width = 4,plotOutput("plot4")),
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
    )
  )
