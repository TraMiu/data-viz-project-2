library(shiny)
library(colourpicker)
library(tidyr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(RColorBrewer)
library(htmlwidgets)
library(webshot)
library(plotly)

# Define server logic
server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file)
    inFile <- input$file
    read.csv(inFile$datapath)
  })
  
  observe({
    data <- dataset()
    cols <- names(data)
    updateSelectInput(session, "xcol1", choices = cols, selected = cols[1])
    updateSelectInput(session, "ycol1", choices = cols, selected = cols[2])
    updateSelectInput(session, "xcol2", choices = cols, selected = "")
    updateSelectInput(session, "ycol2", choices = cols, selected = "")
  })
  
  get_filtered_data <- reactive({
    data <- dataset()
    if (input$removeNA) {
      data <- na.omit(data)
    }
    data
  })
  
  output$plot1 <- renderPlot({
    data <- get_filtered_data()
    create_plot(data, input$xcol1, input$ycol1, input$colorPalette, input$plotType, input$pointSize)
  })
  
  output$plot2 <- renderPlot({
    data <- get_filtered_data()
    create_plot(data, input$xcol2, input$ycol2, input$colorPalette, input$plotType, input$pointSize)
  })
  
  output$table <- renderTable({
    data <- get_filtered_data()
    if (input$wantSecondChart && input$xcol2 != '' && input$ycol2 != '') {
      diffs <- data.frame(X = data[[input$xcol1]], Y1 = data[[input$ycol1]], Y2 = data[[input$ycol2]])
      diffs$Difference <- diffs$Y1 - diffs$Y2
      avg_diff <- diffs %>%
        summarize(X = "Average",
                  Y1 = mean(Y1, na.rm = TRUE),
                  Y2 = mean(Y2, na.rm = TRUE),
                  Difference = mean(Difference, na.rm = TRUE))
      
      diffs <- rbind(diffs, avg_diff)
      diffs
    } else {
      NULL
    }
  })
  
  create_plot <- function(data, xcol, ycol, palette, plotType, pointSize) {
    if (xcol == '' || ycol == '') return(NULL)
    
    p <- ggplot(data, aes_string(x = xcol, y = ycol)) +
      theme_minimal() +
      labs(x = xcol, y = ycol)
    
    color <- switch(palette,
                    "protanopia" = brewer.pal(6, "Blues")[6],
                    "tritanopia" = brewer.pal(8, "Set1")[8],
                    "black")
    
    p <- p + switch(plotType,
                    "scatter" = geom_point(color = color, size = pointSize),
                    "line" = geom_line(color = color, size = pointSize, group = 1),
                    "bar" = geom_bar(stat = "identity", fill = color, width = pointSize / 3))
    
    p
  }
  
  RealTime <- function() {
    read_data <- reactiveFileReader(input$refreshRate * 1000, session, filePath = "data.csv", readFunc = read.csv)
    
    filtered_data <- reactive({
      data <- read_data()  # Call the reactiveFileReader function to get the current data
      
      data <- data[data$Date >= input$dateRange[1] & data$Date <= input$dateRange[2], ]
      data
    })

    
    output$plot3 <- renderPlot({
      
      data <- filtered_data()
      if (input$colorPalette1 == "protanopia") {
        color <- brewer.pal(6, "Blues")[6]  
      } else if (input$colorPalette1 == "tritanopia") {
        color <- brewer.pal(8, "Set1")[8] 
      } else {"black"}
      plot_type <- switch(input$plotType1,
                            "scatter" = geom_point(color = color),
                            "line" = geom_line(color = color),
                            "bar" = geom_boxplot(color = color))
         ggplot(data, aes(x = Date, y = X1,  group = 1)) +
           stat_summary(fun.y = "mean") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        plot_type +
        labs(x = "Date", y = "Price") + 
        ggtitle("Stock Price day by day")
        })
    
    
    
    newest_day_data <- reactive({
      data1 <- read_data()
      latest_date <- max(data1$Date, format = "%m/%d/%Y")
      data1 <- data1[data1$Date == latest_date, ]
      data1
    })
    
    output$plot4 <- renderPlot({
      data <- newest_day_data()
      if (input$colorPalette1 == "protanopia") {
        color <- brewer.pal(6, "Blues")[6]  
      } else if (input$colorPalette1 == "tritanopia") {
        color <- brewer.pal(8, "Set1")[8] 
      } else {"black"}
      
      ggplot(data, aes(x = Date, y = X1)) +
        geom_boxplot(color=color) +
        labs(x = "Date", y = "Price") +
        ggtitle("Stock Price Over Time")})
    

    
    searchedData <- reactive({
      data <- read_data()
      search_term <- tolower(input$searchTerm)
      subset(data, grepl(search_term, tolower(data$Date)) | data$X1 == search_term | data$X2 == search_term )
    })
    
    output$searchResultsTable <- renderDataTable({
      searchedData()
    })
    
    
    output$table1 <- renderTable({
      data <- read_data()
      if (input$wantShowSummary) {
        data %>%
          summarise(across(c(X1, X2), list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE)) %>%
          pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") %>%
          pivot_wider(names_from = "Statistic", values_from = "Value")
      } else {
        NULL
      }
    })
  }
  
  
  observeEvent(input$snapshot_btn, {
    # Default name for the snapshot
    timestamp <- format(Sys.time(), "%Y_%m_%d")
    filename <- paste0("snapshot_", timestamp)
    # Take the snapshot
    shinyscreenshot::screenshot(
      filename = filename
    )
    # Notification about the snapshot location
    notification <- paste("Snapshot saved as", filename, "in the current working directory.")
    showNotification(notification, duration = 5)
  })

  
  observeEvent(input$choice, {
    if (input$choice == "Real Time Data") {
      RealTime()
    }
  })
}
