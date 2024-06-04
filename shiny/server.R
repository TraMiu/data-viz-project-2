library(shiny)
library(colourpicker)
library(tidyr)
library(ggplot2)
library(dplyr) 
library(magrittr)
library(RColorBrewer)
library(showtext)
library(colorblindr)


font_add_google("Montserrat", "Montserrat")
main_font = "Montserrat"


vinuni_palette_main <- c("#35426e", "#d2ae6d", "#c83538", "#2e548a")
vinuni_palette_accents <- c( "#5cc6d0", "#a7c4d2", "#d2d3d5",  "#4890bd", "#0087c3", "#d2ae6d")

# Saving our theme as a function
theme_vinuni <- function(base_size = 11, base_family = main_font, 
                         base_line_size = base_size / 22, 
                         base_rect_size = base_size / 22) {
  # Base our theme on minimal theme
  theme_minimal(
    base_family = base_family,
    base_size = base_size,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.5, face="bold", colour = vinuni_palette_main[3]),
      plot.subtitle = element_text(hjust = 0.5, face="bold"),
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "#bbbbbb", linewidth = 0.2),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(size = rel(1.0)),
      axis.text.x = element_text(face="bold", colour = vinuni_palette_main[1]), 
      axis.text.y = element_text(face="bold", colour = vinuni_palette_main[1]), 
      legend.text = element_text(size = rel(0.9))
    )
}


# Define server logic
server <- function(input, output, session) {
  
  # Input csv
  dataset <- reactive({
    req(input$file)
    inFile <- input$file
    read.csv(inFile$datapath)
  })
  
  # Generate UI for selecting the fill variable
  output$fillcol_ui <- renderUI({
    req(dataset())
    selectInput("fillcol", "Fill Variable", choices = names(dataset()))
  })
  
  # Generate UI for selecting the color variable
  output$colorcol_ui <- renderUI({
    req(dataset())
    selectInput("colorcol", "Color Variable", choices = names(dataset()))
  })
  
  
  
  # Populate the choice for each selection option
  observe({
    data <- dataset()
    updateSelectInput(session, "xcol1", choices = names(data), selected = names(data)[1])
    updateSelectInput(session, "ycol1", choices = names(data), selected = names(data)[2])
    updateSelectInput(session, "xcol2", choices = names(data), selected = "")
    updateSelectInput(session, "ycol2", choices = names(data), selected = "")
  })
  
  output$nonFrequentGuidanceText <- renderText({"Please go to Plot Customization to select another plot type."})
  
  # render plot 1
  output$plot1 <- renderPlot({
    data <- dataset()
    if (input$removeNA) {
      data <- na.omit(data)
    }
    
    if (input$changeLabel1) {
      xlabel <- input$plotX_1label
    } else {
      xlabel <- input$xcol1
    }
    
    if (input$changeLabel1) {
      ylabel <- input$plotY_1label
    } else {
      ylabel <- input$ycol1
    }
    
    p <- create_plot(data, input$xcol1, input$ycol1, xlabel, ylabel)
    p
  })
  
  # Render UI for plot based on checkbox input
  output$dynamicPlot <- renderUI({
    if (input$colorBlind) {
      plotOutput("plot1")
    } else {
      fluidRow(
        column(width = 6, plotOutput("plot1")),
        column(width = 6, 
               conditionalPanel(
                 condition = "input.xcol2 != '' && input.ycol2 != ''",
                 plotOutput("plot2")
               )
        )
      )
    }
  })
  
  # render plot 2
  output$plot2 <- renderPlot({
    data <- dataset()
    if (input$removeNA) {
      data <- na.omit(data)
    }
    
    if (input$changeLabel2) {
      xlabel <- input$plotX_1label
    } else {
      xlabel <- input$xcol1
    }
    
    if (input$changeLabel2) {
      ylabel <- input$plotY_2label
    } else {
      ylabel <- input$ycol2
    }
    
    p <- create_plot(data, input$xcol2, input$ycol2, xlabel, ylabel)
    p
    
  })
  
  # Draw table for different
  output$table <- renderTable({
    data <- dataset()
    if (input$removeNA) {
      data <- na.omit(data)
    }
    if (input$wantSecondChart && input$xcol2 != '' && input$ycol2 != '') {
      diffs <- data.frame(X = data[, input$xcol1], Y1 = data[, input$ycol1], Y2 = data[, input$ycol2])
      diffs$Difference <- diffs$Y1 - diffs$Y2
      avg_diff <- diffs %>%
        summarize(X = "Average",
                  Y1 = mean(Y1, na.rm = TRUE),
                  Y2 = mean(Y2, na.rm = TRUE),
                  Difference = mean(Difference, na.rm = TRUE))
      
      # Combine the summary row with the original table
      diffs <- rbind(diffs, avg_diff)
      
      diffs
      diffs
    } else {
      NULL
    }
  })
  
  create_plot <- function(data, xcol, ycol, xlabel, ylabel) {
    
    if (input$plotType == "scatter") {
      g <- ggplot(data, aes_string(x = xcol, y = ycol))
    } else if (input$plotType == "line") {
      g <- ggplot(data, aes_string(x = xcol, y = ycol))
      req(input$colorcol)
      g <- ggplot(data, aes_string(x = xcol, y = ycol, color = input$colorcol))
    } else if (input$plotType == "bar") {
      g <- ggplot(data, aes_string(x = xcol, y = ycol))
      req(input$fillcol)
      g <- ggplot(data, aes_string(x = xcol, y = ycol, fill = input$fillcol))
    }
    
    
    
    
    if (input$colorPalette == "protanopia") {
      color <- brewer.pal(6, "Blues")[6]  # Color from Brewer palette that is better for Protanopia
    } else if (input$colorPalette == "tritanopia") {
      color <- brewer.pal(8, "Set1")[8]  # Color from Brewer palette that is better for Deuteranopia
    } else {color <- "black"}
    
    
    
    if (input$plotType == "scatter") {
      p <- g + geom_point(color = color, size = input$pointSize)
    } else if (input$plotType == "line") {
      #p <- g + geom_line(color = color, size = input$pointSize, group = 1)
      req(input$colorcol) 
      p <- g + geom_line(color = color, size = input$pointSize, group = 1)
    } else if (input$plotType == "bar") {
      #p <- g + geom_bar(stat = "identity", fill = color, width = (input$pointSize )/3)
      req(input$fillcol) 
      p <- g + geom_col(stat = "identity", position = "dodge", width = (input$pointSize )/3)
    } 
    
    p
    
    # Apply colorblind adjustments if checked
    if (input$colorBlind) {
      p <- cvd_grid(p)
    }
    
    # Apply selected theme
    p <- switch(input$themeInput,
                "minimal" = p + theme_minimal(),
                "dark" = p + theme_dark(),
                "light" = p + theme_light(),
                "grey" = p + theme_grey(),
                "classic" = p + theme_classic(),
                "vinuni" = p + theme_vinuni(),
                "void" = p + theme_void()
    )
    
    # Return the plot
    p + labs(
      x = xlabel, 
      y = ylabel,
      title = ifelse(input$plotTitle == "", input$file, input$plotTitle),
      subtitle = input$plotSubtitle)
  }
  
  
  # This is for the real time data
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
      } else {color <- "black"}
      plot_type <- switch(input$plotType2,
                          "scatter" = geom_point(color = color),
                          "line" = geom_line(color = color, size = pointSize, group = 1),
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
      } else {color <- "black"}
      
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
