library(shiny)
library(colourpicker)
library(colourpicker)

# Define server logic
server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file)
    inFile <- input$file
    read.csv(inFile$datapath)
  })
  
  observe({
    data <- dataset()
    updateSelectInput(session, "xcol1", choices = names(data), selected = names(data)[1])
    updateSelectInput(session, "ycol1", choices = names(data), selected = names(data)[2])
    updateSelectInput(session, "xcol2", choices = names(data), selected = "")
    updateSelectInput(session, "ycol2", choices = names(data), selected = "")
  })
  
 
   output$plot1 <- renderPlot({
    data <- dataset()
    if (input$removeNA) {
      data <- na.omit(data)
    }
    p <- create_plot(data, input$xcol1, input$ycol1)
    p
  })
  
  
  output$plot2 <- renderPlot({
    data <- dataset()
    if (input$removeNA) {
      data <- na.omit(data)
    }
    p <- create_plot(data, input$xcol2, input$ycol2)
    p
  })
  
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
  
  create_plot <- function(data, xcol, ycol) {
    p <- ggplot(data, aes_string(x = xcol, y = ycol)) +
      theme_minimal() +
      labs(x = xcol, y = ycol)
    
    if (input$colorPalette == "protanopia") {
      color <- brewer.pal(6, "Blues")[6]  # Color from Brewer palette that is better for Protanopia
    } else if (input$colorPalette == "tritanopia") {
      color <- brewer.pal(8, "Set1")[8]  # Color from Brewer palette that is better for Deuteranopia
    }
    
    if (input$plotType == "scatter") {
      p <- p + geom_point(color = color, size = input$pointSize)
    } else if (input$plotType == "line") {
      p <- p + geom_line(color = color, size = input$pointSize)
    } else if (input$plotType == "bar") {
      p <- p + geom_bar(stat = "identity", fill = color, width = (input$pointSize )/3)
    }
  }
  
  RealTime <- function() {
    # Function to read data from data.csv
    read_data <- reactiveFileReader((input$refreshRate * 1000), session, filePath = "data.csv", readFunc = read.csv)
    
    
    # Render the plot
    output$plot <- renderPlot({
      data <- read_data()
      if (input$colorPalette1 == "protanopia") {
        color <- brewer.pal(6, "Blues")[6]  # Color from Brewer palette that is better for Protanopia
      } else if (input$colorPalette1 == "tritanopia") {
        color <- brewer.pal(8, "Set1")[8]  # Color from Brewer palette that is better for Deuteranopia
      } else {"black"}
      
      if (!("X2" %in% names(data))) {
        ggplot(data, aes(x = X1)) +
          geom_histogram(fill = color) +
          labs(x = "X Axis", y = "Y Axis") + 
          ggtitle("Dynamic Plot")
      } else {
        plot_type <- switch(input$plotType1,
                            "scatter" = geom_point(color = color),
                            "line" = geom_line(color = color),
                            "bar" = geom_bar(color = color, stat = "identity"),
                            "bar")
        
        
        output$table1 <- renderTable({
          if (input$wantShowSummary) {
            data %>%
              summarise_at(vars(X1, X2), list(mean = mean, sd = sd, min = min, max = max)) %>%
              pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") %>%
              pivot_wider(names_from = "Statistic", values_from = "Value")
          } else {
            NULL
          }
        })
        
        ggplot(data, aes(x = X1, y = X2)) +
          plot_type +  # Dynamically selected plot type
          labs(x = "X Axis Title", y = "Y Axis Title") + 
          ggtitle("Dynamic Plot ")
        
        
        
      }
    })
  }
  
  
  observe({
    if (input$choice == "Real Time Data") {
      RealTime()
    }
  })
  }
  
  