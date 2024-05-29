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
}