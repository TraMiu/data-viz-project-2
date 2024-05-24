library(shiny)
library(ggplot2)
library(RColorBrewer)

# Define server logic
shinyServer(function(input, output, session) {
  
  dataset <- reactive({
    switch(input$data,
           "mtcars" = mtcars,
           "iris" = iris)
  })
  
  observe({
    data <- dataset()
    updateSelectInput(session, "xcol", choices = names(data), selected = names(data)[1])
    updateSelectInput(session, "ycol", choices = names(data), selected = names(data)[2])
  })
  
  output$plot <- renderPlot({
    data <- dataset()
    p <- ggplot(data, aes_string(x = input$xcol, y = input$ycol)) +
      theme_minimal() + labs(x = input$xcol, y = input$ycol)
    
    # Determine the color to use based on the selected palette
    color <- input$col
    if (input$colorPalette == "protanopia") {
      color <- brewer.pal(6, "Blues")[6]  # Color from Brewer palette that is better for Protanopia
      p <- p + theme(
        plot.title = element_text(color = brewer.pal(6, "Blues")[6]),
        axis.title.x = element_text(color = brewer.pal(8, "Dark2")[2]),
        axis.title.y = element_text(color = brewer.pal(8, "Dark2")[2]),
        axis.text.x = element_text(color = brewer.pal(8, "Dark2")[2]),
        axis.text.y = element_text(color = brewer.pal(8, "Dark2")[2]),
        
      )
    }else if (input$colorPalette == "tritanopia") {
      color <- brewer.pal(8, "Set1")[8]  # Color from Brewer palette that is better for Deuteranopia
      p <- p + theme(
        plot.title = element_text(color = brewer.pal(8, "Set1")[2]),
        axis.title.x = element_text(color = brewer.pal(8, "Set1")[2]),
        axis.title.y = element_text(color = brewer.pal(8, "Set1")[2]),
        axis.text.x = element_text(color = brewer.pal(8, "Set1")[2]),
        axis.text.y = element_text(color = brewer.pal(8, "Set1")[2])
      )
    }
    
    # Add plot layers based on the selected plot type
    if (input$plotType == "scatter") {
      p <- p + geom_point(color = color)
    } else if (input$plotType == "line") {
      p <- p + geom_line(color = color)
    } else if (input$plotType == "bar") {
      p <- p + geom_bar(stat = "identity", fill = color)
    }
    
    p
  })
})
