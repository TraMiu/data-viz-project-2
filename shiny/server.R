#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)


# Define server logic required to draw a histogram
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
    
    # Add plot layers based on the selected plot type
    if (input$plotType == "scatter") {
      p <- p + geom_point(color = input$col)
    } else if (input$plotType == "line") {
      p <- p + geom_line(color = input$col)
    } else if (input$plotType == "bar") {
      p <- p + geom_bar(stat = "identity", fill = input$col)
    }
    
    p
  })
})