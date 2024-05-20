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
function(input, output, session) {

    dataset = eventReactive(input$data, {get(input$data)})
    
    observeEvent(input$data, {
      req(dataset())
      choices <- names(dataset())
      updateSelectInput(session,"xcol",choices = choices, selected=choices[1])
      updateSelectInput(session,"ycol",choices = choices, selected=choices[2])
    }, ignoreNULL = FALSE)
    
    output$plot <- renderPlot({

        # # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #      xlab = 'Waiting time to next eruption (in mins)',
        #      main = 'Histogram of waiting times')
      
      ggplot(dataset(), aes(.data[[input$xcol]], .data[[input$ycol]])) + geom_point(color = input$col)

    })

}
