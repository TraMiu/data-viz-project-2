#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Plot customization app"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          # sliderInput("bins",
          #             "Number of bins:",
          #             min = 1,
          #             max = 50,
          #             value = 30)
          selectInput("data", "Choose a database",
                      choices=list("mtcars", "iris"), selected = "mtcars"),
          selectInput("xcol", "Variable X", c()),
          selectInput("ycol", "Variable Y", c())
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("plot")
        )
    )
)
