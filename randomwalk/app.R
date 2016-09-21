#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Random Walk in 1-Dimension"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput(inputId = "num", label = "Number of Steps", value = 100, min = 10,
                     max = 500),
        radioButtons(inputId = "dist", label = "Distribution", 
                     choices = c("Normal" = "norm", "Uniform" = "unif"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput(outputId = "walk")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$walk <- renderPlot({
      dist <- switch(input$dist,
                     norm = cumsum(jitter(rnorm(input$num))),
                     unif = cumsum(jitter(sample(c(-1, 1), prob = c(.5, .5), size = input$num, 
                     replace = TRUE)))
      )
      plot(1:length(dist), dist, 'b', main = "Distance From Start", xlab = "Step",
           ylab = "Distance", col = 'blue')
      lines(0)
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

