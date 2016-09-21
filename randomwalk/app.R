#
# Shiny web application that generates a plot of a random walk
# in one dimension. 
#
# Author: Cory Rindlisbacher
#
#


library(shiny)

# Define UI for application that draws a plot
# and sets options for plot.

ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Random Walk in 1-Dimension"),
   
   # Sidebar with a numeric input for number of steps and radio
   # button to choose the variable using different random walks
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

# Define server logic required to render the plot
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

