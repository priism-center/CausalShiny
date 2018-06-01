# SHINY TEMPLATE










library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Central Limit Theorem"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("distribution", "Distribution Type:", c(Normal = "normal", Beta = "beta", Uniform = "uniform")), 
        conditionalPanel(
        condition = "input.distribution == 'normal'",
          sliderInput("mean", "Mean:", min = 1720, max = 1750, value = 1729),
          sliderInput("sd", "Standard Deviation:", min = 1, max = 20, value = 10),
          sliderInput("nsample", "Number of Samples:", min = 1000, max = 10000, value = 2000),
          sliderInput("sampleSize", "Sample size:", min = 50, max = 100, value = 100)),
        
        conditionalPanel(
        condition = "input.distribution == 'beta'",
          sliderInput("alpha", "Alpha:", min = 1, max = 5, value = 1),
          sliderInput("beta", "Beta:", min = 1, max = 5, value = 1)),
        
        conditionalPanel(
        condition = "input.distribution == 'uniform'",
          sliderInput("min", "Min:", min = 0, max = 5, value = 0),
          sliderInput("interval", "Interval:", min = 1, max = 5, value = 1))
      ),
          
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("normdist"), 
         plotOutput("normhist")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$normdist <- renderPlot({
      # generate bins based on input$bins from ui.R
      x <- matrix(rep(0, input$sampleSize * input$nsample), nrow = input$nsample, ncol = input$sampleSize)
      for (i in 1:input$nsample) {
        if (input$distribution == "normal") {
      x[i, ] <- rnorm(input$sampleSize, input$mean, input$sd)
        }
        else if (input$distribution == "beta") {
      x[i, ] <- rbeta(input$sampleSize, input$alpha, input$beta)
        }
        else if (input$distribution == "uniform") {
      x[i, ] <- runif(input$sampleSize, input$min, input$min + input$interval)    
        }
      }
      y <- apply(x, 1, mean)
      # draw the histogram with the specified number of bins
      plot(density(y), main = "Distribution")
   })
  
   output$normhist <- renderPlot({
     # generate bins based on input$bins from ui.R
     x <- matrix(rep(0, input$sampleSize * input$nsample), nrow = input$nsample, ncol = input$sampleSize)
     for (i in 1:input$nsample) {
       x[i, ] <- rnorm(input$sampleSize, input$mean, input$sd)
     }
     y <- apply(x, 1, mean)
     hist(y)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

