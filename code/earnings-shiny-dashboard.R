rm(list=ls())
gc()
library(shiny)
ui <- fluidPage(
  sliderInput(inputId = "num", 
  label = "Choose mean of distribution", 
  value = 25, 
  min = 0.001, max = 100),
  
  plotOutput(outputId = "hist")
)
server <-  function(input, output){
  output$hist <- renderPlot({
    title <- "Histogram"
    hist(rnorm(100, mean = input$num), main = title)
    })
}
shinyApp(ui = ui, server =  server)


