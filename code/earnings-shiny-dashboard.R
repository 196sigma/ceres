## Reginald Edwards
## 03 July 2018
##
## An interative dashboard to view properties of the distribution of earnings
## TODO:
##  - use ROE and ROA instead of raw net income
##  - beautify graphs

rm(list=ls())
gc()
library(shiny)

load("../0_datasets/comp_fundq.RData")
source("../0_code/useful-fn.R")

X <- comp.fundq[, c("gvkey", "fyearq", "datacqtr", "qtr", "datafqtr", "niq", "sich")]
X$sic2 <- trunc(X$sich/100)
X <- wins_df(X = X, var = "niq", firmid = "gvkey", yearid = "qtr")
industries <- unique(X$sic2)
industries <- industries[!is.na(industries)]

# Use a fluid Bootstrap layout
ui <- fluidPage(
  
  titlePanel("Net Income Distribution"),
  
  sidebarLayout(
    
    # Define the sidebar with inputs
    sidebarPanel(
      sliderInput(inputId = "year", label = "Choose year", value = 1990, min = 1962, max = 2018, step = 1),
      
      checkboxInput(inputId = "density", label = strong("Show density estimate"), value = FALSE),
      
      # Display this only if the density is shown
      conditionalPanel(condition = "input.density == true", 
        sliderInput(inputId = "bw_adjust", 
        label = "Bandwidth adjustment:", 
        min = 0.2, max = 2, value = 1, step = 0.2)),
        
      checkboxGroupInput(inputId = "industry", label = "Choose industry", selected = 36, choices = industries),
    
      hr(),
      helpText("Data from Compustat")),
  
    #mainPanel(plotOutput(outputId = "hist"))
    
    #mainPanel("main panel", 
     #         fluidRow(
      #          splitLayout(
       #           cellWidths = c("50%", "50%"), 
        #          plotOutput("hist"), 
         #         plotOutput("tsplot"))))
    
    mainPanel("main panel", plotOutput("hist"),  plotOutput("tsplot"))
  )
)

server <-  function(input, output){
  output$hist <- renderPlot({
    o <- which(X$fyearq == input$year & X$sic2 %in% input$industry)
    title = paste(length(o), " Firms")
    hist(X[o, "niq"], main = title, xlab = "Net Income", probability = TRUE, breaks = 40)
    
    if (input$density) {
      dens <- density(X[o, "niq"], adjust = input$bw_adjust)
      lines(dens, col = "blue")
    }
  })
  
  output$tsplot <- renderPlot({
    o2 <- which(X$sic2 %in% input$industry)
    niq.agg <- aggregate(X[o2, "niq"], by = list(X[o2, "datacqtr"]), FUN = sum, na.rm = TRUE)
    names(niq.agg) <- c("datacqtr", "niq.sum")
    niq.agg <- niq.agg[complete.cases(niq.agg), ]
    
    plot(niq.agg$niq.sum, type = 'l', main = "Total Net Income")
  })
  
}

shinyApp(ui = ui, server =  server)

