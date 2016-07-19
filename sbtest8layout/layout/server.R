

library(shiny)
require(stats); require(graphics)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$mytable = renderDataTable({
    faithful
  })
  
  output$timePlot <- renderPlot({

    f.tit <-  "faithful data: Eruptions of Old Faithful"
    plot(faithful[, -3], main = f.tit,
         xlab = "Eruption time (min)",
         ylab = "Waiting time to next eruption (min)")
    lines(lowess(faithful$eruptions, faithful$waiting, f = 2/3, iter = 3),
          col = "red")
    
  })
     
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
