

library(shiny)
require(stats); require(graphics)
library(leaflet)
require("uuid")


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

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
  
  #map
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })  
  
  # upload
  values <- reactiveValues(
    file1 = NULL
  )
  observe({
    input$clearFile1
    input$uploadFormat
    values$file1 <- NULL
  })
  observe({
    values$file1 <- input$file1
    # browser()
  })
  output$summary <- renderText({
    return(paste("Uploaded file: ", values$file1$name))
  })
  output$resettableInput <- renderUI({
    input$clearFile1
    input$uploadFormat
    
    fileInput('file1', NULL, width="80%")
  })
    
  
  
})
