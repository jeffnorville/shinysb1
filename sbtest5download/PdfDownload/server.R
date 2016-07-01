# IMPREX download doc test

library(shiny)
shinyServer(
  function(input, output) {
    
    plotInput <- reactive({
      if(input$returnpdf){
        pdf("plot.pdf", width=as.numeric(input$w), height=as.numeric(input$h))
        plot(rnorm(sample(100:1000,1)))
        dev.off()
      }
      plot(rnorm(sample(100:1000,1)))
    })
    
    output$myplot <- renderPlot({ plotInput() })
    output$pdflink <- downloadHandler(
      filename <- "myplot.pdf",
      content <- function(file) {
        file.copy("plot.pdf", file)
      }
    )
  }
)