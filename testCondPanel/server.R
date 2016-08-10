shinyServer(function(input, output,session) {
  
  
  output$test=renderUI({ 
    if(input$checkbox_1==T){
      list(textInput("text", "Text:", "text here"),
           numericInput("num","num",0), numericInput("num1","num1",0))}
  })
  
  observeEvent(input$Submit,{
    output$plot1 <- renderPlot({
      hist(rnorm(isolate(input$n)))
    })
    output$text <- renderText({
      paste("Input text is:", isolate(input$text))
    })
    
  })
  
  
})