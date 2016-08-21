# from EDIT2 http://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices

library(shiny)
runApp(list(
  ui = bootstrapPage(
    textInput("text", "Enter Formula", "a=b+c"),
    uiOutput('variables')
  ),
  server = function(input, output){
    outVar <- reactive({
      vars <- all.vars(parse(text = input$text))
      vars <- as.list(vars)
      return(vars)
    })
    
    output$variables = renderUI({
      selectInput('variables2', 'Variables', outVar())
    })
  }
))