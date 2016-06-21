# http://zevross.com/blog/2016/04/19/r-powered-web-applications-with-shiny-a-tutorial-and-cheat-sheet-with-40-example-apps/#dynamic-ui-with-renderui-and-outputui


server <- function(input, output, session) {
  
  # return a list of UI elements
  output$my_output_UI <- renderUI({
    
    list(
      h4(style = "color:blue;", "Add stuff to this list"),
      selectInput(inputId = "myselect", label="", choices = selections)
    )
  })
  
  # initial selections
  selections <- c("Brier Score", "CRPS", "CRPS Score")
  
  # use observe event to notice when the user clicks the button
  # update the selection list. Note the double assignment <<-
  observeEvent(input$mybutton,{
    selections <<- c(input$mytext, selections)
    updateSelectInput(session, "myselect", choices = selections, selected = selections[1])
  })
  
}

ui <- basicPage(
  
  h3("testing renderUI and uiOutput"),
  uiOutput("my_output_UI"),
  textInput("mytext", ""),
  actionButton("mybutton", "Click to add to Selections")
  
)

shinyApp(ui = ui, server = server)