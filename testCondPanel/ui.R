# from http://stackoverflow.com/questions/33951152/hiding-or-showing-shiny-elements-without-pressing-submit

library(shiny)
shinyUI(fluidPage(
  titlePanel("submitButton example"),
  fluidRow(
    column(3, wellPanel(
      sliderInput("n", "N:", min = 10, max = 1000, value = 200,
                  step = 10),
      checkboxInput("checkbox_1", label = "Message", value = FALSE),
      uiOutput('test')
      
      ,actionButton("Submit",label ="Submit" )
    )),
    column(6,
           plotOutput("plot1", width = 400, height = 300),
           verbatimTextOutput("text")
    )
  )
))