#Mini DB Testdrive
readRenviron("~/R/shinysb1/.Renviron")
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

#connect to db
library(dplyr)
library(RPostgreSQL)

library(shiny)
library(datasets)

db <- src_postgres('postgres',
                   host = 'localhost',
                   port = 5432,
                   user = REuser,
                   password = REpassword)
tbl.scores <- tbl(db, "tblScores")

# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
  
  # By declaring datasetInput as a reactive expression we ensure 
  # that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers 
  #	  (it only executes a single time)
  #
  datasetInput <- reactive({
    # #for testing
    # remote <- filter(tbl.scores, locationID == "I5221010" &&
    #                    LT %in% c(1,2,3,4,5) )
    remote <- filter(tbl.scores, locationID == input$dataset &&
               leadtimeValue == input$lead.times )
    getit <- structure(collect(remote))

  })
  
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  #  1) This function is automatically called to recompute the 
  #     output 
  #  2) The new caption is pushed back to the browser for 
  #     re-display
  # 
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes.
  output$caption <- renderText({
    input$caption
  })
  
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated
  # (i.e. whenever the input$dataset changes)
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so will be re-executed whenever
  # input$dataset or input$obs is changed. 
  # note - little bug to do with dates displayed in renderTable({head()}) I think...
  output$view <- renderTable({
    head(datasetInput(), n = input$lead.times)
  })
})
