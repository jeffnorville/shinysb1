

library(shiny)
require("uuid")

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    ),
    # sidebarPanel(
    #   "Load to Database:"
    #   ),
    mainPanel(
      # tableOutput(head('contents'))
      tableOutput('contents')
      # ,
      # if (is.null(generated.guid))
      #   "Choose a file"
      # else
      #   "Your GUID for this datapackage is: ", generated.guid
    )
  )
))
