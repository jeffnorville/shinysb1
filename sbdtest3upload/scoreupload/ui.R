# IMPREX file picker
library(shiny)
library(uuid)
library(DT)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose .Rdata File', accept = ".Rdata"),
      tags$hr(),
      checkboxInput('metadata', 'Metadata inside?', TRUE)
      ,
      #conditionalInput
      
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
      verbatimTextOutput('summary'),

      tableOutput('contents')
      # DT::dataTableOutput("contents")
      
      # if (is.null(generated.guid))
      #   "Choose a file"
      # else
      #   "Your GUID for this datapackage is: ", input$guid
    )
  )
))
