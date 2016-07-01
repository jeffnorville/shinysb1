# IMPREX file picker
library(shiny)
library(uuid)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose .Rdata File', accept = ".Rdata"),
      tags$hr(),
      checkboxInput('metadata', 'Metadata inside?', TRUE)
      # ,
      # radioButtons('sep', 'Separator',
      #              c(Comma=',',
      #                Semicolon=';',
      #                Tab='\t'),
      #              ','),
      # radioButtons('quote', 'Quote',
      #              c(None='',
      #                'Double Quote'='"',
      #                'Single Quote'="'"),
      #              '"')
    ),
    # sidebarPanel(
    #   "Load to Database:"
    #   ),
    mainPanel(
      # tableOutput(head('contents'))
      
      tableOutput('table1')
      # ,
      # if (is.null(generated.guid))
      #   "Choose a file"
      # else
      #   "Your GUID for this datapackage is: ", generated.guid
    )
  )
))
