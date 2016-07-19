
# LAYOUT only

library(shiny)
library(DT)
library(ggplot2)
library(leaflet)
library(uuid)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  img(src = "imprex.png", height = 100),
  
  titlePanel("Scoreboard"),
  
  navbarPage(title=div("Forecast Verification Tool")
  ),
  tabsetPanel(type = "tabs", 
              
    # new layout
    tabPanel("Main Plot",
             h4("Select and filter data to create "),
             p("Create a plot by selecting data"),
             plotOutput("timePlot")
    ),
    
    tabPanel("Interact with plot",
             h4("Load new data"),
             p("Second tab content"),
             plotOutput("distPlot"),
             sliderInput("bins",
                         "Number of bins:",
                         min = 1,
                         max = 50,
                         value = 30)
    ),
    
    tabPanel("Map",
             h4("Map of selected data"),
             p("Hit the button to get new coordinates (au hazard)"),
             leafletOutput("mymap"),
             p(),
             actionButton("recalc", "New points")
    ),
    
    tabPanel("Table",
             h4("View and export filtered data"),
             p("Fourth tab content here..."),
             h2('Old faithful data'),
             dataTableOutput('mytable')
      ),
    
    tabPanel("Upload",
             h4("Add your score RDS file here"),
             p("Instructions here..."),
             HTML("<button id='clearFile1' class='action-button clearButton'>Clear</button>"),
             uiOutput('resettableInput'),
             
             selectInput('uploadFormat', label = "Select upload format", 
                         choices = c(
                           "RDS" = 'rds',
                           "csv" = 'csv',
                           "tab-delim" = 'txt'),
                         selected = 'rds'),
             h4("Summary"),
             verbatimTextOutput("summary")
             
     )
   ) #tabSetPanel 
  
  ) # fluidPage
) # shinyUI
