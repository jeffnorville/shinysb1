# LAYOUT only

library(shiny)
library(DT)

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
             p("Third tab content")
             
    ),
    
    tabPanel("Table",
             h4("View and export filtered data"),
             p("Fourth tab content here..."),
             h2('Old faithful data'),
             dataTableOutput('mytable')
      )
  )
  
  
  )
)
