#IMPREX Scoreboard
readRenviron("~/R/shinysb1/.Renviron")
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

library(shiny)
library(dplyr)
library(RPostgreSQL)
library(lazyeval)
library(ggplot2)

db <- src_postgres('postgres',
                   host = 'localhost',
                   port = 5432,
                   user = REuser,
                   password = REpassword)
tbl_scores <- tbl(db, "tblScores")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #consider doing an initial data reduce outside reactive()
  
  #filter DB dataframe based on (default) selections
  filtInput <- reactive({

    #if input$lead.times
    tot <- input$lead.times
    toto <- as.numeric(input$lead.times) #debug
    all.lead.times <- as.integer(unlist(input$lead.times))  # strsplit(input$lead.times, split = ":"))
    if (all.lead.times[1] == all.lead.times[2]) {
      toto = toto
    }
    else {
      toto = all.lead.times[1]:all.lead.times[2]
    }

    remote <- filter(tbl_scores, 
                     locationID == input$rtnLocid &&
                     modelVariable == input$rtnModelVariable &&
                     scoreType == input$rtnScoreType &&
                     leadtimeValue %in% toto # span.leadtime
    )
    getit <- structure(collect(remote))
    
    #local <- collect(reduced)
    # browser()
    
  }) #end reactive

  output$summary <- renderPrint({
    dataset <- filtInput()
    summary(dataset)
  })
  
  
  # output$view
  output$seriesPlot <- renderPlot({
    
    loc.sum <- summarySE(filtInput(), measurevar="scoreValue", groupvars=c("locationID", "leadtimeValue"), na.rm=TRUE)
    
    
    loc.sum$locationID <- as.factor(loc.sum$locationID)
    plot(loc.sum$leadtimeValue, loc.sum$scoreValue, col=loc.sum$locationID)
    # ggplot(loc.sum, aes(leadtimeValue, scoreValue)) +
    #   geom_point(aes(color = locationID), size=2)
    
    
  })

  
  }) # end shinyServer
