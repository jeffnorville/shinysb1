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

    #allow ONE leadtime or any other value
    toto <- as.numeric(input$lead.times )
    all.lead.times <- as.integer(unlist(input$lead.times))  # strsplit(input$lead.times, split = ":"))
    if (all.lead.times[1] == all.lead.times[2]) {
      toto = toto 
    }
    else {
      toto = all.lead.times[1]:all.lead.times[2]
    }

    # input$rtnSummarizeBy
    
    # dplyr doesn't hit db here, hence "remote"
    remote <- filter(tbl_scores, 
                     scoreNA == FALSE &&
                     locationID == input$rtnLocid &&
                     modelVariable == input$rtnModelVariable &&
                     scoreType == input$rtnScoreType &&
                     leadtimeValue %in% toto # span.leadtime # note - this %in% HAS to be last criteria
    )
    
    getit <- structure(collect(remote)) #database hit
    # browser()
    
  }) #end reactive
  
  filtNAs <- reactive({
    # keep track of and exclude NAs from plot attempt
    toto <- as.numeric(input$lead.times)
    all.lead.times <- as.integer(unlist(input$lead.times))  # strsplit(input$lead.times, split = ":"))
    if (all.lead.times[1] == all.lead.times[2]) {
      toto = 7*toto 
    }
    else {
      toto = 7*all.lead.times[1]:7*all.lead.times[2]
    }
    db.NAs <- filter(tbl_scores, 
                     scoreNA == TRUE &&
                       locationID == input$rtnLocid &&
                       modelVariable == input$rtnModelVariable &&
                       scoreType == input$rtnScoreType &&
                       leadtimeValue %in% toto # span.leadtime # note - this %in% HAS to be last criteria
    )
    getit <- structure(collect(db.NAs)) #database hit

    # if (input$rtnTimeScale == "Monthly") {
    # mutate...
    # }

    
  })

  output$summary <- renderPrint({
    dataset <- filtInput()
    summary(dataset)
  })

  output$dataNAs <- renderPrint({
    datasetNAs <- filtNAs()
    summary(datasetNAs)
  })
  
  
  # output$view
  output$seriesPlot <- renderPlot({
    
    if(nrow(filtInput()) == 0){
      text(1,1,"filtInput() was empty, try a different combo")
    } else {
      # have data
      loc.sum <- summarySE(filtInput(), measurevar="scoreValue", groupvars=c("locationID", "leadtimeValue"), na.rm=TRUE)
      loc.sum$locationID <- as.factor(loc.sum$locationID)
    }

    if(nrow(filtInput()) == 0) {
      # print error/ warning message
      plot(1,1,col="white")
      text(1,1,"The database doesn't have information on this combination of variables (yet)")
    } else {
      
    ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue))  
    plot(loc.sum$leadtimeValue, loc.sum$scoreValue, col=loc.sum$locationID, 
         xlab = "Lead Times", ylab = "Score")
    }
    # ggplot(loc.sum, aes(leadtimeValue, scoreValue)) +
    #   geom_point(aes(color = locationID), size=2)
    
    
  })

  
  }) # end shinyServer
