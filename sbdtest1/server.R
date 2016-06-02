#IMPREX Scoreboard
readRenviron("~/R/shinysb1/.Renviron")
REhost =     Sys.getenv('pgserver')
REport =     Sys.getenv('pgport')
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

library(shiny)
library(dplyr)
library(RPostgreSQL)
library(lazyeval)
library(ggplot2)

db <- src_postgres(dbname = REdbname,
                   host = REhost,
                   port = REport,
                   user = REuser,
                   password = REpassword)
tbl_scores <- tbl(db, "tblScores")

shinyServer(function(input, output) {

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
                     locationID == input$rtnLocid &&   # %in% # breaks interface
                     modelVariable == input$rtnModelVariable &&
                     forecastType == input$rtnForecastType &&
                     scoreType == input$rtnScoreType &&
                     leadtimeValue %in% toto # span.leadtime # note - this %in% HAS to be last criteria
    )
    
    getit <- structure(collect(remote)) #database hit
    # browser()
    
  }) #end reactive
  
  filtNAs <- reactive({
    # keep track of and exclude NAs from plot attempt
    # lead.time.units <- "days" #update from DB based on selection
    # lead.time.min <- 1
    # lead.time.max <- 90
      
    toto <- as.numeric(input$lead.times)
    all.lead.times <- as.integer(unlist(input$lead.times))  # strsplit(input$lead.times, split = ":"))
    if (all.lead.times[1] == all.lead.times[2]) {
      toto = toto 
    }
    else {
      toto = all.lead.times[1]:all.lead.times[2]
    }
    
    db.NAs <- filter(tbl_scores, 
                     scoreNA == TRUE &&
                       modelVariable == input$rtnModelVariable &&
                       forecastType == input$rtnForecastType &&
                       scoreType == input$rtnScoreType &&
                       locationID == input$rtnLocid && # %in% breaks things
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
      loc.sum <- summarySE(filtInput(), measurevar="scoreValue", 
                           groupvars=c("locationID", "leadtimeValue"), na.rm=TRUE)
      loc.sum$locationID <- as.factor(loc.sum$locationID)
    }

    if(nrow(filtInput()) == 0) {
      # print error/ warning message
      plot(1,1,col="white")
      text(1,1,"The database doesn't have information on this combination of variables (yet)")
    } else {
      
    # plot(loc.sum$leadtimeValue, loc.sum$scoreValue, col=loc.sum$locationID, 
    #      xlab = "Lead Times", ylab = "Score")
      pd <- position_dodge(0.1)
      
      ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
        geom_point(aes(color = locationID, size=3)) +
        geom_errorbar(aes(ymin=scoreValue-ci, ymax=scoreValue+ci), width=.1, color="grey", position=pd) +
        # geom_line(position=pd) +
        geom_hline(aes(yintercept=0), colour="black", linetype="dashed") + # colour="#990000"
        theme(legend.position="none") +
        xlab("Lead Times") + ylab(paste(input$rtnScoreType, " ")) # "Score"
      
      
    # ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
    #   geom_point(aes(color = locationID)) +
    #   geom_hline(aes(yintercept=0), colour="black", linetype="dashed") +
    #   xlab("Lead Times") + ylab(paste(input$rtnScoreType, " ")) # "Score"
    }
    
    
  })

  
  }) # end shinyServer
