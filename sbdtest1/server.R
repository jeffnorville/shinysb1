#IMPREX Scoreboard
readRenviron("~/R/shinysb1/.Renviron")
REhost =     Sys.getenv('pgserver')
REport =     Sys.getenv('pgport')
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

library(shiny)
library(plyr); library(dplyr)
library(RPostgreSQL)
library(lazyeval)
library(ggplot2)
library(DT)

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

    # if 1 locID is chose ... "remote" is the 'local' cursor, hasnt hit db yet
    if(length(input$rtnLocid) == 1) {
      remote <- filter(tbl_scores, 
                       locationID == input$rtnLocid 
      )
    }
    else { # more than one locID chosen
      remote <- filter(tbl_scores, 
                       locationID %in% input$rtnLocid
      )
    }
    
    do.facets = FALSE
    if (input$wants.facets == "TRUE") {
      do.facets = TRUE    
    }
    else {
      do.facets = FALSE
    }
    
    summarize.by <- "All"
    # input$rtnTimeScale
    if (input$rtnTimeScale == "All"){
    } else if (input$rtnTimeScale == "Month"){
      summarize.by <- c(1:12)
    } else if (input$rtnTimeScale == "Spring (MAM)"){
      summarize.by <- c(3:5) # "Spring (MAM)"
    } else if (input$rtnTimeScale == "Winter (DJF)"){
      summarize.by <- c(12,1:2) # "Winter (DJF)"
    } else if (input$rtnTimeScale == "Monsoon (JJAS)"){
      summarize.by <- c(6:9) # "Monsoon (JJAS)"
    } else if (input$rtnTimeScale == "Year"){
      summarize.by <- c(1:12) # "Year"
    } else { # default
      # summarize.by <- "All"
    }
    # timescale may be: All; some combination of months (Seasonal); or ?
    if(input$rtnTimeScale != "All") {
      remote$month <- format(remote$dateValue, "%m")
      remote <- filter(tbl_scores,
                       months %in% summarize.by
      )
    }

    #     remote <- filter(tbl_scores, 
    remote <- filter(remote, 
                     scoreNA == FALSE &&
                     modelVariable == input$rtnModelVariable &&
                     forecastType == input$rtnForecastType &&
                     scoreType == input$rtnScoreType &&
                     leadtimeValue %in% toto # span.leadtime # note - this %in% HAS to be last criteria
    )

    getit <- structure(collect(remote)) #database hit
    
    # browser()
    
  }) #end reactive
  
  
  
# DELETE?
  # filtNAs <- reactive({
  #   # keep track of and exclude NAs from plot attempt
  #   # lead.time.units <- "days" #update from DB based on selection
  #   # lead.time.min <- 1
  #   # lead.time.max <- 90
  #     
  #   toto <- as.numeric(input$lead.times)
  #   all.lead.times <- as.integer(unlist(input$lead.times))  # strsplit(input$lead.times, split = ":"))
  #   if (all.lead.times[1] == all.lead.times[2]) {
  #     toto = toto 
  #   }
  #   else {
  #     toto = all.lead.times[1]:all.lead.times[2]
  #   }
  # 
  #   # find solution to count NAs for this dataset other than doubling the query...    
  #   db.NAs <- filter(tbl_scores, 
  #                    scoreNA == TRUE &&
  #                      locationID %in% input$rtnLocid &&   # %in% # breaks interface
  #                      modelVariable == input$rtnModelVariable &&
  #                      forecastType == input$rtnForecastType &&
  #                      # summarizeByTime == input$rtnTimeScale &&
  #                      scoreType == input$rtnScoreType &&
  #                      leadtimeValue %in% toto # span.leadtime # note - this %in% HAS to be last criteria
  #   )
  #   getit <- structure(collect(db.NAs)) #database hit
  # })

    # if (input$rtnTimeScale == "Monthly") {
    # mutate...
    # }

  output$summary <- renderPrint({
    dataset <- filtInput()
    summary(dataset)
  })

  output$dataset <- renderPrint({
    dataset <- filtInput()
  })
  
  
  # output$dataNAs <- renderPrint({
  #   datasetNAs <- filtNAs()
  #   summary(datasetNAs)
  # })
  
  # a.thing <- output$dataNAs()

  if (do.facets == TRUE) {
    output$seriesPlot <- renderPlot({
      
    ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
      geom_point(aes(color = locationID, size=2)) +
      facet_wrap(~ locationID) +
      geom_errorbar(aes(ymin=scoreValue-ci, ymax=scoreValue+ci), width=.1, color = group, position=pd) + 
      geom_hline(aes(yintercept=0), colour="black", linetype="dashed") +
      xlab("Lead Times") + ylab(paste(input$rtnScoreType, " ")) # "Score"
    })
    
  } else {

    output$seriesPlot <- renderPlot({
      
      if(nrow(filtInput()) == 0){
        text(1,1,"filtInput() was empty, try a different combo")
      } else {  # have data
        filtered.input <- filtInput() # unneccesary step? debugging "rename" call in summarySE
        loc.sum <- summarySE(filtered.input, measurevar="scoreValue", 
                             groupvars=c("locationID", "leadtimeValue"), na.rm=TRUE)
        loc.sum$locationID <- as.factor(loc.sum$locationID)
      }
  
      na.count <- sum(filtered.input$scoreNA) # should report to user since value hidden by summarySE()
  
      # if(nrow(filtInput()) == 0 || length(filtInput()) == 0) {
      if(nrow(filtInput()) == 0) {
        # print error/ warning message
        plot(1,1,col="white")
        text(1,1,"The database doesn't have information on this combination of variables (yet)")
      } else {
        
      # plot(loc.sum$leadtimeValue, loc.sum$scoreValue, col=loc.sum$locationID, 
      #      xlab = "Lead Times", ylab = "Score")
        
        group <- c(1:length(loc.sum$locationID)) # not right
        pd <- position_dodge(0.1) # not working
  
        # works, but slow and not very interesting (unless y-axis transformed?)
        # ggplot(filtered.input, aes(x = leadtimeValue, y = scoreValue, group=leadtimeValue, fill = locationID)) +
        #   geom_boxplot() +
        #   facet_wrap(~ locationID) +
        #   xlab("Lead Times") + ylab("Score") 
          ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
            geom_point(aes(color = locationID, size=2)) + # works
            geom_errorbar(aes(ymin=scoreValue-ci, ymax=scoreValue+ci), width=.1, color = group, position=pd) + 
            geom_hline(aes(yintercept=0), colour="black", linetype="dashed") + # colour="#990000"
            xlab("Lead Times") + ylab(paste(input$rtnScoreType, " ")) # "Score"
          
      # ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
      #   geom_point(aes(color = locationID)) +
      #   geom_hline(aes(yintercept=0), colour="black", linetype="dashed") +
      #   xlab("Lead Times") + ylab(paste(input$rtnScoreType, " ")) # "Score"
      }
  }) # end renderPlot
}
  
  }) # end shinyServer
