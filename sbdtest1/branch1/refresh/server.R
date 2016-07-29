#IMPREX Scoreboard v0.1
setwd("~/R/shinysb1/sbdtest1")
readRenviron("~/R/shinysb1/.Renviron")
REhost =     Sys.getenv('pgserver')
REport =     Sys.getenv('pgport')
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

library(shiny)
# library(plyr)
library(dplyr)
library(RPostgreSQL)
library(lazyeval)
library(ggplot2)
library(DT)

db <- src_postgres(
  dbname = REdbname,
  host = REhost,
  port = REport,
  user = REuser,
  password = REpassword
)
tbl_scores <- tbl(db, "tblScores")

shinyServer(function(input, output, session) {
  filtInput <- reactive({

    if (length(input$rtnLocid) == 1) {
      remote <- filter(tbl_scores,
                       locationID == input$rtnLocid)
    }
    else if (length(input$rtnLocid) > 1){
      remote <- filter(tbl_scores,
                       locationID %in% input$rtnLocid)
    }
    remote <- filter(remote,
      caseStudy == input$rtnCaseStudy &
      forecastSystem == input$rtnForecastSystem  &
      # scoreNA == FALSE & #more like "bad data" now, contains -Infinity too
      modelVariable == input$rtnModelVariable &
      forecastType == input$rtnForecastType &
      scoreType == input$rtnScoreType
    )
    getit <- structure(collect(remote)) #database hit
  }) #end reactive
  
  output$summary <- renderPrint({
    dataset <- filtInput()
    dataset <- within(dataset, rm("row.names", "datePartUnit", "forecastSystem", "forecastRange", "caseStudy",  "leadtimeUnit", "leadtimeValue"))
    summary(dataset)
  })
  
  # output$dataset <- renderDataTable({
  #   dataset <- filtInput()
  # })
  
  

  
  output$seriesPlot <- renderPlot({

    if (nrow(filtInput()) == 0 || length(filtInput()) == 0) {
    # if(length(filtInput()) == 0) {
      plot(1, 1, col = "white")
      text(1,
           1,
           "Select one or more data elements from the Filter to begin")
    }
    else if (nrow(filtInput()) == 0 || length(filtInput()) == 0) {
      text(1, 1, "filtInput() was empty, try a different combo")
    } else {
      # have data
      filtered.input <-
        filtInput() # debug rename in summarySE
      loc.sum <- # filtered.input
        summarySE(
          filtered.input,
          measurevar = "scoreValue",
          groupvars = c("locationID", "leadtimeValue", "scoreType", "forecastType"), # "locationID", "leadtimeValue"
          na.rm = TRUE
        )
      loc.sum$locationID <- as.factor(loc.sum$locationID)
      na.count <-
        sum(filtered.input$scoreNA) # should report to user since value hidden by summarySE()
    } # end else
    
    # if(nrow(filtInput()) == 0 || length(filtInput()) == 0) {
    if (nrow(filtInput()) == 0) {
      # print error/ warning message
      plot(1, 1, col = "white")
      text(1,
           1,
           "The database doesn't have information on this combination of variables (yet)")
    } else {
      # plot(loc.sum$leadtimeValue, loc.sum$scoreValue, col=loc.sum$locationID,
      #      xlab = "Lead Times", ylab = "Score")
      
      pd <- position_dodge(0.2)
      # min.LT <- min(loc.sum$leadtimeValue)
      # max.LT <- max(loc.sum$leadtimeValue)
      
      ggplot(loc.sum,
             aes(color = locationID, x = leadtimeValue, y = scoreValue)) +
        # geom_errorbar(aes(ymin = scoreValue - ci, ymax = scoreValue + ci), position = pd) + # , color="grey"
        geom_line() +
        geom_point(aes(color = locationID)) + # , position = pd
        # geom_hline(aes(yintercept=0), color="blue", linetype="dashed") +
        #   # if (do.facets == TRUE){facet_wrap(~ locationID) } +
        # scale_y_discrete() +
        # scale_y_continuous(breaks = c(min.LT:max.LT)) +
        xlab("Lead Times") + ylab(paste(input$rtnScoreType))
      
    } # end else
  }) # end renderPlot
  
  # output$facetPlot({
  # 
  #   #   ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
  #   #     geom_point(aes(color = locationID, size=2)) +
  #   #     facet_wrap(~ locationID) +
  #   #     geom_errorbar(aes(ymin=scoreValue-ci, ymax=scoreValue+ci), width=.1, color = group, position=pd) +
  #   #     geom_hline(aes(yintercept=0), colour="black", linetype="dashed") +
  #   #     xlab("Lead Times") + ylab(paste(input$rtnScoreType, " ")) # "Score"
  #   
  #   
  #   
  # })
  
}) # end shinyServer
