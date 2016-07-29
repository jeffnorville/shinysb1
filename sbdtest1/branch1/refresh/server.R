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
    validate(
      need(input$rtnLocid != "", "Please select at least one location")
    )
    
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
  
  filtSkillScores <- reactive({
    validate(
      need(input$rtnLocid != "", "Please select at least one location and one or more scores (below)")
    )
    
    list.skill.scores <- input$rtnAllScoreTypes

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
                       scoreType %in% list.skill.scores
    )
    getit <- structure(collect(remote)) #database hit
  }) #end reactive
  
  output$summary <- renderPrint({
    dataset <- filtInput()
    dataset <- within(
      dataset, rm("row.names", "datePartUnit", "forecastSystem", "forecastRange", "caseStudy",  "leadtimeUnit", "leadtimeValue")
      )
    summary(dataset)
  })
  
  output$seriesPlot <- renderPlot({
    if (nrow(filtInput()) == 0 || length(filtInput()) == 0) {
    # if(length(filtInput()) == 0) {
      plot(1, 1, col = "white")
      text(1, 1,"Select one or more data elements from the Filter to begin")
    }
    else if (nrow(filtInput()) == 0 || length(filtInput()) == 0) {
      text(1, 1, "filtInput() was empty, try a different combo")
    } else {
      # have data
      filtered.input <- filtInput() # debug rename in summarySE
      loc.sum <- summarySE(
          filtered.input,
          measurevar = "scoreValue",
          groupvars = c("locationID", "leadtimeValue", "scoreType", "forecastType"), 
          na.rm = TRUE
        )
      loc.sum$locationID <- as.factor(loc.sum$locationID)
      na.count <- sum(filtered.input$scoreNA) 
    } # end else
    
    # if(nrow(filtInput()) == 0 || length(filtInput()) == 0) {
    if (nrow(filtInput()) == 0) {
      # print error/ warning message
      plot(1, 1, col = "white")
      text(1, 1, "The database doesn't have information on this combination of variables (yet)")
    } else {
      pd <- position_dodge(0.2)
      ggplot(loc.sum,
             aes(color = locationID, x = leadtimeValue, y = scoreValue)) +
        # geom_errorbar(aes(ymin = scoreValue - ci, ymax = scoreValue + ci), position = pd) + # , color="grey"
        geom_line() +
        geom_point(aes(color = locationID)) + # , position = pd
        xlab("Lead Times") + 
        ylab(paste(input$rtnScoreType))

    } # end else
  }) # end renderPlot
  
  output$facetPlot <- renderPlot({

    if (nrow(filtSkillScores()) == 0 || length(filtSkillScores()) == 0) {
      plot(1, 1, col = "white")
      text(1,1,"Select one or more data elements from the Filter to begin")
    }
    else if (nrow(filtSkillScores()) == 0 || length(filtSkillScores()) == 0) {
      text(1, 1, "filtInput() was empty, try a different combo")
    } else {
      # have data
      filtered.input <- filtSkillScores() # debug rename in summarySE
      loc.sum <- summarySE(
          filtered.input,
          measurevar = "scoreValue",
          groupvars = c("locationID", "leadtimeValue", "scoreType", "forecastType"), # "locationID", "leadtimeValue"
          na.rm = TRUE
        )
      loc.sum$locationID <- as.factor(loc.sum$locationID)
      na.count <- sum(filtered.input$scoreNA) # should report to user since value hidden by summarySE()
    }

    if (nrow(filtSkillScores()) == 0) {
      plot(1, 1, col = "white")
      text(1, 1, "The database doesn't have information on this combination of variables (yet)")
    } else {

      # pd <- position_dodge(0.2)
      loc.count <- length(loc.sum$locationID)

      ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
        geom_point(aes(color = locationID)) +
        # facet_wrap(scoreType ~ locationID, nrow = loc.count) +
        facet_grid(scoreType ~ locationID) + #margin = TRUE
        # geom_hline(aes(yintercept=0), colour="black", linetype="dashed") +
        xlab("Lead Times") +
        ylab("Scores")
    }
    
  })
  
# main plot
  output$downloadMainPlot <- downloadHandler(
    
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = seriesPlot(), device = "png")
    }
  )  
  
}) # end shinyServer

