#IMPREX Scoreboard
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
tbl.scores <- tbl(db, "tblScores")
tbl.dataload <- tbl(db, "tblDataLoad")
tbl.interface <- tbl(db, "tblInterface")

shinyServer(function(input, output, session) {
  
  
  #filter DB dataframe based on (default) selections
  filtInput <- reactive({

    ############### first select
    
    # ForecastSystem
    if (length(input$rtnForecastSystem) == 1){
      remote <- filter(tbl.scores, forecast.system == input$rtnForecastSystem)
    }
    else if (length(input$rtnForecastSystem) > 1){
      remote <- filter(tbl.scores, forecast.system %in% input$rtnForecastSystem)
    }
    # ForecastType
    if (length(input$rtnForecastType) == 1){
      remote <- filter(tbl.scores, forecastType == input$rtnForecastType)
    }
    else if (length(input$rtnForecastType) > 1){
      remote <- filter(tbl.scores, forecastType %in% input$rtnForecastType)
    }

    remote <- filter(remote,
                       caseStudy == input$rtnCaseStudy &
                       forecastSystem == input$rtnForecastSystem &
                       forecastRange == input$rtnForecastRangeType 
                       # scoreNA == FALSE #more like "bad data" now, contains -Infinity too
    )
    
    getit <- structure(collect(remote)) #database hit

    ############### then filter by location, scoreType, etc
    # locationID
    
    if (length(input$rtnLocid) == 1) {
      filtered.cursor <- filter(getit,
                       locationID == input$rtnLocid)
    }
    else  if (length(input$rtnLocid) > 1) {
      # more than one locID chosen
      filtered.cursor <- filter(getit,
                       locationID %in% input$rtnLocid)
    }
    # ScoreType    
    if (length(input$rtnScoreType) == 1){
      filtered.cursor <- filter(getit, score.type == input$rtnScoreType)
    }
    else if (length(input$rtnScoreType) > 1){
      filtered.cursor <- filter(getit, score.type %in% input$rtnScoreType)
    }
    
    remote <- filter(getit,
      scoreNA == FALSE & #more like "bad data" now, contains -Infinity too
      modelVariable == input$rtnModelVariable &
      scoreType == input$rtnScoreType
    )
    
    filtered.cursor <- structure(collect(getit)) 
    
  }) #end reactive

  # map
  # points <- eventReactive(input$recalc, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)
  # 
  # output$mymap <- renderLeaflet({
  #   leaflet() %>%
  #     addProviderTiles("Stamen.TonerLite",
  #                      options = providerTileOptions(noWrap = TRUE)
  #     ) %>%
  #     addMarkers(data = points())
  # })
  # end map
  

  output$table <- renderDataTable({
    table <- filtInput()
    # head(dataset) # was summary
  })
  
  output$dataset <- renderPrint({
    dataset <- filtInput()
  })
  
  
  # if (do.facets == TRUE) {
  #   output$seriesPlot <- renderPlot({
  #
  #   ggplot(loc.sum, aes(x = leadtimeValue, y = scoreValue ) ) +
  #     geom_point(aes(color = locationID, size=2)) +
  #     facet_wrap(~ locationID) +
  #     geom_errorbar(aes(ymin=scoreValue-ci, ymax=scoreValue+ci), width=.1, color = group, position=pd) +
  #     geom_hline(aes(yintercept=0), colour="black", linetype="dashed") +
  #     xlab("Lead Times") + ylab(paste(input$rtnScoreType, " ")) # "Score"
  #   })
  #
  # } else {
  
  output$seriesPlot <- renderPlot({
    if (nrow(filtInput()) == 0 || length(filtInput()) == 0) {
      text(1, 1, "filtInput() was empty, try a different combo") # this is never hit I think
    } else {
      # we have data
      filtered.input <-
        filtInput() # unneccesary step? debugging "rename" call in summarySE
      
      # do stats for error bars
      if (input$rtnForecastRangeType == "days") {
      loc.sum <-
        summarySE(
          filtered.input,
          measurevar = "scoreValue",
          groupvars = c("locationID", "leadtimeValue", "scoreType", "forecastType"), 
          na.rm = TRUE
        )
      } else { # don't get stats
        ci = 0
        se = 0
        N = 1
        loc.sum <- filtered.input
      }
      
      loc.sum$locationID <- as.factor(loc.sum$locationID)
      

    } # end else
    
    if(length(filtInput()) == 0) {
      plot(1, 1, col = "white")
      text(1,
           1,
           "Select one or more data elements from the Filter to begin")
      }
    else if(nrow(filtInput()) == 0) {
    # if (nrow(filtInput()) == 0) {
      # print error/ warning message
      plot(1, 1, col = "white")
      text(1,
           1,
           "The database doesn't have information on this combination of variables (yet)")
    } else {
      # plot(loc.sum$leadtimeValue, loc.sum$scoreValue, col=loc.sum$locationID,
      #      xlab = "Lead Times", ylab = "Score")
      
      pd <- position_dodge(0.2)
      min.LT <- min(loc.sum$leadtimeValue)
      max.LT <- max(loc.sum$leadtimeValue)
      
      ggplot(loc.sum,
             aes(color = locationID, x = leadtimeValue, y = scoreValue)) +
        geom_errorbar(aes(ymin = scoreValue - ci, ymax = scoreValue + ci), position = pd) + # , color="grey"
        geom_line() +
        geom_point(aes(color = locationID), position = pd) +
        # geom_hline(aes(yintercept=0), color="blue", linetype="dashed") +
        #   # if (do.facets == TRUE){facet_wrap(~ locationID) } +
        # scale_y_discrete() +
        scale_y_continuous(breaks = c(min.LT:max.LT)) +
        xlab("Lead Times") + ylab(paste(input$rtnScoreType)) 
      
    } # end else
  }) # end renderPlot
  
  # PDF export
  plotInput <- reactive({
    if(input$returnpdf){
      pdf("plot.pdf", width=as.numeric(input$w), height=as.numeric(input$h))
      ### copy paste as above plot is refined
      ggplot(loc.sum,
             aes(color = locationID, x = leadtimeValue, y = scoreValue)) +
        geom_errorbar(aes(ymin = scoreValue - ci, ymax = scoreValue + ci), position = pd) +
        geom_line() +
        geom_point(aes(color = locationID), position = pd) +
        scale_y_continuous(breaks = c(min.LT:max.LT)) +
        xlab("Lead Times") + ylab(paste(input$rtnScoreType)) 
      
      ###
      dev.off()
    }
    # plot(rnorm(sample(100:1000,1)))
    g # ref active ggplot above?
  })
  
  output$myplot <- renderPlot({ plotInput() })
  output$pdflink <- downloadHandler(
    filename <- "myplot.pdf",
    content <- function(file) {
      file.copy("plot.pdf", file)
    }
  )
  
  
  
  
}) # end shinyServer
