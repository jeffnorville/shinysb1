#test
# rm(list=objects())


# problems are with second.query()


library(shiny)
# library(plyr)
library(dplyr)
# library(RPostgreSQL)
library(lazyeval)
library(ggplot2)
library(DT)

tbl.scores <- readRDS("ehype.all.RDS")

shinyServer(function(input, output, session) {

  
    
  initial.query <- reactive({
    
    if (length(input$rtnForecastSystem) == 1){
      remote <- filter(tbl.scores, forecastSystem == input$rtnForecastSystem)
    }
    else if (length(input$rtnForecastSystem) > 1){
      remote <- filter(tbl.scores, forecastSystem %in% input$rtnForecastSystem)
    }

    remote <- filter(remote,
                     caseStudy == input$rtnCaseStudy &
                       forecastSystem == input$rtnForecastSystem 
                     # forecastRange == input$rtnForecastRangeType 
                     # scoreNA == FALSE #more like "bad data" now, contains -Infinity too
    )
    
    getit <- structure(collect(remote)) #database hit
    
  }) #end reactive
  
  # browser()
  
  # observe({
  #   print(head(initial.query))
  # })

  # define Filters
  output$Location <- renderUI({
    # if(!is.null(initial.query())) {
    if(length(initial.query()>0)) {
      # Location <- getElement(initial.query(), "locationID")
      Location <- initial.query()[[2]]
      browser()
      # Location <- c(structure(unique(Location)))
      # Location=unique(initial.query()$locationID)
      selectInput("Location","Location Filter", choices = Location, multiple=T)
      
    }
  })
  
  output$ModelVariable <- renderUI({
    ModelVariable=c(1:10) # (unique(initial.query()$modelVariable))
    selectInput("ModelVariable","Variable Filter", choices = ModelVariable, selected = "Streamflow", multiple=F)
  })
  
  output$ForecastType <- renderUI({
    ForecastType=c(11:20) # (unique(initial.query()$forecastType))
    selectInput("ForecastType","Forecast Identifier", choices = ForecastType,  multiple=T)
  })
  
  output$ScoreType <- renderUI({
    ScoreType=c(21:30) # c(unique(initial.query()$scoreType))
    selectInput("ScoreType","Score", choices = ScoreType, selected = "CRPS", multiple=T)
  })
  

  second.query <- reactive({
    if (length(input$Location)>0)
      # browser()
      second = second %>% filter(locationID %in% initial.query())

    # ScoreType
    if (length(input$ScoreType) == 1){
      second <- filter(initial.query(),
                       score.type == input$ScoreType)
    }
    else if (length(input$ScoreType) > 1){
      second <- filter(initial.query(),
                       score.type %in% input$ScoreType)
    }
    
    second <- filter(initial.query(),
                     scoreNA == FALSE & #more like "bad data" now, contains -Infinity too
                       modelVariable == input$ModelVariable &
                       scoreType == input$ScoreType
    )
    # browser()
    
    second <- structure(collect(second))
    
  }) #end reactive
  
# map
  
  
  output$table <- renderDataTable({
    if (!is.null(second.query())) {
      table <- second.query()
    } else if(!is.null(initial.query())) {
      table <- initial.query()
    }
  })
  
  output$dataset <- renderPrint({
    if (!is.null(second.query())) {
      dataset <- second.query()
    } else if(!is.null(initial.query())) {
      dataset <- initial.query()
    }
  })
  
  
  output$aPlot <- renderPlot({

    filtered.input <- second.query() 

    pd <- position_dodge(0.2)
    
loc.sum <- summarySE(filtered.input,
	       measurevar = "scoreValue",
	       groupvars = c("locationID", "leadtimeValue", "scoreType", "forecastType"), 
	       na.rm = TRUE  
)

    ggplot(loc.sum,
           aes(color = locationID, x = leadtimeValue, y = scoreValue)) +
      # geom_errorbar(aes(ymin = scoreValue - ci, ymax = scoreValue + ci), position = pd) + # , color="grey"
      geom_line() +
      geom_point(aes(color = locationID), position = pd) +
      # scale_y_continuous(breaks = c(min.LT:max.LT)) +
      xlab("Lead Times") + ylab(paste(input$rtnScoreType)) 

  })
  
  # output$seriesPlot <- renderPlot({
  #   if (nrow(second.query()) == 0 || length(second.query()) == 0) {
  #     text(1, 1, "second.query() was empty, try a different combo") # this is never hit I think
  #   } else {
  #     # we have data
  #     filtered.input <-
  #       second.query() # unneccesary step? debugging "rename" call in summarySE
  # 
  #     loc.sum$locationID <- as.factor(loc.sum$locationID)
  #     
  #     # do stats for error bars
  #     if (input$rtnForecastRangeType == "day") {
  #       loc.sum <-
  #         summarySE(
  #           filtered.input,
  #           measurevar = "scoreValue",
  #           groupvars = c("locationID", "leadtimeValue", "scoreType", "forecastType"), 
  #           na.rm = TRUE
  #         )
  #     } else { # don't get stats
  #       ci = 0
  #       se = 0
  #       N = 1
  #       loc.sum <- filtered.input
  #     }
  #     
  #     loc.sum$locationID <- as.factor(loc.sum$locationID)
  #   } # end else
  #   
  #   if(length(second.query()) == 0) {
  #     plot(1, 1, col = "white")
  #     text(1,
  #          1,
  #          "Select one or more data elements from the Filter to begin")
  #   }
  #   else if(nrow(second.query()) == 0) {
  #     # if (nrow(filtInput()) == 0) {
  #     # print error/ warning message
  #     plot(1, 1, col = "white")
  #     text(1,
  #          1,
  #          "The database doesn't have information on this combination of variables (yet)")
  #   } else {
  #     # plot(loc.sum$leadtimeValue, loc.sum$scoreValue, col=loc.sum$locationID,
  #     #      xlab = "Lead Times", ylab = "Score")
  #     
  #     pd <- position_dodge(0.2)
  #     min.LT <- min(loc.sum$leadtimeValue)
  #     max.LT <- max(loc.sum$leadtimeValue)
  #     
  #     ggplot(loc.sum,
  #            aes(color = locationID, x = leadtimeValue, y = scoreValue)) +
  #       geom_errorbar(aes(ymin = scoreValue - ci, ymax = scoreValue + ci), position = pd) + # , color="grey"
  #       geom_line() +
  #       geom_point(aes(color = locationID), position = pd) +
  #       # geom_hline(aes(yintercept=0), color="blue", linetype="dashed") +
  #       #   # if (do.facets == TRUE){facet_wrap(~ locationID) } +
  #       # scale_y_discrete() +
  #       scale_y_continuous(breaks = c(min.LT:max.LT)) +
  #       xlab("Lead Times") + ylab(paste(input$rtnScoreType)) 
  #     
  #   } # end else
  # }) # end renderPlot
  
  # # PDF export
  # plotInput <- reactive({
  #   if(input$returnpdf){
  #     pdf("plot.pdf", width=as.numeric(input$w), height=as.numeric(input$h))
  #     ### copy paste as above plot is refined
  #     ggplot(loc.sum,
  #            aes(color = locationID, x = leadtimeValue, y = scoreValue)) +
  #       geom_errorbar(aes(ymin = scoreValue - ci, ymax = scoreValue + ci), position = pd) +
  #       geom_line() +
  #       geom_point(aes(color = locationID), position = pd) +
  #       scale_y_continuous(breaks = c(min.LT:max.LT)) +
  #       xlab("Lead Times") + ylab(paste(input$rtnScoreType)) 
  #     
  #     ###
  #     dev.off()
  #   }
  #   #     g # ref active ggplot above?
  # })
  # 
  # output$myplot <- renderPlot({ plotInput() })
  # output$pdflink <- downloadHandler(
  #   filename <- "myplot.pdf",
  #   content <- function(file) {
  #     file.copy("plot.pdf", file)
  #   }
  # )
  
}) # end shinyServer
