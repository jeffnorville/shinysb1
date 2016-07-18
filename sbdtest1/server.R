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
tbl_scores <- tbl(db, "tblScores")
# do.facets = FALSE

shinyServer(function(input, output) {
  #filter DB dataframe based on (default) selections
  filtInput <- reactive({
    #allows ONE leadtime (LT=1:1), or a combination
    # toto <- as.numeric(input$lead.times)
    # all.lead.times <-
    #   as.integer(unlist(input$lead.times))  # strsplit(input$lead.times, split = ":"))
    # 
    # if (all.lead.times[1] == all.lead.times[2]) {
    #   toto = toto
    # }
    # else {
    #   toto = all.lead.times[1]:all.lead.times[2]
    # }
    
    # if 1 locID is chosen ... "remote" is the 'local' cursor, hasnt hit db yet
    if (is.null(input$rtnLocid)) {
      return(NULL)
    }
    else if (length(input$rtnLocid) == 1) {
      remote <- filter(tbl_scores,
                       locationID == input$rtnLocid)
    }
    else {
      # more than one locID chosen
      remote <- filter(tbl_scores,
                       locationID %in% input$rtnLocid)
    }
    
    # if (input$wants.facets == "TRUE") {
    #   do.facets = TRUE
    # }
    # else {
    #   do.facets = FALSE
    # }
    
    # summarize.by <- "All"

    # timescale may be: All; some combination of months (Seasonal); or ?
    # if (input$rtnTimeScale != "All") {
    #   remote$month <- format(remote$dateValue, "%m")
    #   remote <- filter(tbl_scores,
    #                    months %in% summarize.by)
    # }
    #     remote <- filter(tbl_scores,
    
    remote <- filter(remote,
      scoreNA == FALSE & #more like "bad data" now, contains -Infinity too
      modelVariable == input$rtnModelVariable &
        forecastType == input$rtnForecastType &
        scoreType == input$rtnScoreType # &
        # leadtimeValue %in% toto # note - this %in% HAS to be last criteria
    )
    
    getit <- structure(collect(remote)) #database hit
    
  }) #end reactive
  
  # if (input$rtnTimeScale == "Monthly") {
  # mutate...
  # }
  
  output$summary <- renderPrint({
    dataset <- filtInput()
    head(dataset) # was summary
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
      text(1, 1, "filtInput() was empty, try a different combo")
    } else {
      # have data
      filtered.input <-
        filtInput() # unneccesary step? debugging "rename" call in summarySE
      loc.sum <-
        summarySE(
          filtered.input,
          measurevar = "scoreValue",
          groupvars = c("locationID", "leadtimeValue"),
          na.rm = TRUE
        )
      loc.sum$locationID <- as.factor(loc.sum$locationID)
      na.count <-
        sum(filtered.input$scoreNA) # should report to user since value hidden by summarySE()
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
      
      g <- ggplot(loc.sum,
             aes(color = locationID, x = leadtimeValue, y = scoreValue)) +
        geom_errorbar(aes(ymin = scoreValue - ci, ymax = scoreValue + ci), position = pd) + # , color="grey"
        geom_line() +
        geom_point(aes(color = locationID), position = pd) +
        # geom_hline(aes(yintercept=0), color="blue", linetype="dashed") +
        #   # if (do.facets == TRUE){facet_wrap(~ locationID) } +
        # scale_y_discrete() +
        scale_y_continuous(breaks = c(min.LT:max.LT)) +
        xlab("Lead Times") + ylab(paste(input$rtnScoreType)) # , " removed ", na.count, " NAs"))
      
    } # end else
  }) # end renderPlot
  
  # PDF export
  plotInput <- reactive({
    if(input$returnpdf){
      pdf("plot.pdf", width=as.numeric(input$w), height=as.numeric(input$h))
      plot(rnorm(sample(100:1000,1)))
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
