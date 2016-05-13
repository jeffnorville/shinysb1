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

  #filter DB dataframe based on (default) selections
  filtInput <- reactive({

    remote <- filter(tbl_scores, 
                     locationID == input$rtnLocid &&
                     # leadtimeValue >= min(input$lead.times) &&
                     # leadtimeValue <= max(input$lead.times) &&
                     modelVariable == input$rtnModelVariable &&
                     scoreType == input$rtnScoreType &&
                     leadtimeValue %in% input$lead.times
                       
    )
    getit <- structure(collect(remote))
    # browser()
    
  }) #end reactive

  output$summary <- renderPrint({
    dataset <- filtInput()
    summary(dataset)
  })
  
  # output$view
  output$seriesPlot <- renderPlot({
    
    ggp <- ggplot(filtInput(), aes(x = leadtimeValue, y = (scoreValue - mean(scoreValue)))) +
      # stat_summary(fun.y="mean", geom = "bar") +
      geom_line(aes(color = scoreValue), size=1) +
      geom_hline(aes(yintercept=0), colour="black", linetype="dashed")
    
    
    # ggplot(filtInput, aes(x = LeadTime / 7, y = dateValue)) +
    #   geom_point(aes(color = scoreValue), size=5) +
    #   scale_x_continuous("Lead Time (weeks)") + scale_y_date("Months of 2005 (January omitted)") +
    #   scale_color_gradient(low="yellow", high="darkgreen")
  })

  
  }) # end shinyServer
