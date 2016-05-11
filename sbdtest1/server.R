#IMPREX Scoreboard
readRenviron("~/R/shinysb1/.Renviron")
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

library(shiny)
#connect to db
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
# print(translate_sql( tbl = tbl_scores, window = TRUE))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #filter dataframe from db based on first selections
  filtInput <- reactive({

    remote <- filter(tbl_scores, 
                     locationID == input$rtnLocid &&
                     LT %in% input$lead.times &&
                     modelVariable == input$rtnModelVariable &&
                     scoreType == input$rtnScoreType
    )
    getit <- structure(collect(remote))
  }) #end reactive

    # scoreType <- input$rtnModelVariable
    # minyear <- input$year[1]
    # maxyear <- input$year[2]

  output$summary <- renderPrint({
    dataset <- filtInput()
    summary(dataset)
  })
  
    # output$seriesPlot <- renderPlot({

    # ggplot(filt1,aes(x = LT / 7, y = dateValue)) + 
    #   geom_point(aes(color = scoreValue), size=5) +
    #   scale_x_continuous("Lead Time (weeks)") + scale_y_date("Months of 2005 (January omitted)") +
    #   scale_color_gradient(low="yellow", high="darkgreen")
    # }) 

  
    # filter stuff to draw
    # remote <- select(filter(scores, locationID == 'M0243010' && 
    #                            scoreType == 'Seasonal_LS_month' && 
    #                            LT == 4), dateValue, scoreValue:1)
    #remote2 <- collapse(remote) #unimportant for now
    #cached <- compute(remote2)

  }) # end shinyServer
