#ini file
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

  #using fulldb too big - stick with subset until figure out dplyr issue
  #get the filters to df
  filtInput <- reactive({
    scoreType <- input$ctlScrtype
    locationID <- input$ctlLocid
    # minyear <- input$year[1]
    # maxyear <- input$year[2]
    
  
  # Apply filters
  sm <- db2005 %>%
    filter(
      ScoreType == input$ctlScrtype,
      Location == input$ctlLocid
      # Year <= maxyear,
    ) %>%
#    arrange(locationID)

    output$distPlot <- renderPlot({
    ggplot(sm2,aes(x = LT / 7, y = dateValue)) + 
      geom_point(aes(color = scoreValue), size=5) +
      scale_x_continuous("Lead Time (weeks)") + scale_y_date("Months of 2005 (January omitted)") +
      scale_color_gradient(low="yellow", high="darkgreen")
    }) #end reactive bit

  })
  
    #db
    # scores <- tbl(db, "tblScores")
    # print(translate_sql(rank(), tbl = "tblScores", window = TRUE))
    # #controls  unique(as.character(mpg$manufacturer
    # scrflav <- collect(order(unique(as.character(scores$scoreType))))
    # loc <- unique(as.character(scores$locationID))
    
    
    # filter stuff to draw
    # remote <- select(filter(scores, locationID == 'M0243010' && 
    #                            scoreType == 'Seasonal_LS_month' && 
    #                            LT == 4), dateValue, scoreValue:1)
    #remote2 <- collapse(remote) #unimportant for now
    #cached <- compute(remote2)
    # local  <- collect(remote) #finally hits db with SELECT WHERE


  })

#on unload --- disonnect???


