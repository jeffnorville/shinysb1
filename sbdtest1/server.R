#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#connect to db
library(dplyr)
library(RPostgreSQL)
library(lazyeval)
library(ggplot2)

# db <- src_postgres('postgres',
#                    host = 'localhost',
#                    port = 5432,
#                    user = 'postgres', 
#                    password = 'irstea')
# tbl_scores <- tbl(db, "tblScores")
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
    arrange(locationID)
#    sm <- subset(fulldb, locationID %in% c(ctlLocid))
    #sm <- subset(fulldb, locationID %in% c('S2242510'))
#    sm2 <- subset(sm, dateValue > "2005-01-01" & dateValue < "2005-12-31")
    #sm2 = subset(sm, dateValue > "2005-07-01" & dateValue < "2005-07-31") #no values???
    
    
  output$distPlot <- renderPlot({
    
    ggplot(sm2,aes(x = LT / 7, y = dateValue)) + 
      geom_point(aes(color = scoreValue), size=5) +
      scale_x_continuous("Lead Time (weeks)") + scale_y_date("Months of 2005 (January omitted)") +
      scale_color_gradient(low="yellow", high="darkgreen")
    
  }) #end reactive bit

    
    # x    <- faithful[, 2]  # Old Faithful Geyser data
    # bins <- seq(min(x), max(x), length.out = input$bins * 7 + 1)
    # # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
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

  # generate sampling based on input$leadtimes from ui.R
  # x    <- scores[, 2] 
  # bins <- seq(min(x), max(x), length.out = (input$leadtimes + 1)/7)
  
  


#on unload type of disonnect???


