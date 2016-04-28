#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#library(dplyr)
library(RPostgreSQL)
library(lazyeval)
library(ggplot2)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  img(src = "imprex.png", height = 100),
  titlePanel("Scoreboard"),

  fluidRow(
    column(4,
        wellPanel( 
          h4("Filter"),
           selectInput("ctlScrtype",
                       "Score Type:",
                       c("Seasonal_EDMD_month",
                          "Seasonal_EDMD_month_ByWeek",
                          "Seasonal_LS_month",
                          "Seasonal_LS_month_ByWeek"
                        )
                       ),
          selectInput("ctlLocid",
                      "Location:",
                      c( "A1080330",
                          "B2220010",
                          "H2342020",
                          "H4252010",
                          "H7401010",
                          "H8212010",
                          "I5221010",
                          "J7483010",
                          "K1321810",
                          "K6402520",
                          "L0563010",
                          "L4411710",
                          "M0243010",
                          "M7112410",
                          "S2242510",
                          "U4644010"
                      )
          )
          
          )),

  # Sidebar with a slider input for number of bins / leadtimes?
  sidebarLayout(
    sidebarPanel(

      # selectInput("select", label = h3("Select box"), 
      #             choices = ls_locations, selected = 1)
      #   ),
    

       sliderInput("bins",
                   "Show leadtimes (weeks):",
                   min = 1,
                   max = 90 / 7,
                   value = 30 / 7)
       
    ),

#      ls_locations <- c("A1080330","B2220010", "H2342020"),    
#    ls_locations <-    c("A1080330" = A1080330,
#                         "B2220010" = B2220010,
#                         "H2342020" = H2342020,
#                         "H4252010" = H4252010,
#                         "H7401010" = H7401010,
#                         "H8212010" = H8212010,
#                         "I5221010" = I5221010,
#                         "J7483010" = J7483010,
#                         "K1321810" = K1321810,
#                         "K6402520" = K6402520,
#                         "L0563010" = L0563010,
#                         "L4411710" = L4411710,
#                         "M0243010" = M0243010,
#                         "M7112410" = M7112410,
#                         "S2242510" = S2242510,
#                         "U4644010" = U4644010
# ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot") #,

              #subsubtoto has one locationID, one lead time, one scoreType
       # filtered (timeseries) of scoreValues against dateValues
       # ggplot(data=local,aes(x=dateValue,y=scoreValue)) + geom_point(aes(color=LT),size=1) +
       #   scale_x_date("Month") + scale_y_continuous("Score")
       

       # choose columns to display
       # diamonds2 = diamonds[sample(nrow(diamonds), 1000), ],
       # output$mytable1 <- DT::renderDataTable({
       #   DT::datatable(diamonds2[, input$show_vars, drop = FALSE])
       # })
    ))
  )
))
