#IMPREX Scoreboard
readRenviron("~/R/shinysb1/.Renviron")
# setwd("~/R/shinysb1/sbdtest1")
REhost =     Sys.getenv('pgserver')
REport =     Sys.getenv('pgport')
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

library(shiny)
library(plyr); library(dplyr)
library(RPostgreSQL)
library(lazyeval)
library(ggplot2)

# lead.time.units <- "days" #update from DB based on selection
# lead.time.min <- 1
# lead.time.max <- 90

# test db existance and grab date brackets for ENTIRE dataset
  tmpcon <- dbConnect(PostgreSQL(), host=REhost, user=REuser, password=REpassword, dbname=REdbname) #add error checking here
  qry1e <- "SELECT DISTINCT(\"dateValue\") FROM \"tblScores\" ORDER BY \"dateValue\" LIMIT 1;"
  rs1e <- dbSendQuery(tmpcon,qry1e)
  dttFirstInDB <- fetch(rs1e,n=-1)
  qryDernier <- "SELECT DISTINCT(\"dateValue\") FROM \"tblScores\" ORDER BY \"dateValue\" DESC LIMIT 1;"
  rsDernier <- dbSendQuery(tmpcon,qryDernier)
  dttLastInDB <- fetch(rsDernier,n=-1)
  rm(tmpcon)  #kill connection
  
# REACTIVE? based on daterange, update "Time scale" control to
#   if ((dttLastInDB - dttFirstInDB) < year(1)) [All] ELSE [Monthly , Annual]

db <- src_postgres(dbname = REdbname,
                   host = REhost,
                   port = REport,
                   user = REuser,
                   password = REpassword)
tbl_scores <- tbl(db, "tblScores")

#selectInput boxes
tmpScoreType <- filter(tbl(db, "tblInterface"),ObjectName=="Score Type" & LanguageID == RElanguage)
ctlScoreType <- collect(tmpScoreType)

tmpModelVariable <- filter(tbl(db, "tblInterface"),ObjectName=="Model Variable" & LanguageID == RElanguage)
ctlModelVariable <- collect(tmpModelVariable)

tmpForecastType <- filter(tbl(db, "tblInterface"),ObjectName=="Forecast Type" & LanguageID == RElanguage)
ctlForecastType <- collect(tmpForecastType)

tmpLocationName <- filter(tbl(db, "tblInterface"),ObjectName=="Location Name" & LanguageID == RElanguage)
ctlLocationName <- collect(tmpLocationName)

tmpCaseStudy <- filter(tbl(db, "tblInterface"),ObjectName=="Case Study" & LanguageID == RElanguage)
ctlCaseStudy <- collect(tmpCaseStudy)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  img(src = "imprex.png", height = 100),
  titlePanel("Scoreboard"),

  fluidRow(
    column(4,
        wellPanel( 
          h4("Filter"),
          selectInput("rtnLocid", 
                      multiple=TRUE, # breaks stuff
                      selected = "A1080330",
                      "Location:",
                      c(structure(ctlLocationName$ObjectItemName)) # , selected=NULL
          ),
          
          selectInput("rtnModelVariable",
                       "Model Variable:",
                      c(sort.int(ctlModelVariable$ObjectItemName))
                        ),

          selectInput("rtnForecastType",
                      "Forecast Type:",
                      c(sort.int(ctlForecastType$ObjectItemName))
          ),
          
          selectInput("rtnScoreType",
                      "Score Type:",
                      c(sort.int(ctlScoreType$ObjectItemName))
          ),
          
          sliderInput("lead.times",
                      "Compare lead times (days):",
                      # "Compare lead times (", lead.time.units ,"):",
                      min = 1, # lead.time.min,
                      max = 90, # lead.time.max,
                      value = c(5,10))
                      #value = c(10,10))
          ,
          
          selectInput("rtnTimeScale",
                      "Summarize by:",
                      c("All", 
                        "Month", 
                        "Spring (MAM)", 
                        "Winter (DJF)", 
                        "Monsoon (JJAS)", 
                        "Year")
          ),
          # "Data summarized / averaged by ", summarize.by,
          "Viewing dates between: ", start.date <- as.Date(dttFirstInDB$dateValue), 
          "and: ", end.date <- as.Date(dttLastInDB$dateValue),
          # if daterange is reduced, calc number of records to display  
          dateInput("ctlFirstDate", "Startdate: ", as.Date(start.date)),
          dateInput("ctlEndDate", "Enddate: ", as.Date(end.date)),
          h6("Note - date range pickers not yet implemented")
          
        )),

    mainPanel(
      # TODO read facet() function, use it for series of plots...
      # http://www.cookbook-r.com/Graphs/Facets_%28ggplot2%29/
      # https://plot.ly/ggplot2/facet/
       plotOutput("seriesPlot") ,
       # "note, there", db.NAs ,"NA values in the score database for this selection" ,
       verbatimTextOutput("summary"),
       # verbatimTextOutput("dataNAs$scoreNA"), #this is all I really want
       verbatimTextOutput("dataNAs")
       # ,
       # tableOutput("view")

    ) #mainPanel

      
   ) #sidebarPanel
  ) #sidebarLayout
  
 )
