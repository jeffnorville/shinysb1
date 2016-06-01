#IMPREX Scoreboard
readRenviron("~/R/shinysb1/.Renviron")
REdbname = Sys.getenv('pgdb')
REuser = Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')
RElanguage <- 1 #for now

library(shiny)
library(dplyr)
library(RPostgreSQL)
library(lazyeval)
library(ggplot2)

# test db existance and grab date brackets for ENTIRE dataset
  tmpcon <- dbConnect(PostgreSQL(), user=REuser, password=REpassword, dbname=REdbname) #add error checking here
  qry1e <- "SELECT DISTINCT(\"dateValue\") FROM \"tblScores\" ORDER BY \"dateValue\" LIMIT 1;"
  rs1e <- dbSendQuery(tmpcon,qry1e)
  dttFirstInDB <- fetch(rs1e,n=-1)
  qryDernier <- "SELECT DISTINCT(\"dateValue\") FROM \"tblScores\" ORDER BY \"dateValue\" DESC LIMIT 1;"
  rsDernier <- dbSendQuery(tmpcon,qryDernier)
  dttLastInDB <- fetch(rsDernier,n=-1)
  rm(tmpcon)  #kill connection
  
# REACTIVE? based on daterange, update "Time scale" control to
#   if ((dttLastInDB - dttFirstInDB) < year(1)) [All] ELSE [Monthly , Annual]

db <- src_postgres('postgres',
                   host = 'localhost',
                   port = 5432,
                   user = REuser,
                   password = REpassword)
tbl_scores <- tbl(db, "tblScores")

#selectInput boxes
tmpScoreType <- filter(tbl(db, "tblInterface"),ObjectName=="Score Type" & LanguageID == RElanguage)
ctlScoreType <- collect(tmpScoreType)

tmpModelVariable <- filter(tbl(db, "tblInterface"),ObjectName=="Model Variable" & LanguageID == RElanguage)
ctlModelVariable <- collect(tmpModelVariable)

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
                      "Location:",
                      c(structure(ctlLocationName$ObjectItemName))
          ),
          
          selectInput("rtnModelVariable",
                       "Model Variable:",
                      c(sort.int(ctlModelVariable$ObjectItemName))
                        ),
          
          selectInput("rtnScoreType",
                      "Score Type:",
                      c(sort.int(ctlScoreType$ObjectItemName))
          ),
          
          sliderInput("lead.times",
                      "Compare lead times (weeks):",
                      min = 1,
                      max = 90 / 7,
                      value = c(1,4))
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
