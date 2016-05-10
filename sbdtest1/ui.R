#ini file
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

#test db existance and grab 1ere date
  tmpcon <- dbConnect(PostgreSQL(), user=REuser, password=REpassword, dbname=REdbname) #add error checking here
  qry1e <- "select distinct(\"dateValue\") from \"tblScores\" order by \"dateValue\" limit 1;"
  rs1e <- dbSendQuery(tmpcon,qry1e)
  dttFirstInDB <- fetch(rs1e,n=-1)
  qryDernier <- "select distinct(\"dateValue\") from \"tblScores\" order by \"dateValue\" desc limit 1;"
  rsDernier <- dbSendQuery(tmpcon,qryDernier)
  dttLastInDB <- fetch(rsDernier,n=-1)
  
    #kill connection
  rm(tmpcon)

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



# tmpCaseStudy <- filter(tbl(db, "tblInterface"),ObjectName=="Case Study" & LanguageID == RElanguage)
# ctlCaseStudy <- collect(tmpCaseStudy)


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
          

          #does this make any sense as selection criteria?          
          #insert date-picker to change 1e date of analysis
          "Date range: ", start.date <- as.Date(dttFirstInDB$dateValue), 
          "to: ", end.date <- as.Date(dttLastInDB$dateValue),
          dateInput("ctlFirstDate", "Startdate: ", as.Date(start.date)),
          sliderInput("timeFrame",
                      "Pick analysis timeframe:",
                      min = start.date,
                      max = end.date,
                      value = start.date + 90)
        )),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("seriesPlot") ,

       
       verbatimTextOutput("summary"),
       tableOutput("view"),

       h4("Choose axes"),
       selectInput('xcol', 'X Variable', (names(tbl_scores$row.names))),
       selectInput('ycol', 'Y Variable', (names(tbl_scores$row.names)),
                   selected=names(tbl_scores)[[2]]) #default
         

       # include summary table underneath plot:
       # diamonds2 = diamonds[sample(nrow(diamonds), 1000), ],
       # output$mytable1 <- DT::renderDataTable({
       #   DT::datatable(diamonds2[, input$show_vars, drop = FALSE])
       # })
    ) #mainPanel

    # wellPanel(
    #   span("Records selected:",
    #        textOutput("n_records")
    #   )
      
   ) #sidebarPanel
  ) #sidebarLayout
 )
