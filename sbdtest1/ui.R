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

# lead.time.units <- "days" #update from DB based on selection
# lead.time.min <- 1
# lead.time.max <- 90

# test db existance and grab date brackets for ENTIRE dataset
# tmpcon <-
#   dbConnect(
#     PostgreSQL(),
#     host = REhost,
#     user = REuser,
#     password = REpassword,
#     dbname = REdbname
#   ) #add error checking here
# qry1e <-
#   "SELECT DISTINCT(\"dateValue\") FROM \"tblScores\" ORDER BY \"dateValue\" LIMIT 1;"
# rs1e <- dbSendQuery(tmpcon, qry1e)
# dttFirstInDB <- fetch(rs1e, n = -1)
# #if this is NULL then the table is empty / broken...
# qryDernier <-
#   "SELECT DISTINCT(\"dateValue\") FROM \"tblScores\" ORDER BY \"dateValue\" DESC LIMIT 1;"
# rsDernier <- dbSendQuery(tmpcon, qryDernier)
# dttLastInDB <- fetch(rsDernier, n = -1)
# rm(tmpcon)  #kill connection

# REACTIVE? based on daterange, update "Time scale" control to
#   if ((dttLastInDB - dttFirstInDB) < year(1)) [All] ELSE [Monthly , Annual]

db <- src_postgres(
  dbname = REdbname,
  host = REhost,
  port = REport,
  user = REuser,
  password = REpassword
)
tbl_scores <- tbl(db, "tblScores")
tbl_data_load <- tbl(db, "tblDataLoad")
tbl_interface <- tbl(db, "tblInterface")

#selectInput boxes

tmpScoreType <-
  filter(tbl(db, "tblInterface"),
         ObjectName == "Score Type" & LanguageID == RElanguage)
ctlScoreType <- collect(tmpScoreType)

tmpSkillScoreType <-
  filter(
    tbl(db, "tblInterface"),
    ObjectName == "Score Type" &
      ObjectItemName %like% "%Skill Score" &
      LanguageID == RElanguage
  )
ctlSkillScoreType <- collect(tmpSkillScoreType)

tmpModelVariable <-
  filter(tbl(db, "tblInterface"),
         ObjectName == "Model Variable" & LanguageID == RElanguage)
ctlModelVariable <- collect(tmpModelVariable)

tmpForecastType <-
  filter(tbl(db, "tblInterface"),
         ObjectName == "Forecast Type" & LanguageID == RElanguage)
ctlForecastType <- collect(tmpForecastType)

tmpLocationName <-
  distinct(select(tbl_scores, locationID, dataPackageGUID))
ctlLocationName <- collect(tmpLocationName)
ctlLocationName <-
  arrange_(ctlLocationName, "dataPackageGUID", "locationID")

tmpCaseStudy <-
  filter(tbl(db, "tblInterface"),
         ObjectName == "Case Study" & LanguageID == RElanguage)
ctlCaseStudy <- collect(tmpCaseStudy)

# tmpDataPackageList <- filter(tbl(db, "tblDataLoad"))
tmpDataPackageList <-
  distinct(
    select(
      tbl_data_load,
      dataPackageGUID,
      importResponsable,
      dataPkgFriendlyName,
      validPackage
    )
  )
ctlDataPackageList <- collect(tmpDataPackageList)
ctlDataPackageList <-
  arrange_(ctlDataPackageList, "dataPackageGUID", "dataPkgFriendlyName")

tmpInterface <-
  distinct(select(tbl_interface, ObjectName, ObjectItemName, LanguageID))
ctlInterface <- collect(tmpInterface)


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    # Application title
    img(src = "imprex.png", height = 100),
    titlePanel("Scoreboard"),
    
    fluidRow(column(
      4,
      wellPanel(selectInput(
        "rtnByPackage",
        "Package:",
        c(ctlDataPackageList$dataPkgFriendlyName)
      )),
      
      wellPanel(
        h4("Filter Criteria"),
        
        selectInput("rtnLocid",
                    multiple = TRUE,
                    # selected = "A1080330", #need a default ?
                    "Location:",
                    c(structure(
                      ctlLocationName$locationID
                    ))),
        # , selected=NULL),),
        selectInput("rtnModelVariable",
                    "Variable:",
                    c(
                      sort.int(ctlModelVariable$ObjectItemName)
                    )),
        selectInput("rtnForecastType",
                    "Forecast System:",
                    c(
                      sort.int(ctlForecastType$ObjectItemName)
                    )),
        selectInput("rtnScoreType",
                    "Score:",
                    c(sort.int(
                      ctlScoreType$ObjectItemName
                    ))),
        selectInput("rtnScoreType",
                    "Skill Score:",
                    c(
                      "All Skill Scores", sort.int(ctlScoreType$ObjectItemName)
                    )),
        # max.leadtime.in.db <- c(6.0), # if there are fewer than X LTs, show all by default
        # if (max.leadtime.in.db < 15) {
        show.max.LT <- 90,
        # }
        sliderInput(
          "lead.times",
          "Lead time window:",
          # "Compare lead times (", lead.time.units ,"):",
          min = 1,
          # lead.time.min,
          max = show.max.LT,
          # lead.time.max,
          value = c(5, 10)
        ) # default
      ) # close wellPanel
     # close column?) # close fluidRow?
    ),
    
    mainPanel(
      plotOutput("seriesPlot") ,
      verbatimTextOutput("summary"),
      DT::dataTableOutput("view")
      # ,
      # tableOutput("view")
      
    ) #mainPanel
  )
) #sidebarPanel
) #sidebarLayout)
