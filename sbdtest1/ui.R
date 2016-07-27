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
library(leaflet)

# leaflet
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# dplyr
db <- src_postgres(
  dbname = REdbname,
  host = REhost,
  port = REport,
  user = REuser,
  password = REpassword
)
tbl.scores <- tbl(db, "tblScores")
tbl.dataload <- tbl(db, "tblDataLoad")
tbl.interface <- tbl(db, "tblInterface")

#selectInput boxes

# tmpScoreType <-
#   filter(tbl(db, "tblInterface"),
#          ObjectName == "Score Type" & LanguageID == RElanguage)
# ctlScoreType <- collect(tmpScoreType)

# tmpSkillScoreType <-
#   filter(
#     tbl(db, "tblInterface"),
#     ObjectName == "Score Type" &
#       ObjectItemName %like% "%Skill Score" &
#       LanguageID == RElanguage
#   )
# 
# ctlSkillScoreType <- collect(tmpSkillScoreType)

tmpModelVariable <-
  filter(tbl(db, "tblInterface"),
         ObjectName == "Model Variable" & LanguageID == RElanguage)
ctlModelVariable <- collect(tmpModelVariable)

tmpForecastType <-
  filter(tbl(db, "tblInterface"),
         ObjectName == "Forecast Type" & LanguageID == RElanguage)
ctlForecastType <- collect(tmpForecastType)
# 
# tmpCaseStudy <-
#   filter(tbl(db, "tblInterface"),
#          ObjectName == "Case Study" & LanguageID == RElanguage)
# ctlCaseStudy <- collect(tmpCaseStudy)

# tmpDataPackageList <- filter(tbl(db, "tblDataLoad"))

# tmpDataPackageList <-
#   distinct(
#     select(
#       tbl.dataload,
#       dataPackageGUID,
#       importResponsable,
#       dataPkgFriendlyName,
#       validPackage
#     )
#   )
# ctlDataPackageList <- collect(tmpDataPackageList)
# ctlDataPackageList <-
#   arrange_(ctlDataPackageList, "dataPackageGUID", "dataPkgFriendlyName")

# tmpInterface <-
#   distinct(select(tbl.interface, ObjectName, ObjectItemName, LanguageID))
# ctlInterface <- collect(tmpInterface)


# Define UI for application that draws a histogram
shinyUI(fluidPage(# Application title
  img(src = "imprex.png", height = 100),
  # navbarPage(title=div("Verification Scoreboard")
  # ),
  
  fluidRow(
    column(
      4,
      wellPanel(
        # first input is case studies
        selectInput(
          "rtnCaseStudy",
          "Case Studies:",
          c(
            "Central European Rivers" = 1,
            "South-East French Catchments" = 2,
            "Júcar River Basin (Spain)" = 3,
            "Lake Como Basin (Italy)" = 4,
            "Upper Umeälven River (Sweden)" = 5,
            "Segura River Basin (Iberian Peninsula)" = 6,
            "The Llobregat River Basin (Spain)" = 7,
            "The Messara Valley (Crete)" = 8,
            "Test Case Study (LC)" = 9
          ),
          selected = 1
        ),
        # selectInput(
        #   "rtnForecastRangeType",
        #   "Forecast Range:",
        #   c(
        #     "Short Range Forecast" = "day",
        #     "Medium-Range Forecasts" = "month",
        #     "Long-Range Forecasts" = "year"
        #   ),
        #   selected = "month"
        # ),
        
        # second:  forecast system
        selectInput(
          "rtnForecastSystem",
          "System:",
          c(
            "ECMWF EFAS" = 1,
            "E-HYPE" = 2,
            "System 3" = 3,
            "ECMWF LS Seasonal month" = 4,
            "ECMWF EDMD Seasonal month" = 5,
            "ECMWF LS Seasonal week" = 6,
            "ECMWF EDMD Seasonal week" = 7
          )
        )
      ),
      # wellPanel

      wellPanel(
        h4("Filters"),
        uiOutput("Location"),
        uiOutput("ModelVariable"),
        uiOutput("ForecastType"),
        uiOutput("ScoreType")
      ),
            
      #output pdf
      wellPanel(
        h4("Save Plot") ,
        # sidebarPanel(
        checkboxInput('returnpdf', 'output pdf?', FALSE),
        conditionalPanel(
          condition = "input.returnpdf == true",
          strong("PDF size (cm):"),
          sliderInput(
            inputId = "w",
            label = "width:",
            min = 5,
            max = 50,
            value = 16,
            width = 200,
            ticks = F
          ),
          sliderInput(
            inputId = "h",
            label = "height:",
            min = 5,
            max = 50,
            value = 12,
            width = 200,
            ticks = F
          ),
          br(),
          downloadLink('pdflink')
        )
      )
    ),
    #column 4
    
    column(
      8,
      ### scoreboard
      titlePanel("Scoreboard"),
      # mainPanel(
      # wellPanel(
      #   h4("Filter Criteria"),
      #   # conditionalPanel(
      #   #   # check we have data from DB
      #   #   condition = !is.na("table"), # DT::dataTableOutput("table")
      #   column(2,uiOutput("Location")),
      #   column(2,uiOutput("Variable")),
      #   column(2,uiOutput("Forecast System")),
      #   column(2,uiOutput("Score Type"))
      #   
      # 
      #   # ) # conditionalPanel
      # ), #wellPanel
      
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "Plot",
          h4("Select and filter data to create "),
          p("Create plot by selecting data"),
          plotOutput("seriesPlot")
          
        ),
        tabPanel(
          "Panel plots",
          h4("Select and filter data to create "),
          p("Create plots by selecting data"),
          plotOutput("facetPlot")
        ),
        tabPanel(
          "Table",
          h4("Table of corresponding values"),
          p("Create table by selecting data"),
          DT::dataTableOutput("table")
        )
      )  # tabsetPanel
      
      # wellPanel(
      #   h4("Map of selected locations"),
      #   # map
      #   leafletOutput("mymap"),
      #   p(),
      #   actionButton("recalc", "New points")
      #)
      
    ) #column = 8
  ) #sidebarPanel
  ) #fluidRow
  ) #fluidPage
  