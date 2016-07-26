?cfh
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
  distinct(select(tbl.scores, locationID, dataPackageGUID))
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
      tbl.dataload,
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
  distinct(select(tbl.interface, ObjectName, ObjectItemName, LanguageID))
ctlInterface <- collect(tmpInterface)


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    # Application title
    img(src = "imprex.png", height = 100),
    # navbarPage(title=div("Verification Scoreboard")
    # ),
    
    fluidRow(
      column(
        4,
        wellPanel(
          # first input is forecast type
          selectInput(
            "rtnForecastRangeType",
            "Forecast Range:",
             c("Short Range Forecast" = "days", 
              "Medium-Range Forecasts" = "months",
              "Long-Range Forecasts" = "years")
          ),
          
          # second:  forecast system
          selectInput(
            "rtnForecastSystem",
            "System:",
            c("System 1" = "sys1", 
              "System 2" = "sys2",
              "System 3" = "sys3")
          )
          
          
        #   selectInput(
        #   "rtnByPackage",
        #   "Data source:",
        #   c(ctlDataPackageList$dataPkgFriendlyName)
        # )
        ), # wellPanel
        
        wellPanel(
          h4("Filter Criteria"),
          selectInput("rtnLocid",
                      multiple = TRUE,
                      "Location:",
                      c(structure(
                        ctlLocationName$locationID
                      ))),
          # , selected=NULL),),
          selectInput("rtnModelVariable",
                      "Variable:",
                      c(
                        sort.int(ctlModelVariable$ObjectItemName)
                      ),
                      selected = "Streamflow"),
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
                      ))
        ),
        
        #output pdf
        wellPanel(
          h4("Save Plot") ,
          # sidebarPanel(
            checkboxInput('returnpdf', 'output pdf?', FALSE),
            conditionalPanel(
              condition = "input.returnpdf == true",
              strong("PDF size (cm):"),
              sliderInput(inputId="w", label = "width:", min=5, max=50, value=16, width=200, ticks=F),
              sliderInput(inputId="h", label = "height:", min=5, max=50, value=12, width=200, ticks=F),
              br(),
              downloadLink('pdflink')
            )
          # )
        )
      ) #column
      #fluidRow
    ,

    column(
      8,
      ### scoreboard
      titlePanel("Scoreboard"),
      # mainPanel(
        # new layout
        tabsetPanel(type = "tabs", 
                    
          tabPanel("Plot",
                   h4("Select and filter data to create "),
                   p("Create a plot by selecting data"),
                   plotOutput("seriesPlot")
                   
          ),
          tabPanel("Panel plots",
                   h4("Select and filter data to create "),
                   p("Create a plot by selecting data"),
                   plotOutput("facetPlot")
                   
                   ),
        
          tabPanel("Table",
                   h4("Table of corresponding values"),
                   p("Create a plot by selecting data"),
                   DT::dataTableOutput("table")
                   
                   )
            ), # tabsetPanel
      
          wellPanel(
            h4("Map of selected locations"),
            # map
            leafletOutput("mymap"),
            p(),
            actionButton("recalc", "New points")
            
            
          )
      
      
      ) #column = 8
  )
) #sidebarPanel
)
