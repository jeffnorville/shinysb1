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
    
    navbarPage(title=div("Verification Scoreboard")
               
    ),
    
    fluidRow(
      column(
        4,
        wellPanel(selectInput(
          "rtnByPackage",
          "Data source:",
          c(ctlDataPackageList$dataPkgFriendlyName)
        )),
        
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
              strong("PDF size (inches):"),
              sliderInput(inputId="w", label = "width:", min=3, max=20, value=8, width=100, ticks=F),
              sliderInput(inputId="h", label = "height:", min=3, max=20, value=6, width=100, ticks=F),
              br(),
              downloadLink('pdflink')
            )
          # )
        )
      ) #column
      #fluidRow
    ,

      mainPanel(
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
            ) # tabsetPanel
          
      ) #mainPanel
  )
) #sidebarPanel
)
