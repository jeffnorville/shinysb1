#Mini DB Testdrive
readRenviron("~/R/shinysb1/.Renviron")
REdbname =   Sys.getenv('pgdb')
REuser =     Sys.getenv('api_user')
RElanguage = Sys.getenv('api_language')
REpassword = Sys.getenv('pgpassword')

library(shiny)
library(RPostgreSQL)
library(dplyr)

db <- src_postgres('postgres',
                   host = 'localhost',
                   port = 5432,
                   user = REuser,
                   password = REpassword)
tbl.scores <- tbl(db, "tblScores")

tmpLocationName <- filter(tbl(db, "tblInterface"),ObjectName=="Location Name" & LanguageID == RElanguage)
ctlLocationName <- collect(tmpLocationName)

tmpModelVariable <- filter(tbl(db, "tblInterface"),ObjectName=="Model Variable" & LanguageID == RElanguage)
ctlModelVariable <- collect(tmpModelVariable)
# tmpModelVariable <- filter(tbl(db, "tblInterface"),ObjectName=="Model Variable" & LanguageID == RElanguage)
# ctlModelVariable <- collect(tmpModelVariable)
tmpLocationName <- filter(tbl(db, "tblInterface"),ObjectName=="Location Name" & LanguageID == RElanguage)
ctlLocationName <- collect(tmpLocationName)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Mini DB Testdrive"),
  
  # Note that
  # changes made to the caption in the textInput control are
  # updated in the output area immediately as you type
  sidebarLayout(
    sidebarPanel(
      textInput("caption", "Caption:", "Data Summary"),
      
      selectInput("dataset", "Choose a BV:",
                  choices = c(sort.int(ctlLocationName$ObjectItemName)) #not clear why we use "choices"
                  # c(sort.int(ctlLocationName$ObjectItemName)) #okay, w/o choices the state change isn't detected...
      ),
      
      
      numericInput("lead.times", "Lead times to compare:", 1)
    ),
    
    
    # Show the caption, a summary of the dataset and an HTML 
    # table with the requested number of observations
    mainPanel(
      h3(textOutput("caption", container = span)),
      
      # verbatimTextOutput("summary"), 
      "note: limited to uncorrected CRPS precipitation values on X BVs in France" ,
      tableOutput("view") #commenting this out removes the weird, date-stripping format error:
      #Warning in formatC(x = 4018, format = "f", digits = 2, decimal.mark = ".") :
      # class of 'x' was discarded
      # update (w fix if needed): http://stackoverflow.com/questions/22405550/r-shiny-table-with-dates
    )
  )
))
