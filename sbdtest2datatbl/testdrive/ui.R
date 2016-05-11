library(shiny)
library(RPostgreSQL)
library(dplyr)


db <- src_postgres('postgres',
                   host = 'localhost',
                   port = 5432,
                   user = REuser,
                   password = REpassword)
tbl_scores <- tbl(db, "tblScores")
tmpModelVariable <- filter(tbl(db, "tblInterface"),ObjectName=="Model Variable" & LanguageID == RElanguage)
ctlModelVariable <- collect(tmpModelVariable)


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
                  choices = c(sort.int(ctlLocationName$ObjectItemName))
      ),
      
      
      numericInput("lead.times", "Lead times to compare:", 1)
    ),
    
    
    # Show the caption, a summary of the dataset and an HTML 
    # table with the requested number of observations
    mainPanel(
      h3(textOutput("caption", container = span)),
      
      verbatimTextOutput("summary"), 
      "note: limited to uncorrected CRPS precipitation values on X BVs in France" ,
      
      tableOutput("view")
    )
  )
))
