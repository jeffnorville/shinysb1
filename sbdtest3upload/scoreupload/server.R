# IMPREX file picker
library(shiny)
 require("uuid")
library(uuid)
options(shiny.maxRequestSize=50*1024^2) # 50 mb (SMHI datafile is 40-something)


shinyServer(function(input, output, session) {
  values <- reactiveValues(
    inFile = NULL
  )
  
  observe({
    input$clearFile1
    input$uploadFormat
    values$inFile <- NULL
  })
  
  observe({
    values$file1 <- input$inFile
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL after pageload. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    else {
      generated.guid <- UUIDgenerate(TRUE) # got a file, generate a unique id based on user timestamp
      # output$guid <- renderText({generated.guid}) 
    }
      # read.csv(inFile$datapath, header=input$header, sep=input$sep, 
      #          quote=input$quote)
    
    #testing
    # inFile <- fileInput("file1", "C:/Users/jeffrey.norville/Documents/R/win-library/3.2/lme4/testdata/trees513.Rdata")
    # inFile$datapath = "C:/Users/jeffrey.norville/Documents/R/win-library/3.2/lme4/testdata/trees513.Rdata"
      
      # print(load(inFile$datapath, imported.data <- new.env()))
    # imported.data <- as.data.frame(load(inFile$datapath, import <- new.env()))
    
    # imported.data <- load(inFile$datapath, import <- new.env()) # works for .Rdata
    imported.data <- readRDS(inFile$datapath)
    
    # switch(name) # parse file extension or check filetype
    #   case
    
    # browser()
    
    
    imported.data <- as.data.frame(imported.data) # err
    imported.data$dataPackageGUID <- generated.guid

    # structure <- str(imported.data) # outputs "NULL"?
    # playing with DT
    # imported.data <- datatable(load(inFile$datapath, import<- new.env()))
    # output$table1 <- renderDataTable({imported.data})
    
    # peek <- head(imported.data, 30)
    # output$contents <- renderTable({head(imported.data, 50)})
   #  output$contents <- renderTable({names(imported.data)})
    
    output$summary <- renderText({
      summary(imported.data)
    })
    
    
    output$contents <- renderTable({
      head(imported.data)
      })
    
    # ls.str(imported.data)
    
  })
  
})
