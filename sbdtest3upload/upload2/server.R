# IMPREX Loader v2 (formerly File Picker)
# with adjustment from
# https://gist.github.com/davidarndt/bc09d77fa92457e094c8

require(DT)
require(uuid)
# library(uuid)
options(shiny.maxRequestSize=50*1024^2) # 50 mb (SMHI datafile is 40-something)

shinyServer(function(input, output, session) {

  values <- reactiveValues(
    file1 = NULL
	)
	
	observe({
		input$clearFile1
		input$uploadFormat
		values$file1 <- NULL
	})
	
	observe({
		values$file1 <- input$file1
		
		if(is.null(values$file1)){
		  return(NULL)
		}
		else {
		  # file1 isn't null, let's see if the user asked for a sensible file format
		  if(input$uploadFormat == 'rds'){
		    file.content <- readRDS(file = input$file1$datapath)
		  } 
		  else if(input$uploadFormat == 'csv'){ 
		    file.content <- read.csv(input$file1$datapath, dec=".", header=TRUE, quote = "\"") #sep = .
		    # values$fh <- read.csv2(input$file1$datapath) #sep = ,
		  }
		  else if(input$uploadFormat == 'txt'){ 
		    file.content <- read.csv(input$file1$datapath, sep = "\t", dec=".", header=TRUE, quote = "\"") #sep = .
		  }
		}
		return(file.content)
		# exp.clas.fil <- class(input$file1)
		# browser()
	})

	
		
	# observe({
	# }) # observe

	output$filename <- renderText({
		return(paste("Uploaded file: ", values$file1$name))
	})

	
	output$names <- renderText({
	  return(names(file.content))
	})
	

	output$resettableInput <- renderUI({
		input$clearFile1
		input$uploadFormat

		fileInput('file1', NULL, width="80%")
	})

})