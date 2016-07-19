# IMPREX Loader v2 (formerly File Picker)
# with adjustment from
# https://gist.github.com/davidarndt/bc09d77fa92457e094c8

require(DT)
require("uuid")
library(uuid)
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
	})
	
# 	observe({
#   	values$fh.rds <- readRDS(file = input$file1)
# 	})
	
	
	# load file here?
# 	if(is.null(values$file1))
# 	  return(NULL)
#   else {
#     imported.data <- readRDS(values$file1$datapath)
#   }

	output$summary <- renderText({
		return(paste("Uploaded file: ", values$file1$name))
	})

	#  Load in my data?
	# x <- readRDS(values$file1)
	# browser()
	
	output$resettableInput <- renderUI({
		input$clearFile1
		input$uploadFormat

		fileInput('file1', NULL, width="80%")
	})

})