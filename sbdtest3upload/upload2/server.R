require(DT)

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
	
	output$summary <- renderText({
		return(paste("Uploaded file: ", values$file1$name))
	  
	})

	output$resettableInput <- renderUI({
		input$clearFile1
		input$uploadFormat

		fileInput('file1', NULL, width="80%")
	})

})