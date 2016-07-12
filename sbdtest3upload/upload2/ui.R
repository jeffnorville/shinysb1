require(DT)

shinyUI(bootstrapPage(

	tags$head(
		tags$style(".clearButton {float:right; font-size:12px;}")
	),
	
	headerPanel("Reset file input example"),

	sidebarPanel(
		HTML("<button id='clearFile1' class='action-button clearButton'>Clear</button>"),
		uiOutput('resettableInput'),
		
		selectInput('uploadFormat', label = "Select upload format", 
			choices = c(
				"Option 1" = 'f1',
				"Option 2" = 'f2',
				"Option 3" = 'f3'),
			selected = 'f1')

	),

	mainPanel(
		h4("Summary"),
		verbatimTextOutput("summary")
	)

	)
)