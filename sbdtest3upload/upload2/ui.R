require(DT)

shinyUI(bootstrapPage(

	tags$head(
		tags$style(".clearButton {float:right; font-size:12px;}")
	),
	
	headerPanel("File input example"),

	sidebarPanel(
		HTML("<button id='clearFile1' class='action-button clearButton'>Clear</button>"),
		uiOutput('resettableInput'),
		
		selectInput('uploadFormat', label = "Select upload format", 
			choices = c(
				"RDS" = 'rds',
				"csv" = 'csv',
				"tab-delim" = 'txt'),
			selected = 'f1')

	),

	mainPanel(
		h4("Summary"),
		verbatimTextOutput("summary")
		
	)

	)
)