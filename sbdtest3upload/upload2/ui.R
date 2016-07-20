# with adjustment from
# https://gist.github.com/davidarndt/bc09d77fa92457e094c8

require(DT)
library(uuid)

shinyUI(bootstrapPage(

	tags$head(
		tags$style(".clearButton {float:right; font-size:12px;}")
	),
	
	headerPanel("Loader (v2, resettable)"),

	sidebarPanel(
		HTML("<button id='clearFile1' class='action-button clearButton'>Clear</button>"),
		uiOutput('resettableInput'),
		
		selectInput('uploadFormat', label = "Select upload format", 
			choices = c(
				"RDS" = 'rds',
				"csv" = 'csv',
				"tab-delim" = 'txt'),
			selected = 'rds')

	),

	mainPanel(
		h4("Summary"),
		verbatimTextOutput("filename"),
		verbatimTextOutput("names")
		
		# ,
		# verbatimTextOutput("fh.rds")
		# "output$fh.rds"
		# toto <- readRDS()
		
	)

	)
)