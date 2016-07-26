# with adjustment from
# https://gist.github.com/davidarndt/bc09d77fa92457e094c8

require(DT)
library(uuid)
expected.names <- c('location.name', 
                    'mean.error', 
                    'start.date', 
                    'end.date', 
                    'members', 
                    'lead.times', 
                    'timestep', 
                    'provider.organization.name', 
                    'forecast.system', 
                    'member.count', 
                    'area', 
                    'provider.firstname', 
                    'provider.lastname', 
                    'provider.email', 
                    'provider.phone', 
                    'forecast.frequency', 
                    'lead.time.interval', 
                    'centerX', 
                    'centerY')


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
		verbatimTextOutput("rds.names"),

		h4(paste("The following fields were expected in the RDS file upload: " , expected.names)) ,
	
		
		# find the missing names
		missing.name <- setdiff(expected.names, "rds.names"),
		
		# find the "extra", or unexpected, names
		extra.name <- setdiff("rds.names", expected.names)
		
		
	)

	)
)