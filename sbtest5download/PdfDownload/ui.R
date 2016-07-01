# IMPREX download doc test

require(shiny)
pageWithSidebar(
  headerPanel("Output to PDF"),
  sidebarPanel(
    checkboxInput('returnpdf', 'output pdf?', FALSE),
    conditionalPanel(
      condition = "input.returnpdf == true",
      strong("PDF size (inches):"),
      sliderInput(inputId="w", label = "width:", min=3, max=20, value=8, width=100, ticks=F),
      sliderInput(inputId="h", label = "height:", min=3, max=20, value=6, width=100, ticks=F),
      br(),
      downloadLink('pdflink')
    )
  ),
  mainPanel({ mainPanel(plotOutput("myplot")) })
)