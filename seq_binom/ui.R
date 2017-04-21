library(shiny)
library(BayesFactor)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("spacelab"),
	
	title = "Is it fair?",
	
	# this file is needed for automatic resizing in the shiny apps.org framework
	tags$head(tags$script(src="js/iframeResizer.contentWindow.min.js")),
	
	titlePanel("Is it fair?"),

	fluidRow(
		column(width=5,
			plotOutput("urnplot"),
			actionButton("b_addRed", "+1 RED"),
			actionButton("b_addBlue", "+1 BLUE"),
			br(),
			actionButton("b_addRed10", "+10 RED"),
			actionButton("b_addBlue10", "+10 BLUE"),
			br(),
			actionButton("b_reset", "Reset"),
			actionButton("b_update", "Update plot"),
			checkboxInput("cb_autoupdate", "Update plot after each ball", TRUE)
		),
		column(width=7, 
			plotOutput("seqplot"),
			HTML("The BF computation uses the default prior of the <code>proportionBF</code> function in the BayesFactor package (see ?proportionBF).")
		)
	)
))