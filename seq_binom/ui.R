library(shiny)
library(BayesFactor)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("cosmo"),
	
	title = "Is it fair?",
	
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
			plotOutput("seqplot")
		)
	)
))