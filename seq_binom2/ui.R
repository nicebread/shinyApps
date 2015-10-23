library(shiny)
library(BayesFactor)

shinyUI(fluidPage(theme = shinytheme("cosmo"),
	#------------------------------------------------------------------------------------------
	# Add "busy" indicator and scripts for iframe handling and Google analytics
	tags$head(includeScript("http://shinyapps.org/includes/analyticstracking_for_shiny.js")),

	#------------------------------------------------------------------------------------------
# Application title
	title = "Is it fair?",
	
	titlePanel("Is it fair?"),

	tabsetPanel(
	      tabPanel("Draw a random ball",
			fluidRow(
				column(6,
					plotOutput("urnplot"),
					actionButton("b_draw", "Draw a ball"),
					actionButton("b_reset", "Reset")
				)
			)
		 ),
      tabPanel("Sequential Bayes factor",
		fluidRow(
			column(6,
				plotOutput("seqplot")
			)
		)
	 )
	)
))