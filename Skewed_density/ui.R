library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",
	
	title = "The Skewness Principle",
	
	titlePanel("The Skewness Principle"),

	fluidRow(
		column(6, 
	  		sliderInput("xi", "xi (controls skewness; 1 = symmetric):", min = 0.1, max = 3, value = 1, step=0.05),
		
			helpText("Note: ")
		),
		
		column(6, 
			fluidRow(
				tableOutput("summary.out"),
				plotOutput("plot.out")
			)
		)	
	)  # of fluidrow
))