library(shiny)
library(shinythemes)
library(shinyBS) # Additional Bootstrap Controls

shinyUI(fluidPage(theme = shinytheme("spacelab"),
	title = "Bayesian Credible Interval for an IQ Test Score",
	
	titlePanel("Bayesian Credible Interval for an IQ Test Score"),
	
	fluidRow(
		column(width=5,
			sliderInput("mu", "True effect size:", min = 0, max = 4, value = 0.4, step=.01),
			sliderInput("tau", "Heterogeneity (τ)", min = 0, max = 1, value = 0.1, step=.01),
			sliderInput("w", "prob", min = 0, max = 1, value = 1, step=.01)
		),
		column(width=7,
			htmlOutput("info"),
			plotOutput("Lplot")
		)
		),
	fluidRow(
		column(width=12,
			HTML(
				"Thanks to Wolfgang Viechtbauer for providing the likelihood function.")
		)
	)
))
