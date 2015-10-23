library(shiny)
library(BayesFactor)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("cosmo"),
	
	title = "Feeling Bayes factors: Two group mean difference",
	
	titlePanel("Feeling Bayes factors: Height difference between males and females"),

	fluidRow(
		column(width=4,
	  		sliderInput("d", "Mean difference between groups in cm:", min = 0, max = 30, value = 12, step=0.5),
			sliderInput("n", "Sample size in each group:", min = 5, max = 1000, value = 40, step=1),
			radioButtons("ordering", "Ordering of height lines:",
			             c("Shuffle" = "shuffle",
			               "By height" = "height",
			               "By group" = "group")),
			helpText(	"Women have on average a height of 165.8 cm, males of 177.9 cm.",
						"Difference: 12.1 cm. Pooled standard deviation is 7 cm.",
						"Hence, the standardized effect size is around 1.7.",
						"(Source: http://www.econ.upf.edu/docs/papers/downloads/1002.pdf)")
		),
		column(width=8, 
			fluidRow(
				column(width=12, plotOutput("heightplot"))
			),
			fluidRow(
				column(width=12, plotOutput("densityplot"))
			)			
		)
	)	
))