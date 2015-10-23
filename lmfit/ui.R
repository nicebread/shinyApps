library(shiny)

# Define UI for Bayesfactor
shinyUI(fluidPage(theme = "bootstrap.css",
	
	title = "Fit-a-line!",
	
	titlePanel("Fit-a-line!"),

	fluidRow(
		column(4,
			h4("Coefficients"),
			sliderInput("b0", "Intercept", min = -1.5, max = 1.5, value = 0, step=0.01),
			sliderInput("b1", "Slope", min = -1.5, max = 1.5, value = 0, step=0.01),
			checkboxInput("showFit", "Show optimal fit", value = FALSE),
			actionButton("reset_it", "Reset & new data"),
			actionButton("optim", "Let 'optim' find the best fit!")
		),
		column(8, 
			plotOutput("scatterplot")
		)
	)# ,
# 	
# 	fluidRow(
# 		column(4,
# 			helpText("RSS-Stats")
# 		),
# 		column(8, 
# 			plotOutput("RSS"),
# 			helpText("This widgets relies on the RSA package for plotting.")
# 		)
# 	)   
))

