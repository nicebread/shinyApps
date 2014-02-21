library(shiny)

# Define UI for Bayesfactor
shinyUI(fluidPage(theme = "bootstrap.css",
	
	title = "Response surface explorer",
	
	titlePanel("Response surface explorer"),

	fluidRow(
		column(8, 
			plotOutput("plot"),
			helpText("This widgets relies on the RSA package for plotting.")
		),
		column(4,
			h4("Plot options"),
			sliderInput("xlim", "Range of x-axis", min = -10, max = 10, value = c(-2, 2), step=0.1),
			sliderInput("ylim", "Range of y-axis", min = -10, max = 10, value = c(-2, 2), step=0.1),
			sliderInput("zlim", "Range of z-axis", min = -10, max = 10, value = c(-2, 2), step=0.1),
			selectInput("plottype", "Plot type:", c("3d wireframe" = "3d", "2d contour" = "contour"))
		)
	),
	

	fluidRow(
	    column(4,
	      h4("Second-degree coefficients"),
    		sliderInput("x", "x", min = -2, max = 2, value = 0, step=0.01),
  			sliderInput("x2", "x^2", min = -2, max = 2, value = 0, step=0.01),
  			sliderInput("y", "y", min = -2, max = 2, value = 0, step=0.01),
  			sliderInput("y2", "y^2", min = -2, max = 2, value = 0, step=0.01),
  			sliderInput("xy", "x*y", min = -2, max = 2, value = 0, step=0.01)
	    ),
	    column(4,
			h4("Third-degree coefficients"),
			sliderInput("x3", "x^3", min = -2, max = 2, value = 0, step=0.01),
			sliderInput("y3", "y^3", min = -2, max = 2, value = 0, step=0.01),
			sliderInput("xy2", "x*y^2", min = -2, max = 2, value = 0, step=0.01),
			sliderInput("yx2", "y*x2", min = -2, max = 2, value = 0, step=0.01)
	    )
	  )
   
))

