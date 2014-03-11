library(shiny)

# Define UI for Bayesfactor
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Pick-a-prior"),

  sidebarPanel(
	sliderInput("RANGE", "Range:", min = -100, max = 100, value = c(-3, 3), step=1),
	sliderInput("MEAN", "Mean:", min = -2, max = 2, value = 0, step=0.1),
	sliderInput("SD", "SD:", min = 0, max = 4, value = 1, step=0.1),
	sliderInput("alpha", "alpha:", min = 0, max = 4, value = 0.4, step=0.1),
	sliderInput("beta", "beta:", min = 0, max = 4, value = 0.3, step=0.1),
	
	selectInput("distr", "Distribution:", c(
					"Normal" = "normal",
					"Gamma" = "gamma",
					"Inverse gamma" = "invgamma",
					"Weibull" = "weibull"))
	
  ),

  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3("Distribution"),

    plotOutput("plot")
  )
))

