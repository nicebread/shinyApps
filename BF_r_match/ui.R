library(shiny)

# Define UI for Bayesfactor
shinyUI(pageWithSidebar(

    # Application title
    headerPanel("Bayes factors: Two sample t test"),

    sidebarPanel(
		helpText("Try to drag the sliders!"),
		
		textInput("rs", "Scale parameters for the H1 prior (as comma separated values):", "0.71, 1, 1.41"),
		sliderInput("ds", "Range of effect sizes (Cohen's d):", min = 0, max = 2.5, value = c(0, 1.5), step=.02),
  		sliderInput("N", "Sample size per group:", min = 5, max = 1000, value = 35, step=5),
		checkboxInput("oneSided", "Use one-sided Bayes factor", value = FALSE),
		
		helpText("Note: The widget assumes that a two-sample t test is computed.",
		         "N defines the sample size of each group.")
    ),

    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      h3(""),
      plotOutput("plot")
    )
))

