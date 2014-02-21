library(shiny)

# Define UI for Bayesfactor
shinyUI(pageWithSidebar(

    # Application title
    headerPanel("Robustness analysis for Bayes factors: Two sample t test"),

    sidebarPanel(
		helpText("Try to drag the sliders!"),
		
  		sliderInput("D", "Population effect size (Cohen's d):", min = 0, max = 3, value = 0.4, step=0.01),
		textInput("rs", "Scale parameters for the H1 prior (as comma separated values):", "0.71, 1, 1.41"),
  		sliderInput("max.N", "Maximum sample size per group:", min = 5, max = 2000, value = 250, step=5),
		
		helpText("Note: The widget assumes that a two-sample t test is computed.",
		         "N defines the sample size of each group.")
    ),

    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      h3(""),
      plotOutput("plot")
    )
))

