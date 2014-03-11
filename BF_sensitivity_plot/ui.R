library(shiny)

# Define UI for Bayesfactor
shinyUI(pageWithSidebar(

    # Application title
    headerPanel("Sensitivity analysis for Bayes factors: One sample t test"),

    sidebarPanel(
		helpText("Note: The widget assumes that a one-sample t test is computed. If a one-sided Bayes factor is computed, it is assumed that positive t values correspond to results in the predicted direction and negative t values to results in the unpredicted direction."),
		
		helpText("The number of ts, ns, and labels must be equal."),
		
		textInput("ts", "t values (as comma separated values):", "2.51, 2.55, 2.23"),
		textInput("ns", "Sample sizes (as comma separated values):", "100, 97, 100"),
		textInput("labels", "Label for each study (as comma separated values; can be left blank):", "Bem Study 1, Bem study 2, Bem study 3"),
		
		selectInput("sides", "One- or two-sided t test:", c("One-sided" = "one", "Two-sided" = "two"), selected="two"),
		selectInput("forH1", "Direction of BF:", c("BF_01 (large BF in favor of H0)" = FALSE, "BF_10 (large BF in favor of H1)" = TRUE), selected=FALSE),
		
		downloadButton('downloadPlot', 'Download Plot as PNG')
    ),

    mainPanel(
      h3(""),
      plotOutput("plot")
    )
))

