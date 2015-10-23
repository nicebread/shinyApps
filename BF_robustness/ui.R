library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("cosmo"),

	#------------------------------------------------------------------------------------------
	# Add "busy" indicator and scripts for iframe handling and Google analytics
	tags$head(includeScript("http://shinyapps.org/includes/iframeResizer.contentWindow.min.js")),
	tags$head(includeScript("http://shinyapps.org/includes/analyticstracking_for_shiny.js")),
	tagList(
		tags$head(
			# These are needed to automatically display the "busy indicator"
			tags$link(rel="stylesheet", type="text/css", href="busy.css"),			
			tags$script(type="text/javascript", src = "busy.js"),
			
			# These are needed to show the "loading indicator" at the beginning
		    tags$script(type="text/javascript", src = "showstartmessage.js"),
			tags$link(rel="stylesheet", type="text/css", href="loading.css"),
			
			# This is needed to receive custom JS code from server.ui:
			# --> session$sendCustomMessage(type="jsCode", list(code="$('div.loading').hide('slow');"))
			tags$script(HTML('
				Shiny.addCustomMessageHandler("jsCode", function(message) {eval(message.code);});
			'))
		) # of $head
	),
	
	# These are the actual busy/ loading messages
	div(class = "busy", p("Calculation in progress ..."), img(src="preloader.gif")),
	div(class = "loading", p("Loading data (this takes a couple of seconds) ..."), img(src="preloader.gif")),
	#------------------------------------------------------------------------------------------
    # Application title
    title = "Robustness analysis for Bayes factors: Two sample t test",
	
	titlePanel("Robustness analysis for Bayes factors: Two sample t test"),

	fluidRow(
		column(4, 
			helpText("Try to drag the sliders!"),
		
  			sliderInput("D", "Population effect size (Cohen's d):", min = 0, max = 3, value = 0.4, step=0.01),
			textInput("rs", "Scale parameters for the H1 prior (as comma separated values):", "0.71, 1, 1.41"),
  			sliderInput("max.N", "Maximum sample size per group:", min = 5, max = 5000, value = 250, step=5),
		
			helpText("Note: The widget assumes that a two-sample t test is computed.",
		         "N defines the sample size of each group.")
    			 ),
		column(8, 		 
			plotOutput("plot")
		)
    )
))
