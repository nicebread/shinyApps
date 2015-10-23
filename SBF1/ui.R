library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",

	#------------------------------------------------------------------------------------------
	# Add "busy" indicator
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
	
	title = "SBF Explorer",
	
	titlePanel("Sequential Bayes Factor (SBF) Explorer"),


	fluidRow(
		column(4, 
	  		sliderInput("d0", "H1 effect size (Cohen's d):", min = 0.1, max = 1.5, value = 0.4, step=0.05),
			sliderInput("boundary", "Boundary", min = 3, max = 30, value = 10, step=1),
			selectInput("r0", "Scale parameter r:", c("sqrt(2)/2" = sqrt(2)/2, "1" = 1, "sqrt(2)"=sqrt(2)), selected="1"),
			
			conditionalPanel(
				condition = "input.tab == 'tab1'",
				textInput("qs", "Stopping-n quantiles (as comma separated values):", "0.5, 0.8, 0.9, 0.95")
			),
			conditionalPanel(
				condition = "input.tab == 'tab2'",
				selectInput("ES", "Effect size estimator", c(
					"Naive" = "g.emp", 
					"model-average, p(H1) = .5" = "shrunk_0.5", 
					"model-average, p(H1) = .4" = "shrunk_0.4", 
					"model-average, p(H1) = .3" = "shrunk_0.3", 
					"model-average, p(H1) = .2" = "shrunk_0.2", 
					"model-average, p(H1) = .1" = "shrunk_0.1"))
			),
		
			helpText("Note: The widget assumes that a two-sample t test is computed.",
			         "N defines the sample size of each group.")
			
		),
		
		column(8, 
		tabsetPanel(id="tab",
			tabPanel("Expected sample size", value="tab1",
				fluidRow(
					column(6,
						h3("Under the H0"),
						tableOutput("H0.summary"),
						plotOutput("plotH0")
					),
					column(6,
						h3("Under the H1"),
						tableOutput("H1.summary"),
						plotOutput("plotH1")
					)
				)
			),
			tabPanel("Effect size estimates at stopping point", value="tab2",
				fluidRow(
					column(12,
						h4("Distribution of effect sizes at stopping point"),
						textOutput("biasText"),
						plotOutput("plotBias")
					)
				)
			)
		)
		)
	)
))

