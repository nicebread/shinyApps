library(shiny)
library(shinythemes)
library(shinyBS) # Additional Bootstrap Controls

# Load the panels with the manual etc.
source("snippets/quick_start.R")
source("snippets/about.R")

shinyUI(fluidPage(theme = shinytheme("spacelab"),
	tags$head(tags$link(rel="stylesheet", type="text/css", href="accordion.css")),	
	
	title = "p-hacker: Train your p-hacking skills!",
	
	titlePanel("p-hacker: Train your p-hacking skills!"),
	
	div(class="row", 
	    HTML(qs_panel), 
	    HTML(about_panel)
	),	
	
	# ---------------------------------------------------------------------
	# The actual app ...
	
	fluidRow(
		column(width=3,
			tabsetPanel(id ="tabs1",				
				tabPanel("New study",	
					h3("Settings for initial data collection:"),			
					textInput("label_group1", "Name for experimental group", "Elderly priming"),
					textInput("label_group2", "Name for control group", "Control priming"),
					sliderInput("n_per_group", "Initial # of participants in each group", min=2, max=100, value=20, step=1),
					sliderInput("true_effect", "True effect in population", min=0, max=1.5, value=0, step=0.05),
					sliderInput("dv_n", "Number of DVs", min=2, max=10, value=3, step=1),
					popify(textInput("seed", "Use seed (automatically incremented)", ''), "Hint", "Used to generate random values. Same seed leads to same values!", placement="top"),
					htmlOutput("seed_form"),
					htmlOutput("error_msg"),
					actionButton('generateNewData','Run new experiment'),
					br(),br(),
					p("(Discards previous data)")
				),
				tabPanel("Now: p-hack!", class="disabled",
					h3("Tools to improve your p-value:"),
					checkboxInput("cov_age", "Control for age", FALSE),
					checkboxInput("cov_gender", "Control for gender", FALSE),
					checkboxInput("cov_gender_IA", "Interaction with gender", FALSE),
		      div(class="btn-group-vertical",
					  actionButton('add5','Add 5 new participants'),
					  actionButton('add10','Add 10 new participants')
		      )
				)
			)
		),		
		
		
		# ---------------------------------------------------------------------
		# The output panels, on the right side
		
		column(width=6, 					
			htmlOutput("testoverview"),
			htmlOutput("plotoverview"),
			plotOutput("mainplot", 
			  click="mainplot_clicked"           
			),
			htmlOutput("plothints")
		),
		
		column(width=3,			
			htmlOutput("studystack")
		)
	)	
))
