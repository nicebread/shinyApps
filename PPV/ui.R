library(shiny)
library(shinyjs)
library(shinythemes)

helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-sm", `data-toggle` = "popover",
	  	`data-container`="body",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      icon("question")
    )
  )
}


# Define UI for PPV application
shinyUI(fluidPage(theme = shinytheme("spacelab"),

	title = "Positive Predictive Value (PPV)",
	shinyjs::useShinyjs(),

	h2(HTML("When does a significant <i>p</i>-value indicate a true effect?")),
	h3(HTML("Understanding the Positive Predictive Value (PPV) of a <i>p</i>-value")),

	br(),

	# Sidebar to select inputs
	fluidRow(
		# ---------------------------------------------------------------------
		# Parameter column
		column(4,
			p("Across all investigated hypotheses: What % of them is actually true?", style = "font-style: italic; font-size: 0.85em; color:grey"),    
			sliderInput("percTrue", label = "% of a priori true hypotheses:", min = 0, max = 100, value = 30, step = 1),
			
			p(HTML("What is your Type I error (&alpha;; typically 5%)?"), style = "font-style: italic; font-size: 0.85em; color:grey"),
			sliderInput("alpha", HTML("&alpha; level"), min = 0.01, max = 0.1, value = 0.05, step = 0.01),

                       ## NG 17-04-24: add sample size (n), effect size (d), and power_select
                       p("Do you want to specify power directly or indirectly by specifying sample size per group and effect size? (Assuming a two-group t-test)", style = "font-style: italic; font-size: 0.85em; color:grey"),
                       radioButtons("power_select", label=NULL,
                                    choiceValues=list("power", "nd"),
                                    choiceNames=list(HTML('<span style = "font-size: 0.85em">specify power (1-&beta;) directly</span>'), list(HTML('<span style = "font-size: 0.85em">specify power indirectly through sample size (n) and effect size (d)</span>')))),
                       ## p(HTML("What is the power (&beta;) of the study?"), style = "font-style: italic; font-size: 0.85em; color:grey"),
                       sliderInput("power", label = HTML("Power (1-&beta;)"), min = 0.01, max = 0.99, value = 0.35, step = 0.01),
                       ## p("What is your sample size (n) per group?", style = "font-style: italic; font-size: 0.85em; color:grey"),
											 conditionalPanel("input.power_select == 'nd'",
	                       sliderInput("n", label = "Sample size per group (n)", min = 1, max = 100, value = 16, step = 1),
	                       ## p("What is the standardized effect size (d)?", style = "font-style: italic; font-size: 0.85em; color:grey"),
	                       sliderInput("d", label = "Effect size (d)", min = 0.1, max = 2, value = 1, step = .05)
												), 

			p(HTML("% of studies that report a significant result, although it's not"), helpPopup("What is 'p-hacking' in this context?", "The percentage refers to the proportion of all non-significant studies (both true negatives and false negatives, that are presented as significant. For details, see Ioannidis (2005):\n'Let u be the proportion of probed
analyses that would not have been 'research findings', but nevertheless end up presented and reported as
such, because of bias. [...] Bias can entail manipulation in the analysis or reporting of findings.'", placement='right', trigger='hover'), style = "font-style: italic; font-size: 0.85em; color:grey; line-height:30%"),
			sliderInput("bias", label = "% of p-hacked studies", min = 0, max = 100, value = 0, step = 1),
			selectInput("preset","Presets by Ioannidis (2005)", c(
				"---"="---",
				"1: Adequately powered RCT with little bias and 1:1 pre-study odds"="p1",
				"2: Confirmatory meta-analysis of good-quality RCTs"="p2",
				"3: Meta-analysis of small inconclusive studies"="p3",
				"4: Underpowered, but well-performed phase I/II RCT"="p4",
				"5: Underpowered, poorly performed phase I/II RCT"="p5",
				"6: Adequately powered exploratory epidemiological study"="p6",
				"7: Underpowered exploratory epidemiological study"="p7",
				"8: Discovery-oriented exploratory research with massive testing"="p8",
				"9: As in previous example, but with more limited bias (more standardized)"="p9"
			))
		),	
		
		
		# ---------------------------------------------------------------------
		# Output column
		
		column(8,
			htmlOutput("res")
		)			
	),
	HTML("<b>This <a href='http://www.nicebread.de/whats-the-probability-that-a-significant-p-value-indicates-a-true-effect/'>blog post</a> gives an introduction to the app.</b><br>"),
	HTML("This app is based on Ioannidis, J. P. A. (2005). Why most published research findings are false. PLoS Medicine, 2(8), e124. <a href='http://doi.org/10.1371/journal.pmed.0020124'>http://doi.org/10.1371/journal.pmed.0020124</a><br>
	Thanks to Nat Goodman for adding the option to specify power via effect size and sample size.")
))