library(shiny)
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
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      icon("question")
    )
  )
}


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("spacelab"),

	title = "Positive Predictive Value (PPV)",

	h2(HTML("When does a significant <i>p</i>-value indicate a true effect?")),
	h3(HTML("Understanding the Positive Predictive Value (PPV) of a <i>p</i>-value")),

	br(),

	# Sidebar with a slider input for the number of bins
	fluidRow(
		column(4,
			p("Across all investigated hypotheses: What % of them is actually true?", style = "font-style: italic; font-size: 0.85em; color:grey"),    
			sliderInput("percTrue", label = "% of a priori true hypotheses:", min = 0, max = 100, value = 30, step = 1),
			
			p(HTML("What is your Type I error (&alpha;; typically 5%)?"), style = "font-style: italic; font-size: 0.85em; color:grey"),
			sliderInput("alpha", HTML("&alpha; level"), min = 0.01, max = 0.1, value = 0.05, step = 0.01),
			
			p(HTML("On what power level are the studies conducted?"), style = "font-style: italic; font-size: 0.85em; color:grey"),
			sliderInput("power", label = "Power", min = 0.01, max = 0.99, value = 0.35, step = 0.01),
			
			p(HTML("% of studies that report a significant result, although it's not"), helpPopup("What is 'bias' in this context?", "For details, see Ioannidis (2005):\nLet u be the proportion of probed
analyses that would not have been 'research findings', but nevertheless end up presented and reported as
such, because of bias. [...] Bias can entail manipulation in the analysis or reporting of findings.", placement='bottom', trigger='click'), style = "font-style: italic; font-size: 0.85em; color:grey; line-height:30%"),
			sliderInput("bias", label = "% of p-hacked studies", min = 0, max = 100, value = 0, step = 1)
      	),
		column(8,
			htmlOutput("res")
		)			
	),
	
	HTML("This app is based on Ioannidis, J. P. A. (2005). Why most published research findings are false. PLoS Medicine, 2(8), e124. <a href='http://doi.org/10.1371/journal.pmed.0020124'>http://doi.org/10.1371/journal.pmed.0020124</a>")
))