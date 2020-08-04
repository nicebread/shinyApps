library(shiny)
library(shinythemes)
library(shinyBS) # Additional Bootstrap Controls

shinyUI(fluidPage(theme = shinytheme("spacelab"),
	title = "Bayesian Credible Interval for an IQ Test Score",
	
	titlePanel("Bayesian Credible Interval for an IQ Test Score"),
	
	fluidRow(
		column(width=5,
			h3("Properties of the test instrument"),
			sliderInput("reliability", "Test reliability:", min = 0.2, max = .99, value = 0.8, step=.01),
			textInput("between.SD", "Between-person standard deviation of the test scores (e.g. 15 for IQ scores, or 1 for z scores)", "15"),
			
			h3("Obtained test score"),
			sliderInput("y", "Observed test score", min = 0, max = 180, value = 80, step=1),
			
			h3("Prior"),
			#radioButtons("prior.type", "Prior distribution type:", choices=c("Normal" = "norm","Gamma" = "gamma")),
			textInput("mean.prior", "Mean of prior", "100"),
			textInput("sd.prior", "SD of prior (enter large value, such as 999, for flat prior)", "15")#,
			#textInput("prior.gamma.shape", "Gamma shape parameter", "2"),
			#textInput("prior.gamma.scale", "Gamma scale parameter", "3")
		),
		column(width=7,
			htmlOutput("info"),
			plotOutput("BobsIQplot")
		)
		),
	fluidRow(
		column(width=12,
			HTML(
				"This app extends <a href='https://raw.githubusercontent.com/richarddmorey/CurDirPsySciFig2016/master/utility.R'>code</a> from Quentin Gronau and Richard Morey.<br>",
				"\"Bob's IQ\" is an example from the paper: <a href='https://webfiles.uci.edu/mdlee/WagenmakersEtAl2016.pdf'>Wagenmakers, E.-J., Morey, R. D., & Lee, M. D. (2016). Bayesian benefits for the pragmatic researcher. Current Directions in Psychological Science, 25, 169–176. doi:10.1177/0963721416643289.</a><br>",
				"Technical details: The app assumes a known (fixed) variance for the single data point, which is derived from the reliability of the measurement instrument.")
		)
	)
))
