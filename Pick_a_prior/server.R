# The basic task of a Shiny server script is to define the relationship between inputs and outputs. Our script does this by accessing inputs to perform computations and by assigning reactive expressions to output slots.


library(shiny)
library(MCMCpack)

shinyServer(function(input, output) {

  # # Compute the forumla text in a reactive expression since it is 
  # # shared by the output$caption and output$mpgPlot expressions
  # formulaText <- reactive({
  #   paste("mpg ~", input$variable)
  # })
  # 
  # # Return the formula text for printing as a caption
  # output$caption <- renderText({
  #   formulaText()
  # })

  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$plot <- renderPlot({
	  
  	range <- seq(input$RANGE[1], input$RANGE[2], length.out=100)
  	if (input$distr == "normal") {
  		y <- dnorm(range, input$MEAN, input$SD)
  	}
  	if (input$distr == "gamma") {
  		y <- dgamma(range, input$alpha, input$beta)
  	}
  	if (input$distr == "invgamma") {
  		y <- dinvgamma(range, input$alpha, input$beta)
  	}
  	if (input$distr == "weibull") {
  		y <- dweibull(range, input$alpha, input$beta)
  	}
			
	plot(range, y, type="l", ylab="Density", xlab="x")
  })
})
