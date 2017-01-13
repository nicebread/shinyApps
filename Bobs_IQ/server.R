source("BobsIQplot.R")

shinyServer(function(input, output, session) {
	
	dat <- reactiveValues(
		standard.error = NA,
		standard.error.estimate = NA
	)
	
	observe({
		#dat$standard.error <- as.numeric(input$between.SD) * sqrt(1-input$reliability)
		dat$standard.error <- 15 * sqrt(1-input$reliability)
		dat$standard.error.estimate <- 15 * sqrt(input$reliability*(1-input$reliability))
	})

	
	output$info <- renderUI({
		return(list(
			#HTML("Standard error: ", round(dat$standard.error, 2))
		))
	})

  output$BobsIQplot <- renderPlot({	  
  	BobsIQplot(mean.prior=as.numeric(input$mean.prior), sd.prior=as.numeric(input$sd.prior), y=input$y, reliability=input$reliability, known.sigma = dat$standard.error, known.sigma.regression = dat$standard.error.estimate, xlim=NA)
  })
})

