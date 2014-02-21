library(shiny)
library(lattice)
library(RSA)

shinyServer(function(input, output) {

  output$plot <- renderPlot({

	  # surface parameters are only correct in the second-degree case
	  if (any(c(input$yx2, input$xy2, input$y3, input$x3) != 0)) {
		  param <- FALSE
	  } else {
		  param <- TRUE
	  }
	  
	  suppressWarnings({
	  p1 <- plotRSA(	
		  x		= input$x,
		  y		= input$y,
		  x2	= input$x2,
		  y2	= input$y2,
		  xy	= input$xy,
		  x3	= input$x3,
		  y3	= input$y3,
		  xy2	= input$xy2,
		  x2y	= input$yx2,
		  xlim	= input$xlim,
		  ylim	= input$ylim,
		  zlim	= input$zlim,
		  param	= param,
		  type	= input$plottype
		)	
	})
	
	plot(p1)	
  })
})
