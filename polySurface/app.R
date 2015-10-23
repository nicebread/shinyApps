library(shiny)
library(lattice)
library(RSA)



ui <- shinyUI(fluidPage(theme = "bootstrap.css",
	
	title = "Response surface explorer",
	
	titlePanel("Response surface explorer"),

	fluidRow(
		column(8, 
			plotOutput("plot"),
			helpText("This widgets relies on the RSA package for plotting.")
		),
		column(4,
			h4("Plot options"),
			sliderInput("xlim", "Range of x-axis", min = -10, max = 10, value = c(-2, 2), step=0.1),
			sliderInput("ylim", "Range of y-axis", min = -10, max = 10, value = c(-2, 2), step=0.1),
			sliderInput("zlim", "Range of z-axis", min = -10, max = 10, value = c(-2, 2), step=0.1),
			selectInput("plottype", "Plot type:", c("3d wireframe" = "3d", "2d contour" = "contour")),
			checkboxInput("project_contour", "Project contour", value = FALSE)
		)
	),
	

	fluidRow(
	    column(4,
	      h4("Second-degree coefficients"),
    		sliderInput("x", "x", min = -2, max = 2, value = 0, step=0.01),			
  			sliderInput("x2", "x^2", min = -2, max = 2, value = 0, step=0.01),
  			sliderInput("y", "y", min = -2, max = 2, value = 0, step=0.01),
  			sliderInput("y2", "y^2", min = -2, max = 2, value = 0, step=0.01),
  			sliderInput("xy", "x*y", min = -2, max = 2, value = 0, step=0.01)
	    ),
	    column(4,
			h4("Third-degree coefficients"),
			sliderInput("x3", "x^3", min = -2, max = 2, value = 0, step=0.01),
			sliderInput("y3", "y^3", min = -2, max = 2, value = 0, step=0.01),
			sliderInput("xy2", "x*y^2", min = -2, max = 2, value = 0, step=0.01),
			sliderInput("x2y", "x2*y", min = -2, max = 2, value = 0, step=0.01)
	    ),
	    column(4,
			h4("Constraints"),
			radioButtons("constraint", "",
			             c("Basic squared differences" = "SQD",
			               "Shifted squared differences" = "SSQD",
			               "Rising ridge" = "RR",
			               "Shifted Rising Ridge" = "SRR"))
	    )
	  )
   
))



## ======================================================================
## The server side
## ======================================================================

server <- function(input, output, session) {
	
	RD2 <- reactiveValues(redraw=FALSE)
	
	getCoef <- reactive({
		co1 <- data.frame(
			x   = isolate(input$x),
			y   = isolate(input$y),
			x2  = isolate(input$x2),
			y2  = isolate(input$y2),
			xy  = isolate(input$xy),
			x3  = isolate(input$x3),
			y3  = isolate(input$y3),
			x2y = isolate(input$x2y),
			xy2 = isolate(input$xy2)
		)
		return(co1)
	})
	
	getCoef2 <- reactive({
		co1 <- data.frame(
			x   = input$x,
			y   = input$y,
			x2  = input$x2,
			y2  = input$y2,
			xy  = input$xy,
			x3  = input$x3,
			y3  = input$y3,
			x2y = input$x2y,
			xy2 = input$xy2
		)
		return(co1)
	})
	
	
	# initialize values
	co1 <- NULL
	
	plottype	<- reactive({ input$plottype })
	contour		<- reactive({ ifelse(input$project_contour == TRUE, "contour", "") })	
	
	changed <- function(name, value) {		
		if (isolate(input[[name]] == value)) {
			return(FALSE)
		} else {
			print(paste0("Change: ", name))
			isolate(updateSliderInput(session, name, value = value))
			return(TRUE)
		}
	}
	
	
	
	# # Here the constraints are applied
	observe({
		cat("\n\n######################\nobserver\n")
		
		RD2$redraw <- FALSE

		co0 <- getCoef2()
		if (is.null(co1)) co1 <- co0

  	    if (input$constraint == "SQD") {
			FS <- c()
  	    	co1$x <- 0;				FS <- c(FS, changed("x", value = 0))
  	  		co1$y <- 0;				FS <- c(FS, changed("y", value = 0))
  	  		co1$y2 <- co1$x2; 		FS <- c(FS, changed("y2", value = co1$y2))
  	  		co1$xy <- -2*co1$x2; 	FS <- c(FS, changed("xy", value = co1$xy))
  	    }
		
		# print("co0: "); print(co0)
		# print("co1: "); print(co1)
		# print(all(round(co0, 10) == round(co1, 10)))
		print(paste0("RD2$redraw: ", isolate(RD2$redraw)))
		
		if (all(round(co0, 10) == round(co1, 10))) {
			print("Now I'm ready ...")
			RD2$redraw <- TRUE
			print(paste0("RD2$redraw: ", isolate(RD2$redraw)))
		}
	})



  output$plot <- renderPlot({

	  print("Tyring to plot")
	  print(paste0("RD2$redraw: ", isolate(RD2$redraw)))
	  
	  
	  if (RD2$redraw==TRUE) {
		  if (is.null(co1)) {
			  co1 <- getCoef2()
		  }
		  co <- co1

		  # surface parameters are only correct in the second-degree case
	  	  if (any(c(co$x2y, co$xy2, co$y3, co$x3) != 0)) {
	  		  param <- FALSE
	  	  } else {
	  		  param <- TRUE
	  	  }
	  
	  	  suppressWarnings({
	  	  p1 <- plotRSA(	
	  		  x		= co$x,
	  		  y		= co$y,
	  		  x2	= co$x2,
	  		  y2	= co$y2,
	  		  xy	= co$xy,
	  		  x3	= co$x3,
	  		  y3	= co$y3,
	  		  xy2	= co$xy2,
	  		  x2y	= co$x2y,
	  		  xlim	= input$xlim,
	  		  ylim	= input$ylim,
	  		  zlim	= input$zlim,
	  		  param	= param,
	  		  type	= plottype(),
	  		  project = c(contour())
	  		)	
		  	})
	
			print("PLOT!")
		  	print(p1)			
		}
  })
  
}


shinyApp(ui = ui, server = server)