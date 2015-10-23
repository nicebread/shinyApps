library(shiny)
library(lattice)
library(RSA)



ui <- shinyUI(fluidPage(theme = "bootstrap.css",
	
	title = "Response surface explorer",
	
	# Add "busy" indicator
	tagList(
		tags$head(
			tags$link(rel="stylesheet", type="text/css", href="busy.css"),
			tags$script(type="text/javascript", src = "busy.js"))
	),
	div(class = "busy", p("Calculation in progress.."), img(src="preloader.gif")),
	
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
    		uiOutput("x"),
			uiOutput("y"),
			uiOutput("x2"),
  			uiOutput("y2"),
			uiOutput("xy")
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
			             c("No constraints" = "no_constraints",
							 "Basic squared differences" = "SQD",
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
	
	initialize <- TRUE
	
	# ---------------------------------------------------------------------
	# Define sliders
	output$x <- renderUI({
		if (initialize == TRUE) {
			sliderInput("x", "x", min = -2, max = 2, value = 0, step=0.01)
		} 
	})
	
	output$y <- renderUI({
		if (initialize == TRUE) {
			sliderInput("y", "y", min = -2, max = 2, value = 0, step=0.01)
		}
	})
	
	output$x2 <- renderUI({
		if (initialize == TRUE) {
			sliderInput("x2", "x2", min = -2, max = 2, value = 0, step=0.01)
		}
	})
	
	output$y2 <- renderUI({
		if (initialize == TRUE) {
			sliderInput("y2", "y2", min = -2, max = 2, value = 0, step=0.01)
		}
	})
	
	output$xy <- renderUI({
		if (initialize == TRUE) {
			sliderInput("xy", "xy", min = -2, max = 2, value = 0, step=0.01)
		}
	})
	
	
	
	
	
	
	output$x <- renderUI({
		if (input$constraint == "SQD") 
				sliderInput("x", "x", min = -2, max = 2, value = 0, step=0.01)
	})
	
	output$y <- renderUI({
		if (input$constraint == "SQD") 
				sliderInput("y", "y", min = -2, max = 2, value = 0, step=0.01)
	})
	
	output$x2 <- renderUI({
		if (input$constraint == "XXX") 
				sliderInput("x2", "x2", min = -2, max = 2, value = 0, step=0.01)
	})
	
	output$y2 <- renderUI({
		if (input$constraint == "SQD") {
				print("SQD!!!")
				sliderInput("y2", "y2", min = -2, max = 2, value = input$x2, step=0.01)
			}
	})
	
	output$xy <- renderUI({
		if (input$constraint == "SQD") 
				sliderInput("xy", "xy", min = -2, max = 2, value = -2*input$x2, step=0.01)
	})
	
	
	
	
	redraw <- TRUE
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
		print("coef2")
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
	
	

  output$plot <- renderPlot({
	  initialize <- FALSE	# with the first drawing: turn off initialization
	  print("Tyring to plot")
	  
	 # if (RD2$redraw==TRUE) {
		   print("PLOT!")
		  redraw <- FALSE
		
		  # initialize if necessary
		  # if (is.null(co1)) {
 # 			 co <- getCoef2()
 # 		  } else {
 # 		  	 co <- co1
 # 		  }
	 co <- getCoef2()

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
  		  xlim	= co$xlim,
  		  ylim	= co$ylim,
  		  zlim	= co$zlim,
  		  param	= param,
  		  type	= input$plottype,
  		  project = c(contour())
  		)	
	  	})
	
	  	plot(p1)	
		#}
  })
  
}


shinyApp(ui = ui, server = server)