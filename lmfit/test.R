library(shiny)
runApp(list(
  ui = fluidPage(
    titlePanel("Hello Shiny!"),
    sidebarLayout(
      sidebarPanel(
		  sliderInput("n", "N", min=0, max=100, value=50),
		  actionButton("go", "Go!")
      ),
      mainPanel(
        plotOutput('plot')
      )
    )
  ),
  server = function(input, output, session) {
	  
	output$plot <- renderPlot({
		hist(runif(isolate(input$n)))
	})
	  
    observe({
		if (input$go == 0) return()
			
		isolate({	
			invalidateLater(500, session)
		})
    })
	
  }
))