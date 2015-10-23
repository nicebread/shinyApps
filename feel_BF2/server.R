library(shiny)
library(BayesFactor)

# construct a new environment for persistent data
dat <- new.env()
dat$n <- 40
dat$d <- 12.1		# average height differenc ein cm
dat$offset <- 165.8	# female's average height in cm
dat$SD <- 7			# height SD in cm

new_sample <- function() {
	dat$females <- rnorm(1000, dat$offset, dat$SD)
	dat$males0 <- rnorm(1000, dat$offset, dat$SD)
}

change_height <- function() {
	dat$males <- dat$males0 + dat$d
}

new_sample()
change_height()


shinyServer(function(input, output, session) {

	# Manages the refresh of the plots ...
	RV <- reactiveValues(refresh=0)

	observe({
		dat$d <- input$d
		dat$n <- input$n
		isolate({RV$refresh <- RV$refresh+1})
		change_height()
	})


	output$heightplot <- renderPlot({
		
		# react on reactive value trigger
		RV$refresh
		
		P <- data.frame(h=c(dat$females[1:dat$n], dat$males[1:dat$n]), col=c(rep(0, dat$n), rep(1, dat$n)))
		
		if (input$ordering == "shuffle") {
			P <- P[sample(nrow(P), nrow(P), replace=FALSE), ]
		}
		if (input$ordering == "height") {
			P <- P[order(P$h), ]
		}
	
		BF <- ttestBF(dat$males[1:dat$n], dat$females[1:dat$n])
	
		plot(P$h, col=ifelse(P$col==0, "red", "blue"), type="h", ylim=c(0, 200), main=bquote(BF[10]~" = "~.(round(as.vector(BF), 2))), ylab="Height in cm", xlab="")
	})
	
	
	output$densityplot <- renderPlot({
		
		# react on all button presses
		input$d
		input$n
		
	
		BF <- ttestBF(dat$males[1:dat$n], dat$females[1:dat$n])
	
		plot(density(dat$males[1:dat$n]), col="blue", main=bquote(BF[10]~" = "~.(round(as.vector(BF), 2))), xlab="")
		lines(density(dat$females[1:dat$n]), col="red")
		abline(v=mean(dat$males[1:dat$n]), col="blue", lty="dotted")
		abline(v=mean(dat$females[1:dat$n]), col="red", lty="dotted")
	})
})
