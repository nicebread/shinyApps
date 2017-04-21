library(shiny)
library(ggplot2)
source("seqplot.R")

# construct a new environment for persistent data
dat <- new.env()
reset_dat <- function() {
	dat$bf_seq <- c()	# stores the computed BFs
	dat$n_seq <- c()	# stores the n at which each BF has been computed
	dat$n_red <- 0
	dat$n_blue <- 0	
	dat$refreshs <- 0
}
reset_dat()


shinyServer(function(input, output, session) {

	RV <- reactiveValues(refresh=0)

	# reset function
	observe({
		if (input$b_reset != 0) {
			reset_dat()
		}
	})
	
	observe({
		if (input$b_addRed != 0) dat$n_red <- dat$n_red+1
		if (isolate(input$cb_autoupdate) == TRUE) {isolate({RV$refresh <- RV$refresh+1})}
	})
	
	observe({
		if (input$b_addBlue != 0) dat$n_blue <- dat$n_blue+1
		if (isolate(input$cb_autoupdate) == TRUE) {isolate({RV$refresh <- RV$refresh+1})}				
	})
	
	observe({
		if (input$b_addRed10 != 0) dat$n_red <- dat$n_red+10
		if (isolate(input$cb_autoupdate) == TRUE) {isolate({RV$refresh <- RV$refresh+1})}
	})

	observe({
		if (input$b_addBlue10 != 0) dat$n_blue <- dat$n_blue+10
		if (isolate(input$cb_autoupdate) == TRUE) {isolate({RV$refresh <- RV$refresh+1})}				
	})	



	output$urnplot <- renderPlot({
		
		# react on all button presses
		input$b_addRed
		input$b_addBlue
		input$b_addRed10
		input$b_addBlue10
		input$b_reset
		
		# define dimensions of the urnplot
		if (any(dat$n_blue > 60, dat$n_red > 60)) {
			maxX <- 10
			maxY <- 35
			cex <- 1.5
		} else {
			maxX <- 5
			maxY <- 15
			cex <- 4
		}
		
		plot(NA, xlim=c(1, maxX*2), ylim=c(1, maxY), axes=FALSE, xlab="", ylab="", main=paste0("N = ", dat$n_red+dat$n_blue))
		box(lwd=2, col="darkgrey")
		text((maxX-1)/2 + 1, maxY, bquote(n[red]~" = "~.(dat$n_red)), cex=1.5)
		text((maxX-1)/2 + 1 + maxX, maxY, bquote(n[blue]~" = "~.(dat$n_blue)), cex=1.5)

		if (dat$n_red > 0) {
			for (i in 1:dat$n_red) {
				points(x=((i-1) %% maxX)+1, y=((i-1) %/% maxX) + 1, cex=cex, col="red", pch=20)
			}
		}
		if (dat$n_blue > 0) {
			for (i in 1:dat$n_blue) {
				points(x=((i-1) %% maxX)+maxX+1, y=((i-1) %/% maxX) + 1, cex=cex, col="blue", pch=20)
			}
		}
	})


	output$seqplot <- renderPlot({
		# react on "update" and "reset" button presses, and on a refresh trigger
		input$b_update
		input$b_reset		
		RV$refresh

		if ((dat$n_blue+dat$n_red) > 0) {
			N <- as.vector(dat$n_blue+dat$n_red)
			bf <- proportionBF(y = as.vector(dat$n_red), N = N, p = .5)
			dat$bf_seq <- c(dat$bf_seq, as.vector(bf))
			dat$n_seq <- c(dat$n_seq, N)
			seqBFplot(dat$n_seq, dat$bf_seq, log.it=TRUE, main=bquote(BF[final]~" = "~.(round(as.vector(bf), 2))))
		}
		
	}, res=150)
})
