library(shiny)
library(ggplot2)
source("seqplot.R")

# construct a new environment for persistent data
dat <- new.env()
reset_dat <- function() {
	dat$bf_seq <- c()
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
			dat$bf_seq <- c()
			dat$n_red <- 0
			dat$n_blue <- 0	
		}
	})
	
	observe({
		if (input$b_draw != 0) {
			if (runif(1, min=0, max=1)<=.33) {
				dat$n_blue <- dat$n_blue+1
			} else {
				dat$n_red <- dat$n_red+1
			}
			
			isolate({
				bf <- proportionBF(y = as.vector(dat$n_red), N = as.vector(dat$n_blue+dat$n_red), p = .5)
				dat$bf_seq <- c(dat$bf_seq, as.vector(bf))				
			})	
		}
	})



	output$urnplot <- renderPlot({
		
		# react on button presses
		if (input$b_draw == 0) {return()}
		input$b_reset
		
		plot(NA, xlim=c(1, 10), ylim=c(1, 15), axes=FALSE, xlab="", ylab="", main=paste0("N = ", dat$n_red+dat$n_blue))
		box(lwd=2, col="darkgrey")
		text(3, 15, bquote(n[red]~" = "~.(dat$n_red)), cex=1.5)
		text(8, 15, bquote(n[blue]~" = "~.(dat$n_blue)), cex=1.5)

		if (dat$n_red > 0) {
			for (i in 1:dat$n_red) {
				points(x=((i-1) %% 5)+1, y=((i-1) %/% 5) + 1, cex=4, col="red", pch=20)
			}
		}
		if (dat$n_blue > 0) {
			for (i in 1:dat$n_blue) {
				points(x=((i-1) %% 5)+6, y=((i-1) %/% 5) + 1, cex=4, col="blue", pch=20)
			}
		}
	})


	output$seqplot <- renderPlot({
		# react on button presses
		if (input$b_draw == 0) {return()}
		input$b_reset

		if (length(dat$bf_seq) > 0) {
			seqBFplot(1:length(dat$bf_seq), dat$bf_seq, log.it=TRUE, main=bquote(BF[final]~" = "~.(round(dat$bf_seq[length(dat$bf_seq)], 2))))
		}
		
	}, res=150)
})
