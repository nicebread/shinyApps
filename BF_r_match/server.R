library(shiny)
library(BayesFactor)
library(plyr)

d2t <- Vectorize(function(d, n) (d*sqrt(2*n - 2)) / 2)

shinyServer(function(input, output) {

  output$plot <- renderPlot({
	
	  # import the r's
	  r.range <- as.numeric(strsplit(input$rs, ",")[[1]])
	
	  # define the d's at which the BF is calculated
	  d.range <- seq(input$ds[1], input$ds[2], length.out=50)
	  
	  nullInterval <- NULL
	  if (input$oneSided == TRUE) nullInterval <- c(-Inf, 0)		  
	  
	  # compute the Bayes factor for each d and each r
	  bf <- data.frame()
	  for (d in d.range) {
		  for (r in r.range) {
			  
			  BF0 <- ttest.tstat(t=d2t(d, input$N), n1=input$N, n2=input$N, rscale=r, nullInterval=nullInterval)$bf
			  
			  if (input$oneSided == TRUE) {
				  BF <- BF0["alt"]
			  } else {
				  BF <- BF0
			  }
			  bf <- rbind(bf, data.frame(
				  BF 	= BF,
				  n		= input$N,
				  r		= r,
				  d		= d
			  ))
		  }
	  }
	  

	  # Plot the results
	  par(mar=c(5, 5, 2, 2) + 0.1)
	  plot(NA, 
		  xlim	= c(min(d.range), max(d.range)), 
		  ylim	= c(-log(30), log(30)), 
		  #ylim	= c(-log(30000), log(30000)), 
		  xlab	= "d", 
		  ylab	= "", 
		  las	= 2,
		  yaxt	= "n",
		  main	= paste0("n = ", input$N))
	  
	  # define the y-axis
	  axis(
		  side	= 2, las = 2, 
		  at	= c(-log(c(30, 10, 3)), log(c(1, 3, 10, 30))), 
		  labels = paste0("log(", c(-30, -10, -3, 1, 3, 10, 30), ")")
		)
	  mtext("log(BF)", side=2, line=4, at=0, las=3, cex=1)
	  
	  # plot the lines
	  for (r in r.range) {
		  lines(bf$d[bf$r == r], bf$BF[bf$r == r], col=which(r.range == r), lwd=2)
	  }
	  
	  # annotation
		abline(h=-log(c(30, 10, 3)), lty="dotted")
		abline(h=log(c(30, 10, 3)), lty="dotted")
		abline(h=0, col="blue", lwd=2)


		text(max(d.range), -2.85, expression("Strong"~H[0]), adj=1)
		text(max(d.range), -1.7, expression("Substantial"~H[0]), adj=1)
		text(max(d.range), -0.55, expression("Anectodal"~H[0]), adj=1)
		text(max(d.range), 2.85, expression("Strong"~H[1]), adj=1)
		text(max(d.range), 1.7, expression("Substantial"~H[1]), adj=1)
		text(max(d.range), 0.55, expression("Anectodal"~H[1]), adj=1)

		legend("topleft", lty="solid", lwd=2, col=1:length(r.range), legend=paste0("r = ", round(r.range, 2)))
  })
})
