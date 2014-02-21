library(shiny)
library(BayesFactor)
library(plyr)

d2t <- Vectorize(function(d, n) (d*sqrt(2*n - 2)) / 2)

shinyServer(function(input, output) {

  output$plot <- renderPlot({
	
	  # import the r's
	  r.range <- as.numeric(strsplit(input$rs, ",")[[1]])
	
	  # define the sample sizes at which the BF is calculated
	  n.range <- round(seq(10, input$max.N, length.out = 50))
	  
	  # compute the Bayes factor for each n and each r
	  bf <- data.frame()
	  for (n in n.range) {
		  for (r in r.range) {
			  bf <- rbind(bf, data.frame(
				  BF 	= ttest.tstat(t=d2t(input$D, n), n1=n, n2=n, rscale=r)$bf,
				  n		= n,
				  r		= r
			  ))
		  }
	  }
	  
	  # compute the ratio between the minimal and the maximal BF
	  ratio <- ddply(bf, .(n), function(x) c(ratio=max(exp(x$BF))/min(exp(x$BF))))
	  
	  # Plot the results
	  par(mar=c(5, 5, 2, 2) + 0.1)
	  plot(NA, 
		  xlim	= c(min(n.range), input$max.N*1.25), 
		  ylim	= c(-log(30), log(30)), 
		  xlab	= "n in each group", 
		  ylab	= "", 
		  las	= 2,
		  yaxt	= "n",
		  main	= paste0("Ratio between minimal and maximal BF: ", round(min(ratio$ratio), 2), " - ", round(max(ratio$ratio), 2), "x"))
	  
	  # define the y-axis
	  axis(
		  side	= 2, las = 2, 
		  at	= c(-log(c(30, 10, 3)), log(c(1, 3, 10, 30))), 
		  labels = paste0("log(", c(-30, -10, -3, 1, 3, 10, 30), ")")
		)
	  mtext("log(BF)", side=2, line=4, at=0, las=3, cex=1)
	  
	  # plot the lines
	  for (r in r.range) {
		  lines(bf$n[bf$r == r], bf$BF[bf$r == r], col=which(r.range == r))
	  }
	  
	  # annotation
		abline(h=-log(c(30, 10, 3)), lty="dotted")
		abline(h=log(c(30, 10, 3)), lty="dotted")
		abline(h=0, col="blue", lwd=2)


		text(input$max.N*1.25, -2.85, expression("Strong"~H[0]), adj=1)
		text(input$max.N*1.25, -1.7, expression("Substantial"~H[0]), adj=1)
		text(input$max.N*1.25, -0.55, expression("Anectodal"~H[0]), adj=1)
		text(input$max.N*1.25, 2.85, expression("Strong"~H[1]), adj=1)
		text(input$max.N*1.25, 1.7, expression("Substantial"~H[1]), adj=1)
		text(input$max.N*1.25, 0.55, expression("Anectodal"~H[1]), adj=1)

		legend("topleft", lty="solid", lwd=2, col=1:length(r.range), legend=paste0("r = ", round(r.range, 2)))
  })
})
