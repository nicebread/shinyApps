library(shiny)
library(RSA)

n <- 2000

shinyServer(function(input, output) {
	
	# ---------------------------------------------------------------------
	# Set up some "reactive conductors": Some values should only be computed once
	# These new variables are *functions* - have to called with ()
	# ---------------------------------------------------------------------
	xi0 			<- reactive({ input$xi })
	dist0 			<- reactive({ 
		dist <- rsnorm(10000, mean=0, sd=1, xi=xi0())
		skew <- skewness(dist)
		kurt <- kurtosis(dist)

		df <- data.frame()
		TRAIT <- rnorm(n, 4, 2)
		for (i in 1:n) {		
			df <- rbind(df, rsnorm(3, mean=0, sd=1, xi=xi0())+TRAIT[i])
		}
		colnames(df) <- c("T1", "T2", "T3")
		df$T1_2 <- df$T1^2
		df$T2_2 <- df$T2^2
		df$T1_T2 <- df$T1*df$T2

		r1 <- lm(T3 ~ T1 + T2 + T1_2 + T1_T2 + T2_2, df)
		return(list(r1=coef(r1), skew=skew, kurt=kurt))
	 })
	
	
	
	
	## ======================================================================
	## Outputs
	## ======================================================================
	
	 
	# ---------------------------------------------------------------------
	# Bias plot
	output$summary.out <- renderText({
		# ES2 <- factor(ES(), levels=c("g.emp", "shrunk_0.5","shrunk_0.4","shrunk_0.3","shrunk_0.2","shrunk_0.1"), labels=c("Naive", "model-average, p(H1) = .5","model-average, p(H1) = .4","model-average, p(H1) = .3","model-average, p(H1) = .2","model-average, p(H1) = .1"))
		# m2 <- paste0("Estimator: ", ES2)
		# m2
		
	 })
	
	    output$plot.out <- renderPlot({
			dist <- dist0()
			p1 <- plotRSA(b0=dist$r1[1], x=dist$r1[2], y=dist$r1[3], x2=dist$r1[4], xy=dist$r1[5], y2=dist$r1[6], main=paste0("Skewness: ", round(dist$skew, 3)), legend=FALSE, xlim=c(-2, 2), ylim=c(-2, 2), zlim=c(-2.5, 2.5))
			print(p1)
	  	})
	  
  
})
