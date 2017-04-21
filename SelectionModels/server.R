ll <- function(mu, tau2, yi, vi, method="ML") {
   k <- length(yi)
   wi <- 1/(vi + tau2)
   if (method == "ML") {
      sum(dnorm(yi, mean=mu, sqrt(vi + tau2), log=TRUE))
   } else {
      -1/2 * (k-1) * log(2*pi) + 1/2 * log(k) - 1/2 * sum(log(vi + tau2)) - 1/2 * log(sum(wi)) - 1/2 * sum(wi * (yi - mu)^2)
   }
}

llTPSM <- function(mu, tau2, yi, vi) {
   wi <- 1/(vi + tau2)
   dnorm(yi, mean=mu, sqrt(vi + tau2), log=TRUE)
}

ll2 <- Vectorize(ll, vectorize.args="mu")


# delta <- theta[1]						# True population average effect size
# tau <- theta[2]							# Heterogeneity
# w <- theta[3]							  # Relative likelihood non-stat sig study is reported
onestep.heterogeneous.nll <- function(delta, tau, w, z.obs, n1=100, n2=100, alpha=0.025){
	z.cut <- qnorm(1-alpha)
	d.obs <- z.obs / sqrt((n1*n2)/(n1+n2))
	d.cut <- z.cut / sqrt((n1*n2)/(n1+n2))
	k <- sum(z.obs<z.cut)

	s <- sqrt(tau^2 + 1/n1 + 1/n2)
	ll <- ifelse(k==0, 0, k*log(w))			# Equivalent to defining 0*log(0)=0 as is common
	ll <- ll + sum(dnorm(d.obs, delta, s, log=TRUE))
	#ll <- ll - sum(log( w*pnorm(d.cut,delta,s) + (1 - pnorm(d.cut,delta,s)) ))
	ll
}


shinyServer(function(input, output, session) {

	
	output$info <- renderUI({
		return(list(
			#HTML("Standard error: ", round(dat$standard.error, 2))
		))
	})

  output$Lplot <- renderPlot({	  
  	range <- seq(-1, 2, by=.01)
		L <- exp(TPSM.LL(range, input$tau, w=input$w, z.obs = input$mu))
		plot(range, L, type="l")
  })
})

