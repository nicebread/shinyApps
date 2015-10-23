library(shiny)


# construct a new environment for global data sharing
dat <- new.env()

reset_values <- function() {
	dat$n <- 40
	dat$x <- rnorm(dat$n)
	dat$b0 <- runif(1, -1.5, 1.5)
	dat$b1 <- runif(1, -1, 1)
	dat$y <-  dat$b0 + dat$b1*dat$x + rnorm(dat$n, 0, 0.8)
	
	# These variables store indices of the optimal fit
	dat$l1 <- lm(dat$y~dat$x)
	dat$RSS_opt <- sum(resid(dat$l1)^2)
	
	# These variables store the sequence of (manual) optimization steps
	dat$RSS.manual <- c()
	dat$ESS.manual <- c()

	dat$optim.pars <- data.frame()
	dat$optim.pars.plot <- data.frame()
}
reset_values()


# The minimization function for optim
minimize_RSS <- function(par) {
  y_hat <- par[1] + par[2]*dat$x	 # predicted values
  RSS.optim <- sum((y_hat - dat$y)^2) # residual sum of squares

  # save parameter set for intermediate results
  dat$optim.pars <- rbind(dat$optim.pars, c(par, RSS.optim))
  
  return(RSS.optim)
}

min_function <- minimize_RSS


shinyServer(function(input, output, session) {

  output$scatterplot <- renderPlot({
	  
	  input$reset_it	# this call triggers this function whenever the reset button is clicked.
	  input$optim
	  
	  # if the optim has been called, we want to show the sequence of optimization steps
	  # always increase the to-be-plotted sequence until it reaxhes the end
	  if (nrow(dat$optim.pars.plot) < nrow(dat$optim.pars)) {
		dat$optim.pars.plot <- dat$optim.pars[1:(nrow(dat$optim.pars.plot)+1), ]
	  	invalidateLater(150, session)
	  }

	  y_hat <- input$b0 + input$b1*dat$x	 # predicted values
	  dat$RSS.manual <- c(dat$RSS.manual, sum((y_hat - dat$y)^2)) # residual sum of squares
	  
	  par(mfrow=c(1, 2))
	  # ---------------------------------------------------------------------
	  # Scatterplot
	  
	  if (input$showFit == TRUE) {
		  title <- paste0("Raw data + residuals\nIntercept = ", round(coef(dat$l1)[1], 2), "; Slope = ", round(coef(dat$l1)[2], 2))
	  } else {
		  title <- "Raw data + residuals"
	  }
	  
	  plot(dat$x, dat$y, pch=20, main=title, xlab="X", ylab="Y", las=1)
	  
	  # manual abline
	  abline(a=input$b0, b=input$b1, lwd=1.5)
	  
	  # optim abline
	  if (nrow(dat$optim.pars.plot) > 0)
	  	abline(a=tail(dat$optim.pars.plot[ ,1], 1), b=tail(dat$optim.pars.plot[ ,2], 1), lwd=2, col="darkblue")
	  
	  if (input$showFit == TRUE) abline(dat$l1, col="darkgreen", lty="dashed", lwd=2)
		  
	  # plot distances between points and the regression line
	  segments(dat$x, dat$y, dat$x, y_hat, col="red")
	  
	  
	  ## ======================================================================
	  ## RSS plot
	  ## ======================================================================
	  
	  XLIM <- c(1, max(10, length(dat$RSS.manual), nrow(dat$optim.pars.plot)))
	  if (nrow(dat$optim.pars.plot) > 0) {
		  YLIM <- c(dat$RSS_opt*0.9, max(c(dat$RSS.manual, dat$optim.pars.plot[ ,3]))*1.2)
	  } else {
	  	YLIM <- c(dat$RSS_opt*0.9, max(dat$RSS.manual)*1.2)
	  }
	  
	  # ---------------------------------------------------------------------
	  # Manual optimization steps
	  plot(1:length(dat$RSS.manual), dat$RSS.manual, type="o", pch=20, col="red", 
	  	xlim=XLIM, ylim=YLIM, xlab="Step", ylab="RSS", main="Residual sum of squares\nSmaller values = better fit")
	
  	  # ---------------------------------------------------------------------
  	  # 'optim' optimization steps
	  if (nrow(dat$optim.pars.plot) > 0) {
		  lines(1:nrow(dat$optim.pars.plot), dat$optim.pars.plot[ ,3], type="o", pch=20, col="darkblue")
	  }

	  abline(h=dat$RSS_opt, col="red", lty="dashed")
  })
  
  
  # Observe reset button and do something it has been clicked
  observe({
    if (input$reset_it == 0) return()
    isolate({
  		reset_values()
		updateSliderInput(session, "b0", value = 0)
		updateSliderInput(session, "b1", value = 0)
  		invalidateLater(1, session)
    })
  })
  
  
	 # Observe optim button and do something it has been clicked
	observe({
		if (input$optim == 0) return()
			
		isolate({
			# reset the optimization history
			dat$optim.pars <- data.frame()
			dat$optim.pars.plot <- data.frame()
			
			# optimize parameters. Reduce relative tolerance (otherwise the optimizer stays quite long around the final line)
			result <- optim(par = c(0, 0), min_function, control=list(reltol=.002))
			
			# show the sequence of optimization steps
			invalidateLater(1, session)
			
		})
	})
  
})
