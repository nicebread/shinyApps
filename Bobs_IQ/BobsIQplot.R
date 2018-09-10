# simple wrapper: formats a number in f.2 format
f2 <- function(x, digits=2, prepoint=0, skipZero=FALSE) {
	
	if (skipZero == TRUE) {zero <- "."} else {zero <- "0."}
	
	if (length(dim(x)) == 2) {
		apply(x, 2, function(x2) {gsub("0.", zero, sprintf(paste("%",prepoint,".",digits,"f",sep=""), x2) , fixed=TRUE)})
	} else {
		gsub("0.", zero, sprintf(paste("%",prepoint,".",digits,"f",sep=""), x) , fixed=TRUE)
	}
}




BobsIQplot <- function(mean.prior, sd.prior, y, known.sigma, known.sigma.regression=NA, reliability=NA, xlim=NA, freq.CI=TRUE, digits=1, show=c("prior", "likelihood", "posterior"), show.legend=TRUE, show.CI=TRUE, legend.position="topright", xlab=NA, ylab=NA) {
	
  # compute conjugate posterior for Gaussian distribution with known variance
  mean.posterior <- (sd.prior^2 / (sd.prior^2 + known.sigma^2)) * y + (known.sigma^2 / (sd.prior^2 + known.sigma^2))*mean.prior  
  sd.posterior <- sqrt((sd.prior^2 * known.sigma^2) / (sd.prior^2 + known.sigma^2))
  
  # Compute CIs
  
  ## Bayesian credible interval
  b.CI <- qnorm(c(.025,.975), mean.posterior, sd.posterior)
  
  # frequentist CI
  f.CI <- y + c(-1, 1)*1.96*known.sigma
  
	# regression CI
	if (!is.na(known.sigma.regression) & !is.na(reliability)) {
		y.hat <- reliability*y + mean.prior*(1-reliability)
		r.CI <- y.hat + c(-1, 1)*1.96*known.sigma.regression
	}
   
  ### plot settings
  if (any(is.na(xlim))) {
  	xlim <- range(c(mean.prior+c(-2.5, 2.5)*sd.prior, mean.posterior+c(-2.5, 2.5)*sd.posterior, b.CI, f.CI))
	xlim[1] <- xlim[1] / 1.1
	xlim[2] <- xlim[2] * 1.1
	if (xlim[1] < 0) xlim[1] <- 0
	if (xlim[2] > 200) xlim[2] <- 200
  }
  
  ylim <- c(0, 1.9*dnorm(mean.posterior, mean.posterior, sd.posterior))
  lwd <- 2
  lwd.points <- 2
  lwd.axis <- 1.2
  cex.points <- 1.4
  cex.axis <- 1.2
  cex.text <- 1.1
  cex.labels <- 1.3
  cexLegend <- 1.2
  
  ### create empty canvas
  plot(1, xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "")
    
  ### axes
  axis(1, at = seq(ceiling(xlim[1]/5)*5, floor(xlim[2]/5)*5, 5), cex.axis = cex.axis, lwd = lwd.axis)
  axis(2, labels = FALSE, tck = 0, lwd = lwd.axis, line = -0.5)
  
  ### axes labels
  mtext(ifelse(is.na(xlab), "Value", xlab), side = 1, cex = 1.6, line = 2.4)
  mtext(ifelse(is.na(ylab), "Density", ylab), side = 2, cex = 1.5, line = 0)
  
  ### plot prior and posterior
  
  xx <- sort(c(seq(xlim[1], xlim[2],len=250), qnorm(1:30/31, mean.prior, sd.prior), qnorm(1:30/31, mean.posterior, sd.posterior)))
  
  # prior
	if ("prior" %in% show) {
		lines(xx, dnorm(xx, mean.prior, sd.prior), lwd = lwd, lty = "dashed")
	}
	
	# likelihood
	if ("likelihood" %in% show) {
		lines(xx, dnorm(xx, y, known.sigma), lwd = lwd, lty = "dotted")
  }
	
  # posterior
	if ("posterior" %in% show) {
		lines(xx, dnorm(xx, mean.posterior, sd.posterior), lwd = lwd)
  }
	
	  
	if ("posterior" %in% show & show.CI == TRUE) {
		
	  ### grey dashed lines to prior mean and posterior mean
	  lines(rep(mean.prior, 2), c(0, dnorm(mean.prior, mean.prior, sd.prior)), lty = 2, col = "grey", lwd = lwd)
	  lines(rep(mean.posterior, 2), c(0, dnorm(mean.posterior, mean.posterior, sd.posterior)), lty = 2, col = "grey", lwd = lwd)
		
		
	  ### add point and text at maximum a posteriori value
	  post_dens <- dnorm(mean.posterior, mean.posterior, sd.posterior)
	  points(mean.posterior, post_dens, pch = 22, bg = "white", cex = cex.points, lwd = lwd.points)
	  text(mean.posterior, post_dens, labels = paste0("Mode = ", f2(mean.posterior, digits)), cex = cex.text, pos = 3, offset = 0.5)
		
		
	  # PLot Bayesian credible interval
	  yCI <- post_dens + strheight("XXX")*4
	  arrows(b.CI[1], yCI, b.CI[2], yCI, angle = 90, code = 3, length = 0.1, lwd = lwd)
	  text(mean.posterior, yCI, labels = "95% HDI", cex = cex.text, adj=c(0.5, -0.5))
	  text(b.CI[1], yCI, labels = paste(f2(b.CI[1], digits)), cex = cex.text, pos = 2, offset = 0.3)
	  text(b.CI[2], yCI, labels = paste(f2(b.CI[2], digits)), cex = cex.text, pos = 4, offset = 0.3)
  
	  ### frequentist CI
	  if (freq.CI==TRUE) {
		  f.yCI <- post_dens + strheight("XXX")*7
		  arrows(f.CI[1], f.yCI, f.CI[2], f.yCI, angle = 90, code = 3, length = 0.1, lwd = lwd, col="red")
		  text(mean(y), f.yCI, labels = "95% CI", cex = cex.text, col="red", adj=c(0.5, -0.5))
		  text(f.CI[1], f.yCI, labels = paste(f2(f.CI[1], digits)), cex = cex.text, pos = 2, offset = 0.3, col="red")
		  text(f.CI[2], f.yCI, labels = paste(f2(f.CI[2], digits)), cex = cex.text, pos = 4, offset = 0.3, col="red")
	  }
	   
	  ### regression CI
	 if (!is.na(known.sigma.regression) & !is.na(reliability)) {
		  r.yCI <- post_dens + strheight("XXX")*9
		  arrows(r.CI[1], r.yCI, r.CI[2], r.yCI, angle = 90, code = 3, length = 0.1, lwd = lwd, col="green")
		  text(mean(y), r.yCI, labels = "95% r.CI", cex = cex.text, col="green", adj=c(0.5, -0.5))
		  text(r.CI[1], r.yCI, labels = paste(f2(r.CI[1], digits)), cex = cex.text, pos = 2, offset = 0.3, col="green")
		  text(r.CI[2], r.yCI, labels = paste(f2(r.CI[2], digits)), cex = cex.text, pos = 4, offset = 0.3, col="green")
		}
	}  
	
  ## show obtained test score
  arrows(y, 0, y, ylim[2]*0.1, angle = 30, code = 1, length = 0.1, lwd = lwd*1.6, col="red")
  
  ### legend
	if (show.legend==TRUE) {
	  legend(legend.position, legend = c("Likelihood", "Prior", "Posterior"), lty = c("dotted", "dashed", "solid"),
	         bty = "n", lwd = c(lwd, lwd), cex = cexLegend, xjust = 1, yjust = 1, x.intersp = 0.6, 
	         seg.len = 1.2)
	}
}

# BobsIQplot(mean.prior=100, sd.prior=15, y=80, known.sigma=15 * sqrt(1-.97))
#
# BobsIQplot(mean.prior=100, sd.prior=15, y=80, known.sigma=15 * sqrt(1-.80), show.CI=FALSE)

#
# BobsIQplot(mean.prior=100, sd.prior=15, y=80, known.sigma=15 * sqrt(1-.70), show="likelihood", show.legend=FALSE, xlim=c(60, 141), xlab="Theta", ylab="Plausibility")
#
# BobsIQplot(mean.prior=100, sd.prior=15, y=80, known.sigma=15 * sqrt(1-.98), show="likelihood", show.legend=FALSE, xlim=c(60, 141), xlab="Theta", ylab="Plausibility")
#
#
# BobsIQplot(mean.prior=100, sd.prior=999, y=80, known.sigma=15 * sqrt(1-.85), xlim=c(60, 141))
# BobsIQplot(mean.prior=100, sd.prior=15, y=80, known.sigma=15 * sqrt(1-.85), xlim=c(60, 141))
# BobsIQplot(mean.prior=130, sd.prior=15, y=80, known.sigma=15 * sqrt(1-.85), xlim=c(60, 141))
# BobsIQplot(mean.prior=130, sd.prior=15, y=80, known.sigma=15 * sqrt(1-.60), xlim=c(60, 141))

#BobsIQplot(mean.prior=0.95, sd.prior=5.1, y=18.3, known.sigma=0.1, xlim=c(-1, 20))
#BobsIQplot(mean.prior=0.95, sd.prior=5.1, y=18.3, known.sigma=5, xlim=c(-20, 30), legend.position="topleft")