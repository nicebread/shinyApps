library(shiny)
library(BayesFactor)
library(ggplot2)

BFrobustplot <- function(
	ts, ns, rs=seq(0, 1.5, length.out=50), dots=NULL, plot=TRUE, 
	labels=c(), sides="two", nrow=(length(ts) %/% 3)+1, xticks=3, forH1=TRUE) 
{
	
	sides <- match.arg(sides, c("one", "two"))
	
	if (is.null(dots)) {
		if (sides=="one") dots <- c(0.5, sqrt(2)/2, 1)
		if (sides=="two") dots <- c(sqrt(2)/2, 1, sqrt(2))
	}
	
	# add the dots location to the sequences of r's
	rs <- c(rs, dots)
		
	res <- data.frame()
	for (r in rs) {
		
		# first: calculate two-sided BF
		B_e0 <- c()
		for (i in 1:length(ts)) {
			B_e0 <- c(B_e0, exp(ttest.tstat(t = ts[i], n1 = ns[i], rscale=r)$bf))
		}
		BF <- B_e0
	
		
		# second (if requested): calculate one-sided BF from the two-sided
		if (sides=="one") {
			
			# compute one-sided p-values from ts and ns
			ps <- pt(ts, df=ns-1, lower.tail = FALSE)   # one-sided test
			
			B_r0 <- c()
			for (i in 1:length(ts)) {
				if (ts[i] > 0) {
					# correct direction
					B_r0 <- c(B_r0, (2 - 2*ps[i])*B_e0[i])
				} else {
					# wrong direction
					B_r0 <- c(B_r0, (1 - ps[i])*2*B_e0[i])
				}
			}
			
			BF <- B_r0
		}
	
		res0 <- data.frame(t=ts, n=ns, BF=BF, r=r)
		if (length(labels) > 0) {
			res0$labels <- labels
			res0$heading <- factor(1:length(labels), labels=paste0(labels, "\n(t = ", ts, ", df = ", ns-1, ")"), ordered=TRUE)
		} else {
			res0$heading <- factor(1:length(ts), labels=paste0("t = ", ts, ", df = ", ns-1), ordered=TRUE)
		}
		
		res <- rbind(res, res0)
	}
	
	# Flip BF if requested
	if (forH1 == FALSE) {
		res$BF <- 1/res$BF
	}
		
	
	if (plot==TRUE) {		
		p1 <- ggplot(res, aes(x=r, y=log(BF))) + geom_line() + facet_wrap(~heading, nrow=nrow) + theme_bw() + ylab("log(BF)")
		p1 <- p1 + geom_hline(yintercept=c(c(-log(c(30, 10, 3)), log(c(3, 10, 30)))), linetype="dotted", color="darkgrey")
		p1 <- p1 + geom_hline(yintercept=log(1), linetype="dashed", color="darkgreen")
	
		# add the dots
		p1 <- p1 + geom_point(data=res[res$r %in% dots,], aes(x=r, y=log(BF)), color="red", size=2)
	
		# add annotation
		p1 <- p1 + annotate("text", x=max(rs)*1.8, y=-2.85, label=paste0("Strong~H[", ifelse(forH1==TRUE,0,1), "]"), hjust=1, vjust=.5, size=3, color="black", parse=TRUE)
		p1 <- p1 + annotate("text", x=max(rs)*1.8, y=-1.7 , label=paste0("Substantial~H[", ifelse(forH1==TRUE,0,1), "]"), hjust=1, vjust=.5, size=3, color="black", parse=TRUE)
		p1 <- p1 + annotate("text", x=max(rs)*1.8, y=-.55 , label=paste0("Anectodal~H[", ifelse(forH1==TRUE,0,1), "]"), hjust=1, vjust=.5, size=3, color="black", parse=TRUE)
		p1 <- p1 + annotate("text", x=max(rs)*1.8, y=2.86 , label=paste0("Strong~H[", ifelse(forH1==TRUE,1,0), "]"), hjust=1, vjust=.5, size=3, color="black", parse=TRUE)
		p1 <- p1 + annotate("text", x=max(rs)*1.8, y=1.7  , label=paste0("Substantial~H[", ifelse(forH1==TRUE,1,0), "]"), hjust=1, vjust=.5, size=3, color="black", parse=TRUE)
		p1 <- p1 + annotate("text", x=max(rs)*1.8, y=.55  , label=paste0("Anectodal~H[", ifelse(forH1==TRUE,1,0), "]"), hjust=1, vjust=.5, vjust=.5, size=3, color="black", parse=TRUE)
		
		# set scale ticks
		p1 <- p1 + scale_y_continuous(breaks=c(c(-log(c(30, 10, 3)), 0, log(c(3, 10, 30)))), labels=c("-log(30)", "-log(10)", "-log(3)", "log(1)", "log(3)", "log(10)", "log(30)"))
		p1 <- p1 + scale_x_continuous(breaks=seq(min(rs), max(rs), length.out=xticks))

		return(p1)
	} else {
		return(res)
	}
}



shinyServer(function(input, output) {

  output$plot <- renderPlot({
	
	  # import the ts and ns
	  ts <- as.numeric(strsplit(input$ts, ",")[[1]])
	  ns <- as.numeric(strsplit(input$ns, ",")[[1]])
	  labels <- strsplit(input$labels, ",")[[1]]
	  if (length(labels) == 0) labels <- c()
	
	  p1 <- BFrobustplot(ts=ts, ns=ns, labels=labels, sides=input$sides, forH1=input$forH1)
	  plot(p1)
  })
  
  output$downloadPlot <- downloadHandler(
	  filename = "SensitivityPlot.pdf",
      content = function(file) {
        ggsave(filename=file)
      })
})
