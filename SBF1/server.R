library(shiny)
library(pwr)
library(gsDesign)
library(dplyr)
library(data.table)

d2t <- Vectorize(function(d, n) (d*sqrt(2*n - 2)) / 2)

load("SBF_detailed.RData")
WD$LOG <- as.numeric(as.character(WD$LOG))


shinyServer(function(input, output, session) {

	# When the script starts this function, everything is loaded.
	# Now we can hide the "loading ..." div:
	session$sendCustomMessage(type="jsCode", list(code="$('div.loading').hide('slow');"))
	

	#showModal("BUSY1", session)
	# ---------------------------------------------------------------------
	# Set up some "reactive conductors": Some values should only be computed once
	# These new variables are *functions* - have to called with ()
	# ---------------------------------------------------------------------
	
	# input variables have to be re-assigned; otherwise dplyr crashed because of the '$'
	boundary 	<- reactive({ as.numeric(as.character(input$boundary)) })
	#r0 			<- reactive({ as.numeric(as.character(input$r0)) })
	r0 			<- reactive({ input$r0 })
	ES 			<- reactive({ input$ES })
	d0 			<- reactive({ as.numeric(as.character(input$d0)) })
	TypeI 		<- reactive({ filter(WD, d==0 , LOG==boundary(), r==r0())$err })
	TypeII 		<- reactive({ filter(WD, d==d0(), LOG==boundary(), r==r0())$err })
	ASN.H0 		<- reactive({ filter(SS, d==0 , LOG==boundary(), r==r0())$av.n })
	ASN.H1 		<- reactive({ filter(SS, d==d0() , LOG==boundary(), r==r0())$av.n })
	qs 			<- reactive({ as.numeric(strsplit(input$qs, ",")[[1]]) })	
	
  	# compute matching NHST fixed-n	
	NHST.n 		<- reactive({ ceiling(pwr.t.test(d=d0(), sig.level=TypeI(), power=1-max(TypeII(), 1/25000), type="two.sample", alternative="two.sided")$n) })

	# GS = group sequential design; alpha is always defined as one-sided
	GS 			<- reactive({ gsDesign(k=4, test.type=2, alpha=TypeI()/2, beta=max(TypeII(), 1/25000), n.fix=NHST.n()) })
	GS.avN 		<- reactive({ ceiling(GS()$en[2]) })
	
	
	
	## ======================================================================
	## Outputs
	## ======================================================================
	
	
	# ---------------------------------------------------------------------
	# Under the H0
	
	output$H0.summary <- renderTable({
		data.frame(
			label = c(
				"Type I error (false positive decisions):",
				"Average sample size ASN for hitting the H0 boundary:"
			),
			value = c(
				paste0(round(TypeI()*100, 2), "%"),
				paste0("n = ", ASN.H0())
			)
		)
	 }, include.rownames=FALSE, include.colnames=FALSE)
	
  output$plotH0 <- renderPlot({
	  listname 	<- paste0("0.", r0(), ".", boundary())
	  x.max <- max(c(NHST.n(), GS.avN(), quantile(E[[listname]], prob=max(.98, max(qs())))))+20
	  plot(D[[listname]], 
		  main=paste0("Stopping-n distribution under H0"), 
		  xlab="n", ylab="Density", 
		  xlim=c(0, x.max), ylim=c(-max(D[[listname]]$y)*.2, max(D[[listname]]$y))) 
	  abline(h=0, col="grey")
	  
	  Q <- ceiling(quantile(E[[listname]], prob=qs()))
	  for (i in 1:length(Q)) {
	  		  arrows(Q[i], max(D[[listname]]$y)*0.5, Q[i], 0, col="blue", length=0.1)
	  		  text(x=Q[i], y=-max(D[[listname]]$y)*0.1, label=paste0(qs()[i]*100, "%\n", Q[i]), cex=.8)
	  }
	  # srt = rotation, pos=2: anchor at lower right point
	  text(x=ASN.H0(), y=max(D[[listname]]$y)*1, label=paste0("ASN = ", ASN.H0()), cex=.8, srt=90, pos=2, col="darkgreen")
	  segments(ASN.H0(), max(D[[listname]]$y), ASN.H0(), 0, col="darkgreen", lty="dashed", lwd=2)
	})
	
	
	
	# # ---------------------------------------------------------------------
	# # Under the H1
	  
    	output$H1.summary <- renderTable({
			data.frame(
				label = c(
					"Type II error (false negative decisions):",
					"ASN for hitting the H1 boundary:",
					"Optimal NHST benchmark (fixed-n) with the same error rates:",
					"SBF efficiency gain, compared to NHST:",
					"ASN of group sequential design with 3 interim looks and the same error rates:",
					"SBF efficiency gain, compared to GS:"
				),
				value = c(
					paste0(round(TypeII()*100, 2), "%"),
					paste0("n = ", ASN.H1()),
					paste0("n = ", NHST.n()),
					paste0(round((ASN.H1()/NHST.n() - 1)*100), "%"),
					paste0("n = ", GS.avN()),
					paste0(round((ASN.H1()/GS.avN() - 1)*100), "%")
				)
				
			)
    	 }, include.rownames=FALSE, include.colnames=FALSE)
	
		
	  output$plotH1 <- renderPlot({	
		  listname2 <- paste0(d0(), ".", r0(), ".", boundary())
		  x.max <- max(c(NHST.n(), GS.avN(), quantile(E[[listname2]], prob=max(.98, max(qs())))))+20
	  
		  plot(D[[listname2]], 
			  main=paste0("Stopping-n distribution under H1"), 
			  xlab="n", ylab="Density", 
			  xlim=c(0, x.max), ylim=c(-max(D[[listname2]]$y)*.2, max(D[[listname2]]$y))) 
		  abline(h=0, col="grey")
	  
		  Q <- ceiling(quantile(E[[listname2]], prob=qs()))
		  for (i in 1:length(Q)) {
			  arrows(Q[i], max(D[[listname2]]$y)*0.5, Q[i], 0, col="blue", length=0.1)
			  text(x=Q[i], y=-max(D[[listname2]]$y)*0.1, label=paste0(qs()[i]*100, "%\n", Q[i]), cex=.8)
		  }
	  
		  text(x=ASN.H1(), y=max(D[[listname2]]$y)*1, label=paste0("ASN = ", ASN.H1()), cex=.8, srt=90, pos=2, col="darkgreen")
		  segments(ASN.H1(), max(D[[listname2]]$y), ASN.H1(), 0, col="darkgreen", lty="dashed", lwd=2)
	  
		  text(x=NHST.n(), y=max(D[[listname2]]$y)*1, label=paste0("NHST = ", NHST.n()), cex=.8, srt=90, pos=2, col="darkred")
		  segments(NHST.n(), max(D[[listname2]]$y), NHST.n(), 0, col="darkred", lty="dashed", lwd=2)
	  
		  text(x=GS.avN(), y=max(D[[listname2]]$y)*1, label=paste0("GS = ", GS.avN()), cex=.8, srt=90, pos=2, col="orangered2")
		  segments(GS.avN(), max(D[[listname2]]$y), GS.avN(), 0, col="orangered2", lty="dashed", lwd=2)
		})
	 
	 
	# ---------------------------------------------------------------------
	# Bias plot
	output$biasText <- renderText({
		ES2 <- factor(ES(), levels=c("g.emp", "shrunk_0.5","shrunk_0.4","shrunk_0.3","shrunk_0.2","shrunk_0.1"), labels=c("Naive", "model-average, p(H1) = .5","model-average, p(H1) = .4","model-average, p(H1) = .3","model-average, p(H1) = .2","model-average, p(H1) = .1"))
		m2 <- paste0("Estimator: ", ES2)
		m2
	 })
	
	    output$plotBias <- renderPlot({
	  
			# X selects the list element with the requested distribution
			  X <- paste0(d0(), ".", r0(), ".", boundary())
			  ES2 <- factor(ES(), levels=c("g.emp", "shrunk_0.5","shrunk_0.4","shrunk_0.3","shrunk_0.2","shrunk_0.1"), labels=c("Naive", "model-average, p(H1) = .5","model-average, p(H1) = .4","model-average, p(H1) = .3","model-average, p(H1) = .2","model-average, p(H1) = .1"))
				

			  # define limits of plots
			  xlim <- c(-0.5, 2)
			  ylim <- c(0, 4.5)
			  par(mfcol=c(1, 2))
	  
			  # left plot: h1 and H0-distribution separate
			  plot(ES_H1_hit[[X]][[ES()]], 
			  	xlim=xlim,
			  	ylim=ylim,
			  	xlab="ES estimate",
				col="blue",
			  	main="H0 and H1 boundary hits separate",
				lwd=1.5
			  )
			  lines(ES_H0_hit[[X]][[ES()]], col="darkgreen", lwd=1.5)
			  abline(v=d0(), col="red")
			  text(x=d0(), y=0, label=bquote(delta~"="~.(d0())), cex=1, srt=90, adj=c(0,-0.5), col="red")
	  
			  if (!is.null(ES_H1_hit[[X]][[ES()]])) {
				  points(ES_H1_hit[[X]][[ES()]]$M, 0, pch=19, cex=1.2, col="blue")
				  text(x=ES_H1_hit[[X]][[ES()]]$M, y=0.15, label=bquote("mean empirical ES (H1 hit)= "~.(round(ES_H1_hit[[X]][[ES()]]$M, 2))), cex=1, srt=90, adj=c(0, 0.5), col="blue")
			  }
	  
			  if (!is.null(ES_H0_hit[[X]][[ES()]])) {
				  points(ES_H0_hit[[X]][[ES()]]$M, 0, pch=19, cex=1.2, col="darkgreen")
				  text(x=ES_H0_hit[[X]][[ES()]]$M, y=0.15, label=bquote("mean empirical ES (H0 hit)= "~.(round(ES_H0_hit[[X]][[ES()]]$M, 2))), cex=1, srt=90, adj=c(0,0.5), col="darkgreen")
			  }
			  legend("topright", lty=c("dashed", "dotted"), legend=c("H0 hit", "H1 hit"))
	  
	  
			  # right plot: Joint distribution
			  plot(ES_all[[X]][[ES()]], 
			  	xlim=xlim,
			  	ylim=ylim,
			  	xlab="ES estimate",
			  	main="All boundary hits",
				lwd=1.5
			  )
			  points(ES_all[[X]][[ES()]]$M, 0, pch=19, cex=1.2)
			  abline(v=d0(), col="red")
			  text(x=d0(), y=0, label=bquote(delta~"="~.(d0())), cex=.8, srt=90, adj=c(0,0), col="red")
			  text(x=ES_all[[X]][[ES()]]$M, y=0.15, label=bquote("mean empirical ES = "~.(round(ES_all[[X]][[ES()]]$M, 2))), cex=1, srt=90, adj=c(0, 0.5), col="black")
	  
	  
	  	})
  
})




# modalBusy <- function(id, title, ...){
#
#  msgHandler =  singleton(tags$head(tags$script('Shiny.addCustomMessageHandler("jsCode",
#                                             function(message) {
#                                               console.log(message)
#                                               eval(message.code);
#                                             });'
#                                             )
#                                 )
#                       )
#
#  label_id = paste(id, "label", sep='-')
#  modal_tag <- div(id=id,
#                class="modal hide fade",
#                "aria-hidden"=FALSE,
#                "aria-labelledby"=label_id,
#                "role"="dialog",
#                "tabindex"="-1",
#                "data-keyboard"=FALSE,
#                "data-backdrop"="static")
#  header_tag <- div(class="modal-header",
#                 h3(id=label_id, title))
#  body_tag <- div(class="modal-body",
#               Row(...))
#  footer_tag <- div(class="modal-footer")
#  modal_tag <- tagAppendChildren(modal_tag, header_tag, body_tag, footer_tag)
#  tagList(msgHandler, modal_tag)
# }
#
#
#
# showModal <- function(id, session) {
#   session$sendCustomMessage(type="jsCode", list(code= paste("$('#",id,"').modal('show')", sep="")))
# }
#
# hideModal <- function(id, session) {
#   session$sendCustomMessage(type="jsCode",
#                             list(code= paste("$('#",id,"').modal('hide')", sep="")))
# }