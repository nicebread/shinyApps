library(shiny)
    
# input <- list(percTrue=30, alpha=.05, power=.35, bias=0)
presetselection <- new.env()
presetselection$sel <- "---"
	
# Define server logic for PPV application
shinyServer(function(input, output, session) {
	
	# compute PPV etc only once for all outputs--> reactive values
	computePPV <- reactive({ 
		
		# if user changes a parameter: reset preset selector to "---"
		isolate({updateSelectInput(session, inputId = "preset", selected=presetselection$sel)})
		presetselection$sel <- "---"
		
		c <- 500		# number of studies in plots
		nrow <- 25
		ncol <- c/nrow
		df <- expand.grid(x = c(1:nrow), y = c(1:ncol))

		# compute prestudy odds of true relationships true/false
		R <- input$percTrue/(100-input$percTrue)
		#if (R==0) R <- 0.001
		if (is.infinite(R)) R <- 100000
		
		# Alpha level
		alpha <- input$alpha
		# power
                # NG 17-04-24: let power be set indirectly by setting n and d
                # NG 17-04-24: adjust power or d sliders based on other values
                power <- input$power
                n <- input$n
                d <- input$d
                if (input$power_select == "power") {
                  d <- power.t.test(n=n,delta=NULL,power=power,sig.level=alpha)$delta
                  isolate({updateSelectInput(session, inputId = "d", selected=d)})
                } else {
                  power <- power.t.test(n=n,delta=d,power=NULL,sig.level=alpha)$power
                  isolate({updateSelectInput(session, inputId = "power", selected=power)})
                 }
		# beta Fehler
		beta <- 1 - power
		# bias
		u <- input$bias/100		# Ioannidis calls it "u"
		
		# Hits 
		hit <- (c*power*R + u*c*beta*R)/(R + 1)
		falsealarm <- (c*alpha + u*c*(1 - alpha))/(R + 1)
		miss <- (1-u)*c*beta*R / (R + 1)
		truerejection <- (1-u)*c*(1-alpha)/(R + 1)

		# marginals
		h1 <- round(c * R / (R + 1))
		h0 <- round(c / (R + 1))
		significant <- round(c*(R + alpha - beta*R + u - u*alpha + u*beta*R)/(R + 1))
		not.significant <- round(c*(1-u)*(1 - alpha + beta*R)/(R + 1))
		
		# sanity check:
		if (hit+falsealarm+miss+truerejection != c) print(paste0("Error: 1000 != ", hit+falsealarm+miss+truerejection))
		if (h1 + h0 != c) print(paste0("Error: 1000 != ", h1 + h0))
		if (significant + not.significant != c) print(paste0("Error: 1000 != ", significant + not.significant))

		# positive predictive value
		ppv <- (power * R + u*beta*R) / (R + alpha - beta*R + u - u*alpha + u*beta*R)
		fdr <- 1-ppv

		# fill in the ground truths types
		df$ground <- c(rep(FALSE, times=h0), rep(TRUE, times=h1))

		# fill in the false alarms: h0 is true and test is significant
		df$test <- FALSE
		df$test[df$ground==TRUE][1:hit] <- TRUE
		df$test[df$ground==FALSE][1:falsealarm] <- TRUE
		
		# check
		#table(test=df$test, ground=df$ground)		

		# combine types
		df$type[!df$ground & !df$test] <- "true negative"
		df$type[!df$ground & df$test] <- "false positive"
		df$type[df$ground & df$test] <- "true positive"
		df$type[df$ground & !df$test] <- "false negative"
		df$type <- factor(df$type, levels=c("false negative", "false positive", "true negative", "true positive"))
		return(list(df=df, ppv=ppv, fdr=fdr, hit=hit, falsealarm=falsealarm, miss=miss, truerejection=truerejection, c=c))
	})
	
	observeEvent(input$power_select, {
		if (input$power_select == "nd") {
			shinyjs::disable("power")
		} else {
			shinyjs::enable("power")
		}
	})
	

	
	output$res <- renderUI({
		PPV <- computePPV()
			
		# color translation tables
		colTrans <- c("darkblue", "red", "orange", "green3")
		names(colTrans) <- c("false negative", "false positive", "true negative", "true positive")
		
		colTrans2 <- c("white", "red", "white", "green3")
		names(colTrans2) <- c("false negative", "false positive", "true negative", "true positive")
			
		return(list(
			HTML(paste0(
				"true positives: ", round(PPV$hit/PPV$c*100, 1), "%; false negatives: ", round(PPV$miss/PPV$c*100, 1), 
				"%; true negatives: ", round(PPV$truerejection/PPV$c*100, 1), "%; false positives: ", round(PPV$falsealarm/PPV$c*100, 1), "%<br><br>",
				"<b>Positive predictive value (PPV)</b>: ", round(PPV$ppv*100, 1), "% of claimed findings are true</b><br>
				 <b>False discovery rate (FDR)</b>: ", round(PPV$fdr*100, 1), "% of claimed findings are false</b>")),
			h4("If we consider all findings, it looks like this (each point is one study):"),
			renderPlot({
				par(mar=c(0, 0, 0, 0))
				plot(PPV$df$y, PPV$df$x, col=colTrans[PPV$df$type], pch=20, axes=FALSE, ylim=c(1, max(PPV$df$x)+3), xlab="", ylab="")
				legend("top", pch=20, col=c("red", "orange", "green3", "darkblue"), legend=c("False positives", "True negatives", "True positives", "False negatives"), bty="n", horiz=TRUE, cex=1.3)
			}, height=250),
			br(),
			HTML("<h4>If we consider <b>only the significant</b> findings, the ratio of true to false positives looks like this:</h4>"),
			renderPlot({
				par(mar=c(0, 0, 0, 0))
				plot(PPV$df$y, PPV$df$x, col=colTrans2[PPV$df$type], pch=20, axes=FALSE, xlab="", ylab="")
			}, height=250)
		))
		
	})
	
	# ---------------------------------------------------------------------
	# Load demo data
	observeEvent(input$preset, {
		switch(input$preset,
			"p1" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 0.50*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.80)
				updateSliderInput(session, inputId = "bias", value = 0.10*100)				
				},
			"p2" = {
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 2/3*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.95)
				updateSliderInput(session, inputId = "bias", value = 0.30*100)
				},
			"p3" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 0.25*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.80)
				updateSliderInput(session, inputId = "bias", value = 0.40*100)
				},
			"p4" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/6*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.20)
				updateSliderInput(session, inputId = "bias", value = 0.20*100)
				},
			"p5" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/6*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.20)
				updateSliderInput(session, inputId = "bias", value = 0.80*100)
				},
			"p6" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/11*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.80)
				updateSliderInput(session, inputId = "bias", value = 0.30*100)
				},
			"p7" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/11*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.20)
				updateSliderInput(session, inputId = "bias", value = 0.30*100)
				},
			"p8" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/1001*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.20)
				updateSliderInput(session, inputId = "bias", value = 0.80*100)
				},
			"p9" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/1001*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.20)
				updateSliderInput(session, inputId = "bias", value = 0.20*100)
				}					
		)
		presetselection$sel <- input$preset
	})
})