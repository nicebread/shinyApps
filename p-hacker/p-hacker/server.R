library(shiny)
library(shinyBS) # Additional Bootstrap Controls
library(ggplot2)
library(mvtnorm) # used for generating multivariate distributed random numbers

#source("helpers.R")

shinyServer(function(input, output, session) {
	
  
	dat <- reactiveValues(
		selected = data.frame(),
		allData = data.frame(),
		n = 0,
		n_studies = 0,					# number of studies in stack
		studystack="",
		save_dv_observers = list(), # list of observer objects dynamically produced when displaying table
		counter = 0,
		chosen = '',
		control_for_gender = F,
		control_for_interaction = F,
		next_seed = sample(1:5000,1,replace=TRUE),
		current_seed = NULL,
		dv_names = c(),
		dv_names_all = c(),
		TEST = NULL,
		blub = 0,
		flag_auto_selected = F,
		flag_point_already_excluded = F
	)
	
  
  
	###################################
	#
	# observe & reactive expressions
	#
	###################################

  
	# observe changes in next_seed and update textInput accordingly
	observe({
	  if(is.null(dat$next_seed)) return()
	  
	  isolate({
	    updateTextInput(session, "seed", NULL, dat$next_seed)
	  })
	})
	
	
	
	
	# observe number of participants
	# change dataframe selected accordingly
	observe({
	  if(is.null(dat$current_seed) || is.null(dat$n) || dat$n <= 0) return();
	  
	  isolate({
	    # length of vector included
	    n <- nrow(dat$selected)
	    
	    if ( dat$n < n ) {
	      # limit df to size n
	      dat$selected <- getNRows(dat$selected, dat$n)
	    } else if ( dat$n > n) {
	      # extend df to size n
	      dat$selected[(n + 1):dat$n, ] <- T
	    }
	    
	  })
	})
	
	
	# computes tests and selects DV with lowest p value
	# observed values: dat$currentData, dat$n, dat$included, input$cov_age, input$cov_gender, input$cov_gender_IA
	observe({
	
		if (nrow(dat$selected)==0) return();
		
	 
	  
	  # make a copy of all the data which will be used when creating the plots
	  
	  
		control <- ""		
		if (input$cov_age)       control <- paste0(control, " + age")
		if (dat$control_for_gender)    control <- paste0(control, " + gender")
		if (dat$control_for_interaction) control <- paste0(control, " + gender*group")
		
		isolate({
		  
			dat$TEST <- list()	# keeps the aov objects for each DV
			dat$P <- list()		# stores p values for each DV
			for ( DV in dat$dv_names_all) {
			  # create formula for anova
				f <- formula(paste0(DV, " ~ group", control))
				
				# separete included from excluded data and save them for later usage
				includedData <- getSelectedRows(dat$allData, dat$selected, DV)
				
				#Print(paste0("Compute anova for ", DV))
				
				# compute anova and other tests
				dat$TEST[[DV]] <- aov(f, includedData)
				dat$TEST[[DV]]$DV <- DV
				dat$P[[DV]] <- summary(dat$TEST[[DV]])[[1]]$Pr[1]
			}
			
			
			# automatically select the DV with the lowest p value
			if(dat$counter == 0) {
			  dat$chosen <- names(which.min(dat$P))
			  dat$flag_auto_selected <- T
			}
			
			# increment counter
			dat$counter <- dat$counter + 1
						
		})
	})


	###################################
	#
  # event handlers
	#
	###################################
	
	
	# add 5 participants pressed
	observeEvent(input$add5, {
		dat$n <- dat$n + ( 5 * 2 )
	})
	
	# add 10 participants pressed
	observeEvent(input$add10, {
		dat$n <- dat$n + ( 10 * 2 )
 	})


	# dv chosen
	observeEvent(input$DV_selector, {
	  #Print(paste0("DV selector chosen option: ", input$DV_selector))
	  dat$chosen <- input$DV_selector
	})
	
	# clear stack pressed
	observeEvent(input$clear_stack, {
	  dat$n_studies <- 0
	  dat$studystack <- ""
	})
	
	# control for interaction of gender checkbox changed
	observeEvent(input$cov_gender_IA, {
	  # TODO: If the "Interaction with gender" box is clicked, the "control for gender" box should be checked too
	  if (input$cov_gender_IA) {
	    dat$control_for_interaction = T
	    if(!dat$control_for_gender) {
	      dat$control_for_gender = T
	      updateCheckboxInput(session, "cov_gender", value=TRUE)
	      
	    }
	  } else {
	    dat$control_for_interaction = F
	  }
	})
	
	# control for gender checkbox changed
	observeEvent(input$cov_gender, {
	  # TODO: If the "Interaction with gender" box is clicked, the "control for gender" box should be checked too
	  if (input$cov_gender) {
	    dat$control_for_gender = T
	  } else {
	    dat$control_for_gender = F
	    if(dat$control_for_interaction) {
	      dat$control_for_interaction = F
	      updateCheckboxInput(session, "cov_gender_IA", value=FALSE)
	    }
	  }
	})

	# generate new data pressed
	observeEvent(input$generateNewData, {
	  
	  # validate seed input prior using seed
	  seed <- suppressWarnings(as.numeric(input$seed))
	  if(is.na(seed) || input$seed <= 0) {
	    # set error msg
	    dat$last_error_msg <- 'Seed is not a positive number!'
	    return()
	  } else {
	    # remove error msg
	    dat$last_error_msg <- NULL
	  }
	  
	  # floor seed when accidentally using floating numbers
	  seed <- floor(seed)
	  
	  # set current and next seed and use it for creating random numbers
	  #Print(seed)
    dat$current_seed = seed
    dat$next_seed = seed + 1
    set.seed(seed)
    
    # set number of dvs and create labels accordingly
    dat$dv_names <-  paste0( DV_PREFIX, 1:input$dv_n )
    dat$dv_names_all <- c(dat$dv_names, DV_ALL)
    
	  # reset the settings from the p-hacking tab
	  updateCheckboxInput(session, "cov_age", value=FALSE)
	  updateCheckboxInput(session, "cov_gender", value=FALSE)
	  updateCheckboxInput(session, "cov_gender_IA", value=FALSE)

    # simulate study with N participants (not all are included in analysis)
    n <- 10000 * 2
    
    # set number of participants
    dat$n <- input$n_per_group * 2
    
    
    
    # Create matrix of multinomially distributed random numbers
    # and convert it to data.frame. Each column represents a dv,
    # each row represents a subject.
    p <- 0.5
    sigma <- matrix(p, nrow=input$dv_n, ncol=input$dv_n)
    diag(sigma) <- 1.0
    dat$allData <- as.data.frame(rmvnorm(n, sigma=sigma))
    
    # change column names to dv names
    colnames(dat$allData) <- dat$dv_names
    
    # add column with alternating group 
    dat$allData$group  <- factor(rep_len(c(input$label_group1, input$label_group2), n))
    
    # add column with randomized ages
    dat$allData$age    <-  round(rgamma(n, 4, 0.5) + 18)
    
    # add column with randomized genders
    dat$allData$gender <- factor(sample(0:1, n, replace=TRUE), labels=c("male", "female"))
      
    # create randomized data for other DVs and add fixed effect to all DV values of group1
    for(DV in dat$dv_names) {
      # get indices of all rows belonging to group1
      indices <- which(dat$allData$group == input$label_group1)
      
      # add true effect to all values of group1
      dat$allData[indices, DV] <- dat$allData[indices, DV] + input$true_effect    
    }
  
    # calculate mean of all DVs
    dat$allData[DV_ALL] <- rowMeans(dat$allData[, dat$dv_names])
    
    # reset selection
    dat$selected <- matrix(T, nrow=dat$n, ncol = length(dat$dv_names_all))
    colnames(dat$selected) <- dat$dv_names_all
	  dat$selected <- as.data.frame(dat$selected)
	  
	  # reset counter
	  dat$counter <- 0
	  
	  # reset chosen dv
	  dat$chosen <- NULL
	  
	  # reset controlling variables
	  dat$control_for_gender = F
	  dat$control_for_interaction = F
	  
	  # reset list of tests
	  dat$TEST <- NULL
	  
	})
	
	
	# main plot clicked
	observeEvent(input$mainplot_clicked, {
	  
	  if(is.null(dat$chosen) || nrow(dat$selected) == 0) return()
	  
	  # create new boolean column which indicates whether corresponding point was clicked
	  selectedRows <- nearPoints(dat$allData[1:dat$n,], input$mainplot_clicked, allRows = TRUE)
	  
	  # point clicked?
	  if(T %in% selectedRows$selected_) {
	    # toggle selected rows
	    dat$selected[,dat$chosen] <- xor(dat$selected[,dat$chosen], selectedRows$selected_) 
	    
	    # remove flag
	    dat$flag_point_already_excluded <- T
	  }
	  
	})
	
	
	###################################
	#
	# render output
	#
	###################################

	
	# render table with test results
	output$testoverview <- renderUI({				
	  
	  if (is.null(dat$TEST)) {
	    return(list(HTML("<h4>No study run yet - click on 'Run new experiment' at the bottom of the left panel!</h4>")))
	  }
	  
	  
	  # create table tag
	  table.tag <- tags$table(class="table", 
	                          tags$tr(
	                            tags$th("Name"), 
	                            tags$th("N"),
	                            tags$th("Statistic"), 
	                            tags$th("p-Value"),
	                            tags$th("Sign."),
	                            tags$th("Actions")
	                          )
	  )
	  
	  isolate({
  	  sty <- "vertical-align:middle;"
  	  
  	  for( DV in dat$dv_names_all ) {
  	    #Print(paste0("Show stats for ", DV))
  	    
  	    sum <- p.summary(dat$TEST[[DV]])
  	    
  	    id <- paste0("save_",DV)
  	    
  	    table.tag <- tagAppendChild( table.tag, 
  	      tags$tr(style=paste0("background-color:",sum['color'],";"),
  	        tags$td(style=sty, DV) ,
  	        tags$td(style=sty, sum(dat$selected[[DV]])),
  	        tags$td(style=sty, sum['f']),
  	        tags$td(style=sty, sum['p']),
  	        tags$td(style=sty, sum['stars']),
  	        tags$td(style=sty, actionButton(id, "Save", class="btn-xs"))
  	       
  	      )
  	    )
  	    
  	    
  	    
  	    if( is.null( dat$save_dv_observers[[id]] ) ) {
  	      
  	      dat$save_dv_observers[[id]] <- observe(substitute({
  	        value <- input[[id]]
  	        
  	        isolate({
  	          
  	       
    	        if(!is.null(value) && value != 0) {
    	          dat$n_studies <- dat$n_studies + 1
    	          dat$studystack <- paste0(
    	            dat$studystack,
    	            "hack-o-mat (", format(Sys.time(), "%Y"), ") S", dat$n_studies, ": ",
    	            "F(1, ", dat$TEST[[dv]]$df.residual, ") = ", round(summary(dat$TEST[[dv]])[[1]]$F[1], 2), "; p = ", round(summary(dat$TEST[[dv]])[[1]]$Pr[1], 3), "\n"
    	          )
    	        }
  	        })
  	        
  	        
  	      }, list(id=id,dv=DV)), quoted=TRUE)
  	    }
	     
	    
	    
	  }
	})
	  
	  
	  return(list(			
	    h3("Tests for each DV"),		
	    table.tag
	  ))
	})
	
	

	
	# render plot overview
	output$plotoverview <- renderUI({
	  if(is.null(dat$chosen) || dat$counter < 1) {
	    return()
	  }
	  
	  isolate({
	    #Print(paste0("Plotoverview | dat$chosen = ", dat$chosen, " dat$counter = ", dat$counter, " flag = ", dat$flag_auto_selected))
	    
	    label.tag <- NULL
	    
	    if(dat$flag_auto_selected) {
	      label.tag <- tags$span(class="label label-info", id="selector_label", "Best DV is selected by default")
	      
	      # reset auto flag to avoid printing it next time user changes inputs
	      dat$flag_auto_selected <- F
	    }
	    
	   
	    
	    return(
	      list(
	        selectInput("DV_selector", label="Choose DV to plot", dat$dv_names_all, dat$chosen),
	        label.tag
	      )
	    )
	  })
	 
	  
	})
	
	
	# render main plot
	output$mainplot <- renderPlot({
	  
	  # react on changes in dat$TEST[[1]], dat$currentData, dat$chosen
	  
	  if (is.null(dat$TEST) || is.null(dat$selected) || is.null(dat$chosen) || nrow(dat$selected) == 0) return()
	  
	  isolate({
  	  # TODO: Interaction plot when interaction with gender is chosen
  	  p_overview <- NULL
  	  dv <- dat$chosen
  	  
      # separete included from excluded data and save them for later usage
      includedData <- getSelectedRows(dat$allData, dat$selected, dv)
      excludedData <- getSelectedRows(dat$allData, dat$selected, dv, invert = TRUE)
      
      if(dat$control_for_interaction) {
        # draw interaction plot
        
        
        
        # calculate means for interaction plot
        means <- aggregate(includedData[dv], by=includedData[c("group","gender")], FUN=mean)
        
        p_overview <- ggplot(dat$allData[1:dat$n,], aes_string(x = "group", y = dv, colour = "gender")) + 
          geom_point(data = means, mapping=aes_string(y = dv), shape = 0, size=8) +
          geom_point(data = includedData, shape=16, size=4) +
          geom_point(data = excludedData, shape = 21, size=4, fill = NA, alpha = 0.5) + # show excluded points
          geom_line(data = means, mapping=aes_string(y = dv, group = "gender")) +
          theme_bw()
        
      } else {
        # draw default plot
        
        p_overview <- ggplot(dat$allData[1:dat$n,], aes_string(x="group", y=dv)) + 
          stat_boxplot(geom ='errorbar', data = includedData, color = "grey", width = 0.5) + # draw vertical lines at lower and upper end
          geom_boxplot(data = includedData, fill="grey", colour = "grey", alpha = 0.25) + # draw boxplot
          geom_point(data = includedData, shape = 16, size=4, fill = NA) + # show data points
          geom_point(data = excludedData, shape = 21, size=4, fill = NA, colour = "black", alpha = 0.5) + # show excluded points
          theme_bw()
      }
	   
	    
	    
	  
	  })
	  return(p_overview)
	})
	
	
	# render hints for plot
	output$plothints <- renderUI({
	  if(!is.null(dat$TEST) && !dat$flag_point_already_excluded) {
	    tags$span(class="label label-info", id="selector_label", "Click point to exclude or to reinclude point!")
	  }
	})
	
	# render study stack panel
	output$studystack<- renderUI({
		pchecker_link <- paste0("http://shinyapps.org/apps/p-checker/?syntax=", URLencode(dat$studystack, reserved=TRUE))

		ul.tag <- tags$ul(class="list-group")
		if(is.null(dat$studystack) || dat$studystack == "") {
		  link.tag <- a(class="btn btn-default btn-sm btn-block", href=pchecker_link,target="_blank", disabled="disabled", 'Send to p-checker')
		  ul.tag <- tagAppendChild(ul.tag, tags$li(class="list-group-item disabled", "Empty"))
		  clear.button.tag <- actionButton("clear_stack", 'Clear Stack', icon=icon("trash", lib = "glyphicon"), class="btn-sm btn-block", disabled = "disabled")
		} else {
		  link.tag <- a(class="btn btn-default btn-sm btn-block", href=pchecker_link,target="_blank", 'Send to p-checker')
		  rows <- strsplit(dat$studystack, "\n")[[1]]
		  
		  elements <- list()
		  for(i in 1:length(rows)) {
		    elements[[i]] <- tags$li(class="list-group-item", rows[i])
		  }
		  ul.tag <- tagAppendChildren(ul.tag, list = elements)
		  clear.button.tag <- actionButton("clear_stack", 'Clear Stack', icon=icon("trash", lib = "glyphicon"), class="btn-sm btn-block")
		}
		
	
		
		return(list(
			h3("My study stack"),
			p("Studies that worked!"),
			div(class="form-group", 
			  ul.tag,
			  div(class="btn-group-vertical btn-block", link.tag, clear.button.tag)
			)
		))	
	})
	
  output$seed_form <- renderUI({
    p.tag <- NULL
    
    if(is.null(dat$next_seed)) {
      return()   
    }
    
    if(!is.null(dat$current_seed)) {
      p.tag <- p(paste0("Current seed: ", dat$current_seed))
    }
    
    p.tag
  })
  
  output$error_msg <- renderUI({
    if(is.null(dat$last_error_msg)) return();
    
    div(class="alert alert-danger",role="alert",dat$last_error_msg)
  })

})
