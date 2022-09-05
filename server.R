library(shiny)
library(pwr)

### Function creates disabled (greyed-out) inputs
### Taken from https://groups.google.com/d/msg/shiny-discuss/uSetp4TtW-s/Jktu3fS60RAJ
disable <- function(x) {
  if (inherits(x, 'shiny.tag')) {
    if (x$name %in% c('input', 'select'))
      x$attribs$disabled <- 'disabled'
    x$children <- disable(x$children)
  }
  else if (is.list(x) && length(x) > 0) {
    for (i in 1:length(x))
      x[[i]] <- disable(x[[i]])
  }
  x
}

### Create vectors of all embedded DTRs for each design
designA.DTRs <- c("ArCnrE", "ArCnrF", "ArDnrE", "ArDnrF", "BrGnrI", "BrGnrJ", "BrHnrI", "BrHnrJ")
designB.DTRs <- c("ArCnrD", "ArCnrE", "BrFnrG", "BrFnrH")
designC.DTRs <- c("ArCnrD", "ArCnrE", "BrFnrG")
designD.DTRs <- c("AC", "AD", "BE", "BF")

### Function evaluates full-DTR probabilities; not reactive
fullDTRprob <- function(cell1, resp, cell2){
  pDTR <- cell1 * resp + cell2 * (1-resp)
  return(pDTR)
}

### Create operator to sequentially evaluate need() statements
`%then%` <- shiny:::`%OR%`

### BEGIN SERVER OPERATIONS ###

shinyServer(function(input,output,session){
  
  ##### HOME #####
  ### Watch for clicks on pickTab actionButtons rendered under design diagrams
  ### On click, redirect to appropriate tab. (More intuitive navigation structure)
  
  observe({
    if(input$pickTabA) updateTabsetPanel(session,"SMARTsize",selected="Design A")
    if(input$pickTabB) updateTabsetPanel(session,"SMARTsize",selected="Design B")
    if(input$pickTabC) updateTabsetPanel(session,"SMARTsize",selected="Design C")
    if(input$pickTabD) updateTabsetPanel(session,"SMARTsize",selected="Design D")
  })
  
  ##### DESIGN A #####
  
 
  
  ##### DESIGN A OBSERVERS #####
  
	
### Function evaluates the minimal sample size for SMART pilot study
	
	pilotsizeA <- reactive({
						  validate(
								   need(input$prespA, "Please provide an input for non-response rate."),
								   need(input$SizeThresholdA, "Please provide a value of minimum number of participants for each group."),
								   need(input$ProbThresholdA, "Please provide a minimum probability value for condition to occur.")
								   )
	                #input$SizeThresholdA <- input$SizeThresholdA-1
	                pp <- 1 # This will be used as a probability place holder later
	                range <- seq(1, 250, by = 1)
	                for(OneSide in range){ # OneSide is the number of elements that falls into medication or CBT
	                if(OneSide <= 4*(input$SizeThresholdA-1) + 3){ # where the assumptions fail(region combined), we will just put 0 in probability place holder
	                    pp[OneSide] <- 0 # since NonRes is in the range of even number from 1 to 250, we can just use it as index as well
	                }
	                else{
	                    pptmp <- pbinom(q = OneSide - 2*(input$SizeThresholdA-1) - 2, size = OneSide, prob = input$prespA, lower.tail = TRUE, log.p = FALSE)
	                    pptmp2 <- pbinom(q = 2*(input$SizeThresholdA-1)+1, size = OneSide, prob = input$prespA, lower.tail = TRUE, log.p = FALSE)
	                    pp[OneSide] <- (pptmp - pptmp2)^2
	                }
	            }
	            Nstar <- which(pp > (input$ProbThresholdA))[1]
	            return (2*Nstar)
						  })
						  
	
	
  
  ##### DESIGN A RESULT #####
  
  # Based on provided input probabilities and selected options, compute appropriate arguments to pass to power.prop.test or pwr.norm.test
  

	
   	output$pilotSampleSizeA <- renderUI({
		validate(
		        need(input$prespA < 1, "Sample size is indeterminate for 100 percent non-response rate. Please specify a value strictly between 0 and 1."),
			need(input$prespA > 0, "Sample size is indeterminate for non-positive value of non-response rate. Please specify a value strictly between 0 and 1."),
			need(input$SizeThresholdA > 0, "Sample size is indeterminate for non-positive value of minimum number of participants for each group. Please specify an integer greater than 0."),
			need(input$SizeThresholdA <30, "We do not recommend conducting a SMART pilot study with each subgroup having 30 or more participants. Please specify an integer less than 30."),
			need(input$ProbThresholdA >= 0, "Sample size is indeterminate for negative value of probability value. Please specify a value larger than or equal to 0 but less than 1."),
			need(input$ProbThresholdA < 1, "Sample size is indeterminate for a probability value of 1. Please specify a value larger than or equal to 0 but less than 1.")
			)
		PilotsizeA <- pilotsizeA()
		HTML("<h4> <font color='blue'> N=",paste(PilotsizeA),"</font> </h4>
			 <p><em> We require a sample size of",PilotsizeA,"to guarantee that at least",input$SizeThresholdA, "number of participants to fall in each subgroup to examine the feasibility and acceptability of full-scale 
			  SMART study")
										
	})
  
  
  
  ##### DESIGN B #####
  
  ##### B HEADER #####
  
 
	
### Function evaluates the minimal sample size for SMART pilot study
	
	pilotsizeB <- reactive({
						   validate(
									need(input$prespB, "Please provide an input for non-response rate."),
									need(input$SizeThresholdB, "Please provide a value of minimum number of participants for each group."),
									need(input$ProbThresholdB, "Please provide a minimum probability value for condition to occur.")
									)
	  
	                pp <- 1 # This will be used as a probability place holder later
	                range <- seq(1, 250, by = 1)
	                for(OneSide in range){ #k is the number of elements that falls into medication or CBT
	                  if(OneSide <= 3*input$SizeThresholdB + 2){ # where the assumption fails, we will just put 0 in probability place holder
	                      pp[OneSide] <- 0 # since NonRes is in the range of even number from 1 to 250, we can just use it as index as well
	                  }
	                  else{
	                      pptmp <- pbinom(q = OneSide - input$SizeThresholdB, size = OneSide, prob = input$prespB, lower.tail = TRUE, log.p = FALSE)
	                      pptmp2 <- pbinom(q = 2*input$SizeThresholdB-1 , size = OneSide, prob = input$prespB, lower.tail = TRUE, log.p = FALSE)
	                      pp[OneSide] <- (pptmp - pptmp2)^2
	                  }
	                }
	                Nstar <- which(pp > input$ProbThresholdB)[1]
	                return (2*Nstar)
						   })
	
	
	output$pilotSampleSizeB <- renderUI({
		validate(
		        need(input$prespB < 1, "Sample size is indeterminate for 100 percent non-response rate. Please specify a value strictly between 0 and 1."),
			need(input$prespB > 0, "Sample size is indeterminate for non-positive value of non-response rate. Please specify a value strictly between 0 and 1."),
			need(input$SizeThresholdB > 0, "Sample size is indeterminate for non-positive value of minimum number of participants for each group. Please specify an integer greater than 0."),
			need(input$SizeThresholdB < 30, "We do not recommend conducting a SMART pilot study with each subgroup having 30 or more participants. Please specify an integer less than 30."),
			need(input$ProbThresholdB >= 0, "Sample size is indeterminate for negative value of probability value. Please specify a value larger than or equal to 0 but less than 1."),
			need(input$ProbThresholdB < 1, "Sample size is indeterminate for a probability value of 1. Please specify a value larger than or equal to 0 but less than 1.")
		)
		PilotsizeB <- pilotsizeB()
		HTML("<h4> <font color='blue'> N=",paste(PilotsizeB),"</font> </h4>
			  <p><em> We require a sample size of",PilotsizeB,"to guarantee that at least",input$SizeThresholdB, "number of participants to fall in each subgroup 
				to examine the feasibility and acceptability of full-scale SMART study")
	})
	
 
 ##### DESIGN C #####
 

	
### Function evaluates the minimal sample size for SMART pilot study
	
	pilotsizeC <- reactive({
						   validate(
									need(input$prespC, "Please provide an input for non-response rate."),
									need(input$SizeThresholdC, "Please provide a value of minimum number of participants for each group."),
									need(input$ProbThresholdC, "Please provide a minimum probability value for condition to occur.")
									)
	          #input$SizeThresholdC <- input$SizeThresholdC-1
	          pp <- 1 # This will be used as a probability place holder later
	          range <- seq(1, 250, by = 1)
	          for(OneSide in range){ # OneSide is the number of elements that falls into medication or CBT
	            if(OneSide <= 2*(input$SizeThresholdC-1) + 1){ # where the assumptions fail(region combined), we will just put 0 in probability place holder
	                pp[OneSide] <- 0 # since NonRes is in the range of even number from 1 to 250, we can just use it as index as well
	            }
	            else{
	              pptmp <- pbinom(q = OneSide - (input$SizeThresholdC-1) - 1, size = OneSide, prob = input$prespC, lower.tail = TRUE, log.p = FALSE)
	              pptmp2 <- pbinom(q = 2*(input$SizeThresholdC-1)+1, size = OneSide, prob = input$prespC, lower.tail = TRUE, log.p = FALSE)
	              pptmp3 <- pbinom(q = input$SizeThresholdC-1, size = OneSide, prob = input$prespC, lower.tail = TRUE, log.p = FALSE)
	              pp[OneSide] <- (pptmp - pptmp2)*(pptmp - pptmp3)
	            }
	          }
	          Nstar <- which(pp > (input$ProbThresholdC))[1]
	          return (2*Nstar)
						   })
	
	
	
 ##### DESIGN C RESULT #####

 output$pilotSampleSizeC <- renderUI({
	validate(
	        need(input$prespC < 1, "Sample size is indeterminate for 100 percent non-response rate. Please specify a value strictly between 0 and 1."),
		need(input$prespC > 0, "Sample size is indeterminate for non-positive value of non-response rate. Please specify a value strictly between 0 and 1."),
		need(input$SizeThresholdC > 0, "Sample size is indeterminate for non-positive value of minimum number of participants for each group. Please specify an integer greater than 0."),
		need(input$SizeThresholdC < 30, "We do not recommend conducting a SMART pilot study with each subgroup having 30 or more participants. Please specify an integer less than 30."),
		need(input$ProbThresholdC >= 0, "Sample size is indeterminate for negative value of probability value. Please specify a value larger than or equal to 0 but less than 1."),
		need(input$ProbThresholdC < 1, "Sample size is indeterminate for a probability value of 1. Please specify a value larger than or equal to 0 but less than 1.")
	)
	PilotsizeC <- pilotsizeC()
	HTML("<h4> <font color='blue'> N=",paste(PilotsizeC),"</font> </h4>
		<p><em> We require a sample size of",PilotsizeC,"to guarantee that at least",input$SizeThresholdC, "number of participants to fall in each subgroup 
			to examine the feasibility and acceptability of full-scale SMART study")
 })
	
	
 
 ##### DESIGN D #####
 
 ##### D HEADER #####
 

	
### Function evaluates the minimal sample size for SMART pilot study
	
	pilotsizeD <- reactive({
						   validate(
									need(input$SizeThresholdD, "Please provide a value of minimum number of participants for each group.")
									)
						   return (4*(input$SizeThresholdD))
						   })
	
	
	
 ##### DESIGN D RESULT #####

 output$pilotSampleSizeD <- renderUI({
	validate(
	  need(input$SizeThresholdD < 30, "We do not recommend conducting a SMART pilot study with each subgroup having 30 or more participants. Please specify an integer less than 30."),
		need(input$SizeThresholdD > 0, "Sample size is indeterminate for non-positive value of minimum number of participants for each group. Please specify an integer greater than 0.")
	)
	PilotsizeD <- pilotsizeD()
									 
	HTML("<h4> <font color='blue'> N=",paste(PilotsizeD),"</font> </h4>
			<p><em> We require a sample size of",PilotsizeD,"to guarantee that at least",input$SizeThresholdD, "number of participants to fall in each subgroup 
				to examine the feasibility and acceptability of full-scale SMART study")
 })
	
  
})