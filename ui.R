library(shiny)

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

shinyUI(
  navbarPage("SMART Sample Size Calculator for Pilot studies", id="SMARTsize",

             ##### HOME TAB#####
             
             tabPanel("Home",
                      
                      sidebarPanel(
                                   
                                   h4("Purpose"),
                                   p("The goal of this web application is to compute a minimal sample size for pilot sequential multiple assignment
                                     randomized trials (SMARTs). Pilot SMARTs can be used to examine feasibility and acceptability issues of adaptive 
                                     interventions embedded in a full-scale SMART study.
                                     For more background on SMARTs, please refer to", "Almirall et al. (2012) and Lei et al. (2012). For more background on pilot SMARTs or this sample size calculator, please refer to Kim et al (2016)."),
                                   br(),
                                   h4("Methods"),
                                   p("This web application computes a minimal sample size which assures investigators will observe a pre-determined number of participants for each sequence of treatments (e.g., path A followed by C in design A to the right) with a user-specified probability."),
                                   p("For additional details, please refer to Kim et al. (2016)."),
                                   p("To calculate the minimum sample size for a full-scale SMART study, please see", a(href = "https://nseewald1.shinyapps.io/SMARTsize", "https://nseewald1.shinyapps.io/SMARTsize.")),
                                   br(),
                                   h4("Notation"),
                                   p("Throughout the application, we use the following notations:"),
                                   tags$ul(
                                     tags$li(img(src="images/randomize.gif",width=25),"refers to a randomization, with probability 0.5, of all available individuals into 
                                             two subsequent treatments. For example, in design B at right, the rightmost ",img(src="images/randomize.gif",width=17),"indicates
                                             that those who did not respond to the first-stage treatment are randomized equally between two available second-stage treatments."
                                       ),
                                     tags$li("Adaptive Interventions (AI) are named by concatenating the first-stage treatment, the second-stage treatment for responders and that of non-responders.
                                             For example, the AI 'Give A; then, if response, give C; if no response, give E' is named 'ArCnrE'."
                                       )
                                   ),
								                   p("Please direct correspondence to", a(href = "moloque@umich.edu", "moloque@umich.edu."))
                      ),
                      
                      mainPanel(
                        h1("Sample Size Calculator for SMART pilot studies"),
                        br(),
                        p("Choose a design by clicking the corresponding tab at the top of the window, or the button below the corresponding diagram. 
                           See sidebar for notations."),
                        br(),
                        fluidRow(
                          column(7,
                                 img(src="images/SMARTdesignA__.gif", width = "550px", height = "450px"),
                                 actionButton("pickTabA","Design A"),
                                 helpText("8 embedded adaptive interventions: ArCnrE, ArCnrF, ArDnrE, ArDnrF, BrGnrI, BrGnrJ, BrHnrI, BrHnrJ")
                          )
                        ),
                        br(),
                        tags$hr(),
                        fluidRow(
                          column(7,
                                 img(src="images/SMARTdesignB__.gif", width = "550px", height = "450px"),
                                 actionButton("pickTabB","Design B"),
                                 helpText("4 embedded adaptive interventions: ArCnrD, ArCnrE, BrFnrG, BrFnrH")
                          )
                        ),
                        br(),
                        tags$hr(),
                        fluidRow(
                          column(7,
                                 img(src="images/SMARTdesignC__.gif", width = "550px", height = "450px"),
                                 actionButton("pickTabC","Design C"),
                                 helpText("3 embedded adaptive interventions: ArCnrD, ArCnrE, BrFnrG")
                          )
                        ),
                        br(),
                        tags$hr(),
                        fluidRow(
                          column(7,
                                 img(src="images/SMARTdesignD__.gif", width = "550px", height = "450px"),
                                 actionButton("pickTabD","Design D"),
                                 helpText("4 embedded non-adaptive interventions: AC, AD, BE, BF")
                          )
                        ),
                        br(),  
                       h2("Why Conduct a Pilot SMART?"),
                       		p(strong("The primary goal of a SMART pilot study is to examine acceptability and feasibility considerations prior to conducting a full-scale SMART study."),
                                        "For example, a SMART pilot could be used to investigate whether the adaptive interventions embedded in the full-scale SMART study are acceptable to participants 
                                         or to those delivering the interventions. This may involve examining the feasibility of measuring response/non-response of stage one treatment in actual
                                         clinical practice. Or, it may involve examining the acceptability of second-stage treatment options(e.g., switch in treatment or augmentation in treatment) 
                                         from the perspective of individual participants, family members, or providers."),
                                p("For more information regarding pilot SMARTs or to see concrete examples, 
                                  please refer to additional selected reading references."),                
                      tags$hr(),
                      h4("References"),
                      tags$ol(
                            tags$li("Almirall, D., Compton, S.N., Gunlicks-Stoessel, M., Duan, N., and Murphy, S.A.(2012), Designing a pilot sequential multiple assignment randomized trial for developing an adaptive treatment strategy,", em("Statistics in Medicine,"),"31, 1887-1902."),
                            tags$li("Lei, H., Nahum-Shani, I., Lynch, K., Oslin, D., and Murphy, S.A.(2012), A SMART design for building individualized treatment sequences,", em("The Annual Review of Clinical Psychology,"),"8, 21-48."),
                            tags$li("Gunlicks-Stoessel, M., Mufson, L., Westervelt, A., Almirall D., and Murphy, S.A.(2016), A pilot SMART for developing an adaptive treatment strategy for adolescent depression,", em("Journal of Clinical Child and Adolescent Psychology,"),"45(4), 480-494."),
                            tags$li("August, G., Piehler, T., and Bloomquist, M.(2016), Being SMART about adolescent conduct problems prevention: Executing a SMART pilot study in a juvenile diversion agency,", em("Journal of Clinical Child and Adolescent Psychology,"),"45(4), 495-509."),
                            tags$li("Kim, H., Ionides, E., and Almirall, D.(2016), A sample size calculator for SMART pilot studies,", em("SIAM Undergraduate Research Online,"),"9, 229-250.")
                       )
                 )), 
             
             ##### DESIGN A #####
             
             ##### A SIDEBAR #####
             
             tabPanel("Design A",
                      sidebarPanel(h4("About this design:"),
                                   p("Design A is a SMART in which every single individual is randomized twice. 
                                      Regardless of response to the first-stage treatment 
                                      or the availability of the second-stage treatments, all individuals are rerandomized. Note that subsequent treatment options depend on both the initial treatment received and the response/non-response status."),
                                   p("There are",em("eight"),"adaptive interventions in this design: ArCnrE, ArCnrF, ArDnrE, ArDnrF, BrGnrI, BrGnrJ, BrHnrI, BrHnrJ."),
                                   p("An example of SMART study with design A is a SMART for developing an adaptive reinforcement-based 
                                     behavioral intervention for woman who are pregnant and abusing drugs. For more ongoing SMART studies, please 
                                     refer to", a(href = "https://methodology.psu.edu/ra/adap-inter/projects", "https://methodology.psu.edu/ra/adap-inter/projects.")),
                                   tags$hr(),
                                   h4("Inputs:"),
                                   tags$ul(
                                     tags$li("A minimum value between two anticipated non-response rates for each initial intervention."),
                                     tags$li("A minimum number of participants for each sequence of treatments(subgroups), for example,
                                              if you want at least 3 people for each subgroup, specify the minimum number of participants
                                              for each subgroup as 3."),
				                             tags$li("A minimum probability value for the event to happen, where the event is that the user will
					                                    end up getting at least a user-specified number of participants for each sequence of treatments.")
                                     ),
                                   h5("Input Formatting Rules:"),
                                   tags$li("All inputs must be given in decimal form, and can be precise up to two decimal places. Fractional input is
                                                   disallowed. For example, both '0.07' and '.07' are valid input; '7/100' are invalid. Improperly-formatted 
                                                   input may result in unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
				                            p("Consider a SMART modeled after design A with two anticipated non-response rates, 0.3 and 0.5. 
                                      Suppose we need 4 participants or more to fall in each sequence of treatments with probability greater than 0.8. 
                                      Then we need a sample size of 74 or greater to conduct a pilot SMART."),
				                            p("Please direct correspondence to", a(href = "moloque@umich.edu", "moloque@umich.edu."))
				                              
                      ),
                      
                      mainPanel(
                        
                        ##### A PAGE HEADER #####
                        
                        h1("Design A"),
                        tags$hr(),
                        
                        ##### A OUTCOME SELECT AND RESPONSE INPUT #####
                        # Provide options to select binary/continuous outcome
                        # User input to provide response probabilities

                        p(strong("What is the anticipated rate of non-response at the end of first-stage intervention?"), "(If the non-response rate is expected to differ by initial intervention (A vs B), then provide the smaller of the two rates.)"),
                        numericInput("prespA", 
                                              label = " ",
                                              value=0, min = 0.000001, max = 0.999999, step = 0.000001),
					
                                              
                       fluidRow(
                         tags$hr(),
                         column(7, img(src="images/SMARTdesignA__.gif", width = "450px", height = "350px"))
                       ),
                       
                       fluidRow(
                         tags$hr(),
                         column(12, 
                                p("Note that each letter between C to J represents a distinct", strong("subgroup"), "which is determined by a unique sequence of treatments participants go through.")
                         )
                       ),
                       
                       tags$hr(),
                       
                       fluidRow(
                       p("What is the", strong("minimum number of participants would you like to observe in each subgroup?"), numericInput("SizeThresholdA", label = " ", value = 3, min = 1, max = 30, step = 1)),
                                                                                                                    
                    
                       tags$hr(),
  							       p("With what", strong("minimum probability would you like to observe this many participants in each subgroup?"), numericInput("ProbThresholdA", label = " ", value = 0.8, min = 0, max = 1, step = 0.000001)),
                       tags$hr(),
                       htmlOutput("pilotSampleSizeA") 
                       )
                        
                        
                      )
                   ),
             
             
             ########### DESIGN B ##########
             
             tabPanel("Design B",
                      
                      ##### B SIDEBAR #####
                      
                      sidebarPanel(h4("About this design:"),
                                   p("Design B is a SMART in which only those who do not respond to initial intervention are re-randomized. Individuals who do not respond
                                     to treatment A or B are re-randomized between two options for second-stage treatment, whereas responders are assigned to the single second-stage treatment."),
                                   p("There are",em("four"),"adaptive interventions in this design: ArCnrD, ArCnrE, BrFnrG, BrFnrH."),
                                   p("An example of SMART study with design B is a SMART study for adolescent mariguana usage. For more ongoing SMART studies, please 
                                     refer to", a(href = "https://methodology.psu.edu/ra/adap-inter/projects", "https://methodology.psu.edu/ra/adap-inter/projects.")),
                                   tags$hr(),
                                   h4("Inputs:"),
                                   tags$ul(
                                     tags$li("A minimum value between two anticipated non-response rates for each initial intervention."),
                                     tags$li("A minimum number of participants for each sequence of treatments(subgroups), for example,
                                             if you want at least 3 people for each subgroup, specify the minimum number of participants
                                             for each subgroup as 3."),
                                     tags$li("A minimum probability value for the event to happen, where the event is that the user will
                                             end up getting at least a user-specified number of participants for each sequence of treatments.")
                                     ),
                                   h5("Input Formatting Rules:"),
                                   tags$li("All inputs must be given in decimal form, and can be precise up to two decimal places. Fractional input is
                                                   disallowed. For example, both '0.07' and '.07' are valid input; '7/100' are invalid. Improperly-formatted 
                                                   input may result in unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("Consider a SMART modeled after design B with two anticipated non-response rates, 0.3 and 0.5. 
                                     Suppose we need 4 participants or more to fall in each sequence of treatments with probability greater than 0.8. 
                                     Then we need a sample size of 74 or greater to conduct a pilot SMART."),
                                   p("Please direct correspondence to", a(href = "moloque@umich.edu", "moloque@umich.edu."))
					                      ),
                      
                      mainPanel(
                        
                        ##### B PAGE HEADER #####
                        
                        h1("Design B"),
                        tags$hr(),
                        
                        ##### A OUTCOME SELECT AND RESPONSE INPUT #####
                        # Provide options to select binary/continuous outcome
                        # User input to provide response probabilities
                        
                        p(strong("What is the anticipated rate of non-response at the end of first-stage intervention?"), "(If the non-response rate is expected to differ by initial intervention (A vs B), then provide the smaller of the two rates.)"),
                        numericInput("prespB", 
                                     label = " ",
                                     value=0, min = 0.000001, max = 0.999999, step = 0.000001),
                        
                        
                        fluidRow(
                          tags$hr(),
                          column(7, img(src="images/SMARTdesignB__.gif", width = "450px", height = "350px"))
                        ),  
                        fluidRow(  
                          tags$hr(),
                          column(12,
                                 p("Note that each letter between C to H represents a distinct", strong("subgroup"), "which is determined by a unique sequence of treatments participants go through.")
                          )
                        ),
                        
                        tags$hr(),
                        fluidRow(
                          p("What is the", strong("minimum number of participants would you like to observe in each subgroup?"), numericInput("SizeThresholdB", label = " ", value = 3, min = 1, max = 30, step = 1)),
                          tags$hr(),
                          p("With what", strong("minimum probability would you like to observe this many participants in each subgroup?"), numericInput("ProbThresholdB", label = " ", value = 0.8, min = 0, max = 1, step = 0.000001)),
                          tags$hr(),
                          htmlOutput("pilotSampleSizeB") 
                        )
                    )
             ),
  
             tabPanel("Design C",
                      sidebarPanel(h4("About this design:"),
                                   p("Design C is a SMART in which re-randomization depends on both the first-stage treatment and response 
                                     to the first-stage treatment. Only those individuals who do not respond to treatment A are re-randomized."),
                                   p("There are",em("three"),"embedded adaptive interventions in this design: ArCnrD, ArCnrE, BrFnrG."),
                                   p("An example of SMART study with design C is a SMART study for improving outcomes of a mental disorders program. 
                                      For more ongoing SMART studies, please refer to", a(href = "https://methodology.psu.edu/ra/adap-inter/projects", 
                                       "https://methodology.psu.edu/ra/adap-inter/projects.")),
                                   tags$hr(),
                                   h4("Inputs:"),
                                   tags$ul(
                                     tags$li("A minimum value between two anticipated non-response rates for each initial intervention."),
                                     tags$li("A minimum number of participants for each sequence of treatments(subgroups), for example,
                                             if you want at least 3 people for each subgroup, specify the minimum number of participants
                                             for each subgroup as 3."),
                                     tags$li("A minimum probability value for the event to happen, where the event is that the user will
                                             end up getting at least a user-specified number of participants for each sequence of treatments.")
                                   ),
                                   h5("Input Formatting Rules:"),
                                   tags$li("All inputs must be given in decimal form, and can be precise up to two decimal places. Fractional input is
                                                   disallowed. For example, both '0.07' and '.07' are valid input; '7/100' are invalid. Improperly-formatted 
                                                   input may result in unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("Consider a SMART modeled after design C with two anticipated non-response rates, 0.3 and 0.5. 
                                     Suppose we need 4 participants or more to fall in each sequence of treatments with probability greater than 0.8. 
                                     Then we need a sample size of 66 or greater to conduct a pilot SMART."),
                                   p("Please direct correspondence to", a(href = "moloque@umich.edu", "moloque@umich.edu."))
                      ),
                      mainPanel(
                        
                        ##### C PAGE HEADER #####
                        
                        h1("Design C"),
                        tags$hr(),
                        
                        ##### C OUTCOME SELECT AND RESPONSE INPUT #####
                        # Provide options to select binary/continuous outcome
                        # User input to provide response probabilities
                        p(strong("What is the anticipated rate of non-response at the end of first-stage intervention?"), "(If the non-response rate is expected to differ by initial intervention (A vs B), then provide the smaller of the two rates.)"),
                        numericInput("prespC", 
                                     label = " ",
                                     value=0, min = 0.000001, max = 0.999999, step = 0.000001),
                        
                        
                        fluidRow(
                          tags$hr(),
                          column(7, img(src="images/SMARTdesignC__.gif", width = "450px", height = "350px"))
                        ),
                        fluidRow( 
                          tags$hr(),
                          column(12,
                                 p("Note that each letter between C to G represents a distinct", strong("subgroup"), "which is determined by a unique sequence of treatments participants go through.")
                          )
                        ),
                        
                        tags$hr(),
                        fluidRow(
                          p("What is the", strong("minimum number of participants would you like to observe in each subgroup?"), numericInput("SizeThresholdC", label = " ", value = 3, min = 1, max = 30, step = 1)),
                          tags$hr(),
                          p("With what", strong("minimum probability would you like to observe this many participants in each subgroup?"), numericInput("ProbThresholdC", label = " ", value = 0.8, min = 0, max = 1, step = 0.000001)),
                          tags$hr(),
                          htmlOutput("pilotSampleSizeC") 
                        )
                      )
             ),


			########### DESIGN D ###########

             tabPanel("Design D",
                      sidebarPanel(h4("About this design:"),
                                   p("Design D is a SMART in which all participants are re-randomized, and the second-stage treatment options are",em("neither"),"influenced by
                                     the response/non-response status", em("nor"),"the initial treatment participants went through. The interventions embedded in this SMART are thus non-adaptive, since information observed
                                     first- and second-stage treatments do not impact decisions regarding subsequent treatments."),
                                   p("There are",em("zero"),"embedded adaptive interventions in this design, and",em("four"),"embedded non-adaptive
                                     'intervention paths': AC, AD, BE, BF."),
                                   p("An example of SMART study with design D is a SMART study for adolescent mariguana usage. For more ongoing SMART studies, please 
                                     refer to", a(href = "https://methodology.psu.edu/ra/adap-inter/projects", "https://methodology.psu.edu/ra/adap-inter/projects.")),
                                   tags$hr(),
                                   h4("Inputs:"),
                                   tags$ul(                   
				                           tags$li("For pilot outcomes, the default input is a minimum number of participants for each sequence of treatments.")
                                     ),
                                   h5("Input Formatting Rules:"),
					                         tags$li("All inputs must be given in decimal form, and can be precise to two decimal places. Fractional input is
                                                   disallowed. For example, both '0.07' or '.07' are valid input; '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
				                          p("Consider a SMART modeled after design D. Suppose we need 4 participants or more to fall in each sequence of treatments. Then we need a sample
				                          size of 16 or greater in order to conduct a SMART pilot study."),
					                         p("Please direct correspondence to", a(href = "moloque@umich.edu", "moloque@umich.edu."))
                      ),
                      mainPanel(
                        
                        ##### D PAGE HEADER #####
                        
                        h1("Design D"),
                        fluidRow(
                          tags$hr(),
                          column(7, img(src="images/SMARTdesignD__.gif", width = "450px", height = "350px"))
                        ),
                        fluidRow(  
                          tags$hr(),
                          column(12,
                                 p("Note that each letter between C to F represents a distinct", strong("subgroup"), "which is determined by a unique sequence of treatments participants go through.")
                          )
                        ),
                        
                        tags$hr(),
                        fluidRow(
                        p("What is the", strong("minimum number of participants would you like to observe in each subgroup?"), 
                          numericInput("SizeThresholdD", label = " ",
                                       value = 3, min = 1, max = 30, step = 1) 
      					         ),
      					         tags$hr(),
      					         htmlOutput("pilotSampleSizeD") 
                        )
					    )
             ),
			
collapsable=TRUE))

#footer=HTML("<p>  Please direct correspondence to nseewald@umich.edu </p>")))
