library(shiny)

shinyUI(fluidPage(
  
  #Application Title and Logo
  fluidRow(
    column(12,
           h1("Population PK prediction for 5-Fluorouracil", align = "middle"))
  ),   #Closing fluidrow
  
  hr(),  #Inserting line break and horizontal line
  
  #Choosing overall page layout
  sidebarLayout(
    
    #Sidebar panel with widgets that demonstrate available options
    sidebarPanel(
      #Create the action button to update plot
      actionButton("calculatePK", label = strong(em("Start Simulation"))),
      p("Press the button above to update the plot every time you change dosing regemin"),
      
      
      # Slider input for Treatment duration
      numericInput("tau",
                   "Dosing frequency (days):",
                   min = 0, max = 100, value = 30
      ),  #numericInput
      
      # Slider input for Treatment duration
      numericInput("DAYS",
                   "Treatment duration (days):",
                   min = 1, max = 100, value = 30
      ),  #numericInput
      
      #Creates a dropdown menu for selecting the formulation
      radioButtons("SELECT", "Route of Administration:",
                  c("IV infusion"=1, "Oral First-order Release"=2, "Oral Mixed First- and Zero-order Release"=3), selected=3
      ),   #This closes selectInput widget
      
      
      #Create a conditional panel for each dosing scenario
      conditionalPanel(condition = "input.SELECT == 1",
                       
                       #Slider input for dose
                       sliderInput("IV_DOSE", "Infusion Dose (mg):",
                                   min = 0, max = 2000, value = 800, step = 50)
      ),   #Close conditional panel
      
                  
      #Create a conditional panel for each dosing scenario
      conditionalPanel(condition = "input.SELECT == 1",
                       
                       #Slider input for dose
                       sliderInput("INF_DURATION", "Infusion Duration (hours):",
                                   min = 0.1, max = 4, value = 1, step = 0.5)
      ),   #Close conditional panel
    
      conditionalPanel(condition = "input.SELECT == 1",
                       
                       #Slider input for dose
                       sliderInput("POPVESOIV", "Volume of the esophageous (L):",
                                   min = 0.01, max = 3, value = 2.4, step = 0.01)
      ),   #Close conditional panel
      
      
      conditionalPanel(condition = "input.SELECT == 1",
                       
                       #Slider input for dose
                       sliderInput("POPQESOIV", "Intercompartmental CL between esophageous and plasma (L/h):",
                                   min = 0, max = 10, value = 0.14, step = 0.01)
      ),   #Close conditional panel

     ###################################################
     ###################################################
     #Create a conditional panel for each dosing scenario
      conditionalPanel(condition = "input.SELECT == 2",
                       
                       #Slider input for dose
                       sliderInput("ORAL_DOSE", "Oral dose (mg):",
                                   min = 0, max = 100, value = 20, step = 0.5)
      ),   #Close conditional panel
    

     conditionalPanel(condition = "input.SELECT == 2",
                      
                      p(h3("Set up first-order release parameters:"))
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 2",
                      #Slider input for dose
                      sliderInput("KDISS", "First-order dissolution rate constant (1/h):",
                                  min = 0, max = 0.20, value = 0.0015, step = 0.0005)
     ),   #Close conditional panel
     
     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      #Slider input for dose
                      sliderInput("FRMUCUS", "Fraction of dose dissolved into mucus:",
                                  min = 0, max = 1, value = 0.9, step = 0.1)
     ),   #Close conditional panel
     
     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      p(h3("Set up other parameters:"))
     ),   #Close conditional panel
     
     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      #Slider input for dose
                      sliderInput("FABSMUCUS", "Fraction absorbed from mucus:",
                                  min = 0, max = 1, value = 0.9, step = 0.1)
     ),   #Close conditional panel
     
     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      #Slider input for dose
                      sliderInput("FABSGUT", "Fraction absorbed from the Gut:",
                                  min = 0, max = 1, value = 0.6, step = 0.1)
     ),   #Close conditional panel

     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      #Slider input for dose
                      sliderInput("ThalfABSESO", "Absorption half-life from the esophagus (minutes):",
                                  min = 1, max = 180, value = 20, step = 1)
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      #Slider input for dose
                      sliderInput("ThalfABSGUT", "Absorption half-life  from Gut (minutes):",
                                  min = 1, max = 180, value = 20, step = 1)
     ),   #Close conditional panel

     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      #Slider input for dose
                      sliderInput("KTRDepotGut", "Transit rate constant (Depot to Gut) (1/h):",
                                  min = 0, max = 4, value = 0.2, step = 0.05)
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      #Slider input for dose
                      sliderInput("KTRMucusGut", "Transit rate constant (Mucus to Gut) (1/h):",
                                  min = 0, max = 4, value = 0.2, step = 0.05)
     ),   #Close conditional panel
     
     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      p(h3("Vd and clearance for the esophageous compartments:"))
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      #Slider input for dose
                      sliderInput("POPVMUCUS", "Volume of mucus (L):",
                                  min = 0.01, max = 0.30, value = 0.20, step = 0.01)
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      #Slider input for dose
                      sliderInput("POPVESO", "Volume of esophageous (L):",
                                  min = 0.01, max = 3, value = 2.4, step = 0.01)
     ),   #Close conditional panel
     
     
     conditionalPanel(condition = "input.SELECT == 2",
                      
                      #Slider input for dose
                      sliderInput("POPQESO", "Intercompartmental CL between esophageous and plasma (L/h):",
                                  min = 0, max = 10, value = 0.14, step = 0.01)
     ),   #Close conditional panel
     
     
     ##########################################
     #########ZERO-ORDER#########################

     #Create a conditional panel for each dosing scenario
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("ORAL_DOSE0", "Oral dose (mg):",
                                  min = 0, max = 100, value = 20, step = 0.5)
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      p(h3("Set up zero-order release parameters:"))
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("FRZERO", "Fraction of dose undergoing zero-order release:",
                                  min = 0, max = 1, value = 1, step = 0.1)
     ),   #Close conditional panel
     
     #Create a conditional panel for each dosing scenario
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("DISS_DURATION", "Zero-order dissolution Duration (Days):",
                                  min = 1, max = 100, value = 10, step = 1)
     ),   #Close conditional panel
     

     conditionalPanel(condition = "input.SELECT == 3",
                      
                      p(h3("Set up first-order release parameters:"))
     ),   #Close conditional panel
     
     
     conditionalPanel(condition = "input.SELECT == 3",
                      #Slider input for dose
                      sliderInput("KDISS0", "First-order dissolution rate constant (1/h):",
                                  min = 0, max = 0.20, value = 0.0015, step = 0.0005)
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("FRMUCUS0", "Fraction of dose dissolved into mucus:",
                                  min = 0, max = 1, value = 0.9, step = 0.1)
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      p(h3("Set up other parameters:"))
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("FABSMUCUS0", "Fraction absorbed from mucus:",
                                  min = 0, max = 1, value = 0.9, step = 0.1)
     ),   #Close conditional panel
     
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("FABSGUT0", "Fraction absorbed from the Gut:",
                                  min = 0, max = 1, value = 0.6, step = 0.1)
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("ThalfABSESO0", "Absorption half-life from the esophagus (minutes):",
                                  min = 1, max = 180, value = 20, step = 1)
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("ThalfABSGUT0", "Absorption half-life  from Gut (minutes):",
                                  min = 1, max = 180, value = 20, step = 1)
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("KTRDepotGut0", "Transit rate constant (Depot to Gut) (1/h):",
                                  min = 0, max = 4, value = 0.2, step = 0.05)
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("KTRMucusGut0", "Transit rate constant (Mucus to Gut) (1/h):",
                                  min = 0, max = 4, value = 0.2, step = 0.05)
     ),   #Close conditional panel
     
     
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      p(h3("Vd and clearance for the esophageous compartments:"))
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("POPVMUCUS0", "Volume of mucus (L):",
                                  min = 0.01, max = 0.30, value = 0.20, step = 0.01)
     ),   #Close conditional panel
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("POPVESO0", "Volume of esophageous (L):",
                                  min = 0.01, max = 3, value = 2.4, step = 0.01)
     ),   #Close conditional panel
     
     
     conditionalPanel(condition = "input.SELECT == 3",
                      
                      #Slider input for dose
                      sliderInput("POPQESO0", "Intercompartmental CL between esophageous and plasma (L/h):",
                                  min = 0, max = 10, value = 0.14, step = 0.01)
     ),   #Close conditional panel
     

      #Create a download button to download dataset
      downloadButton("downloadsimdata",
                     "Download Simulated Data"),
      
      align = "left"),   #Closing sidebarpanel and aligning left side of page
    
    #starting the main panel
    mainPanel(    
      
      #Create panel with tabs   
      tabsetPanel(
        
        #First tab
        tabPanel("5-Fluorouracil", 
                 br(), 
                 
                 fluidRow(
                     
                     column(5, checkboxInput("CI", "90% confidence interval", 
                                             value = TRUE))
                     
                 ),    #close fluidrow 
                 
                 #for NCA
                 fluidRow(
                   
                   column(5, checkboxInput("NCA", "NCA metrics for the last dosing interval", 
                                           value = FALSE))
                   
                 ),    #close fluidrow 
                 
                plotOutput("PLOTMADRS", height = 1400, width = 950)
                 
        ), #Closing first tab
        
        
        
        #Third tab
        tabPanel("About", 
                 br(), 
                 p("This", a("Shiny", 
                             href = "http://www.rstudio.com/shiny"), 
                   "application is based on a previously published population pharmacokinetic model of 5-Fluorouracil in colorectal cancer patients [1]"),
                 p("Simulated concentrations in this application are based on 1000 simulated subjects with the selected dosing design."),
                 p(strong("Important!"),"Click the", strong(em("Start Simulation")),  "button to update the plot every time you change the dosing regemin or the slider values."),
                 br(),
                 br(),
                 br(),
                 img(src = "5FU.png"),
                 br(),
                 br(),
                 p(strong("Figure.1 A schematic diagram for the", em("5-FU PK model") )),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 p(strong("Developer:")),
                 p("Ahmad Abuhelwa, PhD"),
                 p("Post-doctoral Research Fellow"),
                 p("Australian Center for Pharmacometrics"),
                 p("School of Pharmacy and Medical Sciences"),
                 p("University of South Australia"),
                 a("Ahmad.Abuhelwa@mymail.unisa.edu.au", href = "mailto:Ahmad.Abuhelwa@mymail.unisa.edu.au"),
                 br(),
                 br(),
                 p(strong("References:")),
                 p("[1] Porta-Oltra B, Perez-Ruixo J, Climente-Marti M, Merino-Sanjuan M, Almenar-Cubells D, Jimenez-Torres N. Population pharmacokinetics of 5-fluorouracil in colorectal cancer patients. J Oncol Pharm Pract. 2004;10(3):155-67.")
                 
                 

        ) #Closing About tab
        
      ) #Closing tabsetpanel
     
    )  #Closing main panel
    
  )   #Closing sidebarlayout
  
))  #Closing fluidpage and shiny UI



