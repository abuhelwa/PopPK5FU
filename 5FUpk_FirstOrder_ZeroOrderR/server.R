#server.R script for Ketamine PKPDmodel
#Load package libraries
library(mrgsolve)
library(dplyr)
library(ggplot2) 
library(grid)
library(pracma)
library(data.table)
#---------------------------------------------------------
shinyServer(function(input,output){
  
  SIM.data <- reactive({
    
    input$calculatePK  #Make the execution of the reactive function depend only on the actionbutton being clicked

  isolate({           #Isolate the rest of the reactive function, so it doesnt execute when all the sliders are changed          
    SELECT     <- input$SELECT
    
  if(SELECT=="1") {
      
      DAYS      <- input$DAYS
      tau       <- input$tau
      IV_DOSE   <- input$IV_DOSE
      INF_DURATION <- input$INF_DURATION
      POPVESOIV    <- input$POPVESOIV
      POPQESOIV    <- input$POPQESOIV

      #set up sim dataframe
      ADD <- round(DAYS/(tau/24) - 1 + 0.0000000001, digits = 0)
      INFRATE <- IV_DOSE*1000/INF_DURATION  #ug/hour

      tlast <- DAYS*24
      tau   <- tau*24
      TIME.tgrid <- c(tgrid(0,tlast,1))

      #dosing record dataset
      events <-  ev(ID=1:n, amt=IV_DOSE*1000, rate = INFRATE, cmt=2, addl=ADD, ii= tau)

      #update parameters
      mod <- param(mod, THETA8 = POPVESOIV,
                   THETA7 = POPQESOIV)
    
  }
 
  #start SELECT ==2   
  if (SELECT=="2") {
    DAYS        <- input$DAYS
    tau         <- input$tau
    ORAL_DOSE   <- input$ORAL_DOSE
    KDISS       <- input$KDISS
    
    KAESO       <- input$KAESO
    KA          <- input$KA
    KTRDepotGut <- input$KTRDepotGut
    KTRMucusGut <- input$KTRMucusGut
    POPVMUCUS   <- input$POPVMUCUS

    POPVESO <- input$POPVESO
    POPQESO <- input$POPQESO
    FRMUCUS <- input$FRMUCUS
    FABSGUT <- input$FABSGUT
    FABSMUCUS <- input$FABSMUCUS
    
    tlast <- DAYS*24
    tau   <- tau*24
    TIME.tgrid <- c(tgrid(0,tlast,1))
    
    #additional doses
    ADD <- round(DAYS/(tau/24) - 1 + 0.0000000001, digits = 0)
    
    events <-  ev(ID=1:n,amt=ORAL_DOSE*1000, cmt=6, addl=ADD, ii= tau)  
    
    #update parameters
    mod <- param(mod, THETA12=KDISS,
                 THETA9= KAESO,
                 THETA5=KA,
                 THETA10 = KTRDepotGut,
                 THETA11 = KTRMucusGut,
                 THETA13 = POPVMUCUS,
                 THETA8 = POPVESO,
                 THETA7 = POPQESO,
                 THETA14 = FRMUCUS,
                 THETA15 = FABSGUT,
                 THETA16 = FABSMUCUS)
        
  } #end of select ==2
    
    #start SELECT ==3  
    if(SELECT=="3") {
        DAYS        <- input$DAYS
        tau         <- input$tau
        ORAL_DOSE0   <- input$ORAL_DOSE0
        KDISS0        <- input$KDISS0
        
        KAESO0       <- input$KAESO0
        KA0          <- input$KA0
        KTRDepotGut0 <- input$KTRDepotGut0
        KTRMucusGut0 <- input$KTRMucusGut0
        POPVMUCUS0   <- input$POPVMUCUS0
        
        POPVESO0 <- input$POPVESO0
        POPQESO0 <- input$POPQESO0
        FRMUCUS0 <- input$FRMUCUS0
        FABSGUT0 <- input$FABSGUT0
        FABSMUCUS0 <- input$FABSMUCUS0
        
        DISS_DURATION <- input$DISS_DURATION
        FRZERO        <- input$FRZERO
        
        tlast <- DAYS*24
        tau   <- tau*24
        TIME.tgrid <- c(tgrid(0,tlast,1))
        
        DISSRATE <- ORAL_DOSE0*1000/(DISS_DURATION*24)
        
        #additional doses
        ADD <- round(DAYS/(tau/24) - 1 + 0.0000000001, digits = 0)
        
        event1 <-  ev(ID=1:n,amt=ORAL_DOSE0*1000, cmt=6, addl=ADD, ii= tau, REL=2)
        event2 <-  ev(ID=1:n,amt=ORAL_DOSE0*1000, rate = DISSRATE ,cmt=5, addl=ADD, ii= tau, REL=2) 
        events <- event1 + event2
        #update parameters
        mod <- param(mod, THETA12=KDISS0,
                     THETA9= KAESO0,
                     THETA5=KA0,
                     THETA10 = KTRDepotGut0,
                     THETA11 = KTRMucusGut0,
                     THETA13 = POPVMUCUS0,
                     THETA8 = POPVESO0,
                     THETA7 = POPQESO0,
                     THETA14 = FRMUCUS0,
                     THETA15 = FABSGUT0,
                     THETA16 = FABSMUCUS0,
                     THETA17 = FRZERO)
        
    }
    
    #process sim
    SIM.data <- mod %>% ev(events) %>% mrgsim(tgrid= TIME.tgrid, carry.out="rate,amt,cmt,addl,ii") %>% as.data.frame
    
    #round
    SIM.data[,c("rate")] <- round(SIM.data[,c("rate")],2)
    
    #calculate NCAmetrics
    NCAdata <- subset(SIM.data, time >= (tlast-tau))
    NCAdata <- subset(NCAdata, select=c(ID,time,IPREDESO,IPREDCENT) )
    NCAdata <- melt(NCAdata, id=c("ID","time"), value.name="IPRED", variable="DVID")
    
    NCAdatares <- NCAdata %>%
      group_by(ID,DVID) %>%
      summarise(AUC.SIM = trapz(time,IPRED),
                Cmax.SIM=max(IPRED))
    
    NCAdatares2 <- NCAdatares %>%
      group_by(DVID) %>%
      summarise(medianAUC = median(AUC.SIM),
                SD_AUC    = sd(AUC.SIM),
                medianCmax = median(Cmax.SIM),
                SD_Cmax = sd(Cmax.SIM))
    #round
    NCAdatares2[,c("medianAUC","SD_AUC","medianCmax","SD_Cmax")] <- round(NCAdatares2[,c("medianAUC","SD_AUC","medianCmax","SD_Cmax")],2)
    
  
}) #end of isolate
  
  SIM.data <- as.data.frame(SIM.data)
  NCAdatares2 <- as.data.frame(NCAdatares2)
  list(SIM.data,NCAdatares2)
 

}) #end of reactive function

#Generate plot 
output$PLOTMADRS <- renderPlot({
    
    XMAX <- max(SIM.data()[[1]]$time/24)
    FREQ = 1
    if(XMAX > 10) {FREQ = 2}
    if(XMAX > 18) {FREQ = 4}
    
    yscalemax <- max(SIM.data()[[1]]$IPREDCENT)*0.75    #set values for plot limits
    xscalemax <- max(SIM.data()[[1]]$time/24)*0.5
    
    #for DISSRATE
    yscalemax2 <- max(SIM.data()[[1]]$MUCUS)*0.75    #set values for plot limits
    
    #Plot Plasma Concentrations
    plotobj <- NULL
    plotobj <- ggplot(SIM.data()[[1]])

    #titletext <- expression(atop("Predicted 5-FU plasma concentrations",
     #                            atop("Median and 90% CI of predicted concentrations",
      #                                italic("(1000 simulated subjects)"))))
    
    if(input$CI==TRUE) {
        plotobj <- plotobj + stat_summary(aes(x=time/24, y=IPREDCENT), geom="ribbon", fill="red",fun.ymin="CI90lo", fun.ymax="CI90hi", alpha=0.3)
    } #end  
    
    plotobj <- plotobj + stat_summary(aes(x=time/24, y=IPREDCENT), color="red",fun.y=median, geom="line",  size=1)
    #plotobj <- plotobj + ggtitle(titletext) 
    plotobj <- plotobj + scale_y_continuous("Plasma concentration (ng/mL) \n" ) #, lim=c(1,400)
    plotobj <- plotobj +  scale_x_continuous("\nTime after dose (days)", breaks = seq(0,XMAX, FREQ))
    plotobj <- plotobj + scale_colour_brewer(name="Response", palette="Set1")  
    #plotobj <- plotobj + ggtitle(titletext)
    
    if(input$NCA==TRUE){
      plotobj <- plotobj + geom_text(aes(label = paste("median AUC (ng.h/ml) =",medianAUC[2])), data = SIM.data()[[2]] ,x = xscalemax*0.8, y = yscalemax*0.2, colour = "red", size = 5)
      plotobj <- plotobj + geom_text(aes(label = paste("SD AUC =",SD_AUC[2])), data = SIM.data()[[2]], x = xscalemax*0.8, y = yscalemax*0.17, colour = "red", size = 5)
      plotobj <- plotobj + geom_text(aes(label = paste("median Cmax (ng/ml) =",medianCmax[2])),data = SIM.data()[[2]], x = xscalemax*0.8, y = yscalemax*0.14, colour = "red", size = 5)
      plotobj <- plotobj + geom_text(aes(label = paste("SD Cmax =", SD_Cmax[2])),data = SIM.data()[[2]], x = xscalemax*0.8, y = yscalemax*0.1, colour = "red", size = 5)
    }#end
    
    #Plot esophageous Concentrations
    plotobj2 <- NULL
    plotobj2 <- ggplot(SIM.data()[[1]])
    
    yscalemax <- max(SIM.data()[[1]]$IPREDESO)*0.75    #set values for plot limits
    
    titletext <- expression(atop("Predicted 5-FU esophageous (top) and plasma (bottom) concentrations",
                                 atop("Median and 90% CI of predicted concentrations",
                                      italic("(1000 simulated subjects)"))))
    
    if(input$CI==TRUE) {
      plotobj2 <- plotobj2 + stat_summary(aes(x=time/24, y=IPREDESO), geom="ribbon", fill="blue",fun.ymin="CI90lo", fun.ymax="CI90hi", alpha=0.3)
    } #end  
    
    plotobj2 <- plotobj2 + stat_summary(aes(x=time/24, y=IPREDESO), color="blue",fun.y=median, geom="line",  size=1)
    plotobj2 <- plotobj2 + scale_y_continuous("Esophageous concentration (ng/mL) \n") #, lim=c(1,400)
    plotobj2 <- plotobj2 +  scale_x_continuous("\nTime after dose (days)", breaks = seq(0,XMAX, FREQ))
    plotobj2 <- plotobj2 + scale_colour_brewer(name="Response", palette="Set1")  
    plotobj2 <- plotobj2 + ggtitle(titletext)
    
    if(input$NCA==TRUE){
    plotobj2 <- plotobj2 + geom_text(aes(label = paste("median AUC (ng.h/ml) =",medianAUC[1])), data = SIM.data()[[2]] ,x = xscalemax*0.8, y = yscalemax*0.4, colour = "blue", size = 5)
    plotobj2 <- plotobj2 + geom_text(aes(label = paste("SD AUC =",SD_AUC[1])), data = SIM.data()[[2]], x = xscalemax*0.8, y = yscalemax*0.32, colour = "blue", size = 5)
    
    plotobj2 <- plotobj2 + geom_text(aes(label = paste("median Cmax (ng/ml) =",medianCmax[1])),data = SIM.data()[[2]], x = xscalemax*0.8, y = yscalemax*0.25, colour = "blue", size = 5)
    plotobj2 <- plotobj2 + geom_text(aes(label = paste("SD Cmax =", SD_Cmax[1])),data = SIM.data()[[2]], x = xscalemax*0.8, y = yscalemax*0.18, colour = "blue", size = 5)

    }
    
    #Plot amount dissolved
    plotobj3 <- NULL
    plotobj3 <- ggplot(SIM.data()[[1]])
    
    #titletext <- expression(atop("Amount of drug released",
    #                            atop("Median and 90% CI of predicted amount",
    #                                italic("(1000 simulated subjects)"))))
    
    if(input$CI==TRUE) {
        plotobj3 <- plotobj3 + stat_summary(aes(x=time/24, y=MUCUS), geom="ribbon", fill="blue",fun.ymin="CI90lo", fun.ymax="CI90hi", alpha=0.3)
    } #end  
    
    plotobj3 <- plotobj3 + stat_summary(aes(x=time/24, y=MUCUS), color="blue",fun.y=median, geom="line",  size=1)
    plotobj3 <- plotobj3 + scale_y_continuous("Amount released in mucus (ug) \n") 
    plotobj3 <- plotobj3 +  scale_x_continuous("\nTime after dose (days)", breaks = seq(0,XMAX, FREQ))
    plotobj3 <- plotobj3 + scale_colour_brewer(name="Response", palette="Set1") 
    plotobj3 <- plotobj3 + geom_text(aes(label = paste("Zero-order Dissolution Rate (ug/h) =",rate[3])), data = SIM.data()[[1]] ,x = xscalemax*0.8, y = yscalemax2*0.5, colour = "red", size = 5)
    
    
     # 2 ggplot2 graphs in a grid layout
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(3 , 4)))
    
    #Stack
    plotobj_stack1 <- NULL
    plotobj_stack2 <- NULL
    plotobj_stack3 <- NULL
    
    plotobj_stack1 <- plotobj2
    print(plotobj_stack1, vp=vplayout(1,1:4))
    
    #Plot 2
    plotobj_stack2 <- plotobj
    print(plotobj_stack2, vp=vplayout(2,1:4))
    
    #Plot 2
    plotobj_stack3 <- plotobj3
    print(plotobj_stack3, vp=vplayout(3,1:4))

    
}) #end of render plot


#For downloading simulated data
output$downloadsimdata <- downloadHandler(
  filename = function() {
    "SIM.data.csv"
  },
  
  content = function(file) {
    write.csv(SIM.data()[[1]],row.names = FALSE, file)		
  }
)   #Close downloadHandler

  
}) #this for the beging of shiny


