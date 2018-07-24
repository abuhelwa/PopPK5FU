    #Load package libraries
    library(mrgsolve)
    library(dplyr)
    library(ggplot2) 
    library(grid)
    
    
    #----------------------------------------------------------------
    #Customize ggplot2 theme - R 2.15.1
    theme_bw2 <- theme_set(theme_bw(20))  
    theme_bw2 <- theme_update(plot.margin = unit(c(1.5,1.5,3,1.5), "lines"),
                              axis.title.x=element_text(size = 16, vjust = 0),
                              axis.title.y=element_text(size = 16, vjust = 1, angle = 90))  
    
    #--------------------------------------------------------------- 
    #Define population PKPD model
    #-------------------------------------------------------------- 
    # Set number of individuals that make up the 95% prediction intervals
    n <- 3
    # 95% prediction interval functions - calculate the 2.5th and 97.5th percentiles
    CI95lo <- function(x) quantile(x,probs = 0.025)
    CI95hi <- function(x) quantile(x,probs = 0.975)
    # 90% prediction interval functions - calculate the 5th and 95th percentiles
    CI90lo <- function(x) quantile(x,probs = 0.05)
    CI90hi <- function(x) quantile(x,probs = 0.95)    
    
    #-----------------------------------------------------------------------------------------------    
    code <- '
    
    $GLOBAL
    // #define k23 (Q3/V2)        
    
    $SET delta=1
    
    $CMT GUT CENT PER ESO MUCUS DEPOT 
    
    $PARAM       //these are default values
    REL =1 

    
    $THETA @annotated
    65.3  : POPCL
    14.7  : POPVC
    334   : POPVP
    19.6  : POPQ
    2.2   : POPKA
    
    0.7   : POPFGUT
    19.6  : POPQESO
    0.20  : POPVESO
    2.2   : POPKAESO
    0.5   : POPKTRDepotGut
    
    1.5   : POPKTRMucusGut
    0.15   : POPKDISS
    0.10  : POPVMUCUS
    0.90  : POPFRMUCUS     //fraction dissolved into the mucus
    0.60  : FABSGUT       //fraction absorbed from the GUT

    0.90  : FABSMUCUC //Fraction absobed from the mucus
    1     : FRZERO
    
    
    $MAIN
    
    //PK-5FU
    double TVCL=THETA1;
    double CL=TVCL*exp(ETA(1));
    
    double TVVC=THETA2;
    double VC=TVVC*exp(ETA(2));
    
    double TVVP=THETA3;
    double VP=TVVP*exp(ETA(3));
    
    double TVQ=THETA4;
    double Q=TVQ*exp(ETA(4));
    
    double TVKA=THETA5;
    double KA=TVKA*exp(ETA(5));
    
    if (REL == 1) F_DEPOT = 1;  //when it is all firts-order
    if (REL == 2) F_MUCUS = THETA17;   // Fraction absorbed by zero-order 
    if (REL == 2) F_DEPOT = (1-F_MUCUS); // Fraction absorbed by 1st-order 
    

    double QESO = THETA7*exp(ETA(11));
    double VESO = THETA8*exp(ETA(12));
    
    double TVKAESO=THETA9;
    double KAESO=TVKAESO*exp(ETA(6));
    
    double KTRDepotGut = THETA10*exp(ETA(7));
    double KTRMucusGut = THETA11*exp(ETA(8));
    double KDISS = THETA12*exp(ETA(9));
    
    double VMUCUS = THETA13*exp(ETA(10));

    double FRMUCUS = THETA14;  //Fraction going into mucus
    double FRGUT = (1-THETA14);  //Fraction going into GUT

    double FABSGUT = THETA15;  //Fraction absorbed from the GUT 
    double FABSMUCUC = THETA16; //Fraction absorbed from mucus
    double FTRANSITMUCUSGUT = (1-THETA16);

    $ODE
    double C2 = CENT/VC;        //Central CMT
    double C3 = PER/VP;         //Peripheral CMT
    double C4 = ESO/VESO;       //Esophageous CMT
 
    dxdt_GUT    = FRGUT*KTRDepotGut*DEPOT + FTRANSITMUCUSGUT*KTRMucusGut*MUCUS-FABSGUT*KA*GUT;
    dxdt_CENT   =  FABSGUT*KA*GUT + QESO*C4 +  Q*C3 - Q*C2 - CL*C2 - QESO*C2;
    dxdt_PER    = Q*C2 - Q*C3;
    
    dxdt_ESO    =  FABSMUCUC*KAESO*MUCUS + QESO*C2 - QESO * C4;
    dxdt_MUCUS  =  FRMUCUS*KDISS*DEPOT - FABSMUCUC*KAESO*MUCUS - FTRANSITMUCUSGUT*KTRMucusGut*MUCUS;   //MUCUS CMT
    dxdt_DEPOT   = -FRMUCUS*KDISS*DEPOT - FRGUT*KTRDepotGut*DEPOT;     //DEPOT CMT

    $OMEGA @name BSV @annotated
    PPV_CL: 0 : PPV on CL
    PPV_VC: 0  : PPV on VC
    PPV_VP: 0   : PPV on VP
    PPV_Q: 0   : PPV on  Q
    PPV_KA: 0  : PPV on  KA
    PPV_KAESO: 0  : PPV on  KAESO
    PPV_KTRDepotGut: 0  : PPV on  KTRDepotGut
    PPV_KTRMucusGut: 0  : PPV on  KTRMucusGut
    PPV_DISSRATE: 0  : PPV on  KDISS
    PPV_VMUCUS: 0 : PPV on  VMUCUS
    PPV_QESO: 0  : PPV on  QESO
    PPV_VESO: 0  : PPV on  VESO
    
    $SIGMA
    0
    
    $TABLE
    double CFU   = CENT/VC;
    double CESO  = ESO/VESO;
    
    double IPREDCENT;
    double DVCENT;
    
    double IPREDESO;
    double DVESO;
    
    IPREDCENT=CFU;
    IPREDESO =CESO;
    DVCENT=IPREDCENT+EPS(1);
    DVESO=IPREDESO+EPS(1);
    
    $CAPTURE REL CL VC VP Q C2 C3 KDISS FRMUCUS FRGUT KTRDepotGut IPREDCENT IPREDESO DVCENT DVESO 
    '
    mod <- mcode("5FUpk", code)
