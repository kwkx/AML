library(shiny)
library(shinydashboard)
library(rintrojs)
library(shinyhelper)
library(magrittr)
library(nnet)
ui <- dashboardPage(
  dashboardHeader(title = "IPRP Signature"),
  #introjsUI(),

      dashboardSidebar( 
      introjsUI(),
      sidebarMenu(
      introBox(
        data.step = 1,
        data.intro = "You can get access to our prediction app, nomogram and contact here.",
        br(),
        h4(menuItem("Prediction", tabName = "prediction", icon = icon("book"))),
        h4(menuItem("Nomogram", tabName = "nomogram", icon = icon("book"))),
        h4(menuItem("Contact", tabName = "contact", icon = icon("home")))
      )

    )),  
  
  
  
  dashboardBody(
    #introjsUI(),
    tabItems(
      # First tab content
      tabItem(tabName = "prediction",
              fluidRow(
                HTML('<meta name="viewport" content="width=1024">'),
                #titlePanel(title = div(img(src = "logo.png",height=90,width=150), "Prognostic prediction of AML patients based on immunity and pyroptosis gene-pair signature (IPRP)")),
                titlePanel(title = div("Prognostic prediction of AML patients based on immunity and pyroptosis gene-pair signature (IPRP)")),
                
                #titlePanel("Prognosis predicting app of AML patients based on IPRP score."),
                sidebarLayout(

                  sidebarPanel(
                    introBox(
                      actionButton("help", "Click for instructions"),
                      data.step = 2,
                      data.intro = "Input these 3 clinical information, and please use the age at diagnosis.",
                      helpText(h5("Please enter the patient's clinical information")),
                      selectInput("prior","PriorMalignancyNonMyeloid", 
                                  choices = list("No" = "No", "Yes" = "Yes"), selected = 1),
                      selectInput("ethnicity","Ethnicity", 
                                  choices = list("White" = "White", "Other" = "Other"), selected = 1),
                      textInput("age", label = "Age", value = "30")
                      #####
                    ),

                    #####
                    helpText(h5("Please enter the expression levels of these genes"))%>%
                      helper(icon = "question",
                             colour = "green",
                             type = "markdown",
                             content = "mymarkdown"),
                    
                    
                    introBox(
                      #actionButton("button1","Note the data type!"),
                      data.step = 3,
                      data.intro = "Please use microarray data or scaled RNA-seq data where the sequencing depth effect has been normalized such as FPKM, RPKM, TPM.",
                      #helpText("Notice: Do use Microarray data or scaled RNA-seq data after eliminating the sequencing depth effect, such as FPKM, RPKM, TPM, etc."),
                      textInput("COL9A2", label = "COL9A2", value = "0"),
                      
                      textInput("NPDC1", label = "NPDC1", value = "0"),
                      textInput("PLXNC1", label = "PLXNC1", value = "0"),
                      textInput("SLC24A3", label = "SLC24A3", value = "0"),
                      textInput("FZD6", label = "FZD6", value = "0"),
                      textInput("CYP2E1", label = "CYP2E1", value = "0"),
                      textInput("MYO1B", label = "MYO1B", value = "0"),
                      textInput("TCF4", label = "TCF4", value = "0"),
                      textInput("TAF1C", label = "TAF1C", value = "0"),
                      textInput("ACSL3", label = "ACSL3", value = "0"),
                      textInput("CRTAP", label = "CRTAP", value = "0"),
                      textInput("IGLL1", label = "IGLL1", value = "0"),
                      textInput("DNMT3B", label = "DNMT3B", value = "0"),
                      textInput("SLC36A1", label = "SLC36A1", value = "0"),
                      textInput("FSTL1", label = "FSTL1", value = "0")
                    )
                    
                   
                    
                  ),
                  mainPanel(
                    hr(),
                    helpText(h3("Below are the points of each factor and the final prediction results:")),
                    hr(),
                    introBox(
                      data.step = 4,
                      data.intro = "You can get points of 3 clinical information here.",
                      helpText(h3("The points of PriorMalignancy:")),
                      textOutput("point_prior"),
                      
                      helpText(h3("The points of Ethnicity:")),
                      textOutput("point_Ethnicity"),
                      
                      
                      
                      helpText(h3("The points of AgeatDiagnosis:")),
                      textOutput("point_age")
                    ),
                   
                    introBox(
                      data.step = 5,
                      data.intro = "You can get IPRP score related result here.",
                      helpText(h3("The Risk score value:")),
                      textOutput("riskscore"),
                      helpText(h3("The label of Risk score:")),
                      textOutput("point_risk"),
                      helpText(h3("The points of Risk score:")),
                      textOutput("pointsss_risk")
                      
                      ),
                    introBox(
                      data.step = 6,
                      data.intro = "You can get total points and predicted mortality of this patient.",
                      helpText(h3("Total points:")),
                      textOutput("point_total"),
                      
                      h3("Mortality of"),
                      textOutput("rate_year"),
                      tags$head(tags$style("#rate_year{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                      )
                      )
                      
                      
                      # helpText(h3("Mortality of")),
                      # textOutput("rate_year"),
                      # tags$head(tags$style("#clustercontents{color: red;
                      #            font-size: 20px;
                      #            font-style: italic;
                      #            }"
                      # )
                      # )
                      
                      
                    )

                  )
                )
                
                
                
                
                
                
              )
      ),
      
      # Second tab content
      
      tabItem(tabName = "nomogram",
              h2("The patient-estimated nomogram based on the IPRP score"),
              img(src = "nomo1.png",height=400)
      ),
      tabItem(tabName = "contact",
              h2("If you have any problems with using the web-app, or suggestions for improvement, please contact Dr Liye He at liye.he@helsinki.fi"),
              hr(),
              hr(),
              h4("Institute for Molecular Medicine Finland (FIMM)"),
              h4("Nordic EMBL Partnership for Molecular Medicine"),
              h4("Biomedicum Helsinki 2U"),
              h4("P.O. Box 20 (Tukholmankatu 8)"),
              h4("FI-00014 University of Helsinki"),
              h4("Finland")
      )
      
      
  )
 )
)

server <- function(input, output, session) {
  
  observe_helpers(help_dir = "helpfiles")
  observeEvent(input$button1,
               introjs(session))
  
  output$point_prior=renderText(
    {
      if(input$prior=="Yes")
      {
        "80"
      }
      else
      {
        "65"
      }
    }
  )
  output$point_Ethnicity=renderText(
    {
      if(input$ethnicity=="White")
      {
        "37"
      }
      else if(input$ethnicity=="Other")
      {
        "65"
      }
      
    }
  )

  output$point_age=renderText(
    {
      
      if(is.na(as.numeric(input$age)))
      {
        "Please input valid age"
      }
      else
      {
        agenum=as.numeric(input$age)
        pointthis=(106/90)*agenum-2
        pointthis=round(pointthis,digits = 2)
        as.character(pointthis)
      }
    }
  )
  
  output$riskscore=renderText(
    {
      COL9A2=as.numeric(input$COL9A2)
      NPDC1=as.numeric(input$NPDC1)
      PLXNC1=as.numeric(input$PLXNC1)
      SLC24A3=as.numeric(input$SLC24A3)
      FZD6=as.numeric(input$FZD6)
      CYP2E1=as.numeric(input$CYP2E1)
      MYO1B=as.numeric(input$MYO1B)
      TCF4=as.numeric(input$TCF4)
      TAF1C=as.numeric(input$TAF1C)
      ACSL3=as.numeric(input$ACSL3)
      CRTAP=as.numeric(input$CRTAP)
      IGLL1=as.numeric(input$IGLL1)
      DNMT3B=as.numeric(input$DNMT3B)
      SLC36A1=as.numeric(input$SLC36A1)
      FSTL1=as.numeric(input$FSTL1)
      
      
      if(is.na(COL9A2)|is.na(NPDC1)|is.na(PLXNC1)|is.na(SLC24A3)|is.na(FZD6)|is.na(CYP2E1)|
         is.na(MYO1B)|is.na(TCF4)|is.na(TAF1C)|is.na(ACSL3)|is.na(CRTAP)|is.na(IGLL1)|
         is.na(DNMT3B)|is.na(SLC36A1)|is.na(FSTL1))
      {
        "Please input valid expression data"
      }
      
      else
      {
        scorevector=vector(length=10)
        
        scorevector[1]=ifelse(COL9A2>NPDC1, 1, 0)
        scorevector[2]=ifelse(PLXNC1>SLC24A3, 1, 0)
        scorevector[3]=ifelse(FZD6>MYO1B, 1, 0)
        scorevector[4]=ifelse(TCF4>TAF1C, 1, 0)
        scorevector[5]=ifelse(TAF1C>ACSL3, 1, 0)
        scorevector[6]=ifelse(ACSL3>CRTAP, 1, 0)
        scorevector[7]=ifelse(ACSL3>IGLL1, 1, 0)
        scorevector[8]=ifelse(ACSL3>DNMT3B, 1, 0)
        scorevector[9]=ifelse(CYP2E1>MYO1B, 1, 0)
        scorevector[10]=ifelse(SLC36A1>FSTL1, 1, 0)
        
        
        scorefinal=exp(scorevector[1]*(-0.38387)+scorevector[2]*(0.435025)+scorevector[3]*(0.441214)+
                         scorevector[4]*(0.361899)+scorevector[5]*(-0.5695)+scorevector[6]*(0.432523)+
                         scorevector[7]*(0.378103)+scorevector[8]*(-0.78788)+scorevector[9]*(-0.27675)+
                         scorevector[10]*(-0.46531))
        scorefinal=round(scorefinal,digits = 2)
        as.character(scorefinal)
      }
    }
  )
  output$point_risk=renderText(
    {
      COL9A2=as.numeric(input$COL9A2)
      NPDC1=as.numeric(input$NPDC1)
      PLXNC1=as.numeric(input$PLXNC1)
      SLC24A3=as.numeric(input$SLC24A3)
      FZD6=as.numeric(input$FZD6)
      CYP2E1=as.numeric(input$CYP2E1)
      MYO1B=as.numeric(input$MYO1B)
      TCF4=as.numeric(input$TCF4)
      TAF1C=as.numeric(input$TAF1C)
      ACSL3=as.numeric(input$ACSL3)
      CRTAP=as.numeric(input$CRTAP)
      IGLL1=as.numeric(input$IGLL1)
      DNMT3B=as.numeric(input$DNMT3B)
      SLC36A1=as.numeric(input$SLC36A1)
      FSTL1=as.numeric(input$FSTL1)
      
      if(is.na(COL9A2)|is.na(NPDC1)|is.na(PLXNC1)|is.na(SLC24A3)|is.na(FZD6)|is.na(CYP2E1)|
         is.na(MYO1B)|is.na(TCF4)|is.na(TAF1C)|is.na(ACSL3)|is.na(CRTAP)|is.na(IGLL1)|
         is.na(DNMT3B)|is.na(SLC36A1)|is.na(FSTL1))
      {
        "Please input valid expression data"
      }
      else
      {
        scorevector=vector(length=10)
        
        scorevector[1]=ifelse(COL9A2>NPDC1, 1, 0)
        scorevector[2]=ifelse(PLXNC1>SLC24A3, 1, 0)
        scorevector[3]=ifelse(FZD6>MYO1B, 1, 0)
        scorevector[4]=ifelse(TCF4>TAF1C, 1, 0)
        scorevector[5]=ifelse(TAF1C>ACSL3, 1, 0)
        scorevector[6]=ifelse(ACSL3>CRTAP, 1, 0)
        scorevector[7]=ifelse(ACSL3>IGLL1, 1, 0)
        scorevector[8]=ifelse(ACSL3>DNMT3B, 1, 0)
        scorevector[9]=ifelse(CYP2E1>MYO1B, 1, 0)
        scorevector[10]=ifelse(SLC36A1>FSTL1, 1, 0)
        
        
        scorefinal=exp(scorevector[1]*(-0.38387)+scorevector[2]*(0.435025)+scorevector[3]*(0.441214)+
                         scorevector[4]*(0.361899)+scorevector[5]*(-0.5695)+scorevector[6]*(0.432523)+
                         scorevector[7]*(0.378103)+scorevector[8]*(-0.78788)+scorevector[9]*(-0.27675)+
                         scorevector[10]*(-0.46531))
        if(scorefinal>0.6844)
        {
          "high"
        }
        else
        {
          "low"
        }
      }
      
      
     
    }
  )
  
  
  output$pointsss_risk=renderText(
    {
      COL9A2=as.numeric(input$COL9A2)
      NPDC1=as.numeric(input$NPDC1)
      PLXNC1=as.numeric(input$PLXNC1)
      SLC24A3=as.numeric(input$SLC24A3)
      FZD6=as.numeric(input$FZD6)
      CYP2E1=as.numeric(input$CYP2E1)
      MYO1B=as.numeric(input$MYO1B)
      TCF4=as.numeric(input$TCF4)
      TAF1C=as.numeric(input$TAF1C)
      ACSL3=as.numeric(input$ACSL3)
      CRTAP=as.numeric(input$CRTAP)
      IGLL1=as.numeric(input$IGLL1)
      DNMT3B=as.numeric(input$DNMT3B)
      SLC36A1=as.numeric(input$SLC36A1)
      FSTL1=as.numeric(input$FSTL1)
      
      
      if(is.na(COL9A2)|is.na(NPDC1)|is.na(PLXNC1)|is.na(SLC24A3)|is.na(FZD6)|is.na(CYP2E1)|
         is.na(MYO1B)|is.na(TCF4)|is.na(TAF1C)|is.na(ACSL3)|is.na(CRTAP)|is.na(IGLL1)|
         is.na(DNMT3B)|is.na(SLC36A1)|is.na(FSTL1))
      {
        "Please input valid expression data"
      }
      else
      {
        scorevector=vector(length=10)
        
        scorevector[1]=ifelse(COL9A2>NPDC1, 1, 0)
        scorevector[2]=ifelse(PLXNC1>SLC24A3, 1, 0)
        scorevector[3]=ifelse(FZD6>MYO1B, 1, 0)
        scorevector[4]=ifelse(TCF4>TAF1C, 1, 0)
        scorevector[5]=ifelse(TAF1C>ACSL3, 1, 0)
        scorevector[6]=ifelse(ACSL3>CRTAP, 1, 0)
        scorevector[7]=ifelse(ACSL3>IGLL1, 1, 0)
        scorevector[8]=ifelse(ACSL3>DNMT3B, 1, 0)
        scorevector[9]=ifelse(CYP2E1>MYO1B, 1, 0)
        scorevector[10]=ifelse(SLC36A1>FSTL1, 1, 0)
        
        
        scorefinal=exp(scorevector[1]*(-0.38387)+scorevector[2]*(0.435025)+scorevector[3]*(0.441214)+
                         scorevector[4]*(0.361899)+scorevector[5]*(-0.5695)+scorevector[6]*(0.432523)+
                         scorevector[7]*(0.378103)+scorevector[8]*(-0.78788)+scorevector[9]*(-0.27675)+
                         scorevector[10]*(-0.46531))
        if(scorefinal>0.6844)
        {
          "65"
        }
        else
        {
          "39"
        }
      }
      
      
    }
  )
  
  output$point_total=renderText(
    {
      COL9A2=as.numeric(input$COL9A2)
      NPDC1=as.numeric(input$NPDC1)
      PLXNC1=as.numeric(input$PLXNC1)
      SLC24A3=as.numeric(input$SLC24A3)
      FZD6=as.numeric(input$FZD6)
      CYP2E1=as.numeric(input$CYP2E1)
      MYO1B=as.numeric(input$MYO1B)
      TCF4=as.numeric(input$TCF4)
      TAF1C=as.numeric(input$TAF1C)
      ACSL3=as.numeric(input$ACSL3)
      CRTAP=as.numeric(input$CRTAP)
      IGLL1=as.numeric(input$IGLL1)
      DNMT3B=as.numeric(input$DNMT3B)
      SLC36A1=as.numeric(input$SLC36A1)
      FSTL1=as.numeric(input$FSTL1)
      
      if(is.na(COL9A2)|is.na(NPDC1)|is.na(PLXNC1)|is.na(SLC24A3)|is.na(FZD6)|is.na(CYP2E1)|
         is.na(MYO1B)|is.na(TCF4)|is.na(TAF1C)|is.na(ACSL3)|is.na(CRTAP)|is.na(IGLL1)|
         is.na(DNMT3B)|is.na(SLC36A1)|is.na(FSTL1)|is.na(as.numeric(input$age)))
      {
        "Please input valid age and expression data"
      }
      else
      {
        scorevector=vector(length=10)
        
        scorevector[1]=ifelse(COL9A2>NPDC1, 1, 0)
        scorevector[2]=ifelse(PLXNC1>SLC24A3, 1, 0)
        scorevector[3]=ifelse(FZD6>MYO1B, 1, 0)
        scorevector[4]=ifelse(TCF4>TAF1C, 1, 0)
        scorevector[5]=ifelse(TAF1C>ACSL3, 1, 0)
        scorevector[6]=ifelse(ACSL3>CRTAP, 1, 0)
        scorevector[7]=ifelse(ACSL3>IGLL1, 1, 0)
        scorevector[8]=ifelse(ACSL3>DNMT3B, 1, 0)
        scorevector[9]=ifelse(CYP2E1>MYO1B, 1, 0)
        scorevector[10]=ifelse(SLC36A1>FSTL1, 1, 0)
        
        
        scorefinal=exp(scorevector[1]*(-0.38387)+scorevector[2]*(0.435025)+scorevector[3]*(0.441214)+
                         scorevector[4]*(0.361899)+scorevector[5]*(-0.5695)+scorevector[6]*(0.432523)+
                         scorevector[7]*(0.378103)+scorevector[8]*(-0.78788)+scorevector[9]*(-0.27675)+
                         scorevector[10]*(-0.46531))
        sumvalue=0
        if(scorefinal>0.6844)
        {
          sumvalue=sumvalue+65
        }
        else
        {
          sumvalue=sumvalue+39
        }
        ######################################
        if(input$prior=="Yes")
        {
          sumvalue=sumvalue+80
        }
        else
        {
          sumvalue=sumvalue+65
        }
        #########################
        if(input$ethnicity=="White")
        {
          sumvalue=sumvalue+37
        }
        else if(input$ethnicity=="Other")
        {
          sumvalue=sumvalue+65
        }

        ##############################
        agenum=as.numeric(input$age)
        pointthis=(106/90)*agenum-2
        sumvalue=sumvalue+pointthis
        sumvalue=round(sumvalue,digits = 2)
        as.character(sumvalue)
      }
      
      
    }
  )
  
  #############we will calculate the final prob
  output$rate_year=renderText(
    {
      COL9A2=as.numeric(input$COL9A2)
      NPDC1=as.numeric(input$NPDC1)
      PLXNC1=as.numeric(input$PLXNC1)
      SLC24A3=as.numeric(input$SLC24A3)
      FZD6=as.numeric(input$FZD6)
      CYP2E1=as.numeric(input$CYP2E1)
      MYO1B=as.numeric(input$MYO1B)
      TCF4=as.numeric(input$TCF4)
      TAF1C=as.numeric(input$TAF1C)
      ACSL3=as.numeric(input$ACSL3)
      CRTAP=as.numeric(input$CRTAP)
      IGLL1=as.numeric(input$IGLL1)
      DNMT3B=as.numeric(input$DNMT3B)
      SLC36A1=as.numeric(input$SLC36A1)
      FSTL1=as.numeric(input$FSTL1)
      
      
      if(is.na(COL9A2)|is.na(NPDC1)|is.na(PLXNC1)|is.na(SLC24A3)|is.na(FZD6)|is.na(CYP2E1)|
         is.na(MYO1B)|is.na(TCF4)|is.na(TAF1C)|is.na(ACSL3)|is.na(CRTAP)|is.na(IGLL1)|
         is.na(DNMT3B)|is.na(SLC36A1)|is.na(FSTL1)|is.na(as.numeric(input$age)))
      {
        "Please input valid age and expression data"
      }
      else
      {
        scorevector=vector(length=10)
        
        scorevector[1]=ifelse(COL9A2>NPDC1, 1, 0)
        scorevector[2]=ifelse(PLXNC1>SLC24A3, 1, 0)
        scorevector[3]=ifelse(FZD6>MYO1B, 1, 0)
        scorevector[4]=ifelse(TCF4>TAF1C, 1, 0)
        scorevector[5]=ifelse(TAF1C>ACSL3, 1, 0)
        scorevector[6]=ifelse(ACSL3>CRTAP, 1, 0)
        scorevector[7]=ifelse(ACSL3>IGLL1, 1, 0)
        scorevector[8]=ifelse(ACSL3>DNMT3B, 1, 0)
        scorevector[9]=ifelse(CYP2E1>MYO1B, 1, 0)
        scorevector[10]=ifelse(SLC36A1>FSTL1, 1, 0)
        
        
        scorefinal=exp(scorevector[1]*(-0.38387)+scorevector[2]*(0.435025)+scorevector[3]*(0.441214)+
                         scorevector[4]*(0.361899)+scorevector[5]*(-0.5695)+scorevector[6]*(0.432523)+
                         scorevector[7]*(0.378103)+scorevector[8]*(-0.78788)+scorevector[9]*(-0.27675)+
                         scorevector[10]*(-0.46531))
        sumvalue=0
        if(scorefinal>0.6844)
        {
          sumvalue=sumvalue+65
        }
        else
        {
          sumvalue=sumvalue+39
        }
        ######################################
        if(input$prior=="Yes")
        {
          sumvalue=sumvalue+80
        }
        else
        {
          sumvalue=sumvalue+65
        }
        #########################
        if(input$ethnicity=="White")
        {
          sumvalue=sumvalue+37
        }
        else if(input$ethnicity=="Other")
        {
          sumvalue=sumvalue+65
        }

        ##############################
        agenum=as.numeric(input$age)
        pointthis=(106/90)*agenum-2
        sumvalue=sumvalue+pointthis
        
        if(sumvalue>120&sumvalue<=140)
        {
          onevalue=((0.0291-0.0141)/20)*(sumvalue-120)+0.0141
          
          twovalue=((0.0485-0.0236)/20)*(sumvalue-120)+0.0236
          
          threevalue=((0.0692-0.0339)/20)*(sumvalue-120)+0.0339
        }
        else if(sumvalue>140&sumvalue<=160)
        {
          onevalue=((0.0595-0.0291)/20)*(sumvalue-140)+0.0291
          
          twovalue=((0.0982-0.0485)/20)*(sumvalue-140)+0.0485
          
          threevalue=((0.1385-0.0692)/20)*(sumvalue-140)+0.0692
        }
        else if(sumvalue>160&sumvalue<=180)
        {
          onevalue=((0.1198-0.0595)/20)*(sumvalue-160)+0.0595
          
          twovalue=((0.1933-0.0982)/20)*(sumvalue-160)+0.0982
          
          threevalue=((0.2664-0.1385)/20)*(sumvalue-160)+0.1385
        }
        else if(sumvalue>180&sumvalue<=200)
        {
          onevalue=((0.2329-0.1198)/20)*(sumvalue-180)+0.1198
          
          twovalue=((0.3600-0.1933)/20)*(sumvalue-180)+0.1933
          
          threevalue=((0.4747-0.2664)/20)*(sumvalue-180)+0.2664
        }
        else if(sumvalue>200&sumvalue<=220)
        {
          onevalue=((0.4236-0.2329)/20)*(sumvalue-200)+0.2329
          
          twovalue=((0.6045-0.3600)/20)*(sumvalue-200)+0.3600
          
          threevalue=((0.7376-0.4747)/20)*(sumvalue-200)+0.4747
        }
        else if(sumvalue>220&sumvalue<=240)
        {
          onevalue=((0.6817-0.4236)/20)*(sumvalue-220)+0.4236
          
          twovalue=((0.8545-0.6045)/20)*(sumvalue-220)+0.6045
          
          threevalue=((0.9380-0.7376)/20)*(sumvalue-220)+0.7376
        }
        else if(sumvalue>240&sumvalue<=260)
        {
          onevalue=((0.9074-0.6817)/20)*(sumvalue-240)+0.6817
          
          twovalue=((0.9818-0.8545)/20)*(sumvalue-240)+0.8545
          
          threevalue=((0.9969-0.9380)/20)*(sumvalue-240)+0.9380
        }
        else if(sumvalue>260&sumvalue<=280)
        {
          onevalue=((0.9929-0.9074)/20)*(sumvalue-260)+0.9074
          
          twovalue=((0.9998-0.9818)/20)*(sumvalue-260)+0.9818
          
          threevalue=((1-0.9969)/20)*(sumvalue-260)+0.9969
        }
        else if(sumvalue>280&sumvalue<=300)
        {
          onevalue=((1-0.9929)/20)*(sumvalue-280)+0.9929
          
          twovalue=((1-0.9998)/20)*(sumvalue-280)+0.9998
          
          threevalue=((1-1)/20)*(sumvalue-280)+1
        }
        a=round(onevalue*100,2)
        b=round(twovalue*100,2)
        c=round(threevalue*100,2)
        paste0("1-year:",as.character(a),"%;  ","2-year:",as.character(b),"%;  ",
               "3-year:",as.character(c),"%;")
      }
      
      
      
    }
  )

  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Prev",
                                               "skipLabel"="End tour")
                       )
  )
  
  
}

shinyApp(ui, server)

