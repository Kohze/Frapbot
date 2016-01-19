#frapbot 1.1
# server.R
library(ggplot2)
library(shiny)
library(minpack.lm)
library(dplyr)
library(Cairo)
library(grid)
library(DT)
library(data.table)



options(shiny.usecairo=T)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

shinyServer(
  function(input, output) {
    
    d <- reactive({
      
      inFile <- input$file
      if (is.null(inFile)) {
        return(NULL)
      } else {
        inFile %>%
          rowwise() %>%
          do({
            fread(.$datapath, header = TRUE, data.table = FALSE)
            
          })
      }
      })
    
    output$mainOutput <- renderUI({
      if(is.null(d())){return()}
      tabsetPanel(
        tabPanel("Frap Plot",plotOutput("main2")),
        tabPanel("Dataset")
        ,id = "mainPanel")
    })
    
    output$update1 <- renderUI ({
      if(is.null(d())){return()}
      splitLayout(
        downloadButton("download", label="Download")
      )
    })
    
    output$qualitySlider <- renderUI ({
      if(is.null(d())){return()}
      sliderInput("quality","Standart Error Slider",0.0,1.0,0.9)
    })
    
    output$update2 <- renderUI ({
      if(is.null(d())){return()}
      splitLayout(
        actionButton("action","Apply changes",icon=NULL)
      )
    })
    
    output$automaticROI <- renderUI({
      if (is.null(d())){return()}
      selectInput("ROI","",c("Automatic ROI selection"=1, "Manual ROI selection"=2))
    })
    
    output$bleachTime <- renderUI({
      if (is.null(d())){return()}
      selectInput("Btime","",c("Automatic Pre-bleach finder"=2,"Manual Pre-Bleach input"=1,"Manual Input"=3))
    })  
    
    output$noBackground <- renderUI({
      if (is.null(d())){return()}
      checkboxInput("noBG","enable Background correction",TRUE)
    })
    
    output$mbleachTime <- renderUI({
      if(is.null(input$Btime) | is.null(d())){return()}
      if(input$Btime != 1){return()}
      a = nrow(d())/4
      sliderInput("blti","",0,a,5,1)
    })
    
    output$dataSetChoice <- renderUI({
      if(is.null(d())){return()}
      d = d()
      for (i in 1:ncol(d)) {
        if (d[1,i]+2 == d[3,i]) {
          sc = d[i]
        } 
      }
      sc1b = rbind(tail(sc,nrow(sc)-1),tail(sc,1))
      sct = sc-sc1b
      sctn = c(0,c(which(sct>2)),nrow(sc))
      if(length(c(which(sct>2)))==0){return(NULL)}
      clen=length(sctn)-1 
      sliderInput("SlChoice","Dataset Slider",1,clen,1,1)
    })
    
    output$manualInput <- renderUI({
      if(is.null(input$Btime) | is.null(d())){return()}
      if(input$Btime != 3){return()}
      numericInput("numInp","Manual Input of Pre-Bleach", 20,1,10000)
    })
    
    output$fitting <- renderUI ({
      if(is.null(d())){return()}
      selectInput("fit","",c("Exponential Fitting"=1,"Double Exponential Fitting"=2, "Custom Formula"=3))
    })
    
    output$ownFormula <- renderUI({
      if(is.null(d())){return()}
      if (is.null(input$fit) | is.null(d())){return()}
      if(!(input$fit == 3)){return()}
      textInput("customFormula","",value = "b*(1-exp(-x*tm))+c")
    })
    
    output$norm <- renderUI({
      if(is.null(d())){return()}
      selectInput("normal","",c("Direct Normalization"=1,"averaged Normalization"=2))
    })
    
    output$normSlider <- renderUI({
      if(is.null(d())){return()}
      if (is.null(input$normal) | is.null(d())){return()}
      if(!(input$normal == 2)){return()}
      sliderInput("NormalizationSlider","",0,2,1.1,0.1)
    })
    
    output$scanTime <- renderUI({
      if(is.null(d())){return()}
        sliderInput("t","Scan Time in ms", 1,1000,79)
    })
    
   
    
    output$contents <- DT::renderDataTable({
      if (is.null(d())  | is.null(input$mainPanel)){return()}
      if(input$mainPanel == "Frap Plot" & !(input$mainPanel == "Dataset")){return()}
      DT::datatable(d(), options = list(pageLength = 20))
     
      
      })
    
   
    ######################UI for manual ROI########################### 
    
    output$bleachROI <- renderUI({
      if(is.null(input$ROI)){return()}
      if (!(input$ROI == 2) | is.null(input$ROI) | is.null(d())){return()}
      colnames <- colnames(d())
      selectInput("bleachROIin","Bleach ROI", c(colnames),selectize=FALSE)
    })
    
    output$controlROI <- renderUI({
      if(is.null(input$ROI)){return()}
      if (!(input$ROI == 2) | is.null(d())){return()}
      colnames <- colnames(d())
      selectInput("controlROIin","Total ROI",c(colnames),selectize=FALSE)
    })
    
    output$bgROI <- renderUI({
      if(is.null(input$ROI)){return()}
      if (!(input$ROI == 2) | is.null(input$ROI) | is.null(d())){return()}
      colnames <- colnames(d())
      selectInput("bgROI","Bg ROI",c(colnames),selectize=FALSE)
    })
    
    output$areaROI <- renderUI({
      if(is.null(input$ROI)){return()}
      if (!(input$ROI == 2) | is.null(input$ROI) | is.null(d())){return()}
      colnames <- colnames(d())
      selectInput("areaROI","Bleach Area",c(colnames),selectize=FALSE)
    })
    
    #########################main code################################
    
    
    output$main2 <- renderPlot({
      d = d()
      
      alternate=NULL
      
      for (i in 1:ncol(d)) {
        if (d[1,i]+2 == d[3,i]) {
          sc = d[i]
        }
      }
      
      if(!(is.null(input$SlChoice))){
        choice=input$SlChoice
      } else {
        choice=1
      }
      
      if(!(is.null(sc) & !(is.null(input$SlChoice)))){
        sc1b = rbind(tail(sc,nrow(sc)-1),tail(sc,1))
        sct = sc-sc1b
        sctn = c(0,c(which(sct>2)),nrow(sc))
        if(length(sctn)>2) {
          d = d[(sctn[choice]+1):(sctn[choice+1]),]
        }
      } 
      
      
      ######################################################
      ######################################################
      ######################################################
      
      
      ScatterPlot <- function(Data,FileChoice){
        
        d = Data
        choice = FileChoice
        
        alternate=NULL
        
        for (i in 1:ncol(d)) {
          if (d[1,i]+2 == d[3,i]) {
            sc = d[i]
          }}
        
        
        if(!(is.null(sc) & !(is.null(input$SlChoice)))){
          sc1b = rbind(tail(sc,nrow(sc)-1),tail(sc,1))
          sct = sc-sc1b
          sctn = c(0,c(which(sct>2)),nrow(sc))
          if(length(sctn)>2) {
            d = d[(sctn[choice]+1):(sctn[choice+1]),]
          }
        } 
        
        #convert datasets into variable
        #Mean1 = Bleach Region; Mean2 = Control Region; Mean3 = Background; Area1 = Bleach Area
        
        ### the automatic row to area/signal finding sub-routine
        if (!(exists("ms"))){
          ms = matrix(nrow=nrow(d))
          ar = matrix(nrow=nrow(d))
          
        } else {
        
          ms = matrix(nrow=nrow(d))
          ar = matrix(nrow=nrow(d))
        }
        
        for (i in 1:ncol(d)) {
          if (d[1,i]+2 == d[3,i]) {
            ## input scan time to t algorithm
            t = d[i]
           
          }
          if(!(head(d[i],1) == tail(d[i],1)) & !head(d[i],1)%%1 == 0 ) {
            ms = cbind(ms,d[i])
            ar = cbind(ar,d[i-1])
          }
        }
        
        
        #deleting the NA in the matrix colunm 1
        m = ms[2:ncol(ms)]
        as1 = ar[2:ncol(ar)]
        
        ranking = rank(colSums(m))
        
        for (i in 1:ncol(m)) {
          if (ranking[i] == 1) {
            m3 = m[i]
          } 
          if (ranking[i] == 2) {
            m1 = m[i]
            a1 = head(as1[i],1)
          }
          if (ranking[i] == 3) {
            m2 = m[i]
          }
        }
        
        # variables from the auto finder need to be unlisted (to a vector) to proceed
        m1 = unlist(m1)
        m2 = unlist(m2)
        m3 = unlist(m3)
        a1 = unlist(a1)
        t = unlist(t)
        
        ##manual selection of ROI  
        
        if(!(is.null(input$ROI)) & !(is.null(d()))){
          if(input$ROI == 2 & input$action){
            input$action
            isolate({
              m1 = unlist(d[,input$bleachROIin])
              m2 = unlist(d[,input$controlROIin])
              m3 = unlist(d[,input$bgROI])
              a1 = unlist(d[,input$areaROI])
            })
          }}
        
        bleach=0
        
        if(!(is.null(alternate))) {
          rm(alternate)
        }
        ## end
        ## automatic bleach timepoint finder
        if(!(is.null(m1))){
          m1b = c((tail(m1,(length(m1)-1))),tail(m1,1))
          bt = m1-m1b
          if(max(m1-m1b) < (max(m2)*0.4)){
            alternate = 1
            bleach=0
          } else {
            bleach = which(max(m1-m1b)==bt) 
          }
        }
        
        ## end 
        ## pre-Bleach slider to bleach
        if(!(is.null(input$Btime))){ 
          if(input$Btime == 1 & input$action) {
            if(input$action > 0){
              isolate({
                input$action
                bleach <- as.numeric(input$blti)
              }) 
            }
          }
        }
      
        
        ## end
        if(!(is.null(d())) & input$normal == 2) {
          m2 = predict(smooth.spline(m2, spar = input$NormalizationSlider))$y
          m3 = predict(smooth.spline(m3, spar = input$NormalizationSlider))$y
        }

        #Subtract Background Noise
        if(input$noBG == TRUE){
        mx1 = m1-m3
        mx2 = m2-m3
        } else {
          mx1 = m1
          mx2 = m2
        }
        
        #correlation factor
        #all values devided by first post bleach integer of CTRL
        
        cfm2=mx2/mx2[bleach+1]
        
        if(!(is.null(input$Btime))){
          if(input$Btime == 3 | !(is.null(alternate)) & input$action) {
            if(input$action > 0){
              isolate({
                input$action
                cfm2 = mx2/mx2[1]
              }) 
            }
          } 
        }
        
        #correct for correlation factor
        mxx1=mx1/cfm2
        mxx2=mx2/cfm2
        
        mxxp1 = mxx1/median(mxx1[1:bleach])
        #Normalize  n= 6 to 100% of flourescence in region
        #n-> first afterbleach image
        if(!(is.null(input$Btime))){
          if(input$Btime == 3 & input$action) {
            if(input$action > 0){
              isolate({
                input$action
                mxxp1 = mxx1/(as.numeric(input$numInp))
              }) 
            }
          }
        }
        
        if(!(is.null(alternate))) {
          mxxp1 = mxx1/median(mxx2[1:(bleach)])
        }
        
        if(!(is.null(input$Btime))){ 
          if(!(is.null(alternate)) & input$action) {
            input$action
            mxxp1 = mxx1/median(mxx2)
          }
        } 
        
       
    
     
        
        #FRAP formula: http://www.embl.de/eamnet/downloads/courses/FRAP2005/tzimmermann_frap.pdf
        # the Formeula is: f(t) = A(1-exp(-tau*t)) where t1,2 = ln0.5/-tau
        
        tm = tail(t,length(t)-bleach)
        mxxp1m = tail(mxxp1,length(mxxp1)-bleach)
        # c is minimum observed value of all measurements - needed for fitting
        c = min(mxxp1m)
        #b is the range of values (from 100%-x%) - also called data span of FRAP values
        b = median(tail(mxxp1m,20)) - c
        
        b = b[1]
        c = c[1]
        
        #command line output for tests
        #cat(c("start",min(mxxp1m),b,c,"end"), file=stderr())
        #cat(c("P",length(tm),"t12",length(mxxp1m),length(b),length(c),"/n","PS"), file=stderr())
        
        #make data.frame for further analysis with post bleach values
        mxf = data.frame(tm,mxxp1m,b,c)
        
        #levenberg marquard algorithm
        if(input$fit == 3 & input$action){
            formulaInput = input$customFormula
            formulaChoice = as.formula(paste("mxxp1m ~",formulaInput))
        if(length(grep("x2",formulaInput))>=1){
            fit = nlsLM(formulaChoice, data = mxf, start =list(x=0.01,x2=0.01)) 
        } else {
            fit = nlsLM(formulaChoice, data = mxf, start =list(x=0.05)) 
          }
          
        } else if(input$fit == 2){
          fit = nlsLM(mxxp1m ~ bs*(1-exp(-x*tm))+bs2*(1-exp(-x2*tm))+c, data = mxf, start =list(bs=0.5,bs2 = 1,x=0.05,x2=0.1))
          
        } else {
          formulaChoice = as.formula("mxxp1m ~ b*(1-exp(-x*tm))+c")
          fit = nlsLM(formulaChoice, data = mxf, start =list(x=0.001))
          
        }
        
        

        if(!is.null(summary(fit)$sigma)){
          r = summary(fit)$sigma
          rif=1-summary(fit)$sigma
        }
        
        #adjusting for the input time - was set behind the fitting to reduce singularities
        timeCoeff = input$t*0.001
        tm = tm*timeCoeff
        
        m1m = tail(m1,length(m1)-bleach)
        m2m = tail(m2,length(m2)-bleach)
        m3m = tail(m3,length(m3)-bleach)
       
        #show fit
        fitpre = predict(fit)
        fitpre = tail(fitpre,length(fitpre))
        #fitpre = tail(fitpre,length(fitpre)-bleach)
        
        
        diff = mxxp1m-fitpre

        
        #extract the fitted tau
        #D calculation with D = 0.25*r^2 / t12
        #http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3731631/
        if(!(is.na(as.vector(coef(fit)[2])))){
          tau = ((as.vector(coef(fit))[1])/(timeCoeff))
          tau2 = ((as.vector(coef(fit))[2])/(timeCoeff))
          t12 = ((log(0.5))/(-tau))
          t122 = ((log(0.5))/(-tau2))
          
          D = 0.25*a1*1.94/t12
          D2 = 0.25*a1*1.94/t122
          
          } else {
          tau = ((as.vector(coef(fit))[1])/(timeCoeff))
          t12 = ((log(0.5))/(-tau))
          D = 0.25*a1*1.94/t12
          
          tau2 = FALSE
          t122 = FALSE
          D2 = FALSE
          }

        a1 = a1[1]
        D = D[1]
        D2 = D2[1]
        
        #command line output for tests
        #cat(c(length(t12),"t12",length(rif),length(m1m),length(m2m),length(m3m),length(diff),length(fitpre),length(tm),length(mxxp1m),length(tau),length(m1m),length(m2m),length(m3m),length(a1),length(D),length(bleach),length(tau2),length(tau2),length(t122),length(D2)), file=stderr())
        
        output = data.frame(t12,rif,m1m,m2m,m3m,diff,fitpre,tm,mxxp1m,tau,a1,D,bleach,tau2,t122,D2)
        
        colnames(output) = c("half time","quality",names(m1[1]),names(m2[1]),names(m3[1]),"diff","fitpre","tm","mxxp1m","tau",names(a1[1]),"diffusion","bleach","tau2","half time 2","diffusion 2")
        
        return(output)
      }
      
      ################## ScatterPlot <- function(Data,FileChoice){####################################
      tScatterVector = c()
      tHalf = c()
      fitQuality = c()
      
      if(!is.null(d())){
        for(i in 1:(length(sctn)-1)){
          matrixH=ScatterPlot(d(),i)
          
          tHalf=c(c(tHalf),matrixH[1,1])
          fitQuality=c(c(fitQuality),matrixH[2,1])
          
          if(choice == i){
            t12=matrixH[1,1]
            t122=matrixH[1,15]
            
            rif2=matrixH[1,2]
            
            #unprocessed ROI means
            m1m=matrixH[,3]
            m2m=matrixH[,4]
            m3m=matrixH[,5]
            
            #names of areas
            m1=matrixH[3]
            m2=matrixH[4]
            m3=matrixH[5]
            a1=matrixH[11]
            
            
            diff=matrixH[,6]
            fitpre=matrixH[,7]
            tm=matrixH[,8]
            mxxp1m=matrixH[,9]
            
            tau=matrixH[1,10]
            tau2=matrixH[1,14]
            
            
            D=matrixH[1,12]
            D2=matrixH[1,16]
            
            bleach=matrixH[1,13]
          }
        }
        
        tHalf=tHalf[fitQuality>input$quality]
        tScatterVector=tHalf
        #sNumber=seq(1,1,length.out=length(tScatterVector))
        sNumber=rep("all fittings",length(tScatterVector))
        
      }
      
      ######################################################
      #adding data together for ggplot2
      
      p1 = data.frame(mxxp1m,tm,fitpre,diff,m1m,m2m,m3m)
      
      if(length(sctn)>3){
      p2 = data.frame(tScatterVector,sNumber,t12)
      #p3 = data.frame()
      }
      
      ######################################################
      #data explorer
      
      
      ## graph plot code - if fitting quality is bad, no fitting difference plot is shown (sigma value)
      
      if(rif2>=input$quality){
        adv <- ggplot(p1,aes(y=mxxp1m,x=tm))+geom_point(aes(y=mxxp1m,x=tm),color="black",size=0.3)+geom_abline(intercept=0,slope=0,size=0.8,alpha=0.7,linetype=3)
        adv <- adv+geom_abline(intercept=1,slope=0,alpha=0.7,size=0.8,linetype=3)+geom_point()+geom_path(alpha=0.3)
        adv <- adv+geom_line(x = tm, y = fitpre,alpha=0.7, color="steelblue3",linetype=1,size=1.2)+expand_limits(y= c(0,1.1))
        adv <- adv+theme(aspect.ratio=0.35)+xlab("time in sec")+ylab("relative signal")
        
        bdv <- ggplot(p1,aes(y=diff,x=tm))+geom_abline(intercept=0,slope=0,size=0.5,alpha=0.5)
        bdv <- bdv+geom_path(alpha=0.2,size = 0.1)
        bdv <- bdv+geom_point(size=0.5)+theme(aspect.ratio=0.35)+xlab("time in sec")+ylab("fitting difference")+stat_smooth(level = 0,method=loess,alpha=0.7, color="steelblue3",linetype=1,size=0.9)
        
        cdv <- ggplot(data = p1,aes(y=m1m,x=tm))+geom_point(aes(y=m1m,x=tm),color="green",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m2m,x=tm),color="blue",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m3m,x=tm),color="black",size=0.8)+theme(aspect.ratio=0.35) 
        cdv <- cdv+xlab("time in sec")+ylab("signal")
        
        if(length(sctn)>3){
          edv <- ggplot(data=p2,aes(y=tScatterVector,x=sNumber))+geom_boxplot()+geom_point(size=3,alpha=0.6)+theme(aspect.ratio=0.35)
          edv <- edv + geom_point(data=p2, aes(y=t12, x=sNumber), color ="red") + xlab("Scatter Plot") + ylab("t/12 in sec")
          multiplot(cdv,adv,edv,bdv,cols=2)
        } else {
          multiplot(cdv,adv,bdv,cols=2)
        }
      }
      
      
      if(rif2<input$quality) {
        adv <- ggplot(p1,aes(y=mxxp1m,x=tm))+geom_point(aes(y=mxxp1m,x=tm),color="black",size=0.3)+geom_abline(intercept=0,slope=0,size=0.8,alpha=0.7,linetype=3)
        adv <- adv+geom_abline(intercept=1,slope=0,alpha=0.7,size=0.8,linetype=3)+geom_point()+geom_path(alpha=0.3)
        adv <- adv+expand_limits(y= c(0,1.1))
        adv <- adv+theme(aspect.ratio=0.35)+xlab("time in sec")+ylab("relative signal")
        
        bdv <- ggplot(p1,aes(y=diff,x=tm))+geom_abline(intercept=0,slope=0,size=0.5,alpha=0.5)
        bdv <- bdv+geom_point(size=0.5)
        bdv <- bdv+theme(aspect.ratio=0.35)+xlab("time in sec")+ylab("fitting difference")+stat_smooth(level = 0,method=loess,alpha=0.7, color="steelblue3",linetype=1,size=0.9)+geom_path(alpha=0.3,size=0.1)
        
        cdv <- ggplot(data = p1,aes(y=m1m,x=tm))+geom_point(aes(y=m1m,x=tm),color="green",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m2m,x=tm),color="blue",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m3m,x=tm),color="black",size=0.8)+theme(aspect.ratio=0.35) 
        cdv <- cdv+xlab("time in sec")+ylab("signal")
        
        if(length(sctn)>3){
        edv <- ggplot(data=p2,aes(y=tScatterVector,x=sNumber))+geom_boxplot()+geom_point(size=3,alpha=0.6)+theme(aspect.ratio=0.35)
        edv <- edv + geom_point(data=p2, aes(y=t12, x=sNumber), color ="red") + xlab("Scatter Plot") + ylab("t/12 in sec")
        multiplot(cdv,adv,edv,cols=2)
        } else {
          multiplot(cdv,adv,cols=2)
        }
      }
      
      
      ################Result display##############################
      
      output$resultOutput <- renderUI({
        if(is.null(d()) | is.null(input$mainPanel)){return()}
        if(input$mainPanel == "Dataset"){return()}
        if(input$ROI == 2) {return ()}
        is <- fluidPage( 
          br(),
          h5("Bleach ROI :",names(m1[1])),
          h5("Total ROI :",names(m2[1])),
          h5("BG ROI :",names(m3[1])),
          h5("Bleach Area :",names(a1[1]))
        )
      })
      
      output$resultOutput3 <- renderUI({
        if(is.null(d()) | is.null(input$mainPanel)){return()}
        if(input$mainPanel == "Dataset"){return()}
        if(input$ROI == 2) {return ()}
        is <- fluidPage( 
          br(),
          if(!(is.null(input$quality))){
            if(rif2>=input$quality){
              h5("Fitting quality: Accepted!")
            } else {
              code("fitting quality: Not accepted!")
            }
          },
          br(),
          if(!(is.null(input$Btime))){ 
            if(input$Btime == 3 & input$action) {
              if(input$action > 0){
                isolate({
                  input$action
                  h5("Pre-bleach value:",input$numInp)
                }) 
              }
            } else {
              if(is.null(bleach)){
                h5("Pre-bleach measurements:",bleach)
              } else {
                code("No pre-bleach values!")
                
              }
            }
          },
          br(),
          
          if(((median(m1m))/(median(m3m))) <= 2){
              code("The Background/Bleach ratio is high!")
          }
          
        )
      })
      
      output$spacingLine <- renderUI({
        if(is.null(d()) | is.null(input$mainPanel)){return()}
        if(input$mainPanel == "Dataset"){return()}
        is <- fluidPage(
          br(),
          h6("'"),
          tags$hr()
        )
      })
      
      output$spacingLine2 <- renderUI({
        if(is.null(d()) | is.null(input$mainPanel)){return()}
        if(input$mainPanel == "Dataset"){return()}
        is <- fluidPage(
          br(),
          tags$hr()
        )
      })
      
      output$resultOutput2 <- renderUI({
        if(is.null(d()) | is.null(input$mainPanel)){return()}
        if(input$mainPanel == "Dataset"){return()}
        if(t122 == FALSE){
        is <- fluidPage(
  
          tags$div( class = "test1",
          br(),
          h5("Tau: ",round(tau,4)," seconds^-1"),
          h5("Apparent D:",round(head(D,1),4), "µmeter² per second"),
          h5("T1/2: ",round(t12,4)," seconds"),
          h5("Standart Error:",round(rif2,4))
          )
        )
        } else {
          is <- fluidPage(
          br(),
          h5("Tau: ",round(tau,4)," seconds^-1"),
          h5("Tau2: ",round(tau2,4)," seconds^-1"),
          h5("Apparent D:",round(head(D,1),4), "µmeter² per second"),
          h5("Apparent D2:",round(head(D2,1),4), "µmeter² per second"),
          h5("T1/2: ",round(t12,4)," seconds"),
          h5("T1/2 2: ",round(t122,4)," seconds"),
          h5("Standart Error:",round((rif2),4))
          )}
        
      })
    }
    )
  }
)