#frapbot alpha 3
# server.R
library(ggplot2)
library(shiny)
library(minpack.lm)
library(dplyr)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
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
            read.table(.$datapath, header=TRUE, sep=",")
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
      sliderInput("blti","",1,a,5,1)
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
      selectInput("fit","",c("Exponential Fitting"=1,"Double Exponential Fitting"=2))
    })
    
    output$norm <- renderUI({
      if(is.null(d())){return()}
      selectInput("normal","",c("Full & Direct Normalization"=1,"averaged Normalization"=2))
    })
    
    output$scanTime <- renderUI({
      if(is.null(d())){return()}
        sliderInput("t","Scan Time in ms", 1,1000,79)
    })
    
    output$UIslider <- renderUI({
      if (is.null(d())  | is.null(input$mainPanel)){return() }
      if(input$mainPanel == "Frap Plot"){return()}
      a = nrow(d())
      sliderInput("slider","",1,a,25)
    })
    
    output$contents <- renderTable({
      if (is.null(input$slider)){return()}
      if (is.null(d())  | is.null(input$mainPanel)){return()}
      if(input$mainPanel == "Frap Plot" & !(input$mainPanel == "Dataset")){return()}
      head(d(),input$slider)
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
      selectInput("controlROIin","Control ROI",c(colnames),selectize=FALSE)
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
            t = d[i]*input$t*0.001
           
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
            a = head(as1[i],1)
          }
          if (ranking[i] == 3) {
            m2 = m[i]
          }
        }
        
        # variables from the auto finder need to be unlisted (to a vector) to proceed
        m1 = unlist(m1)
        m2 = unlist(m2)
        m3 = unlist(m3)
        a = unlist(a)
        t = unlist(t)
        
        ##manual selection of ROI  
        
        if(!(is.null(input$ROI)) & !(is.null(d()))){
          if(input$ROI == 2 & input$action){
            input$action
            isolate({
              m1 = unlist(d[,input$bleachROIin])
              m2 = unlist(d[,input$controlROIin])
              m3 = unlist(d[,input$bgROI])
              a = unlist(d[,input$areaROI])
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
          if(max(m1-m1b) < 20){
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
        
            
        
        
        ## end
        a1 = a
        
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
        
        # c is minimum observed value of all measurements - needed for fitting
        c = min(mxxp1)
      
        #FRAP formula: http://www.embl.de/eamnet/downloads/courses/FRAP2005/tzimmermann_frap.pdf
        # the Formeula is: f(t) = A(1-exp(-tau*t)) where t1,2 = ln0.5/-tau
        #b is the range of values (from 100%-x%) - also called data span of FRAP values
        #b = median(mxxp1[0:5]) - median(mxxp1[7:15])
        b = median(tail(mxxp1,20))-median(mxxp1[1:4])
        
        if(!(is.null(input$Btime))){ 
          if(input$Btime == 3 | !(is.null(alternate)) & input$action) {
            if(input$action > 0){
              isolate({
                input$action
                b = median(tail(mxxp1,20))-median(mxxp1[1:4])
              }) 
            }
          } else {
            b = median(tail(mxxp1,20))-median(mxxp1[(bleach+1):(bleach+4)])
          }
        }
        
        tm = t
        mxxp1m = mxxp1 
        #select only post bleach values
        if(!(is.null(input$Btime))){ 
          if(input$Btime == 3 | !(is.null(alternate)) & input$action) {
            if(input$action > 0){
              isolate({
                input$action
                tm = t
                mxxp1m = mxxp1          
              }) 
            }
          } else {
            tm = tail(t,length(t)-bleach)
            mxxp1m = tail(mxxp1,length(mxxp1)-bleach)
          }
        }
        
        #make data.frame for further analysis with post bleach values
        mxf = data.frame(tm,mxxp1m,b,c)
        
        #levenberg marquard algorithm
        fit = nlsLM(mxxp1~ b-b*exp(-x*t)+c, data = mxf, start =list(x=0.01))
        
        if(!is.null(summary(fit)$sigma)){
          r = summary(fit)$sigma
          rif=1-summary(fit)$sigma
        }
        
        #show fit
        m1m = tail(m1,length(m1)-bleach)
        m2m = tail(m2,length(m2)-bleach)
        m3m = tail(m3,length(m3)-bleach)
        
        fitpre = predict(fit)
        diff = mxxp1m-fitpre
        mad1 = mad(abs(diff))
        mad2 = mad(diff)
        
        #extract the fitted tau
        tau = min(coef(fit)) 
        
        #calculate t1/2
        t12 = (log(0.5))/(-tau)
        
        #D calculation with D = 0.25*r^2 / t12
        #http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3731631/
        #1.94 for 110nm resolution - function coming soon
        D = 0.25*a1*1.94/t12
        
        output = data.frame(t12,rif,m1m,m2m,m3m,diff,fitpre,tm,mxxp1m,tau,m1,m2,m3,a,D,bleach,mad1,mad2)
        colnames(output) = c("half time","quality","m1m","m2m","m3m","diff","fitpre","tm","mxxp1m","tau",names(m1[1]),names(m2[1]),names(m3[1]),names(a[1]),"diffusion","bleach","mad1","mad2")
        
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
            rif2=matrixH[1,2]
            m1m=matrixH[,3]
            m2m=matrixH[,4]
            m3m=matrixH[,5]
            diff=matrixH[,6]
            fitpre=matrixH[,7]
            tm=matrixH[,8]
            mxxp1m=matrixH[,9]
            tau=matrixH[1,10]
            m1=matrixH[11]
            m2=matrixH[12]
            m3=matrixH[13]
            a=matrixH[14]
            D=matrixH[1,15]
            bleach=matrixH[1,16]
            mad1=matrixH[1,17]
            mad2=matrixH[1,18]
          }
        }
        
        tHalf=tHalf[fitQuality>input$quality]
        tScatterVector=tHalf
        sNumber=seq(1,1,length.out=length(tScatterVector))
        
      }
      
      ######################################################
      #adding data together for ggplot2
      
      p1 = data.frame(mxxp1m,tm,fitpre,diff,m1m,m2m,m3m)
      
      if(length(sctn)>3){
      p2 = data.frame(tScatterVector,sNumber)
      }
      
      ######################################################
      #data explorer
      
      
      ## graph plot code - if fitting quality is bad, no fitting difference plot is shown (sigma value)
      
      if(rif2>=input$quality){
        adv <- ggplot(p1,aes(y=mxxp1m,x=tm))+geom_point(aes(y=mxxp1m,x=tm),color="black",size=0.4)+geom_abline(intercept=0,slope=0,size=0.8,alpha=0.7,linetype=3)
        adv <- adv+geom_abline(intercept=1,slope=0,alpha=0.7,size=0.8,linetype=3)+geom_point()+geom_path(alpha=0.3)
        adv <- adv+geom_line(x = tm, y = fitpre,alpha=0.7, color="steelblue3",linetype=1,size=1.2)+expand_limits(y= c(0,1.1))
        adv <- adv+theme(aspect.ratio=0.4)+xlab("time in sec")+ylab("relative signal")
        
        bdv <- ggplot(p1,aes(y=diff,x=tm))+geom_abline(intercept=0,slope=0,size=0.8,alpha=0.5)
        bdv <- bdv+geom_path(alpha=0.2)+stat_smooth(level = 0,method=loess,alpha=0.7, color="steelblue3",linetype=1,size=1.2)
        bdv <- bdv+geom_point()+theme(aspect.ratio=0.4)+xlab("time in sec")+ylab("fitting difference")
        
        cdv <- ggplot(data = p1,aes(y=m1m,x=tm))+geom_point(aes(y=m1m,x=tm),color="green",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m2m,x=tm),color="blue",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m3m,x=tm),color="black",size=0.8)+theme(aspect.ratio=0.4) 
        cdv <- cdv+xlab("time in sec")+ylab("signal")
        
        if(length(sctn)>3){
          edv <- ggplot(data=p2,aes(y=tScatterVector,x=sNumber))+geom_boxplot()+geom_point(size=3,alpha=0.6)+theme(aspect.ratio=0.4)
          multiplot(cdv,adv,edv,bdv,cols=2)
        } else {
          multiplot(cdv,adv,bdv,cols=2)
        }
      }
      
      
      if(rif2<input$quality) {
        adv <- ggplot(p1,aes(y=mxxp1m,x=tm))+geom_point(aes(y=mxxp1m,x=tm),color="black",size=0.4)+geom_abline(intercept=0,slope=0,size=0.8,alpha=0.7,linetype=3)
        adv <- adv+geom_abline(intercept=1,slope=0,alpha=0.7,size=0.8,linetype=3)+geom_point()+geom_path(alpha=0.3)
        adv <- adv+expand_limits(y= c(0,1.1))
        adv <- adv+theme(aspect.ratio=0.4)+xlab("time in sec")+ylab("relative signal")
        
        bdv <- ggplot(p1,aes(y=diff,x=tm))+geom_abline(intercept=0,slope=0,size=0.8,alpha=0.5)
        bdv <- bdv+geom_point()
        bdv <- bdv+theme(aspect.ratio=0.4)+xlab("time in sec")+ylab("fitting difference")+stat_smooth(level = 0,method=loess,alpha=0.7, color="steelblue3",linetype=1,size=1.2)+geom_path(alpha=0.3)
        
        cdv <- ggplot(data = p1,aes(y=m1m,x=tm))+geom_point(aes(y=m1m,x=tm),color="green",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m2m,x=tm),color="blue",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m3m,x=tm),color="black",size=0.8)+theme(aspect.ratio=0.4) 
        cdv <- cdv+xlab("time in sec")+ylab("signal")
        
        if(length(sctn)>3){
        edv <- ggplot(data=p2,aes(y=tScatterVector,x=sNumber))+geom_boxplot()+geom_point(size=3,alpha=0.6)+theme(aspect.ratio=0.4)
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
          h6("Bleach ROI :",names(m1[1])),
          h6("Ctrl ROI :",names(m2[1])),
          h6("BG ROI :",names(m3[1])),
          h6("Bleach Area :",names(a[1]))
          
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
              code("Fitting quality: Accepted!")
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
                  h6("Pre-bleach value:",input$numInp)
                }) 
              }
            } else {
              if(is.null(bleach)){
                h6("Pre-bleach measurements:",bleach)
              } else {
                code("No pre-bleach values!")
                
              }
            }
          }
          
        )
      })
      
      output$resultOutput2 <- renderUI({
        if(is.null(d()) | is.null(input$mainPanel)){return()}
        if(input$mainPanel == "Dataset"){return()}
        is <- fluidPage( 
          br(),
          h6("Tau: ",tau," seconds^-1"),
          h6("D:",head(D,1), "µmeter² per second"),
          h6("T1/2: ",t12," seconds"),
          h6("Standart Error:",rif2),
          h6("MAD(abs): ",mad1),
          h6("MAD: ",mad2)
        )
        
      })
    }
    )
  }
)