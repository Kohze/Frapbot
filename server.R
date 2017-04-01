#frapbot 1.9
# server.R
library(ggplot2)
library(shiny)
library(minpack.lm)
library(Cairo)
library(data.table)
library(grid)
library(DT)
library(dtplyr)
library(dplyr)

options(shiny.usecairo=T)
options(shiny.autoreload.interval = 200)

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
        tabPanel("Frapbot 1.9",plotOutput("main2")),
        tabPanel("")
        ,id = "mainPanel")
    })
    
    output$update1 <- renderUI ({
      if(is.null(d())){return()}
      splitLayout(
        downloadButton("downloadData", label="Download")
      )
    })
    
    output$downloadData <- downloadHandler(
      filename = 'FrapBot.zip',
      content = function(fname) {
        fs <- c("Fitted_Curves.csv", "Half_Time.csv", "sumOfRows.csv")
        write.csv(allFitsTable, file = "Fitted_Curves.csv", sep =",")
        write.csv(halfTimeGlobal, file = "Half_Time.csv", sep =",")
        write.csv(sumOfRows, file = "sumOfRows.csv", sep =",")
        
        zip(zipfile=fname, files=fs)
        if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
      },
      contentType = "application/zip"
    )
    
    output$qualitySlider <- renderUI ({
      if(is.null(d())){return()}
      sliderInput("quality","Standart Error Slider (quality filter)",0.0,1.0,0.9)
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
      sc = findCountCol(d)
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
        sliderInput("t","Scan time in ms", 1,1000,79)
    })
    
    output$contents <- DT::renderDataTable({
      if (is.null(d())  | is.null(input$mainPanel)){return()}
      if(input$mainPanel == "Frap Plot" & !(input$mainPanel == "Dataset")){return()}
      DT::datatable(d(), options = list(pageLength = 20))
    })
    
    #UI for manual ROI
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
    
    #main code - preprocessing
    extractCols <- function(d){
      
      extractTimeCol = function(d){
        if (d[1]+2 == d[3] && d[4]+2 == d[6] && d[6]+2 == d[8]) {
          return(TRUE)
        } else { 
          return(FALSE)
        }
      }
    
      extractRegion = function(d){
        if (!(head(d,1) == tail(d,1)) & !head(d,1)%%1 == 0) {
          return(TRUE)
        } else { 
          return(FALSE)
        }
      }
      
      timeCol = lapply(d, extractTimeCol)
      regionCol = lapply(d, extractRegion)
      
      t = d[which(timeCol == TRUE)]
      region = d[which(regionCol == TRUE)]
      
      return(data.frame(t,region))
    }
    
    slopeInc <- function(x){
      return((max(tail(x,length(x)/2))/min(head(x,length(x)/2)))*100)
    }
    
    absRange <- function(x){
      return(max(x)-min(x))
    }
    
    #the main pattern recognition
    assignCols <- function(x, withBG = TRUE){
      analysisFrame = data.table(
        "name" = names(x), 
        "sum" = colSums(x), 
        "range" = as.vector(as.vector(sapply(x,absRange))),
        "slope" = as.vector(as.vector(sapply(x,slopeInc)))
        )
    
      firstSort = head(analysisFrame[order(-sum)],2)
      firstSort[,mult := range*(slope^2)*sum]
      firstSort = firstSort[order(-mult)]
      
      ControlROIname = firstSort[2]$name
      BleachROIname = firstSort[1]$name
      
      ControlROI = x[[ControlROIname]]
      BleachROI = x[[BleachROIname]]
      
      if(withBG == TRUE){
         BGname = tail(analysisFrame[order(-sum)]$name,1)
         BGROI = x[[BGname]]
      } else {
        BGROI = FALSE
      }
      
      output = list("ConROI" = ControlROI, "BleROI" = BleachROI , "BgROI" = BGROI, "AreaCOL" = BleachROIname)
      return(output)
    } 
    
    findCountCol <- function(d){
      if(length(d) > 4) {
      for (i in 1:(ncol(d))) {
        if (d[1,i]+2 == d[3,i] && d[4,i]+2 == d[6,i] && d[6,i]+2 == d[8,i]) {
          sc = d[i]
        }
      }
      } else {
        sc = d[1]
      }  
      return(sc)
    }
    
    findBleach <- function(m1){
      m1b = c((tail(m1,(length(m1)-1))),tail(m1,1))
      bt = m1-m1b
      if(max(bt) < (max(m1)*0.4)){
        alternate = 1
        bleach = 1
      } else {
        alternate = NULL
        bleach = which(max(m1-m1b) == bt)+1 
      }
      return(list("bleach" = bleach, "alternate" = alternate))
    }
    
    fittingFunction <- function(mxf){
      if(input$fit == 3 & input$action){
        formulaInput = input$customFormula
        formulaChoice = as.formula(paste("mxxp1m ~",formulaInput))
        if(length(grep("x2",formulaInput))>=1){
          fit = nlsLM(formulaChoice, data = mxf, start =list(x=0.01,x2=0.01)) 
        } else {
          fit = nlsLM(formulaChoice, data = mxf, start =list(x=0.05)) 
        }
      } else if(input$fit == 2){
        fit = nlsLM(mxxp1m ~ -(bs1*exp(-x*tm))-((bs2)*exp(-x2*tm))+c1, data = mxf, start = list(bs1 = 1, bs2 = 0.1, x = 0.1, x2 = 4, c1 = 0.2), lower = c(0,0,0,0,0), control = list(maxiter = 1024))
      } else {
        formulaChoice = as.formula("mxxp1m ~ b*(1-exp(-x*tm))+c")
        fit = nlsLM(formulaChoice, data = mxf, start =list(x=0.001))
      }
      return(fit)
    }
    
    #function to take care of traces with different length
    cbind.fill <- function(...){
      nm <- list(...) 
      nm <- lapply(nm, as.matrix)
      n <- max(sapply(nm, nrow)) 
      do.call(cbind, lapply(nm, function (x) 
        rbind(x, matrix(, n-nrow(x), ncol(x))))) 
    }
    
    merged_list <- function(x){
      x[,1] = 0
      x[,2] = 0
      output = rowSums(x)
      return(output)
    }
    
    output$main2 <- renderPlot({
      d = d()
      sc = findCountCol(d)
      
      if(!(is.null(input$SlChoice))){
        choice = input$SlChoice
      } else {
        choice = 1
      }
      
      if(!(is.null(sc) & !(is.null(input$SlChoice)))){
        sc1b = rbind(tail(sc,nrow(sc)-1),tail(sc,1))
        sct = sc-sc1b
        sctn = c(0,c(which(sct>2)),nrow(sc))
      }
      
      #main function
      ScatterPlot <- function(Data,FileChoice){
        
        d = Data
        if(length(d) == 4) {
          sc = d[1]
        } else {
          sc = findCountCol(d)
        }
        alternate = NULL
        
        if(!(is.null(sc) & !(is.null(input$SlChoice)))){
          sc1b = rbind(tail(sc,nrow(sc)-1),tail(sc,1))
          sct = sc-sc1b
          sctn = c(0,c(which(sct>2)),nrow(sc))
          if(length(sctn)>2) {
            d = d[(sctn[FileChoice]+1):(sctn[FileChoice+1]),]
          }
        }
        
        #convert datasets into variable
        #Mean1 = Bleach Region; Mean2 = Control Region; Mean3 = Background; Area1 = Bleach Area
        
        ### the automatic row to area/signal finding sub-routine
        ms = matrix(nrow = nrow(d))
        
        if(ncol(d) > 4) {
          for (i in 1:ncol(d)) {
            if (d[1,i]+2 == d[3,i] && d[4,i]+2 == d[6,i] && d[6,i]+2 == d[8,i]) {
              ## input scan time to t algorithm
              t = d[i]
            }
            if(!(head(d[i],1) == tail(d[i],1)) && !head(d[i],1)%%1 == 0) {
              ms = ifelse(exists("ms"), cbind(ms,d[i]), matrix(nrow = nrow(d)))
              #ms = cbind(ms,d[i])
            }
          }
        } else {
          t = d[1]
        }  

        #deleting the NA in the matrix colunm 1
        if(ncol(d) > 4) {
          m = extractCols(d)
        } else {
          m = d
        }
        
        colList = assignCols(m[2:4])
       
        if(ncol(d) > 4){
          namesOfInp = names(d)
          nameMean1 = colList$AreaCOL
          columnFinder = which(nameMean1 == namesOfInp)
          areaName = namesOfInp[columnFinder-1]
          a1 = d[[areaName]]
          has4col = FALSE
        } else {
          a1 = 1
          has4col = TRUE
        }
        t = m[1]
       
        m3 = colList[["BgROI"]]
        m2 = colList[["ConROI"]]
        m1 = colList[["BleROI"]]

        # variables from the auto finder need to be unlisted (to a vector) to proceed
        # manual selection of ROI  
        if(!(is.null(input$ROI)) & !(is.null(d())) & !(is.null(m2))){
          if(input$ROI == 2 & input$action){
            input$action
            isolate({
              m1 = unlist(d[,input$bleachROIin])
              m2 = unlist(d[,input$controlROIin])
              m3 = unlist(d[,input$bgROI])
              a1 = unlist(d[,input$areaROI])
              t = unlist(t)
            }) 
          } else {
            m1 = unlist(m1)
            m2 = unlist(m2)
            m3 = unlist(m3)
            a1 = unlist(a1)
            t = unlist(t)
          }
        }
        
        # automatic bleach timepoint finder
        bleachPoint = findBleach(m1)
        bleach = unlist(bleachPoint$bleach)
        alternate = unlist(bleachPoint$alternate)
        
        # pre-Bleach slider to bleach
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
      
        # end
        # The normalizing smoothing function
        if(!(is.null(d())) & input$normal == 2) {
          m2 = predict(smooth.spline(m2, spar = input$NormalizationSlider))$y
          m3 = predict(smooth.spline(m3, spar = input$NormalizationSlider))$y
        }

        # Subtract Background Noise
        if(input$noBG == TRUE){
        mx1 = m1-m3
        mx2 = m2-m3
        } else {
          mx1 = m1
          mx2 = m2
        }
        
        #correlation factor
        #all values devided by first post bleach integer of CTRL
        cfm2 = mx2/mx2[bleach]
        
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
        
        # correct for correlation factor
        mxx1 = mx1/cfm2
        mxx2 = mx2/cfm2
        
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
        
        tm = tail(t,length(t)-(bleach-1))
        tm = tm - tm[1]
        
        mxxp1m = tail(mxxp1,length(mxxp1)-(bleach-1))
        # c is minimum observed value of all measurements - needed for fitting
        c = min(mxxp1m)
        # b is the range of values (from 100%-x%) - also called data span of FRAP values
        b = median(tail(mxxp1m,20)) - c
        
        b = b[1]
        c = c[1]
        
        # command line output for tests
        #cat(c("start",min(mxxp1m),b,c,"end"), file=stderr())
        #cat(c("P",length(tm),"t12",length(mxxp1m),length(b),length(c),"/n","PS"), file=stderr())
        
        #make data.frame for further analysis with post bleach values
        mxf = data.frame(tm,mxxp1m,b,c)
        fit = fittingFunction(mxf)
        # levenberg marquard algorithm
        
        
        if(!is.null(summary(fit)$sigma)){
          r = summary(fit)$sigma
          rif=1-summary(fit)$sigma
        }
        
        # adjusting for the input time - was set behind the fitting to reduce singularities
        if(has4col){
          timeCoeff = 1
        } else {
          timeCoeff = input$t*0.001
          tm = tm*timeCoeff
        }
        
        
        m1m = tail(m1,length(m1)-(bleach-1))
        m2m = tail(m2,length(m2)-(bleach-1))
        m3m = tail(m3,length(m3)-(bleach-1))
       
        # show fit
        fitpre = predict(fit)
        fitpre = tail(fitpre,length(fitpre))
        # fitpre = tail(fitpre,length(fitpre)-bleach)
        
        diff = mxxp1m-fitpre
        
        # extract the fitted tau
        # D calculation with D = 0.25*r^2 / t12
        # http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3731631/
        if(!(is.na(as.vector(coef(fit)[2])))){
          tau = ((as.vector(coef(fit))[1])/(timeCoeff))
          tau2 = ((as.vector(coef(fit))[2])/(timeCoeff))
          t12 = ((log(0.5))/(-tau))
          t122 = ((log(0.5))/(-tau2))
          
          D = 0.25*a1/(t12)
          D2 = 0.25*a1/(t122)
          
        } else {
          tau = ((as.vector(coef(fit))[1])/(timeCoeff))
          t12 = ((log(0.5))/(-tau))
          D = 0.25*a1/t12
          
          tau2 = FALSE
          t122 = FALSE
          D2 = FALSE
        }

        a1 = a1[1]
        D = D[1]
        D2 = D2[1]
        
        output = list("half time" = t12,"quality" = rif,"m1m" = m1m, "m2m" = m2m,"m3m" = m3m,"diff" = diff,"fitpre" = fitpre,
                      "tm" = tm, "mxxp1m" = mxxp1m,"tau" = tau,"area" = a1,"diffusion" = D, "diffusion2" = D2,
                      "bleach" = bleach, "tau2" = tau2, "half time2" = t122, "name-m1" = names(m1[1]), 
                      "name-m2" = names(m2[1]), "name-m3" = names(m3[1]), "names-a1" = names(a1[1]) ) 
        
        return(output)
      }
      
      tScatterVector = c()
      tHalf = c()
      fitQuality = c()
      allFits = c()

      if(!is.null(d())){
        for(i in 1:(length(sctn)-1)){
          matrixH = ScatterPlot(d(),i)
          
          tHalf = c(c(tHalf),matrixH[["half time"]])
          fitQuality = c(c(fitQuality),matrixH[["quality"]])
          
          if(is.null(allFits)){
            allFits = cbind(matrixH[["tm"]],matrixH[["fitpre"]])
          } else {
            #allFits = cbind(allFits,matrixH[["fitpre"]])
            allFits = cbind.fill(allFits,matrixH[["fitpre"]])
          }
          
      tHalf = tHalf
      tScatterVector = tHalf
      sNumber = rep("all fittings",length(tScatterVector))
          if(choice == i){
            t12 = matrixH[["half time"]]
            t122 = matrixH[["half time2"]]
            rif2 = matrixH[["quality"]]
            
            #unprocessed ROI means
            m1m = matrixH[["m1m"]]
            m2m = matrixH[["m2m"]]
            m3m = matrixH[["m3m"]]
            
            #names of areas
            m1Name = matrixH[["name-m1"]]
            m2Name = matrixH[["name-m2"]]
            m3Name = matrixH[["name-m3"]]
            a1Name = matrixH[["names-a1"]]
            
            diff = matrixH[["diff"]]
            fitpre = matrixH[["fitpre"]]
            tm = matrixH[["tm"]]
            mxxp1m = matrixH[["mxxp1m"]]
            
            tau=matrixH[["tau"]]
            tau2 = matrixH[["tau2"]]
            
            D = matrixH[["diffusion"]]
            D2 = matrixH[["diffusion2"]]
            bleach = matrixH[["bleach"]]
          }
        }
        
        sumOfRows <<- merged_list(allFits)
        halfTimeGlobal <<- tHalf
        allFitsTable <<- allFits 
        
        tScatterVector = tHalf
        sNumber = rep("all fittings",length(tScatterVector))
      }

      # adding data together for ggplot2
      
      p1 = data.frame(mxxp1m,tm,fitpre,diff,m1m,m2m,m3m)
      
      if(t122 == FALSE) {
        p2 = data.frame(tScatterVector,sNumber,t12)
      } else {
        p2_1 = data.frame(tScatterVector,sNumber,"t12" = t12, "group" = "t1/2 1")
        p2_2 = data.frame(tScatterVector,sNumber,"t12" = t122, "group" = "t1/2 2")
        p2 = rbind(p2_1,p2_2)
      }
      
      if(rif2 >= input$quality){
        adv <- ggplot(p1,aes(y=mxxp1m,x=tm)) +geom_point(aes(y=mxxp1m,x=tm),color="black",size=0.3)+geom_abline(intercept=0,slope=0, size=0.8, alpha=0.7,linetype=3)
        adv <- adv+geom_abline(intercept=1,slope=0,alpha=0.7,size=0.8,linetype=3)+geom_point()+geom_path(alpha=0.3)
        adv <- adv+geom_path(x = tm, y = fitpre,alpha=0.7, color="steelblue3",linetype=1,size=1.2)+expand_limits(y= c(0,1.1))
        adv <- adv+theme(aspect.ratio=0.35)+xlab("time in sec")+ylab("relative signal")
        
        bdv <- ggplot(p1,aes(y=diff,x=tm))+geom_abline(intercept=0,slope=0,size=0.5,alpha=0.5)
        bdv <- bdv+geom_path(alpha=0.2,size = 0.1)
        bdv <- bdv+geom_point(size=0.5)+theme(aspect.ratio=0.35)+xlab("time in sec")+ylab("fitting difference")
        bdv <- bdv+stat_smooth(level = 0,method=loess,alpha=0.7, color="steelblue3",linetype=1,size=0.9)
        
        cdv <- ggplot(data = p1,aes(y=m1m,x=tm))+geom_point(aes(y=m1m,x=tm),color="green",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m2m,x=tm),color="blue",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m3m,x=tm),color="black",size=0.8)+theme(aspect.ratio=0.35) 
        cdv <- cdv+xlab("time in sec")+ylab("signal")
        
        if(length(sctn)>3){
          if(!(t122 == FALSE)){  
            edv <- ggplot(data=p2,aes(y=tScatterVector,x=group))+geom_boxplot()+geom_point(size=3,alpha=0.6)+theme(aspect.ratio=0.35)
            edv <- edv + geom_point(data=p2, aes(y=t12, x=group), color ="red") + xlab("Scatter Plot") + ylab("t/12 in sec")
            multiplot(cdv,adv, bdv, edv,cols=2)
          } else {
            edv <- ggplot(data=p2,aes(y=tScatterVector,x=sNumber))+geom_boxplot()+geom_point(size=3,alpha=0.6)+theme(aspect.ratio=0.35)
            edv <- edv + geom_point(data=p2, aes(y=t12, x=sNumber), color ="red") + xlab("Scatter Plot") + ylab("t/12 in sec")
            multiplot(cdv,adv,bdv,edv,cols=2)
          }
        } else {
          multiplot(cdv,adv,bdv,cols=2)
        }
      }
      
      if(rif2 < input$quality) {
        adv <- ggplot(p1,aes(y=mxxp1m,x=tm))+geom_point(aes(y=mxxp1m,x=tm),color="black",size=0.3)+geom_abline(intercept=0,slope=0,size=0.8,alpha=0.7,linetype=3)
        adv <- adv+geom_abline(intercept=1,slope=0,alpha=0.7,size=0.8,linetype=3)+geom_point()+geom_path(alpha=0.3)
        adv <- adv+expand_limits(y= c(0,1.1))
        adv <- adv+theme(aspect.ratio=0.35)+xlab("time in sec")+ylab("relative signal")
        
        bdv <- ggplot(p1,aes(y=diff,x=tm))+geom_abline(intercept=0,slope=0,size=0.5,alpha=0.5)
        bdv <- bdv+geom_point(size=0.5)
        bdv <- bdv+theme(aspect.ratio=0.35)+xlab("time in sec")+ylab("fitting difference")
        bdv <- bdv+stat_smooth(level = 0,method=loess,alpha=0.7, color="steelblue3",linetype=1,size=0.9)+geom_path(alpha=0.3,size=0.1)
        
        cdv <- ggplot(data = p1,aes(y=m1m,x=tm))+geom_point(aes(y=m1m,x=tm),color="green",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m2m,x=tm),color="blue",size=0.8) 
        cdv <- cdv+geom_point(aes(y=m3m,x=tm),color="black",size=0.8)+theme(aspect.ratio=0.35) 
        cdv <- cdv+xlab("time in sec")+ylab("signal")
        
        if(length(sctn)>3){
          if(t122 == TRUE){  
            edv <- ggplot(data=p2,aes(y=tScatterVector,x=group))+geom_boxplot()+geom_point(size=3,alpha=0.6)+theme(aspect.ratio=0.35)
            edv <- edv + geom_point(data=p2, aes(y=t12, x=group), color ="red") + xlab("Scatter Plot") + ylab("t/12 in sec")
            multiplot(cdv,adv,edv,cols=2)
          } else {
            edv <- ggplot(data=p2,aes(y=tScatterVector,x=sNumber))+geom_boxplot()+geom_point(size=3,alpha=0.6)+theme(aspect.ratio=0.35)
            edv <- edv + geom_point(data=p2, aes(y=t12, x=sNumber), color ="red") + xlab("Scatter Plot") + ylab("t/12 in sec")
            multiplot(cdv,adv,edv,cols=2)
          }
        } else {
          multiplot(cdv,adv,cols=2)
        }
      }
      
      #Result display
      
      output$resultOutput <- renderUI({
        if(is.null(d()) | is.null(input$mainPanel)){return()}
        if(input$mainPanel == "Dataset"){return()}
        if(input$ROI == 2) {return ()}
        is <- fluidPage( 
          br(),
          h5("Bleach ROI :", m1Name),
          h5("Total ROI :", m2Name),
          h5("BG ROI :", m3Name),
          h5("Bleach Area :", a1Name)
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