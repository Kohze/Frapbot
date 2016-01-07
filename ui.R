library(ggplot2)
library(shiny)



shinyUI(fluidPage(
                      fluidRow(
                          column(12,
                                 fluidRow(
                                   column(3,
                              wellPanel(
                                fileInput('file','Choose CSV File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv'), multiple=TRUE),
                                uiOutput("dataSetChoice"),
                                uiOutput("qualitySlider"),
                                uiOutput("noBackground"),
                                uiOutput("norm"),
                                uiOutput("fitting"),
                                uiOutput("bleachTime"),
                                uiOutput("manualInput"),
                                uiOutput("mbleachTime"),
                                uiOutput("automaticROI"),
                                splitLayout(
                                  uiOutput("bleachROI"),
                                  uiOutput("controlROI")),
                                splitLayout(
                                  uiOutput("bgROI"),
                                  uiOutput("areaROI")),
                                uiOutput("COlumns"),
                                uiOutput("scanTime"),
                                uiOutput("Columns1"),
                                br(),
                                splitLayout(
                                  uiOutput("update1"),
                                  uiOutput("update2"))
                                )),
                              mainPanel(
                                fluidRow(
                                  column(12,
                                         uiOutput("mainOutput")
                                  ),
                                  column(3),
                                  column(3,
                                         uiOutput("resultOutput")
                                  ),
                                  column(4,
                                         uiOutput("resultOutput2")
                                  ),
                                  uiOutput("UIslider"),
                                  tableOutput("contents")
                                )
                                
                              ))
                              
                              
                      
      
                          
                        
                       
                 
                                
                            
                         
                  
                     )
                )
  
  ))