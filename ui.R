library(ggplot2)
library(shiny)
library(Cairo)
library(DT)
library(data.table)

options(shiny.usecairo = TRUE)
options(shiny.autoreload.interval = 200)

shinyUI(fluidPage(
  
  tags$style("
             body {
             font-family: Roboto;
             }
             "
  ),
                      fluidRow(
                          column(12,
                                 fluidRow(
                                   column(3,
                              wellPanel(
                                fileInput('file','Frapbot 1.7: Choose CSV File', multiple=TRUE),
                                uiOutput("dataSetChoice"),
                                uiOutput("qualitySlider"),
                                uiOutput("noBackground"),
                                uiOutput("norm"),
                                uiOutput("normSlider"),
                                uiOutput("fitting"),
                                uiOutput("ownFormula"),
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
                                         uiOutput("mainOutput", height="800px")
                                      
                                         
                                  ),
                                  uiOutput("spacingLine"),
                      
                                  column(1),
                                  column(3,
                                         uiOutput("resultOutput")
                                  ),
                                  column(4,
                                           uiOutput("resultOutput2")
                                           
                                  ),
                                  column(4,
                                         uiOutput("resultOutput3")
                                  )
                                  
                                ),
                                uiOutput("spacingLine2"),
                                
                                br(),
                                DT::dataTableOutput("contents")
                              ))
                     )
                )
  ))