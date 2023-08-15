#' @import shiny
#' @import shinydashboard
#' @import shinyjs

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyterra)
library(SpatialFeatureExperiment)
source("appFuncs.R")
source("../in_progress/wrap_unwrap_SFE_raster.R")
source("shinyModules.R")
source("shinyDash.R")

ui <- dashboardPage(
  header,
  sidebar,
  dashBody, shinyjs::useShinyjs()
)

server <- function(input, output, session){
  options(shiny.maxRequestSize=30*1024^3)
  
  controlVars <- reactiveValues(fileUploaded = FALSE, dropdownsLoaded = FALSE)
  spatialObj <- reactiveVal()
  
  observeEvent(input$file1, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- readRDS(inFile$datapath) %>% unwrapSFE()
    spatialObj(data)
    controlVars$fileUploaded <- TRUE
  })
  
  
  
  observeEvent(input$file1, {
    if(controlVars$fileUploaded){
      updateSelectInput(session, NS("test","gene"), 
                        choices = rownames(spatialObj()), selected = NULL)
      updateSelectInput(session, NS("test","sample"), 
                        choices = unique(spatialObj()$sample_id), selected = NULL)
      updateSelectInput(session, NS("qc1","sample1"), 
                        choices = unique(spatialObj()$sample_id), selected = NULL)
      updateSelectInput(session, NS("qc1","metric1"), 
                        choices = colnames(colData(spatialObj())), selected = NULL)
      updateSelectInput(session, NS("qc2","sample1"), 
                        choices = unique(spatialObj()$sample_id), selected = NULL)
      updateSelectInput(session, NS("qc2","metric1"), 
                        choices = colnames(colData(spatialObj())), selected = NULL)
    }
      controlVars$dropdownsLoaded <- TRUE
  }) 
  
  observeEvent(input$file1, {
    if(controlVars$dropdownsLoaded){
      qcPlotServer("qc1", spatialObj())
      qcPlotServer("qc2", spatialObj())
      spanielPlotServer("test", spatialObj())
      
    }
    
    })
  
   
}

# Run app ----
shinyApp(ui, server)