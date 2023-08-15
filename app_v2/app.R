#' @import shiny
#' @import shinydashboard
#' @import shinyjs

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyjs)
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
  
  spatialObj <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- readRDS(inFile$datapath) %>% unwrapSFE()
    return(data)
  })
  
  observe({
    # doesn't actually do anything...
    if(!is.null(inFile)){
      updateSelectInput(session, NS("test","gene"), 
                        choices = rownames(spatialObj()))
      updateSelectInput(session, NS("test","sample"), 
                        choices = unique(spatialObj()$sample_id))
      spanielPlotServer("test", spatialObj())
    }
  })
  
  
 
  
   
}

# Run app ----
shinyApp(ui, server)