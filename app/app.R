options(shiny.maxRequestSize=30*1024^2)

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

ui <- dashboardPage(
  header,
  sidebar,
  dashBody, shinyjs::useShinyjs()
)

server <- function(input, output, session){
  options(shiny.maxRequestSize=30*1024^3)
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- readRDS(inFile$datapath) %>% unwrapSFE()
    return(data)
  })
  
  
  
  dataR <- reactiveVal()
  sfObj <- reactiveVal()
  markersR <- reactiveVal()
  sfpoints <- reactiveVal()
  
  plotReady <- reactiveValues(qc = FALSE, dimred = FALSE, image = FALSE, markers =FALSE, 
                              imagegene = FALSE)
  # RQ
  # Modularise me!
  observe({
    shinyjs::disable("QCbtn")
    shinyjs::show("QCtext")
    print('click')
    dataIn <- myData()
    print(dataIn)
    #dataIn <- NormFindVarFeats(dataIn)
    print(dataIn)
    print('boop')
    dataR(dataIn)
    plotReady$qc <- TRUE
    shinyjs::enable("Dimredbtn")
  }) %>% bindEvent(.,input$QCbtn)
  
  observe({
    shinyjs::disable("Dimredbtn")
    shinyjs::show("Dimredtext")
    print('click')
    dataIn <- dataR()
    print(dataIn)
    dataIn <- SpClusters(dataIn)
    print(dataIn)
    print('boop')
    dataR(dataIn)
    plotReady$dimred <- TRUE
  }) %>% bindEvent(.,input$Dimredbtn)
  
  observe({
    #shinyjs::disable("imagebtn")
    shinyjs::show("Imagetext")
    print('click')
    sfe <- dataR()
    sf <- colGeometries(sfe)[[1]]
    sf$mid <- sf::st_centroid(sf$geometry)
    sf$clusters <- sfe$clusters
    sfObj(sf)
    print('boop')
    plotReady$image <- TRUE
  }) %>% bindEvent(.,input$imagebtn)
  
  observeEvent(input$geneVisu, { 
    gene<<-input$geneVisu
  })
  
  observe({
    #shinyjs::disable("imagegenesbtn")
    #shinyjs::show("Imagegenestext")
    print('click')
    sfe <- dataR()
    sf <- colGeometries(sfe)[[1]]
    sf$mid <- sf::st_centroid(sf$geometry)
    sf$clusters <- sfe$clusters
    sf$gene <- sfe@assays@data$counts[input$geneVisu,]
    sfObj(sf)
    print('boop')
    plotReady$imagegene <- TRUE
  }) %>% bindEvent(.,input$imagebtn)


  observe({
    shinyjs::disable("markersbtn")
    shinyjs::show("markertext")
    print('click')
    sfe <- dataR()
    #toprank <- Markers(sfe, "clusters", "1")
    #markersR(toprank)
    plotReady$markers <- TRUE
  }) %>% bindEvent(.,input$markersbtn)
  
  observe({
    if(plotReady$qc)
      updateSelectInput(session, "markerVisu", 
                        choices = rownames(dataR()))
    updateSelectInput(session, "geneVisu", 
                      choices = rownames(dataR()))
  })
  
  
  output$qc <-renderPlot({
    print(plotReady$qc)
    if (plotReady$qc) {
      #shinyjs::enable("QC")
      shinyjs::hide("QCtext")
      print(plotReady$qc)
      print(dataR())
      plot_grid(plotColData(dataR(), x = 'sample_id', y = 'detected', colour_by = 'sample_id'),
                plotColData(dataR(), y = "total", x = "sample_id", colour_by = "sample_id"))
      plot_grid(plotColData(dataR(), y = "detected", x = "sample_id", colour_by = "sample_id"),
                plotColData(dataR(), y = "total", x = "sample_id", colour_by = "sample_id"),
                plotColData(dataR(), y = "subsets_mt_percent",x = "sample_id", colour_by = "sample_id"),
                plotColData(dataR(), y = "subsets_ribo_percent",x = "sample_id", colour_by = "sample_id"),
                ncol = 2)
    }
  })

  output$dimred <-renderPlot({
    print(plotReady$dimred)
    if (plotReady$dimred) {
      #shinyjs::enable("QC")
      shinyjs::hide("Dimredtext")
      print(plotReady$dimred)
      print(dataR())
      plotUMAP(dataR(), colour_by='clusters')
    }
  })

  output$image <-renderPlot({
    print(plotReady$image)
    if (plotReady$image) {
      #shinyjs::enable("QC")
      shinyjs::hide("Imagetext")
      ggplot(sfObj()) +
        geom_sf(aes(fill = clusters, geometry = geometry))
    }
  })
  
  output$image <-renderPlot({
    print(plotReady$image)
    if (plotReady$image) {
      #shinyjs::enable("QC")
      shinyjs::hide("Imagetext")
      #ggplot(sfObj()) +
      #  background_image(imgRaster(dataR())) + geom_sf(aes(fill = clusters, geometry = geometry))
      plotVisium(sfObj(), fill = clusters, highlight = in_tissue)
      }
  })
  
  output$imagegene <-renderPlot({
    print(plotReady$image)
    if (plotReady$image) {
      #shinyjs::enable("QC")
      #shinyjs::hide("Imagetext")
      # ggplot(sfObj()) +
      #   background_image(imgRaster(dataR())) + geom_sf(aes(fill = gene, geometry = geometry)) + scale_fill_viridis_c()
      #plotVisium(my(), fill = 'gene', highlight = in_tissue)
      plotSFE(sfObj(), 'cluster')
    }
  })
  
  output$marker <-renderPlot({
    print(plotReady$markers)
    if (plotReady$markers) {
      print(markersR())
      #shinyjs::enable("QC")
      print(input$markerVisu)
      marker_list <- as.vector(input$markerVisu)
      print(marker_list)
      shinyjs::hide("markertext")
      MarkerPlots(dataR(), marker_list, "clusters")
    }
  })
  
  output$plot1 <- renderPlot({
    ggplot(sfpoints()) +
      #geom_sf(aes(fill = clusters, geometry = mid)) +
      geom_point(data = selected(), colour = "red")
  })
  
  output$click_info <- renderPrint({
    nearPoints(sfePoints, input$plot1_click, addDist = TRUE)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(sfePoints, input$plot1_brush)
  })
  
  # RQ
  # switch me to pixel coords
  
  selected <- reactive({
    # add clicked
    print(sfObj())
    sfePoints <- sf::st_coordinates(sfObj()$mid)
    sfpoints(sfePoints)
    selected_points <- sfePoints[0, ]
    selected_points <<- rbind(selected_points, nearPoints(sfePoints, input$plot1_click), brushedPoints(sfePoints, input$plot1_brush))
    # remove _all_ duplicates if any (toggle mode) 
    # http://stackoverflow.com/a/13763299/3817004
    selected_points <<- 
      selected_points[!(duplicated(selected_points) | 
                          duplicated(selected_points, fromLast = TRUE)), ]
    str(selected_points)
    return(selected_points)
  })
  
  output$selectedPoints <- renderPrint({
    selected()
  })
}

# Run app ----
shinyApp(ui, server)
