markersUI <- function(id) {
  
  ns <- NS(id)
  fluidRow(
    box(
      #width = NULL,
      "plot",
      #DT::dataTableOutput(ns("table")),
      plotOutput("vlnplot")
    ),
    
    box(#width = NULL, 
      #title = "input",
      selectizeInput(ns("cluster_or_domain"), "Cluster or domain column", choices = grep("^clust", names(colData(sfe)), value = T), selected = NULL, multiple = FALSE),
      selectizeInput(ns("indiv_clust"), "Individual cluster or domain", choices = NULL, selected = NULL, multiple = FALSE),
      
    ),
    
    box(
      
    )
  )
}

markersServer <- function(id, sfe) {
  
  moduleServer(id, function(input, output, session) {
    markersReady <- reactiveValues(generated = FALSE, clusters_loaded = FALSE)
    markersReactive <- reactiveVal()
    if(class(sfe) == "SpatialFeatureExperiment"){
      
      observeEvent(c(input$cluster_or_domain), {
        #if(input$cluster_or_domain == ""){return()}
        clust <<- input$cluster_or_domain
        markers <- findMarkers(sfe, sfe[[clust]], test.type = "wilcox", add.summary = TRUE)
        print(markers)
        markers <- lapply(markers, as.data.frame)
        markersReactive(markers)
        print(markersReactive())
        markersReady$generated <- TRUE
      })
      
      observeEvent(markersReady$generated, {
      if (markersReady$generated){
        print(names(markersReactive())[1])
        updateSelectizeInput(session, "indiv_clust", 
                             choices = names(markersReactive()), selected = names(markersReactive())[1], server = TRUE)
        print("ok!")
        print(input$indiv_clust)
        markersReady$clusters_loaded <- TRUE
      }
      })
      
      observeEvent(input$indiv_clust, {
        if (markersReady$clusters_loaded){
          print(input)
          cluster_name <<- input$indiv_clust
          print(paste0("input", input$indiv_clust))
          print("table ready!")
          
          output$table <- DT::renderDataTable(
            markersReactive()[[cluster_name]]
            
          
          )
          
          output$vlnplot <- renderPlot({
            scater::plotExpression(sfe, features = rownames(markersReactive()[[cluster_name]])[1:5], x = clust, colour_by = clust)
          })
        }
      })
      
      
      # observeEvent(c(input$cluster_or_domain, input$sample1), {
      #   if(input$cluster_or_domain == "" || input$sample1 == ""){return()}
      #   print(input$sample)
      #   sample_id <<- input$sample1
      #   sr <- getImg(sfe, sample_id)@image
      #   sfeSample <- sfe[, sfe$sample_id == sample_id]
      #   sf <- colGeometries(sfeSample)[[1]]
      #   sf$barcodes <- rownames(sf)
      #   md_df <- colData(sfeSample) %>% as.data.frame()
      #   # not actually barcodes??
      #   md_df$barcodes <- rownames(sf)
      #   sf <- sf %>% left_join(md_df, by = 'barcodes')
      #   
      #   output$plot <-renderPlot({
      #     ggplot(sf) +
      #       geom_spatraster_rgb(data = sr) + geom_sf(data = sf, aes(fill = .data[[metric]] ) )
      #   }, res = 96)
      #   
      #   
      # })
      
      
    }
  }
  )
}

ui <- fluidPage(
  markersUI("hist1")
)
server <- function(input, output, session) {
  markersServer("hist1", sfe)
}


shinyApp(ui, server)  