spanielPlotUI <- function(id) {
  fluidPage(
  fluidRow(
    
  tabBox(#width = NULL, 
    #title = "input",
    tabPanel("Input",
      selectizeInput(NS(id, "gene"), "Genes", choices = NULL, selected = NULL, multiple = FALSE),
      selectizeInput(NS(id, "sample"), "Sample", choices = NULL, selected = NULL, multiple = FALSE),
      selectizeInput(NS(id, "clustering"), "Clustering method", choices = NULL, selected = NULL, multiple = FALSE)),
    tabPanel("UMAP", plotOutput(NS(id, "umapall")))
      
  ),
  tabBox(
    tabPanel(
      "Violin Plot",
    plotOutput(NS(id, "vlnplot"))
    ),
    tabPanel(
      "Spatial Plot",
      plotOutput(NS(id, "plot"))
    ),
    tabPanel(
      "UMAP",
      plotOutput(NS(id, "umapplot"))
    )
  )
  ),
    fluidRow(
  
  box(
    #width = NULL,
    "plot",
  ),
  
  box(
   # width = NULL,
    "umap",
  )
)
)

  
}

spanielPlotServer <- function(id, sfe) {
  
  moduleServer(id, function(input, output, session) {
    if(class(sfe) == "SpatialFeatureExperiment"){
      observeEvent(c(input$gene, input$sample, input$clustering), {
        if(input$gene == "" || input$sample == "" || input$clustering == ""){return()}
        print(input$sample)
        sample_id <<- input$sample
        clust <<- input$clustering
        
        sr <- getImg(sfe, sample_id)@image
        sfeSample <- sfe[, sfe$sample_id == sample_id]
        sf <- colGeometries(sfeSample)[[1]]
        sf$clust <- sfeSample$clust
        sf$barcodes <- rownames(sf)
        counts_df <- as.data.frame(t(as.matrix(sfeSample@assays@data$logcounts)))
        counts_df$barcodes <- rownames(counts_df)
        sf <- sf %>% left_join(counts_df, by = 'barcodes')
        
        gene_name <<- input$gene
        output$plot <-renderPlot({
          ggplot(sf) +
            geom_spatraster_rgb(data = sr) + geom_sf(data = sf, aes(fill = .data[[gene_name]] ) )
        }, res = 96)
        
        output$umapplot <- renderPlot({
          scater::plotUMAP(sfeSample, colour_by = gene_name, text_by = clust)
        })
        
        output$vlnplot <- renderPlot({
          scater::plotExpression(sfeSample, features = gene_name, x = clust, colour_by = clust)
        })
        
        output$umapall <- renderPlot({
          scater::plotUMAP(sfe, colour_by = clust, text_by = clust)
          
        })
      })
    }
    }
    )
}


qcPlotUI <- function(id) {
  fluidRow(
      box(
        #width = NULL,
        "plot",
        plotOutput(NS(id, "plot"))
      ),
      
      box(#width = NULL, 
        #title = "input",
        selectizeInput(NS(id, "metric1"), "Metrics", choices = NULL, selected = NULL, multiple = FALSE),
        selectizeInput(NS(id, "sample1"), "Sample", choices = NULL, selected = NULL, multiple = FALSE),
        
      )
    )
}

qcPlotServer <- function(id, sfe) {
  
  moduleServer(id, function(input, output, session) {
    if(class(sfe) == "SpatialFeatureExperiment"){
      observeEvent(c(input$metric1, input$sample1), {
        if(input$metric1 == "" || input$sample1 == ""){return()}
        print(input$sample)
        sample_id <<- input$sample1
        sr <- getImg(sfe, sample_id)@image
        sfeSample <- sfe[, sfe$sample_id == sample_id]
        sf <- colGeometries(sfeSample)[[1]]
        sf$barcodes <- rownames(sf)
        md_df <- colData(sfeSample) %>% as.data.frame()
        # not actually barcodes??
        md_df$barcodes <- rownames(sf)
        sf <- sf %>% left_join(md_df, by = 'barcodes')
        
        metric <<- input$metric1
        
        output$plot <-renderPlot({
          ggplot(sf) +
            geom_spatraster_rgb(data = sr) + geom_sf(data = sf, aes(fill = .data[[metric]] ) )
        }, res = 96)
        

      })
    }
  }
  )
}


