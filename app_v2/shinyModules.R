spanielPlotUI <- function(id) {
  fluidPage(
  fluidRow(

  box(#width = NULL, 
    "input",
      selectInput(NS(id, "gene"), "Genes", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(NS(id, "sample"), "Sample", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(NS(id, "clustering"), "Clustering method", choices = c("clust"), selected = NULL, multiple = FALSE)
      
  ),
  box(
    "Violin plot",
    plotOutput(NS(id, "vlnplot"))
  )
  ),
    fluidRow(
  
  box(
    #width = NULL,
    "plot",
    plotOutput(NS(id, "plot"))
  ),
  
  box(
   # width = NULL,
    "umap",
    plotOutput(NS(id, "umapplot"))
  )
)
)

  
}

spanielPlotServer <- function(id, sfe) {
  
  moduleServer(id, function(input, output, session) {
    if(class(sfe) == "SpatialFeatureExperiment"){
      observeEvent(c(input$gene, input$sample), {
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
      }
      )
    }})
}