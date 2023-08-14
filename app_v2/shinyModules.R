spanielPlotUI <- function(id) {
  tagList(
    selectInput(NS(id, "gene"), "Genes", choices = NULL, selected = NULL, multiple = FALSE),
    selectInput(NS(id, "sample"), "Sample", choices = NULL, selected = NULL, multiple = FALSE),
    plotOutput(NS(id, "plot"))
  )
}

spanielPlotServer <- function(id, sfe) {
  
  moduleServer(id, sfe, function(input, output, session) {
    if(class(sfe) == "SpatialFeatureExperiment"){
      observeEvent(c(input$gene, input$sample), {
        sample_id <<- input$sample
        sr <- getImg(sfe, sample_id)@image
        sfeSample <- sfe[, sfe$sample_id == sample_id]
        sf <- colGeometries(sfeSample)[[1]]
        sf$clust <- sfeSample$clust
        sf$barcodes <- rownames(sf)
        counts_df <- as.data.frame(t(as.matrix(sfeSample@assays@data$counts)))
        counts_df$barcodes <- rownames(counts_df)
        sf <- sf %>% left_join(counts_df, by = 'barcodes')
        
        gene_name <<- input$gene
        output$plot <-renderPlot({
          ggplot(sf) +
            geom_spatraster_rgb(data = sr) + geom_sf(data = sf, aes(fill = .data[[gene_name]] ) )
        }, res = 96)
      }
      )
    }})
}