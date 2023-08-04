library(Spaniel)
library(ggplot2)
library(dplyr)
library(SpatialFeatureExperiment)
library(scran)
library(scater)
library(scuttle)
library(cowplot)
library(bluster)



#' QC wrapper for shiny app
#'
#' @param data 
#'
#' @return A SFE object with QC columns
#' @export
#'
#' @examples
QC <- function(data){
  qc.metrics <- scater::perCellQCMetrics(data)
  colData(data) <- cbind(colData(data), qc.metrics)
  # Mitochondrial genes
  mito_genes <- rownames(data)[grep("^MT-", rownames(data))]
  # Ribosomal genes
  ribo_genes <- rownames(data)[grep("^RP[SL]", rownames(data))]
  data <- addPerCellQC(data, flatten = T, subsets = list(mt = mito_genes, ribo = ribo_genes))
  return(data)
}


#' Normalise counts & find variable features wrapper for shiny app
#'
#' @param data 
#'
#' @return A SFE object with log normalised counts, 
#'         containing only genes that are highly variable.
#' @export
#'
#' @examples
NormFindVarFeats <- function(data){
  data <- dataIn[,data$in_tissue]
  data <- scater::logNormCounts(data)
  data.model <- modelGeneVar(data)
  hvg <- getTopHVGs(data.model, prop=0.1)
  data <- data[hvg,]
  return(data)
}

#' PCA, UMAP wrapper for shiny app
#'
#' @param data 
#'
#' @return An updated SFE object with PCA and UMAP reduced dimensions
#' @export
#'
#' @examples
DimRed <- function(data){
  redDim_pca <- calculatePCA(data)
  reducedDims(data) <- list(PCA = redDim_pca)
  redDim_umap <- calculateUMAP(data, pca = 30)
  reducedDims(data) <- list(PCA = redDim_pca, UMAP = redDim_umap)
  return(data)
}

#' Cluster cells wrapper for shiny app
#'
#' @param data 
#'
#' @return Updated SFE object with column containing clustering info
#' @export
#'
#' @examples
SpClusters <- function(data){
  nn.clust <- clusterCells(data, BLUSPARAM=NNGraphParam(cluster.fun="louvain"), use.dimred="PCA", full=TRUE)
  data$clusters <- nn.clust$clusters
  return(data)
}

#' findMarkers wrapper for shiny app
#'
#' @param data 
#' @param colLabelName 
#' @param cluster 
#'
#' @return Top ranked markers for each cluster in SFE object
#' @export
#'
#' @examples
Markers <- function(data, colLabelName, cluster){
  markers <- findMarkers(data, colData(data)[colLabelName] %>% unlist(), test.type = "wilcox", pval.type = "all")
  # need to add support for other clusters etc
  markers <- markers[[1]]
  top.ranked <- row.names(markers)[1:10]
  return(top.ranked)
}

#' Plot QC data in shiny app
#'
#' @param data 
#'
#' @return Plots of QC metrics
#' @export
#'
#' @examples
QCplots <- function(data){
  plot_grid(plotColData(data, x = 'sample_id', y = 'detected', colour_by = 'sample_id'),
            plotColData(data, y = "total", x = "sample_id", colour_by = "sample_id"))
  plot_grid(plotColData(data, y = "detected", x = "sample_id", colour_by = "sample_id"), 
            plotColData(data, y = "total", x = "sample_id", colour_by = "sample_id"), 
            plotColData(data, y = "subsets_mt_percent",x = "sample_id", colour_by = "sample_id"), 
            plotColData(data, y = "subsets_ribo_percent",x = "sample_id", colour_by = "sample_id"), 
            ncol = 2)
}

#' Plot reduced dimensions of SFE object
#'
#' @param dataIn 
#' @param reduction 
#' @param mdCol 
#'
#' @return Plots of a reduced dimension in SFE object
#' @export
#'
#' @examples
RedDimsPlot <- function(dataIn, reduction='PCA', mdCol =''){
  if(mdCol != ''){
    plotReducedDim(dataIn, reduction, colour_by = mdCol)}
  else{
    plotReducedDim(dataIn, reduction)
  }
  
}

#' Plot top markers of SFE object
#'
#' @param data 
#' @param top 
#' @param colLabel 
#'
#' @return
#' @export
#'
#' @examples
MarkerPlots <- function(data, top, colLabel){
  #plotGroupedHeatmap(data, features=top, group=colLabel, 
  #                   center=TRUE, zlim=c(-3, 3))
  plotExpression(data, top, x=colLabel, colour_by = colLabel)
  
}

#' Plot a feature from SFE object on top of Visium image
#'
#' @param data 
#' @param fill_feat 
#' @param rotate 
#' @param flip 
#'
#' @details Might not work with current SFE version?
#'  
#' @return Plot of a feature on top of visium image
#' @export
#'
#' @examples
plotSFE <- function(data, fill_feat, rotate = 0, flip = NULL){
  sf <- colGeometries(data)[[1]]
  spi <- getImg(data)
  spi <- rotateImg(spi, rotate)
  
  if (!is.null(flip)){
    spi <- mirrorImg(spi, flip)
  }
  sf$clusters <- data$clusters
  sf$barcodes <- rownames(sf)
  sf$geometry_ds <- sf$geometry
  counts_df <- as.data.frame(as.matrix(t(data@assays@data$counts)))
  counts_df$barcodes <- rownames(counts_df)
  sf <- sf %>% left_join(counts_df, by = 'barcodes')
  ggplot(sf) +
    background_image(imgRaster(spi)) + geom_sf(aes(fill = {{fill_feat}}, geometry = geometry_ds))
  #geom_sf_label(aes(label = clusters))
}
