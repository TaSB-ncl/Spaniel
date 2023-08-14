source("in_progress/wrap_unwrap_SFE_raster.R")
library(SpatialFeatureExperiment)
library(tidyterra)
library(ggplot2)

sample_id = "C01"
sfe <- readRDS("testData/rObjects/wrapped_sfe.rds") %>% unwrapSFE()



sample_id = "C01"


### get spatraster
sr <- getImg(sfe, sample_id)@image


## pixel coordinates from sfe
coords_sample <- spatialCoords(sfe[, sfe$sample_id == sample_id]) %>% 
  data.frame()

coords_sample$X <- coords_sample$pxl_col_in_fullres
coords_sample$Y <- coords_sample$pxl_row_in_fullres
coords_sample$Y <- coords_sample$Y * -1


## test plot
#df <- data.frame(X = 0:13636, Y = 0:13636  -13636)
p1 <- ggplot(coords_sample) +
  geom_spatraster_rgb(data = sr) + geom_point(aes(X, Y)) + scale_y_reverse()



## rotate spatraster
p2 <- p1 + scale_y_reverse()
p2

p2 + geom_point(aes(X, Y))

## using sf

sfeSample <- sfe[, sfe$sample_id == sample_id]

sf <- colGeometries(sfeSample)[[1]]
sf$clust <- sfeSample$clust
sf$barcodes <- rownames(sf)
counts_df <- as.data.frame(t(as.matrix(sfeSample@assays@data$counts)))
counts_df$barcodes <- rownames(counts_df)
sf <- sf %>% left_join(counts_df, by = 'barcodes')
ggplot(sf) +
  geom_spatraster_rgb(data = sr) + geom_sf(aes(fill = clust))
