library(SpatialFeatureExperiment)
library(terra)
library(Spaniel)
library(dplyr)

sfe_control <- readRDS("testData/rObjects/wrapped_sfe_control.rds") %>% unwrapSFE()

sfe <- readRDS("testData/rObjects/wrapped_sfe_single.rds") %>% unwrapSFE()

grps <- c("clust", "domain", "radiation_field")
qc <- c("total", "sum", "subsets_mito_percent", "in_tissue")
exp_cond <- c("condition")
labelSFE <- function(sfe, 
                     grp_cols, 
                     qc_cols,
                     exp_cond_cols){
  
  md_names <- colnames(colData(sfe))
  md_names[md_names %in% grp_cols] <- 
    paste0("grp_", md_names[md_names %in% grp_cols])
  md_names[md_names %in% qc_cols] <- 
    paste0("qc_", md_names[md_names %in% qc_cols])
  md_names[md_names %in% exp_cond_cols] <- 
    paste0("expCond_", md_names[md_names %in% exp_cond_cols])
  names(colData(sfe)) <- md_names
  return(sfe)
  
}

sfe_control <- labelSFE(sfe_control, grps, qc, exp_cond)
sfe_control %>% wrapSFE()
## reload sfe and test
## save sfe
## document, add to git, add to workshop
## change shiny to select for columns
## 
sfe %>% wrapSFE() %>% saveRDS("testData/rObjects/wrapped_sfe_single.rds")
