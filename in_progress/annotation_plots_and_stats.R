library(ggplot2)
library(dplyr)

summariseData <- function(sfe, comp1, comp2) {
  
  colData(sfe) %>% 
    data.frame() %>% 
    rename(group1 = !!comp1, group2 = !!comp2) %>%
    group_by(group1, group2) %>%
    summarise(number_cells = n()) %>% 
    filter(!is.na(group1) | !is.na(group2)) 
}

#get_means(cats, weight, type)
summariseData(sfe, "clust", "domain") %>% 
  ggplot(aes(x = group1, y=number_cells, fill = group2)) + 
  geom_bar(stat="identity", position = "stack")

sfe$domain <- sfe$domain %>% gsub("left-venrticle", "left-ventricle", .)


levs <- c("left-ventricle", "XR-LV",  "right-ventricle", "XR-RV", "left-atrium", "right-atrium",  "valves", "centre")
sfe$domain <- factor(sfe$domain, levels = levs)

summariseData(sfe, "domain", "clust") %>% 
  ggplot(aes(x = group1, y=number_cells, fill = group2)) + 
  geom_bar(stat="identity", position = "stack")





numberSpotsPerDomain <- function(sfe){
  colData(sfe) %>% 
    data.frame() %>%
    group_by(sample_id, domain) %>% 
    summarise(number_cells = n())
    
  
}


numberSpotsPerDomain(sfe)
  


spanielPlot_SFE(sfe,  
                 sample_id = "C01", 
                 plot_feat = "domain", ptSizeMin = 0, 
                 ptSizeMax = 2)
