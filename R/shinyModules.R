#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' 
instructions <- shiny::HTML(paste("How to use the app",  
                                  "step 1'", sep = "</br>"))

spanielPlotUI <- function(id) {
  fluidPage(
    
    fluidRow(
      shinydashboard::tabBox(
        shiny::tabPanel(
        "Input",
        selectizeInput(
          NS(id, "gene"),
          "Genes",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        selectizeInput(
          NS(id, "sample"),
          "Sample",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        selectizeInput(
          NS(id, "group"),
          "Groups (eg clusters, domains)",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        )
      ),
      shiny::tabPanel("Instructions", instructions)),
   
    
      shinydashboard::tabBox(
        shiny::tabPanel("Spatial Plot",
               plotOutput(NS(id, "spaniel_plot"))),
        shiny::tabPanel("Spatial - Clusters",
               plotOutput(NS(id, "spaniel_cluster")))
    )
    
    
    
  ),
  
  fluidRow(
    shinydashboard::box(title = "Violin Plot",
        plotOutput(NS(id, "vlnplot"))),
    shinydashboard::tabBox(
      shiny::tabPanel("UMAP Plot",
        plotOutput(NS(id, "umapplot"))),
      shiny::tabPanel("UMAP - Clusters",
               plotOutput(NS(id, "umapall")))
    )
  ))
}


spanielPlotServer <- function(id, sfe) {
  moduleServer(id, function(input, output, session) {
    if (class(sfe) == "SpatialFeatureExperiment") {
      observeEvent(c(input$gene, input$sample, input$group), {
        if (input$gene == "" ||
            input$sample == "" || input$group == "") {
          return()
        }
        print(input$sample)
        sample_id <<- input$sample
        clust <<- input$group
        sfeSample <- sfe[, sfe$sample_id == sample_id]
        gene_symbol <<- input$gene
        gene_id <- rownames(rowData(sfe))[rowData(sfe)$symbol== gene_symbol]
       
        
        output$spaniel_plot <- renderPlot({
          Spaniel::spanielPlot_SFE(sfeSample,  
                        sample_id = sample_id, 
                        plot_feat =gene_symbol,
                        plot_type = "gene",
                        ptSizeMin = 0, 
                        ptSizeMax = 2) 
        })
        
        output$spaniel_cluster <- renderPlot({
          Spaniel::spanielPlot_SFE(sfeSample,  
                                   sample_id = sample_id, 
                                   plot_feat = clust,
                                   plot_type = "metadata") 
        })
        
        
        output$umapplot <- renderPlot({
          scater::plotUMAP(sfeSample,
                           colour_by = gene_id,
                           text_by = clust)
        })
        
        output$vlnplot <- renderPlot({
          scater::plotExpression(
            sfeSample,
            features = gene_id,
            x = clust,
            colour_by = clust
          )
        })
        
        output$umapall <- renderPlot({
          scater::plotUMAP(sfe, colour_by = clust, text_by = clust)
          
        })
      })
    }
  })
}


qcPlotUI <- function(id) {
  fluidRow(shinydashboard::box(#width = NULL,
    "plot",
    plotOutput(NS(id, "qc_plot"))),
    
    shinydashboard::box(
      #width = NULL,
      #title = "input",
      selectizeInput(
        NS(id, "metric1"),
        "Metrics",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      selectizeInput(
        NS(id, "sample1"),
        "Sample",
        choices = NULL,
        selected = NULL,
        multiple = FALSE
      ),
      
    ))
}

qcPlotServer <- function(id, sfe) {
  moduleServer(id, function(input, output, session) {
    if (class(sfe) == "SpatialFeatureExperiment") {
      observeEvent(c(input$metric1, input$sample1), {
        if (input$metric1 == "" || input$sample1 == "") {
          return()
        }
        print(input$sample)
        sample_id <<- input$sample1
        sfeSample <- sfe[, sfe$sample_id == sample_id]
        metric <<- input$metric1
        
        output$qc_plot <- renderPlot({
          Spaniel::spanielPlot_SFE(sfeSample,  
                                   sample_id = sample_id, 
                                   plot_feat = metric,
                                   plot_type = "metadata") 
        })
        
        
      })
    }
  })
}


