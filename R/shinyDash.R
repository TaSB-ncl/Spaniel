#' @include shinyModules.R

welcomeMessage <-  shiny::HTML(paste("Spaniel is an shiny app for sharing 
        and exploring Spatial Transcriptomics data.",  
        "To get started click on the <b>'upload 
        data'</b> option from the left-hand menubar.", sep = "</br>"))

howToUpload <- shiny::HTML(paste("1) Click on <b>'Browse...'</b>" ,
"2) Select a processed <b>SpatialFeatureExperiment</b> object 
from your computer and click on <b>'Open'</b>",
"3) Wait for the blue <b>'Upload to complete'</b> message to appear.",
"4) You can now browse the dataset by clicking on the <b>Quality 
Control</b> or <b>SpanielPlot</b> tabs", sep = "</br>"))


header <- shinydashboard::dashboardHeader(title = paste0("Spaniel X.0.0") )

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Main", tabName = "Main", 
                             icon = icon('home')),
    shinydashboard::menuItem("Upload Object", tabName = "obj", 
                             icon = icon('upload')),
    shinydashboard::menuItem("Quality control", tabName = "QC", 
                             icon = icon("dashboard")),
    #menuItem("Point Selection", tabName = "pselection", icon = icon("dna")),
    shinydashboard::menuItem("Gene Expression", tabName = "spotplot", 
                             icon = icon('dna')),
    shinydashboard::menuItem("Compare Sections", tabName = "spotplot", 
                             icon = icon('dog'))
  )
)

dashBody <- shinydashboard::dashboardBody(
  shinydashboard::tabItems(
    shinydashboard::tabItem(tabName = "Main",   fluidRow(
      shinydashboard::box(
        title = "Welcome to Spaniel!", status = "primary", welcomeMessage, 
        width = 12
      ))),
    shinydashboard::tabItem(tabName = "obj", 
            fluidRow( 
              shinydashboard::box(
                title = "Upload SpatialFeatureExperiment object", 
                status = 'primary',
                fileInput('file1', 'Choose rds File',
                          accept=c('.rds'))),
              shinydashboard::box(
                title = "How to upload", howToUpload
                )
              )
            ),
    shinydashboard::tabItem(tabName = "spotplot",
                spanielPlotUI("test")
            ),
    shinydashboard::tabItem(tabName = "QC",
            fluidPage(
              qcPlotUI("qc1"),
              qcPlotUI("qc2")
            ))
  )
)
