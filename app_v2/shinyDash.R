welcomeMessage <-  HTML(paste("Spaniel is an shiny app for sharing and exploring 
        Spatial Transcriptomics data.",  "To get started click on the <b>'upload 
        data'</b> option from the left-hand menubar.", sep = "</br>"))

howToUpload <- HTML(paste("1) Click on <b>'Browse...'</b>" ,
"2) Select a processed <b>SpatialFeatureExperiment</b> object 
from your computer and click on <b>'Open'</b>",
"3) Wait for the blue <b>'Upload to complete'</b> message to appear.",
"4) You can now browse the dataset by clicking on the <b>Quality 
Control</b> or <b>SpanielPlot</b> tabs", sep = "</br>"))


header <- dashboardHeader(title = paste0("Spaniel X.0.0") )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Main", tabName = "Main", icon = icon('home')),
    menuItem("Upload Object", tabName = "obj", icon = icon('upload')),
    menuItem("Quality control", tabName = "QC", icon = icon("dashboard")),
    #menuItem("Point Selection", tabName = "pselection", icon = icon("dna")),
    menuItem("Gene Expression", tabName = "spotplot", icon = icon('dna')),
    menuItem("Compare Sections", tabName = "spotplot", icon = icon('dog'))
  )
)

dashBody <- dashboardBody(
  tabItems(
    tabItem(tabName = "Main",   fluidRow(
      box(
        title = "Welcome to Spaniel!", status = "primary", welcomeMessage, 
        width = 12
      ))),
    tabItem(tabName = "obj", 
            fluidRow( 
              box(
                title = "Upload SpatialFeatureExperiment object", 
                status = 'primary',
                fileInput('file1', 'Choose rds File',
                          accept=c('.rds'))),
              box(
                title = "How to upload", howToUpload
                )
              )
            ),
    tabItem(tabName = "spotplot",
                spanielPlotUI("test")
            ),
    tabItem(tabName = "QC",
            fluidPage(
              qcPlotUI("qc1"),
              qcPlotUI("qc2")
            ))
  )
)
