header <- dashboardHeader(title = paste0("Spaniel X.0.0") )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Main", tabName = "Main", icon = icon('home')),
    menuItem("Upload Object", tabName = "obj", icon = icon('upload')),
    menuItem("Quality control", tabName = "QC", icon = icon("dashboard")),
    menuItem("Point Selection", tabName = "pselection", icon = icon("dna")),
    menuItem("SpanielPlot", tabName = "spotplot", icon = icon('dog'))
  )
)

dashBody <- dashboardBody(
  tabItems(
    tabItem(tabName = "Main",   fluidRow(
      box(
        title = "Welcome to Spaniel!", status = "primary",
        "A really cool introduction and a biorxiv link.", width = 12
      ))),
    tabItem(tabName = "obj", 
            fluidRow( 
              box(
                title = "Upload SpatialFeatureExperiment object", status = 'primary',
                fileInput('file1', 'Choose rds File',
                          accept=c('.rds'))),
              box(
                title = "How to upload", "Instructions go here"
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