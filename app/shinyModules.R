sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Main", tabName = "Main", icon = icon('home')),
    menuItem("Upload Object", tabName = "obj", icon = icon('upload')),
    menuItem("Quality control", tabName = "QC", icon = icon("dashboard")),
    menuItem("Dimensionality reduction", tabName = "Dimensionalityreduction", icon = icon("magnifying-glass-chart")),
    menuItem("Markers", tabName = "markers", icon = icon("table")),
    menuItem("Point Selection", tabName = "pselection", icon = icon("dna")),
    menuItem("Spaniel plot", tabName = "spotplot", icon = icon('dog'))
  )
)

header <- dashboardHeader(title = paste0("Spaniel X.0.0") )

dashBody <- dashboardBody(
  tabItems(
    tabItem(tabName = "Main",   fluidRow(
      box(
        title = "Welcome to Spaniel!", status = "primary",
        "A really cool introduction and a biorxiv link.", width = 12
      ))),
    tabItem(tabName = "obj", fluidRow( 
      box(
        title = "Upload SpatialFeatureExperiment object", status = 'primary',
        fileInput('file1', 'Choose rds File',
                  accept=c('.rds'))),
      box(
        title = "How to upload", "Instructions go here"
      ))),
    tabItem(tabName = "QC", fluidRow(
      box(
        plotOutput("qc")),
      box(actionButton("QCbtn", "Go!"),
          shinyjs::hidden(p(id = "QCtext", "Processing...")) ) )),
    tabItem(tabName = "Dimensionalityreduction", fluidRow(
      box(plotOutput("dimred")),
      tabBox(id = 'maindimred', tabPanel('Main', actionButton("Dimredbtn", "Go!"),
                                         shinyjs::hidden(p(id = "Dimredtext", "Processing...")) ), 
             tabPanel('PCA', 'filler'), tabPanel('UMAP', 'filler')) ) ),
    tabItem(tabName = "spotplot",
            tabBox(id = 'imagetabbed', tabPanel('Cluster',
                                                plotOutput("image")),tabPanel('Gene', plotOutput('imagegene') )),
            box(selectInput("geneVisu", label="SELECT GENE",
                            choices = NULL, selected = "Full", multiple = FALSE),
                actionButton("imagebtn", "Go!"),
                shinyjs::hidden(p(id = "Imagetext", "Processing...")) ) ),
    tabItem(tabName = "markers",
            box(plotOutput("marker"), width = 12),
            box(actionButton("markersbtn", "Go!"),
                shinyjs::hidden(p(id = "markertext", "Processing...")),
                selectInput("markerVisu", label="SELECT GENE",
                            choices = NULL, selected = "Full", multiple = TRUE)) ),
    
    tabItem(tabName = "pselection", fluidRow(
      column(width = 4,
             plotOutput("plot1", height = 600, width = 600,
                        # Equivalent to: click = clickOpts(id = "plot_click")
                        click = "plot1_click",
                        brush = brushOpts(
                          id = "plot1_brush", resetOnNew = FALSE
                        )
             )
      )
    ),
    fluidRow(
      column(width = 6,
             h4("Selected points"),
             verbatimTextOutput("selectedPoints")
      )#,
      # column(width = 6,
      #        h4("Brushed points"),
      #        verbatimTextOutput("brush_info")
      # )
    )
    )
  )
)