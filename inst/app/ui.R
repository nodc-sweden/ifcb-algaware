ui <- bslib::page_sidebar(
  title = tags$div(
    style = "display: flex; align-items: center; width: 100%;",
    tags$span(paste("AlgAware-IFCB", utils::packageVersion("algaware"))),
    tags$img(src = "logo.png", class = "navbar-logo")
  ),
  theme = bslib::bs_theme(
    bootswatch = "flatly", version = 5,
    bg = "#fff", fg = "#333",
    primary = "#2c5f7c",
    "navbar-bg" = "#1b3a4b"
  ),

  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(HTML("document.title = 'AlgAware-IFCB';")),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32",
              href = "favicon-32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16",
              href = "favicon-16.png"),
    tags$script(src = "gallery.js"),
    tags$style(HTML("
      .navbar .navbar-logo {
        margin-left: auto;
        height: 30px;
      }
    "))
  ),

  # Sidebar
  sidebar = bslib::sidebar(
    width = 350,

    # Settings button
    mod_settings_ui("settings"),

    hr(),

    # Data loader
    mod_data_loader_ui("data_loader"),

    hr(),

    # Validation controls
    conditionalPanel(
      condition = "output.data_loaded",
      mod_validation_ui("validation")
    ),

    # Report generation
    conditionalPanel(
      condition = "output.data_loaded",
      mod_report_ui("report")
    )
  ),

  # Main panel with tabs
  bslib::navset_card_tab(
    id = "main_tabs",

    bslib::nav_panel(
      "Validate",
      icon = shiny::icon("images"),
      mod_gallery_ui("gallery")
    ),

    bslib::nav_panel(
      "Maps",
      icon = shiny::icon("map"),
      conditionalPanel(
        condition = "output.data_loaded",
        plotOutput("image_count_map", height = "400px"),
        plotOutput("biomass_map", height = "400px"),
        plotOutput("chl_map", height = "400px")
      )
    ),

    bslib::nav_panel(
      "Plots",
      icon = shiny::icon("chart-bar"),
      conditionalPanel(
        condition = "output.data_loaded",
        div(
          style = "overflow-y: auto; max-height: 85vh;",
          h4("Baltic Sea"),
          plotOutput("baltic_heatmap", height = "800px"),
          plotOutput("baltic_stacked_bar", height = "600px"),
          hr(),
          h4("West Coast"),
          plotOutput("westcoast_heatmap", height = "800px"),
          plotOutput("westcoast_stacked_bar", height = "600px")
        )
      )
    ),

    bslib::nav_panel(
      "Summary",
      icon = shiny::icon("table"),
      conditionalPanel(
        condition = "output.data_loaded",
        DT::DTOutput("summary_table")
      )
    )
  )
)
