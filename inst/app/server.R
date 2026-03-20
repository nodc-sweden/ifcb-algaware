server <- function(input, output, session) {

  # Reactive values for app state
  rv <- reactiveValues(
    dashboard_metadata = NULL,
    cruise_numbers = character(0),
    matched_metadata = NULL,
    classifications_raw = NULL,
    classifications = NULL,
    invalidated_classes = character(0),
    taxa_lookup = NULL,
    station_summary = NULL,
    baltic_wide = NULL,
    westcoast_wide = NULL,
    baltic_samples = character(0),
    westcoast_samples = character(0),
    class_list = character(0),
    current_class_idx = 1L,
    current_region = "EAST",
    selected_images = character(0),
    corrections = data.frame(
      sample_name = character(0),
      roi_number = integer(0),
      original_class = character(0),
      new_class = character(0),
      stringsAsFactors = FALSE
    ),
    ferrybox_chl = NULL,
    image_counts = NULL,
    cruise_info = "",
    classifier_name = NULL,
    data_loaded = FALSE
  )

  # Config (settings)
  settings <- load_settings()
  config <- do.call(reactiveValues, settings)

  # Flag for conditional panels
  output$data_loaded <- reactive({ isTRUE(rv$data_loaded) })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  # Initialize modules
  mod_settings_server("settings", config)
  mod_data_loader_server("data_loader", config, rv)
  mod_gallery_server("gallery", rv, config)
  mod_validation_server("validation", rv, config)
  mod_report_server("report", rv, config)

  # Cached biomass maps (invalidates when station_summary changes)
  biomass_maps <- reactive({
    req(rv$station_summary)
    create_biomass_maps(rv$station_summary)
  })

  # Maps
  output$image_count_map <- renderPlot({
    req(rv$image_counts, nrow(rv$image_counts) > 0)
    create_image_count_map(rv$image_counts)
  })

  output$biomass_map <- renderPlot({
    id <- showNotification("Generating maps...", type = "message",
                           duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    biomass_maps()$biomass_map
  })

  output$chl_map <- renderPlot({
    biomass_maps()$chl_map
  })

  # Heatmaps
  output$baltic_heatmap <- renderPlot({
    req(rv$baltic_wide, ncol(rv$baltic_wide) > 1)
    id <- showNotification("Generating plots...", type = "message",
                           duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    create_heatmap(rv$baltic_wide, taxa_lookup = rv$taxa_lookup,
                   title = "Baltic Sea")
  })

  output$westcoast_heatmap <- renderPlot({
    req(rv$westcoast_wide, ncol(rv$westcoast_wide) > 1)
    create_heatmap(rv$westcoast_wide, taxa_lookup = rv$taxa_lookup,
                   title = "West Coast")
  })

  # Stacked bar charts
  output$baltic_stacked_bar <- renderPlot({
    req(rv$baltic_wide, ncol(rv$baltic_wide) > 1)
    create_stacked_bar(rv$baltic_wide, taxa_lookup = rv$taxa_lookup,
                       title = "Baltic Sea")
  })

  output$westcoast_stacked_bar <- renderPlot({
    req(rv$westcoast_wide, ncol(rv$westcoast_wide) > 1)
    create_stacked_bar(rv$westcoast_wide, taxa_lookup = rv$taxa_lookup,
                       title = "West Coast")
  })

  # Summary table
  output$summary_table <- DT::renderDT({
    req(rv$station_summary)
    display <- rv$station_summary[, c(
      "STATION_NAME_SHORT", "COAST", "visit_date", "name",
      "counts_per_liter", "biovolume_mm3_per_liter", "carbon_ug_per_liter"
    )]
    names(display) <- c("Station", "Region", "Date", "Taxon",
                         "Counts/L", "Biovolume mm3/L", "Carbon ug/L")
    DT::datatable(display,
                  options = list(pageLength = 25, scrollX = TRUE),
                  rownames = FALSE) |>
      DT::formatRound(c("Counts/L", "Biovolume mm3/L", "Carbon ug/L"), 3)
  })
}
