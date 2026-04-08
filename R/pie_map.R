scale_pie_radii <- function(raw, size_range = c(0.15, 0.40)) {
  raw <- as.numeric(raw)
  if (all(!is.finite(raw)) || max(raw, na.rm = TRUE) <= 0) {
    return(rep(mean(size_range), length(raw)))
  }
  s <- sqrt(pmax(raw, 0))
  s <- (s - min(s, na.rm = TRUE)) /
    max(diff(range(s, na.rm = TRUE)), .Machine$double.eps)
  size_range[1] + s * (size_range[2] - size_range[1])
}

#' Pie chart map with displacement and leader lines
#'
#' Draws a pie chart at each station on a map. When pies would overlap in
#' crowded regions, the pies are displaced asymmetrically away from their
#' true station coordinates and a leader line + anchor dot is drawn so the
#' viewer can still tell which pie belongs to which station. Works with any
#' grouping (phytoplankton groups, zooplankton orders, microbial phyla, …)
#' and any numeric value (biomass, biovolume, abundance, …).
#'
#' @param data A long-format data.frame with one row per
#'   (station, group). Required columns are configurable through the
#'   \code{*_col} arguments and default to \code{station}, \code{lon},
#'   \code{lat}, \code{group}, \code{value}.
#' @param station_col,lon_col,lat_col,group_col,value_col Column names in
#'   \code{data}. Defaults match SHARK conventions:
#'   \code{"station_name"}, \code{"sample_longitude_dd"},
#'   \code{"sample_latitude_dd"}, \code{"group"}, \code{"value"}.
#' @param label_col Column to use for the on-map station label. Defaults to
#'   \code{station_col}. Set to \code{NULL} (and \code{show_labels = FALSE})
#'   to omit labels entirely.
#' @param group_levels Optional character vector controlling the legend and
#'   slice ordering. Groups not present in \code{data} are dropped.
#' @param group_colors Optional named character vector of colours, keyed by
#'   group name. If \code{NULL}, ggplot's default discrete palette is used.
#' @param group_labels Optional named character vector of legend labels,
#'   keyed by group name. Labels may include HTML markup.
#' @param radius Pie radius in latitude degrees. Default \code{0.28}.
#' @param size_by Optional. \code{NULL} (default) draws all pies at
#'   \code{radius}. \code{"total"} scales each pie's radius by the square
#'   root of the station's total value. Any other character value is
#'   interpreted as the name of a numeric column on the wide-format station
#'   table; pass that to scale by an external metric (e.g. chlorophyll).
#'   Scaled pie sizes are relative within the current plot only; no size
#'   legend is drawn.
#' @param size_range Numeric length-2: minimum and maximum radius (in
#'   latitude degrees) when \code{size_by} is set. Default
#'   \code{c(0.15, 0.40)}.
#' @param repel Logical. Run the displacement algorithm? Default \code{TRUE}.
#' @param min_sep Minimum centre-to-centre separation between two pies,
#'   expressed as a multiple of the larger of the two radii. Default
#'   \code{2.40}.
#' @param min_disp Minimum displacement for a pie that has been moved at
#'   all, as a multiple of its radius. Default \code{1.60}; values \eqn{>1}
#'   guarantee that the anchor sits outside the displaced pie.
#' @param show_labels Logical. Draw station labels next to each pie?
#'   Default \code{TRUE}.
#' @param label_size ggplot text size for the station labels. Default
#'   \code{3}.
#' @param pie_border_color,pie_border_width Aesthetics for the slice
#'   borders. Defaults: \code{"white"}, \code{0.3}.
#' @param leader_color,leader_width Aesthetics for the leader segments
#'   drawn from anchor to displaced pie edge. Defaults: \code{"gray20"},
#'   \code{0.5}.
#' @param anchor_color,anchor_fill,anchor_size Aesthetics for the dot
#'   drawn at the true station location of each displaced pie. Defaults:
#'   \code{"gray10"}, \code{"white"}, \code{1.8}.
#' @param basemap Optional ggplot layer (or list of layers) used as the
#'   base map. If \code{NULL}, a coastline polygon from
#'   \code{rnaturalearth} is drawn.
#' @param basemap_scale Resolution passed to
#'   \code{rnaturalearth::ne_countries()} when \code{basemap} is
#'   \code{NULL}. One of \code{"small"}, \code{"medium"} or \code{"large"}.
#' @param basemap_fill,basemap_border,sea_color Colours for the default
#'   coastline basemap.
#' @param xlim,ylim Optional numeric length-2 vectors. If supplied they
#'   override the auto-fitted map extent.
#' @param pad Padding (in degrees) added around station bounds when
#'   auto-fitting the extent. Default \code{1.0}.
#' @param title,legend_title Optional plot title and legend title. The
#'   legend defaults to "Group".
#' @return A \code{ggplot} object.
#' @examples
#' # 1. Minimal example: 5 made-up stations on the Swedish west coast
#' #    with three plankton groups. Note the two close stations (S1, S2)
#' #    that will get displaced.
#' # Standard SHARK column names used by default (with "Other" group)
#' df <- data.frame(
#'   station_name        = rep(c("S1", "S2", "S3", "S4", "S5"), each = 4),
#'   sample_longitude_dd = rep(c(11.4, 11.6, 12.5, 14.0, 18.0), each = 4),
#'   sample_latitude_dd  = rep(c(58.0, 58.1, 56.6, 55.5, 57.5), each = 4),
#'   group = rep(c("Diatoms", "Dinoflagellates", "Cyanobacteria", "Other"), 5),
#'   value = c(50, 30, 15, 5,  10, 60, 25, 5,  60, 25, 10, 5,
#'             20, 50, 25, 5,  35, 35, 25, 5)
#' )
#' create_pie_map(df, radius = 0.25)
#'
#' \donttest{
#' # 2. Custom colours, ordered legend, custom title
#' create_pie_map(
#'   df,
#'   group_levels = c("Diatoms", "Dinoflagellates", "Cyanobacteria", "Other"),
#'   group_colors = c(Diatoms = "#1f77b4",
#'                    Dinoflagellates = "#d62728",
#'                    Cyanobacteria = "#2ca02c",
#'                    Other = "#7f7f7f"),
#'   title = "Plankton composition",
#'   legend_title = "Taxon group"
#' )
#'
#' # 3. Pie size proportional to total value at each station.
#' #    Sizes are relative within the figure; no size legend is shown.
#' df_variable <- data.frame(
#'   station_name        = rep(c("S1", "S2", "S3", "S4", "S5"), each = 4),
#'   sample_longitude_dd = rep(c(11.4, 11.6, 12.5, 14.0, 18.0), each = 4),
#'   sample_latitude_dd  = rep(c(58.0, 58.1, 56.6, 55.5, 57.5), each = 4),
#'   group = rep(c("Diatoms", "Dinoflagellates", "Cyanobacteria", "Other"), 5),
#'   value = c(50, 30, 15, 5,  70, 50, 20, 10,  30, 20, 20, 10,
#'             100, 80, 50, 20,  20, 15, 20, 5)
#' )
#' create_pie_map(df_variable, size_by = "total", size_range = c(0.15, 0.45))
#'
#' # 4. Non-SHARK column names (e.g. zooplankton abundance dataset)
#' zoo <- data.frame(
#'   site_id   = rep(c("A", "B", "C"), each = 4),
#'   longitude = rep(c(12, 16, 20), each = 4),
#'   latitude  = rep(c(56, 58, 57), each = 4),
#'   order     = rep(c("Calanoida", "Cyclopoida", "Cladocera", "Other"), 3),
#'   counts    = c(120, 80, 40, 20,  60, 100, 30, 10,  90, 70, 50, 25)
#' )
#' create_pie_map(
#'   zoo,
#'   station_col  = "site_id",
#'   lon_col      = "longitude",
#'   lat_col      = "latitude",
#'   group_col    = "order",
#'   value_col    = "counts",
#'   legend_title = "Order"
#' )
#'
#' # 5. Disable displacement (pies will overlap if crowded)
#' create_pie_map(df, repel = FALSE)
#' }
#' @export
create_pie_map <- function(data,
                           station_col   = "station_name",
                           lon_col       = "sample_longitude_dd",
                           lat_col       = "sample_latitude_dd",
                           group_col     = "group",
                           value_col     = "value",
                           label_col     = station_col,
                           group_levels  = NULL,
                           group_colors  = NULL,
                           group_labels  = NULL,
                           radius        = 0.28,
                           size_by       = NULL,
                           size_range    = c(0.15, 0.40),
                           repel         = TRUE,
                           min_sep       = 2.40,
                           min_disp      = 1.60,
                           show_labels   = TRUE,
                           label_size    = 3,
                           pie_border_color = "white",
                           pie_border_width = 0.3,
                           leader_color  = "gray20",
                           leader_width  = 0.5,
                           anchor_color  = "gray10",
                           anchor_fill   = "white",
                           anchor_size   = 1.8,
                           basemap       = NULL,
                           basemap_scale = "medium",
                           basemap_fill  = "gray95",
                           basemap_border = "gray70",
                           sea_color     = "aliceblue",
                           xlim          = NULL,
                           ylim          = NULL,
                           pad           = 1.0,
                           title         = NULL,
                           legend_title  = "Group") {
  required <- c(station_col, lon_col, lat_col, group_col, value_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[value_col]])) {
    stop("`", value_col, "` must be numeric.", call. = FALSE)
  }
  if (!is.null(label_col) && !label_col %in% names(data)) {
    stop("`label_col` ('", label_col, "') not found in `data`.",
         call. = FALSE)
  }

  # 1. Aggregate to one row per (station, group) in case the input has
  #    duplicates (e.g. multiple taxa per group).
  long <- data.frame(
    station = as.character(data[[station_col]]),
    lon     = as.numeric(data[[lon_col]]),
    lat     = as.numeric(data[[lat_col]]),
    group   = as.character(data[[group_col]]),
    value   = as.numeric(data[[value_col]]),
    label   = if (!is.null(label_col)) as.character(data[[label_col]])
              else as.character(data[[station_col]]),
    stringsAsFactors = FALSE
  )
  long <- long[!is.na(long$value), , drop = FALSE]
  long <- stats::aggregate(
    value ~ station + lon + lat + group + label,
    data = long, FUN = sum, na.rm = TRUE
  )

  # 2. Determine the active set of groups (respect group_levels ordering
  #    if supplied; otherwise sorted unique).
  if (is.null(group_levels)) {
    group_levels <- sort(unique(long$group))
  }
  long <- long[long$group %in% group_levels, , drop = FALSE]
  if (nrow(long) == 0) {
    stop("No rows left after filtering by `group_levels`.", call. = FALSE)
  }

  # 3. Pivot to wide format (one row per station, one column per group).
  wide <- tidyr::pivot_wider(
    long,
    id_cols     = c("station", "lon", "lat", "label"),
    names_from  = "group",
    values_from = "value",
    values_fill = 0
  )
  wide <- as.data.frame(wide)
  for (g in group_levels) if (!g %in% names(wide)) wide[[g]] <- 0
  group_cols <- intersect(group_levels, names(wide))

  # 4. Per-station radius (uniform or scaled).
  if (is.null(size_by)) {
    wide$r_pie <- radius
  } else {
    raw <- if (identical(size_by, "total")) {
      rowSums(wide[, group_cols, drop = FALSE], na.rm = TRUE)
    } else if (size_by %in% names(wide)) {
      wide[[size_by]]
    } else {
      stop("`size_by` must be NULL, 'total', or a column name in the ",
           "wide station table.", call. = FALSE)
    }
    raw <- as.numeric(raw)
    wide$r_pie <- scale_pie_radii(raw, size_range = size_range)
  }

  # 5. Determine map extent.
  if (is.null(xlim)) xlim <- range(wide$lon) + c(-pad, pad)
  if (is.null(ylim)) ylim <- range(wide$lat) + c(-pad, pad)
  wide <- clamp_pie_centers(wide, map_xlim = xlim, map_ylim = ylim)

  # 6. Displace pie centres so they don't overlap (or skip if repel=FALSE).
  if (isTRUE(repel) && nrow(wide) > 1) {
    wide <- repel_pie_centers(
      wide,
      map_xlim = xlim, map_ylim = ylim,
      min_sep  = min_sep, min_disp = min_disp
    )
  } else {
    wide$anchor_lon <- wide$lon
    wide$anchor_lat <- wide$lat
  }
  wide <- clamp_pie_centers(wide, map_xlim = xlim, map_ylim = ylim)

  # 7. Mark which pies actually moved, and compute leader endpoints at the
  #    pie edge facing the anchor.
  move_thresh <- 0.05 * radius
  wide$is_displaced <-
    abs(wide$lon - wide$anchor_lon) > move_thresh |
    abs(wide$lat - wide$anchor_lat) > move_thresh

  cos_lat <- cos(wide$lat * pi / 180)
  dx_iso  <- (wide$anchor_lon - wide$lon) * cos_lat
  dy_iso  <- (wide$anchor_lat - wide$lat)
  d_iso   <- sqrt(dx_iso^2 + dy_iso^2)
  d_iso[d_iso < 1e-9] <- 1
  wide$leader_x <- wide$lon + (wide$r_pie * dx_iso / d_iso) / cos_lat
  wide$leader_y <- wide$lat + (wide$r_pie * dy_iso / d_iso)

  # 8. Polygons for the slices, and label positions.
  pie_data <- build_pie_polygons(wide, group_cols)
  pie_data$group <- factor(pie_data$group, levels = group_levels)

  wide$r_lon <- wide$r_pie / cos(wide$lat * pi / 180)
  if (isTRUE(show_labels)) {
    label_char_w <- min(max(diff(xlim) * 0.0046, 0.014), 0.055)
    label_char_h <- min(max(diff(ylim) * 0.0092, 0.024), 0.055)
    wide <- place_pie_labels(
      wide,
      map_xlim = xlim,
      map_ylim = ylim,
      char_w = label_char_w,
      char_h = label_char_h
    )
  }
  displaced <- wide[wide$is_displaced, , drop = FALSE]

  # 9. Assemble the ggplot.
  p <- ggplot2::ggplot()

  if (is.null(basemap)) {
    world <- rnaturalearth::ne_countries(scale = basemap_scale,
                                         returnclass = "sf")
    p <- p + ggplot2::geom_sf(data = world, fill = basemap_fill,
                              color = basemap_border)
  } else {
    p <- p + basemap
  }

  p <- p +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::geom_polygon(
      data  = pie_data,
      ggplot2::aes(x = .data$x, y = .data$y,
                   group = .data$slice_id, fill = .data$group),
      color = pie_border_color, linewidth = pie_border_width
    ) +
    ggplot2::geom_segment(
      data = displaced,
      ggplot2::aes(x    = .data$anchor_lon,
                   y    = .data$anchor_lat,
                   xend = .data$leader_x,
                   yend = .data$leader_y),
      color = leader_color, linewidth = leader_width
    ) +
    ggplot2::geom_point(
      data = displaced,
      ggplot2::aes(x = .data$anchor_lon, y = .data$anchor_lat),
      color = anchor_color, fill = anchor_fill, shape = 21,
      size = anchor_size, stroke = 0.6
    )

  if (!is.null(group_colors)) {
    if (is.null(group_labels)) {
      p <- p + ggplot2::scale_fill_manual(values = group_colors,
                                          name = legend_title, drop = TRUE)
    } else {
      p <- p + ggplot2::scale_fill_manual(values = group_colors,
                                          labels = group_labels,
                                          name = legend_title, drop = TRUE)
    }
  } else {
    if (is.null(group_labels)) {
      p <- p + ggplot2::scale_fill_discrete(name = legend_title, drop = TRUE)
    } else {
      p <- p + ggplot2::scale_fill_discrete(labels = group_labels,
                                            name = legend_title, drop = TRUE)
    }
  }

  uses_markdown_labels <- !is.null(group_labels) &&
    any(grepl("<[^>]+>", unname(group_labels)))

  if (isTRUE(show_labels)) {
    p <- p + ggplot2::geom_text(
      data = wide,
      ggplot2::aes(x = .data$label_x, y = .data$label_y,
                   label = .data$label,
                   hjust = .data$hjust, vjust = .data$vjust),
      size = label_size,
      na.rm = TRUE
    )
  }

  p <- p +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = sea_color,
                                               color = NA),
      axis.title = ggplot2::element_blank(),
      legend.position = "right",
      legend.key.size = ggplot2::unit(0.6, "cm"),
      legend.key.height = ggplot2::unit(0.9, "cm"),
      legend.title = ggplot2::element_text(hjust = 0.5),
      legend.text = if (uses_markdown_labels) ggtext::element_markdown()
                    else ggplot2::element_text()
    )

  if (!is.null(title)) p <- p + ggplot2::ggtitle(title)
  p
}
