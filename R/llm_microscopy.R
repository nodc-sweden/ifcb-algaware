# LLM prompts for microscopy stations (qualitative Plankton Toolbox data).
# These stations have abundance classes instead of biovolume, so they get
# their own prompt text and are never mixed into the quantitative IFCB lines.

#' Region label for a coast code
#' @keywords internal
coast_region_label <- function(coast, short = FALSE) {
  if (short) {
    ifelse(coast == "EAST", "Baltic", "West")
  } else {
    ifelse(coast == "EAST", "Baltic Sea", "West Coast (Skagerrak/Kattegat)")
  }
}

#' Per-group overview of one microscopy station
#' @keywords internal
microscopy_group_overview <- function(obs) {
  groups <- unique(obs$phyto_group)
  rows <- lapply(groups, function(g) {
    d <- obs[obs$phyto_group == g, , drop = FALSE]
    top <- d[which.max(d$abundance_class), ]
    list(group = g, class = top$abundance_class, label = top$abundance_label,
         n = nrow(d))
  })
  rows[order(-vapply(rows, `[[`, integer(1), "class"),
             -vapply(rows, `[[`, integer(1), "n"))]
}

#' Format one microscopy station for an LLM prompt
#'
#' @param obs Enriched microscopy observations for one station visit.
#' @param chl_measure Chlorophyll measurement terminology key.
#' @return Character string.
#' @keywords internal
format_microscopy_station_for_prompt <- function(obs,
                                                 chl_measure = "fluorescence") {
  obs <- obs[order(-obs$abundance_class, obs$display_name), , drop = FALSE]
  first <- obs[1, ]

  # Sampler, depth and analyst stay out of the prompt, as for IFCB stations
  # (no depth information is sent to the LLM); the report introduction
  # states them instead.
  chl_line <- ""
  if ("chl_mean" %in% names(obs) && !is.na(first$chl_mean)) {
    chl_line <- sprintf("%s: %.2f\n", chl_measure_terms(chl_measure)$data_label,
                        first$chl_mean)
  }

  group_lines <- vapply(microscopy_group_overview(obs), function(g) {
    sprintf("  %s: highest class %d (%s), %d %s", g$group, g$class, g$label,
            g$n, if (g$n == 1) "taxon" else "taxa")
  }, character(1))

  trophic <- c(HT = "heterotrophic", MX = "mixotrophic")
  taxa_lines <- vapply(seq_len(nrow(obs)), function(i) {
    tags <- obs$phyto_group[i]
    tt <- trophic[obs$trophic_type[i]]
    if (!is.na(tt)) tags <- paste0(tags, "; ", tt)
    sprintf("  %s [%s]%s: class %d (%s)", obs$display_name[i], tags,
            if (isTRUE(obs$HAB[i])) " [HAB]" else "",
            obs$abundance_class[i], obs$abundance_label[i])
  }, character(1))

  paste0(
    "Station: ", first$STATION_NAME_SHORT, " (", first$STATION_NAME, ")\n",
    "Region: ", coast_region_label(first$COAST), "\n",
    "Date: ", as.character(first$visit_date), "\n",
    "Method: qualitative light microscopy (Plankton Toolbox abundance ",
    "classes 1-5)\n",
    chl_line,
    "Taxa observed: ", nrow(obs), "\n",
    "Phytoplankton groups present (use these exact group names; do not ",
    "infer group membership from scientific names):\n",
    paste(group_lines, collapse = "\n"), "\n",
    "Taxa by abundance class (highest first):\n",
    paste(taxa_lines, collapse = "\n")
  )
}

#' Build the prompts for a microscopy station description
#'
#' @param obs Enriched microscopy observations for one station visit.
#' @param all_microscopy Observations of all microscopy stations of the
#'   cruise, used as comparison context (only other microscopy stations are
#'   comparable).
#' @param chl_measure Chlorophyll measurement terminology key.
#' @return A list with \code{system} and \code{user} prompt strings.
#' @keywords internal
build_microscopy_station_prompts <- function(obs, all_microscopy = NULL,
                                             chl_measure = "fluorescence") {
  guide <- load_writing_guide()
  chl_terms <- chl_measure_terms(chl_measure)

  context <- ""
  if (!is.null(all_microscopy) &&
      length(unique(all_microscopy$visit_id)) > 1) {
    visits <- unique(all_microscopy[, c("visit_id", "STATION_NAME_SHORT",
                                        "COAST")])
    lines <- vapply(seq_len(nrow(visits)), function(i) {
      n_taxa <- sum(all_microscopy$visit_id == visits$visit_id[i])
      sprintf("  %s (%s): %d taxa%s", visits$STATION_NAME_SHORT[i],
              coast_region_label(visits$COAST[i], short = TRUE), n_taxa,
              if (visits$visit_id[i] == obs$visit_id[1]) " <-- this station" else "")
    }, character(1))
    context <- paste0(
      "\n\nOther microscopy stations of this cruise (the only stations this ",
      "one may be compared with):\n", paste(lines, collapse = "\n")
    )
  }

  system_prompt <- paste0(
    "You are a marine biologist writing phytoplankton monitoring reports ",
    "for the Swedish AlgAware programme (SMHI). You write in English. ",
    "Follow the writing guide exactly.\n\n",
    "WRITING GUIDE:\n", guide
  )

  user_prompt <- paste0(
    "Write a station description for the following station visit. This ",
    "station has no IFCB data; its phytoplankton was analysed by ",
    "qualitative light microscopy (see 'Microscopy stations' in the writing ",
    "guide).\n\n",
    format_microscopy_station_for_prompt(obs, chl_measure),
    context,
    "\n\nWrite in English. Follow the station description pattern and ",
    "harmful taxa terminology in the writing guide, adapted to qualitative ",
    "data: characterise the community from the abundance classes ",
    "(5 = dominant, 4 = very common, 3 = common, 1-2 = present in low numbers). ",
    "Base the diversity statement on the number of taxa observed. ",
    "State briefly that the sample was analysed by light microscopy. ",
    "Do not state or estimate biovolume, carbon, cell concentrations ",
    "(cells/L) or warning levels, and do not compare abundances with the ",
    "IFCB stations of this cruise. ",
    "If you mention the chlorophyll value, call it '", chl_terms$en, "'. ",
    "Use the provided phytoplankton group assignments in the prompt data. ",
    "Do not infer group membership from scientific names yourself, and use ",
    "the group names consistently throughout. ",
    "Provide a detailed species breakdown only for the groups with the ",
    "highest abundance classes; mention remaining groups in a single brief ",
    "sentence. ",
    "Taxa marked [HAB] in the data are potentially harmful and must be ",
    "mentioned. If a [HAB] taxon has one of the highest abundance classes, ",
    "describe it as dominant in the community description. ",
    "Output ONLY the description text, no headings or station name. ",
    microscopy_output_rules()
  )

  list(system = system_prompt, user = user_prompt)
}

#' Output format rules shared by the microscopy prompts
#' @keywords internal
microscopy_output_rules <- function() {
  paste0(
    "Output plain text only -- no markdown formatting whatsoever. ",
    "The only asterisk allowed is the harmful taxon marker placed after the ",
    "FULL taxon mention. Place it after 'spp.' or 'sp.' when present (e.g. ",
    "'Pseudochattonella spp.*', NOT 'Pseudochattonella* spp.*'). Never place ",
    "an asterisk after the genus name alone when 'spp.' or 'sp.' follows."
  )
}

#' Format the microscopy stations for the cruise summary prompt
#'
#' @param microscopy Enriched microscopy observations, or NULL.
#' @param chl_measure Chlorophyll measurement terminology key.
#' @return Character string (empty without microscopy data).
#' @keywords internal
format_microscopy_summary_block <- function(microscopy,
                                            chl_measure = "fluorescence") {
  if (is.null(microscopy) || nrow(microscopy) == 0) return("")
  visits <- unique(microscopy[, c("visit_id", "STATION_NAME_SHORT", "COAST",
                                  "visit_date")])
  visits <- visits[order(factor(visits$COAST, levels = c("WEST", "EAST")),
                         visits$visit_date), , drop = FALSE]
  chl_abbr <- if (identical(chl_measure, "concentration")) {
    "Chl-a conc."
  } else {
    "Chl fluoresc."
  }

  lines <- vapply(seq_len(nrow(visits)), function(i) {
    d <- microscopy[microscopy$visit_id == visits$visit_id[i], , drop = FALSE]
    d <- d[order(-d$abundance_class, d$display_name), , drop = FALSE]
    star <- ifelse(d$HAB %in% TRUE, "*", "")
    top <- utils::head(d[d$abundance_class >= 3, , drop = FALSE], 6)
    if (nrow(top) == 0) top <- utils::head(d, 5)
    top_names <- sprintf("%s%s (class %d)", top$display_name,
                         star[match(top$display_name, d$display_name)],
                         top$abundance_class)
    hab <- d$display_name[d$HAB %in% TRUE]
    groups <- vapply(microscopy_group_overview(d), function(g) {
      sprintf("%s (highest class %d)", g$group, g$class)
    }, character(1))
    chl <- if ("chl_mean" %in% names(d) && !is.na(d$chl_mean[1])) {
      sprintf(" | %s: %.2f", chl_abbr, d$chl_mean[1])
    } else {
      ""
    }
    sprintf("  %s (%s, %s): %d taxa | Highest classes: %s%s | Groups: %s%s",
            visits$STATION_NAME_SHORT[i],
            coast_region_label(visits$COAST[i], short = TRUE),
            as.character(visits$visit_date[i]), nrow(d),
            paste(top_names, collapse = ", "),
            if (length(hab) > 0) {
              paste0(" | HAB: ", paste0(hab, "*", collapse = ", "))
            } else {
              ""
            },
            paste(groups, collapse = ", "), chl)
  }, character(1))

  paste0(
    "\n\nMicroscopy stations (no IFCB data; qualitative light microscopy, ",
    "abundance classes 1-5 where 5 = 50-100% and 1 = observed; NOT ",
    "comparable with the IFCB values above):\n",
    paste(lines, collapse = "\n")
  )
}
