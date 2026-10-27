# Taxon information for microscopy observations: HAB flags, italics and
# phytoplankton groups. Microscopy taxa are mostly absent from the IFCB
# taxa_lookup (which is keyed to classifier classes), so without this step
# harmful taxa would silently lose their asterisk in the report.

.microscopy_cache <- new.env(parent = emptyenv())

# Ranks printed in italics (genus and below).
italic_ranks <- c("Genus", "Subgenus", "Section", "Subsection", "Series",
                  "Species", "Subspecies", "Variety", "Subvariety", "Forma",
                  "Subforma")

#' Look up taxa in WoRMS
#'
#' @param names Character vector of scientific names.
#' @return A data.frame with \code{name}, \code{AphiaID} and \code{rank}.
#' @keywords internal
fetch_worms_records <- function(names) {
  # marine_only = FALSE: Baltic samples contain brackish and freshwater taxa
  # that a marine-only match would drop.
  records <- SHARK4R::match_worms_taxa(names, marine_only = FALSE,
                                       verbose = FALSE)
  data.frame(name = records$name, AphiaID = records$AphiaID,
             rank = records$rank, stringsAsFactors = FALSE)
}

#' Fetch the IOC-UNESCO list of harmful algae (cached per session)
#'
#' @return A data.frame with \code{AphiaID} and \code{name}, including both
#'   the listed and the accepted names.
#' @keywords internal
fetch_hab_list <- function() {
  if (is.null(.microscopy_cache$hab_list)) {
    hab <- SHARK4R::get_hab_list(verbose = FALSE)
    .microscopy_cache$hab_list <- unique(data.frame(
      AphiaID = c(hab$AphiaID, hab$AphiaID_accepted),
      name = c(hab$ScientificName, hab$ScientificName_accepted),
      stringsAsFactors = FALSE
    ))
  }
  .microscopy_cache$hab_list
}

#' Add HAB flags, italics and phytoplankton groups to microscopy taxa
#'
#' Uses WoRMS (AphiaID and rank), the IOC-UNESCO HAB list and
#' \code{assign_phyto_groups()}, so groups match those of the IFCB taxa.
#' When a service is unavailable it falls back to offline rules: groups from
#' the Plankton Toolbox taxon class via \code{phyto_groups.yaml}, italics
#' from the name, and HAB flags from \code{taxa_lookup} only.
#'
#' A taxon present in \code{taxa_lookup} keeps its curated HAB flag; other
#' taxa are flagged on an exact HAB-list match by name or AphiaID.
#'
#' @param obs Microscopy observations with \code{name} and
#'   \code{taxon_class}.
#' @param taxa_lookup Optional IFCB taxa lookup with \code{name} and
#'   \code{HAB}.
#' @return A list with \code{obs} (with \code{AphiaID}, \code{HAB},
#'   \code{italic} and \code{phyto_group} added) and \code{notes} (messages
#'   for the user about lookups that failed).
#' @export
enrich_microscopy_taxa <- function(obs, taxa_lookup = NULL) {
  notes <- character(0)
  taxa <- unique(obs[, c("name", "taxon_class")])
  taxa <- taxa[!duplicated(taxa$name), ]

  worms <- tryCatch(fetch_worms_records(taxa$name), error = function(e) NULL)
  idx <- if (is.null(worms)) rep(NA_integer_, nrow(taxa)) else
    match(taxa$name, worms$name)
  taxa$AphiaID <- if (is.null(worms)) NA_integer_ else
    as.integer(worms$AphiaID[idx])
  taxa$rank <- if (is.null(worms)) NA_character_ else worms$rank[idx]
  if (is.null(worms)) {
    notes <- c(notes, paste(
      "WoRMS could not be reached; italics and groups of microscopy taxa",
      "were set from their names and classes."
    ))
  }

  taxa$italic <- ifelse(is.na(taxa$rank), italic_from_name(taxa$name),
                        taxa$rank %in% italic_ranks)

  hab_list <- tryCatch(fetch_hab_list(), error = function(e) NULL)
  if (is.null(hab_list)) {
    notes <- c(notes, paste(
      "The HAB list could not be fetched; microscopy taxa are only marked",
      "as potentially harmful when the IFCB taxa list flags them."
    ))
  }
  taxa$HAB <- hab_flags(taxa$name, taxa$AphiaID, taxa_lookup, hab_list)

  groups <- tryCatch(
    assign_phyto_groups(taxa$name,
                        if (all(is.na(taxa$AphiaID))) NULL else taxa$AphiaID),
    error = function(e) NULL
  )
  taxa$phyto_group <- if (is.null(groups)) {
    group_from_taxon_class(taxa$taxon_class)
  } else {
    groups
  }

  keep <- c("name", "AphiaID", "HAB", "italic", "phyto_group")
  obs <- obs[, setdiff(names(obs), keep[-1]), drop = FALSE]
  obs$.row <- seq_len(nrow(obs))
  obs <- merge(obs, taxa[, keep], by = "name", all.x = TRUE, sort = FALSE)
  obs <- obs[order(obs$.row), , drop = FALSE]
  obs$.row <- NULL
  rownames(obs) <- NULL
  list(obs = obs, notes = notes)
}

#' HAB flags for microscopy taxa
#' @keywords internal
hab_flags <- function(names, aphia_ids, taxa_lookup, hab_list) {
  vapply(seq_along(names), function(i) {
    if (!is.null(taxa_lookup) && "HAB" %in% names(taxa_lookup) &&
        names[i] %in% taxa_lookup$name) {
      return(any(taxa_lookup$HAB[taxa_lookup$name == names[i]] == TRUE,
                 na.rm = TRUE))
    }
    if (is.null(hab_list)) return(FALSE)
    names[i] %in% hab_list$name ||
      (!is.na(aphia_ids[i]) && aphia_ids[i] %in% hab_list$AphiaID)
  }, logical(1))
}

#' Guess whether a name is printed in italics when its rank is unknown
#'
#' Names of more than one word are species-level; one-word names are taken
#' as genera unless they end like a higher rank (e.g. "Centrales",
#' "Cryptophyceae").
#' @keywords internal
italic_from_name <- function(names) {
  higher_rank <- "(ales|aceae|phyceae|phycidae|phyta|idae|ineae|oideae|mycota|ozoa)$"
  grepl(" ", trimws(names)) | !grepl(higher_rank, trimws(names))
}

#' Phytoplankton group from a taxon class (offline fallback)
#'
#' Applies the class rules of \code{phyto_groups.yaml}; taxa whose group is
#' defined by phylum or genus rules end up as "Other".
#' @keywords internal
group_from_taxon_class <- function(taxon_class) {
  cfg <- load_phyto_group_config()
  role_names <- c(diatoms = "Diatoms", dinoflagellates = "Dinoflagellates",
                  cyanobacteria = "Cyanobacteria")
  rules <- c(
    stats::setNames(lapply(names(cfg$core), function(r) cfg$core[[r]]$class),
                    role_names[names(cfg$core)]),
    lapply(cfg$custom, function(g) g$class)
  )
  groups <- rep("Other", length(taxon_class))
  for (group in names(rules)) {
    groups[taxon_class %in% rules[[group]]] <- group
  }
  groups
}

#' Extend the taxa lookup with microscopy taxa for one report
#'
#' Italics, HAB asterisks and binomial abbreviation in the report text are
#' all driven by \code{taxa_lookup}. Appending the microscopy taxa to a
#' report-only copy makes them work for microscopy stations without touching
#' the formatting code. Taxa already in the lookup keep their curated row.
#'
#' @param taxa_lookup IFCB taxa lookup, or NULL.
#' @param microscopy_obs Enriched microscopy observations (from
#'   \code{enrich_microscopy_taxa()}), or NULL.
#' @return \code{taxa_lookup} with one row added per new microscopy taxon
#'   (only the microscopy rows when \code{taxa_lookup} is NULL).
#' @keywords internal
build_report_taxa_lookup <- function(taxa_lookup, microscopy_obs) {
  if (is.null(microscopy_obs) || nrow(microscopy_obs) == 0) return(taxa_lookup)
  taxa <- unique(microscopy_obs[, c("name", "AphiaID", "HAB", "italic",
                                    "phyto_group")])
  taxa <- taxa[!duplicated(taxa$name) & !taxa$name %in% taxa_lookup$name, ]
  if (nrow(taxa) == 0) return(taxa_lookup)

  added <- data.frame(
    clean_names = paste0("microscopy:", taxa$name),
    name = taxa$name,
    sflag = "",
    AphiaID = taxa$AphiaID,
    HAB = taxa$HAB,
    warning_level = NA_real_,
    italic = taxa$italic,
    is_diatom = taxa$phyto_group == "Diatoms",
    stringsAsFactors = FALSE
  )
  if (is.null(taxa_lookup)) return(added)
  added <- added[, intersect(names(taxa_lookup), names(added)), drop = FALSE]
  for (col in setdiff(names(taxa_lookup), names(added))) {
    added[[col]] <- rep(taxa_lookup[[col]][NA_integer_], nrow(added))
  }
  rbind(taxa_lookup, added[, names(taxa_lookup)])
}
