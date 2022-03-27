get_links <- function(conn, ID_ENTITY) {
  conn %>%
    dabr::select("SELECT * FROM external_link WHERE ID_ENTITY = ", ID_ENTITY)
}

rm_links <- function(conn, ID_ENTITY, external_source) {
  links <- get_links(conn, ID_ENTITY)
  conn %>%
    dabr::delete("DELETE FROM external_link WHERE ID_ENTITY = ",
                 ID_ENTITY, " AND external_source = ",
                 dabr::quote(external_source))
  return(links)
}

#' Create a snapshot of the data linked to entities
#'
#' Create a snapshot of the data linked to entities. Including metadata,
#' dates, samples, age models and pollen counts.
#'
#' @param x This object accepts different classes. If the given object is
#'     a database connection, then extracts data from the database using the
#'     `ID_SITE`, `ID_ENTITY` or `entity_name` (these should be provided after
#'     the connection object). Alternatively, if the given object is a vector,
#'     then it will retrieve the records from an internal snapshot of the
#'     database, included in this package.
#' @param ... Optional parameters.
#'
#' @rdname snapshot
#' @export
snapshot <- function(x, ...) {
  if (missing(x))
    return(snapshot.default(x, ...))
  UseMethod("snapshot", x)
}

# @param x DB connection object.
#' @param ID_SITE Optional, if `ID_ENTITY` or `entity_name` are provided.
#' @param ID_ENTITY Optional, if `ID_SITE` or `entity_name` are provided.
#' @param entity_name Optional, if `ID_SITE` or `ID_ENTITY` are provided.
#' @param quiet Boolean flag to indicate if queries should be displayed.
#'
#' @rdname snapshot
#' @return List with the individual tables.
#' @export
snapshot.MariaDBConnection <- function(x,
                                       ID_SITE,
                                       ID_ENTITY,
                                       entity_name,
                                       quiet = TRUE) {
  if (!missing(ID_SITE)) {
    .snapshot_by_site(x, ID_SITE, quiet = quiet)
  } else if (!missing(ID_ENTITY)) {
    .snapshot_by_entity(x, ID_ENTITY, quiet = quiet)
  } else if (!missing(entity_name)) {
    .snapshot_by_entity_name(x, entity_name, quiet = quiet)
  } else {
    message("At least one of the following is required:\n",
            "- ID_SITE\n- ID_ENTITY\n- entity_name\n")
  }
}

#' @rdname snapshot
#' @export
snapshot.character <- function(x, ...) {
  # Local bindings
  . <- amalgamation_level <- count <- taxon_name <- NULL
  ID_SAMPLE <- ID_SAMPLE <- ID_TAXON <- NULL

  # dot_params <- list(...)
  # if (!("entity_name" %in% names(dot_params))) {
  #   message("Expecting at least one value for `entity_name`")
  #   return(NULL)
  # }
  entity_tb <- special.epd::entity %>%
    dplyr::filter(entity_name %in% x)
  if (nrow(entity_tb) == 0) {
    message("No records were found!")
    return(NULL)
  }

  date_info_tb <- special.epd::date_info %>%
    dplyr::filter(ID_ENTITY %in% entity_tb$ID_ENTITY)
  sample_tb <- special.epd::sample %>%
    dplyr::filter(ID_ENTITY %in% entity_tb$ID_ENTITY)
  if (nrow(sample_tb) > 0) {
    tryCatch({
      age_model_tb <- special.epd::age_model %>%
        dplyr::filter(ID_SAMPLE %in% sample_tb$ID_SAMPLE) %>%
        dplyr::left_join(special.epd::model_name, by = "ID_MODEL")
    }, error = function(e) {
      age_model_tb <- NULL
    })
    tryCatch({
      pollen_count_tb <- special.epd::pollen_count %>%
        dplyr::filter(ID_SAMPLE %in% sample_tb$ID_SAMPLE) %>%
        dplyr::left_join(special.epd::taxon_name, by = "ID_TAXON") %>%
        split(.$amalgamation_level) %>%
        purrr::map(function(counts) {
          counts %>%
            dplyr::select(-ID_TAXON, -amalgamation_level) %>%
            dplyr::filter(!is.na(count)) %>%
            tidyr::pivot_wider(id_cols = c(ID_SAMPLE),
                               names_from = taxon_name,
                               values_from = count) %>%
            dplyr::select(1, order(colnames(.)[-1]) + 1)
        }) %>%
        magrittr::set_names(names(.) %>%
                              stringr::str_replace_all("0", "clean") %>%
                              stringr::str_replace_all("1", "intermediate") %>%
                              stringr::str_replace_all("2", "amalgamated"))
    }, error = function(e) {
      pollen_count_tb <- NULL
    })
  } else {
    age_model_tb <- NULL
    pollen_count_tb <- NULL
  }
  list(
    entity = entity_tb,
    date_info = date_info_tb,
    sample = sample_tb,
    age_model = age_model_tb,
    pollen_count = pollen_count_tb
  ) %>%
    magrittr::set_class(c("special.epd", class(.)))
}

#' @param use_id_site Boolean flag to indicate whether to search using
#'     `ID_ENTITY` (default) or `ID_SITE`, using the values in `x`.
#' @rdname snapshot
#' @export
snapshot.numeric <- function(x, use_id_site = FALSE, ...) {
  if (use_id_site) {
    entity_tb <- special.epd::entity %>%
      dplyr::filter(ID_SITE %in% x)
  } else {
    entity_tb <- special.epd::entity %>%
      dplyr::filter(ID_ENTITY %in% x)
  }
  if (nrow(entity_tb) == 0) {
    message("No records were found!")
    return(NULL)
  }
  return(snapshot(entity_tb$entity_name))
}

#' @rdname snapshot
#' @export
snapshot.tbl_df <- function(x, ...) {
  NextMethod("snapshot")
}

#' @rdname snapshot
#' @export
snapshot.tbl <- function(x, ...) {
  NextMethod("snapshot")
}

#' @rdname snapshot
#' @export
snapshot.data.frame <- function(x, ...) {
  message("Calling data.frame snapshot...")
}


#' @rdname snapshot
#' @export
snapshot.default <- function(x,
                             ID_SITE,
                             ID_ENTITY,
                             entity_name) {
  message("Calling default snapshot...")
}

#' @keywords internal
.snapshot_by_site <- function(x, ID_SITE, quiet = TRUE) {
  entity_tb <- x %>%
    dabr::select("SELECT ID_ENTITY FROM entity WHERE ID_SITE IN (",
                 paste0(ID_SITE, collapse = ", "),
                 ")",
                 quiet = quiet)
  return(.snapshot_by_entity(x, entity_tb$ID_ENTITY, quiet = quiet))
}

#' @keywords internal
.snapshot_by_entity <- function(x, ID_ENTITY, quiet = TRUE) {
  # Local bindings
  . <- amalgamation_level <- count <- taxon_name <- NULL
  ID_SAMPLE <- ID_SAMPLE <- ID_TAXON <- NULL

  entity_tb <- x %>%
    dabr::select("SELECT * FROM entity WHERE ID_ENTITY IN (",
                 paste0(ID_ENTITY, collapse = ", "),
                 ")",
                 quiet = quiet)
  if (nrow(entity_tb) == 0) {
    message("No records were found!")
    return(NULL)
  }
  date_info_tb <- x %>%
    dabr::select("SELECT * FROM date_info WHERE ID_ENTITY IN (",
                 paste0(ID_ENTITY, collapse = ", "),
                 ")",
                 quiet = quiet)
  sample_tb <- x %>%
    dabr::select("SELECT * FROM sample WHERE ID_ENTITY IN (",
                 paste0(ID_ENTITY, collapse = ", "),
                 ")",
                 quiet = quiet)
  if (nrow(sample_tb) > 0) {
    tryCatch({
      model_name_tb <- dabr::select_all(x, "model_name", quiet = TRUE)
      age_model_tb <- x %>%
        dabr::select("SELECT * FROM age_model WHERE ID_SAMPLE IN (",
                     paste0(sample_tb$ID_SAMPLE, collapse = ", "),
                     ")",
                     quiet = quiet) %>%
        dplyr::left_join(model_name_tb, by = "ID_MODEL")
    }, error = function(e) {
      age_model_tb <- NULL
    })
    tryCatch({
      taxon_name_tb <- dabr::select_all(x, "taxon_name", quiet = TRUE)
      pollen_count_tb <- x %>%
        dabr::select("SELECT * FROM pollen_count WHERE ID_SAMPLE IN (",
                     paste0(sample_tb$ID_SAMPLE, collapse = ", "),
                     ")",
                     quiet = quiet) %>%
        dplyr::left_join(taxon_name_tb, by = "ID_TAXON") %>%
        # dplyr::mutate(amalgamation_level = dplyr::case_when(
        #   amalgamation_level == 0 ~ "clean",
        #   amalgamation_level == 1 ~ "intermediate",
        #   amalgamation_level == 2 ~ "amalgamated",
        #   TRUE ~ NA_character_
        # )) %>%
        split(.$amalgamation_level) %>%
        purrr::map(function(counts) {
          counts %>%
            dplyr::select(-ID_TAXON, -amalgamation_level) %>%
            dplyr::filter(!is.na(count)) %>%
            tidyr::pivot_wider(id_cols = c(ID_SAMPLE),
                               names_from = taxon_name,
                               values_from = count) %>%
            dplyr::select(1, order(colnames(.)[-1]) + 1)
        }) %>%
        magrittr::set_names(names(.) %>%
                              stringr::str_replace_all("0", "clean") %>%
                              stringr::str_replace_all("1", "intermediate") %>%
                              stringr::str_replace_all("2", "amalgamated"))
    }, error = function(e) {
      pollen_count_tb <- NULL
    })
  } else {
    age_model_tb <- NULL
    pollen_count_tb <- NULL
  }
  # a <- entity_tb %>%
  #   purrr::pmap_df(function(ID_SITE, ID_ENTITY, site_name, entity_name, ...) {
  #     tibble::tibble(
  #       ID_SITE,
  #       ID_ENTITY,
  #       site_name,
  #       entity_name,
  #       metadata = entity_tb %>%
  #         dplyr::filter(ID_SITE == !!ID_SITE, ID_ENTITY == !!ID_ENTITY) %>%
  #         dplyr::select(-c(1:4)) %>%
  #         list(),
  #       dates = date_info_tb %>%
  #         dplyr::filter(ID_ENTITY == !!ID_ENTITY) %>%
  #         dplyr::select(-c(1)) %>%
  #         list(),
  #       samples = sample_tb %>%
  #         dplyr::filter(ID_ENTITY == !!ID_ENTITY) %>%
  #         dplyr::select(-c(1)) %>%
  #         list(),
  #       age_model = age_model_tb %>%
  #         dplyr::filter(ID_SAMPLE %in% purrr::pluck(samples, "ID_SAMPLE")) %>%
  #         dplyr::select(-c(1)) %>%
  #         list(),
  #       pollen_counts = pollen_count_tb %>%
  #         tibble::as_tibble() %>%
  #         purrr::map(~print(purrr::pluck(samples, "ID_SAMPLE")))
  #         #              .x %>%
  #         #              dplyr::filter(ID_SAMPLE %in%
  #         #                              samples$ID_SAMPLE) %>%
  #         #              dplyr::select(-c(1))) %>%
  #         # list()
  #     )
  #   })

  list(
    entity = entity_tb,
    date_info = date_info_tb,
    sample = sample_tb,
    age_model = age_model_tb,
    pollen_count = pollen_count_tb
  ) %>%
    magrittr::set_class(c("special.epd", class(.)))
}

#' @keywords internal
.snapshot_by_entity_name <- function(x, entity_name, quiet = TRUE) {
  entity_tb <- x %>%
    dabr::select("SELECT ID_ENTITY FROM entity WHERE entity_name IN (",
                 paste0(dabr::quote(entity_name), collapse = ", "),
                 ")",
                 quiet = quiet)
  if (nrow(entity_tb) == 0) {
    message("No records were found!")
    return(NULL)
  }
  return(.snapshot_by_entity(x, entity_tb$ID_ENTITY, quiet = quiet))
}

#' Write DB snapshot to disk
#' Write DB snapshot to disk as individual CSV files.
#'
#' @param .data DB snapshot (object of `special.epd` class).
#' @param prefix String with a prefix path where the data should be stored.
#'
#' @return Invisibly returns the input DB snapshot.
#' @export
write_csvs <- function(.data, prefix) {
  if (!("special.epd" %in% class(.data)))
    stop("The given object does not look like a valid snapshot from the ",
         "`SPECIAL-EPD database. Try using the function `snapshot` first.",
         call. = FALSE)
  if (!dir.exists(dirname(prefix)))
    stop("The provided directory, `", dirname(prefix), "`, does not exist.",
         call. = FALSE)
  .data$entity %>%
    readr::write_excel_csv(file = paste0(prefix, "_metadata.csv"), na = "")
  .data$date_info %>%
    readr::write_excel_csv(file = paste0(prefix, "_dates.csv"), na = "")
  .data$sample %>%
    readr::write_excel_csv(file = paste0(prefix, "_samples.csv"), na = "")
  .data$age_model %>%
    readr::write_excel_csv(file = paste0(prefix, "_age_model.csv"), na = "")
  .data$pollen_count$clean %>%
    readr::write_excel_csv(file = paste0(prefix, "_pollen_counts_clean.csv"))
  .data$pollen_count$intermediate %>%
    readr::write_excel_csv(file = paste0(prefix, "_pollen_counts_intermediate.csv"))
  .data$pollen_count$amalgamated %>%
    readr::write_excel_csv(file = paste0(prefix, "_pollen_counts_amalgamated.csv"))
  return(.data)
}
