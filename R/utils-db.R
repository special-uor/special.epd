#' Create a snapshot of the data linked to entities
#'
#' Create a snapshot of the data linked to entities. Including metadata,
#' dates, samples, age models and pollen counts.
#'
#' @param conn DB connection object.
#' @param ID_SITE Optional, if `ID_ENTITY` or `entity_name` are provided.
#' @param ID_ENTITY Optional, if `ID_SITE` or `entity_name` are provided.
#' @param entity_name Optional, if `ID_SITE` or `ID_ENTITY` are provided.
#' @param quiet Boolean flag to indicate if queries should be displayed.
#'
#' @return List with the individual tables.
#' @export
snapshot <- function(conn, ID_SITE, ID_ENTITY, entity_name, quiet = TRUE) {
  if (!missing(ID_SITE)) {
    .snapshot_by_site(conn, ID_SITE, quiet = quiet)
  } else if (!missing(ID_ENTITY)) {
    .snapshot_by_entity(conn, ID_ENTITY, quiet = quiet)
  } else if (!missing(entity_name)) {
    .snapshot_by_entity_name(conn, entity_name, quiet = quiet)
  } else {
    message("At least of the following is required:\n",
            "- ID_SITE\n- ID_ENTITY\n- entity_name\n")
  }
}

#' @keywords internal
.snapshot_by_site <- function(conn, ID_SITE, quiet = TRUE) {
  entity_tb <- conn %>%
    dabr::select("SELECT ID_ENTITY FROM entity WHERE ID_SITE IN (",
                 paste0(ID_SITE, collapse = ", "),
                 ")",
                 quiet = quiet)
  return(.snapshot_by_entity(conn, entity_tb$ID_ENTITY, quiet = quiet))
}

#' @keywords internal
.snapshot_by_entity <- function(conn, ID_ENTITY, quiet = TRUE) {
  entity_tb <- conn %>%
    dabr::select("SELECT * FROM entity WHERE ID_ENTITY IN (",
                 paste0(ID_ENTITY, collapse = ", "),
                 ")",
                 quiet = quiet)
  if (nrow(entity_tb) == 0) {
    message("No records were found!")
    return(NULL)
  }
  date_info_tb <- conn %>%
    dabr::select("SELECT * FROM date_info WHERE ID_ENTITY IN (",
                 paste0(ID_ENTITY, collapse = ", "),
                 ")",
                 quiet = quiet)
  sample_tb <- conn %>%
    dabr::select("SELECT * FROM sample WHERE ID_ENTITY IN (",
                 paste0(ID_ENTITY, collapse = ", "),
                 ")",
                 quiet = quiet)
  if (nrow(sample_tb) > 0) {
    tryCatch({
      model_name_tb <- dabr::select_all(conn, "model_name", quiet = TRUE)
      age_model_tb <- conn %>%
        dabr::select("SELECT * FROM age_model WHERE ID_SAMPLE IN (",
                     paste0(sample_tb$ID_SAMPLE, collapse = ", "),
                     ")",
                     quiet = quiet) %>%
        dplyr::left_join(model_name_tb, by = "ID_MODEL")
    }, error = function(e) {
      age_model_tb <- NULL
    })
    tryCatch({
      taxon_name_tb <- dabr::select_all(conn, "taxon_name", quiet = TRUE)
      pollen_count_tb <- conn %>%
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
.snapshot_by_entity_name <- function(conn, entity_name, quiet = TRUE) {
  entity_tb <- conn %>%
    dabr::select("SELECT ID_ENTITY FROM entity WHERE entity_name IN (",
                 paste0(dabr::quote(entity_name), collapse = ", "),
                 ")",
                 quiet = quiet)
  return(.snapshot_by_entity(conn, entity_tb$ID_ENTITY, quiet = quiet))
}

#' Write DB snapshot to disk
#' Write DB snapshot to disk as individual CSV files.
#'
#' @param x DB snapshot (object of `special.epd` class).
#' @param prefix String with a prefix path where the data should be stored.
#'
#' @return Invisibly returns the input DB snapshot.
#' @export
write_csvs <- function(x, prefix) {
  if (!("special.epd" %in% class(x)))
    stop("The given object does not look like a valid snapshot from the ",
         "`SPECIAL-EPD database. Try using the function `snapshot` first.",
         call. = FALSE)
  if (!dir.exists(dirname(prefix)))
    stop("The provided directory, `", dirname(prefix), "`, does not exist.",
         call. = FALSE)
  x$entity %>%
    readr::write_excel_csv(file = paste0(prefix, "_metadata.csv"), na = "")
  x$date_info %>%
    readr::write_excel_csv(file = paste0(prefix, "_dates.csv"), na = "")
  x$sample %>%
    readr::write_excel_csv(file = paste0(prefix, "_samples.csv"), na = "")
  x$age_model %>%
    readr::write_excel_csv(file = paste0(prefix, "_age_model.csv"), na = "")
  x$pollen_count$clean %>%
    readr::write_excel_csv(file = paste0(prefix, "_pollen_counts_clean.csv"))
  x$pollen_count$intermediate %>%
    readr::write_excel_csv(file = paste0(prefix, "_pollen_counts_intermediate.csv"))
  x$pollen_count$amalgamated %>%
    readr::write_excel_csv(file = paste0(prefix, "_pollen_counts_amalgamated.csv"))
  return(invisible(x))
}
