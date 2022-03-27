#' Print Values
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @rdname print
#' @export
NULL

#' Print Values
#' @rdname print
#' @export
print.snapshot <- function(x, stats_only = TRUE, ...) {
  if (stats_only)
    return(print_stats(x))
  x
}

#' @keywords internal
extract_element <- function(x, element_name, IDs, ID_ENTITY = TRUE) {
  tryCatch({
    data <- x %>%
      purrr::pluck(element_name)
    # purrr::keep(names(.) == element_name,
    #             .default = tibble::tibble()) %>%
    if (ID_ENTITY)
      data <- dplyr::filter(data, ID_ENTITY %in% IDs)
    else
      data <- dplyr::filter(data, ID_SAMPLE %in% IDs)
  }, error = function(e) {
    return(tibble::tibble())
  })
  return(data)
}

#' @keywords internal
print_stats <- function(x) {
  tables <- names(x)
  id_entities <- unique(x$entity$ID_ENTITY)
  stats <- id_entities %>%
    purrr::map_df(function(ID) {
      entity <- extract_element(x, "entity", ID)
      dates <- extract_element(x, "date_info", ID)
      samples <- extract_element(x, "sample", ID)
      age_model <- extract_element(x, "age_model", samples$ID_SAMPLE, FALSE)
      pollen_counts <- extract_element(x, "pollen_count",
                                       samples$ID_SAMPLE, FALSE)
      pollen_counts_clean <- extract_element(pollen_counts,
                                             "clean",
                                             samples$ID_SAMPLE,
                                             FALSE)
      pollen_counts_intermediate <- extract_element(pollen_counts,
                                                    "intermediate",
                                                    samples$ID_SAMPLE,
                                                    FALSE)
      pollen_counts_amalgamated <- extract_element(pollen_counts,
                                                   "amalgamated",
                                                   samples$ID_SAMPLE,
                                                   FALSE)
      entity %>%
        dplyr::select(ID_SITE, ID_ENTITY, site_name, entity_name) %>%
        dplyr::bind_cols(
          tibble::tibble(
            dates = nrow(dates),
            samples = nrow(samples),
            age_model = nrow(age_model),
            pollen_counts = tibble::tibble(
              clean = nrow(pollen_counts_clean),
              intermediate = nrow(pollen_counts_intermediate),
              amalgamated = nrow(pollen_counts_amalgamated)
            )
          )
        )
    })
  print(stats)
  invisible(x)
}
