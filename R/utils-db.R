#' Dump all the data linked to entities
#'
#' @param conn DB connection object.
#' @param ID_SITE Optional, if `ID_ENTITY` or `entity_name` are provided.
#' @param ID_ENTITY Optional, if `ID_SITE` or `entity_name` are provided.
#' @param entity_name Optional, if `ID_SITE` or `ID_ENTITY` are provided.
#' @param quiet Boolean flag to indicate if queries should be displayed.
#'
#' @return List with the individual tables.
#' @export
dump_all <- function(conn, ID_SITE, ID_ENTITY, entity_name, quiet = TRUE) {
  if (!missing(ID_SITE)) {
    .dump_all_by_site(conn, ID_SITE, quiet = quiet)
  } else if (!missing(ID_ENTITY)) {
    .dump_all_by_entity(conn, ID_ENTITY, quiet = quiet)
  } else if (!missing(entity_name)) {
    .dump_all_by_entity_name(conn, entity_name, quiet = quiet)
  } else {
    message("At least of the following is required:\n",
            "- ID_SITE\n- ID_ENTITY\n- entity_name\n")
  }
}

#' @keywords internal
.dump_all_by_site <- function(conn, ID_SITE, quiet = TRUE) {
  entity_tb <- conn %>%
    dabr::select("SELECT ID_ENTITY FROM entity WHERE ID_SITE IN (",
                 paste0(ID_SITE, collapse = ", "),
                 ")",
                 quiet = quiet)
  return(.dump_all_by_entity(conn, entity_tb$ID_ENTITY, quiet = quiet))
}

#' @keywords internal
.dump_all_by_entity <- function(conn, ID_ENTITY, quiet = TRUE) {
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
    model_name_tb <- dabr::select_all(conn, "model_name", quiet = TRUE)
    age_model_tb <- conn %>%
      dabr::select("SELECT * FROM age_model WHERE ID_SAMPLE IN (",
                   paste0(sample_tb$ID_SAMPLE, collapse = ", "),
                   ")",
                   quiet = quiet) %>%
      dplyr::left_join(model_name_tb, by = "ID_MODEL")
    taxon_name_tb <- dabr::select_all(conn, "taxon_name", quiet = TRUE)
    pollen_count_tb <- conn %>%
      dabr::select("SELECT * FROM pollen_count WHERE ID_SAMPLE IN (",
                   paste0(sample_tb$ID_SAMPLE, collapse = ", "),
                   ")",
                   quiet = quiet) %>%
      dplyr::left_join(taxon_name_tb, by = "ID_TAXON")
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
  )
}

#' @keywords internal
.dump_all_by_entity_name <- function(conn, entity_name, quiet = TRUE) {
  entity_tb <- conn %>%
    dabr::select("SELECT ID_ENTITY FROM entity WHERE entity_name IN (",
                 paste0(dabr::quote(entity_name), collapse = ", "),
                 ")",
                 quiet = quiet)
  return(.dump_all_by_entity(conn, entity_tb$ID_ENTITY, quiet = quiet))
}
