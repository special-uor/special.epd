#' Get entities from the entity table
#'
#' @param conn DB connection object.
#' @param ID_ENTITY Numeric vector with IDs linked to entities.
#'
#' @return Tibble with entities and external links.
#' @export
get_entity <- function(conn, ID_ENTITY) {
  conn %>%
    dabr::select("SELECT * FROM entity",
                 "RIGHT JOIN external_link ON",
                 "entity.ID_SITE = external_link.ID_SITE AND",
                 "entity.ID_ENTITY = external_link.ID_ENTITY",
                 "WHERE entity.ID_ENTITY IN (",
                 paste0(ID_ENTITY, collapse = ", "),
                 ")") %>%
    dplyr::select(-dplyr::contains(".."))
    # dabr::select_all("entity") %>%
    # dplyr::filter(ID_ENTITY %in% !!ID_ENTITY) %>%
    # dplyr::left_join(dabr::select_all(conn, "external_link"))
}

# get_entity(conn, 780)

# EPD_COUNTS[-c(1:16)] %>% purrr::map_df(~.x %>% purrr::map_dbl(~ifelse(is.na(.x) | .x == 0, 0, 1)))
# smpds::rm_na_taxa(EPD_COUNTS[c(1:16, 17:517)], cols = 1:16)
# EPD_COUNTS[17:117] %>% purrr::map_df(~.x %>% purrr::map_dbl(~ifelse(is.na(.x) | .x == 0, 0, 1)))
# EPD_COUNTS[118:217] %>% purrr::map_df(~.x %>% purrr::map_dbl(~ifelse(is.na(.x) | .x == 0, 0, 1)))
