dim(entity)
IDs <- entity$ID_ENTITY
ID_ENTITY_dates <- date_info %>%
  dplyr::filter(ID_ENTITY %in% IDs) %>%
  dplyr::distinct(ID_ENTITY) %>%
  purrr::flatten_dbl()
length(ID_ENTITY_dates)
ID_SAMPLES <- sample %>%
  dplyr::filter(ID_ENTITY %in% IDs) %>%
  dplyr::distinct(ID_ENTITY, ID_SAMPLE) %>%
  dplyr::arrange()
ID_ENTITY_samples <- unique(ID_SAMPLES$ID_ENTITY)
length(ID_ENTITY_samples)
ID_ENTITY_age_model <- age_model %>%
  dplyr::filter(ID_SAMPLE %in% ID_SAMPLES$ID_SAMPLE) %>%
  dplyr::left_join(ID_SAMPLES) %>%
  dplyr::distinct(ID_ENTITY) %>%
  purrr::flatten_dbl()
length(ID_ENTITY_age_model)
ID_ENTITY_pollen_count <- pollen_count %>%
  dplyr::filter(ID_SAMPLE %in% ID_SAMPLES$ID_SAMPLE) %>%
  dplyr::left_join(ID_SAMPLES) %>%
  dplyr::distinct(ID_ENTITY) %>%
  purrr::flatten_dbl()
length(ID_ENTITY_pollen_count)

ID_ENTITY_all <- IDs %>%
  intersect(ID_ENTITY_dates) %>%
  intersect(ID_ENTITY_samples) %>%
  intersect(ID_ENTITY_age_model) %>%
  intersect(ID_ENTITY_pollen_count)
length(ID_ENTITY_all)

sn <- conn %>%
  special.epd::dump_all(ID_ENTITY = ID_ENTITY_all) %>%
  special.epd::write_csvs("~/Downloads/special-epd_2022-03-13")
