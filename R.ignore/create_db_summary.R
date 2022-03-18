special_epd <- dabr::select_all(conn, "entity")

special_epd_db_dump <- conn %>%
  special.epd::snapshot(entity_name = special_epd$entity_name)
special_epd_entities_with_dates <-
  special_epd_db_dump$date_info %>%
  dplyr::group_by(ID_ENTITY) %>%
  dplyr::summarise(n_dates = dplyr::n()) %>%
  dplyr::left_join(special_epd_db_dump$entity %>%
                     dplyr::select(1:4),
                   by = "ID_ENTITY") %>%
  dplyr::mutate(DATES = TRUE)

special_epd_entities_with_samples <-
  special_epd_db_dump$sample %>%
  # dplyr::distinct(ID_ENTITY) %>%
  dplyr::group_by(ID_ENTITY) %>%
  dplyr::summarise(n_samples = dplyr::n()) %>%
  dplyr::left_join(special_epd_db_dump$entity %>%
                     dplyr::select(1:4),
                   by = "ID_ENTITY") %>%
  dplyr::mutate(SAMPLES = TRUE)

special_epd_entities_with_am <-
  special_epd_db_dump$age_model %>%
  dplyr::distinct(ID_SAMPLE) %>%
  dplyr::left_join(special_epd_db_dump$sample,
                   by = "ID_SAMPLE") %>%
  dplyr::group_by(ID_ENTITY) %>%
  dplyr::summarise(n_am = dplyr::n()) %>%
  dplyr::left_join(special_epd_db_dump$entity %>%
                     dplyr::select(1:4),
                   by = "ID_ENTITY") %>%
  dplyr::mutate(AM = TRUE)

special_epd_entities_with_counts_0 <-
  special_epd_db_dump$pollen_count$clean %>%
  dplyr::distinct(ID_SAMPLE) %>%
  dplyr::left_join(special_epd_db_dump$sample,
                   by = "ID_SAMPLE") %>%
  dplyr::group_by(ID_ENTITY) %>%
  dplyr::summarise(n_counts_0 = dplyr::n()) %>%
  dplyr::left_join(special_epd_db_dump$entity %>%
                     dplyr::select(1:4),
                   by = "ID_ENTITY") %>%
  dplyr::mutate(COUNTS_0 = TRUE)
special_epd_entities_with_counts_1 <-
  special_epd_db_dump$pollen_count$intermediate %>%
  dplyr::distinct(ID_SAMPLE) %>%
  dplyr::left_join(special_epd_db_dump$sample,
                   by = "ID_SAMPLE") %>%
  dplyr::group_by(ID_ENTITY) %>%
  dplyr::summarise(n_counts_1 = dplyr::n()) %>%
  dplyr::left_join(special_epd_db_dump$entity %>%
                     dplyr::select(1:4),
                   by = "ID_ENTITY") %>%
  dplyr::mutate(COUNTS_1 = TRUE)
special_epd_entities_with_counts_2 <-
  special_epd_db_dump$pollen_count$amalgamated %>%
  dplyr::distinct(ID_SAMPLE) %>%
  dplyr::left_join(special_epd_db_dump$sample,
                   by = "ID_SAMPLE") %>%
  dplyr::group_by(ID_ENTITY) %>%
  dplyr::summarise(n_counts_2 = dplyr::n()) %>%
  dplyr::left_join(special_epd_db_dump$entity %>%
                     dplyr::select(1:4),
                   by = "ID_ENTITY") %>%
  dplyr::mutate(COUNTS_2 = TRUE)


special_epd %>%
  dplyr::select(-site_name) %>%
  dplyr::left_join(special_epd_entities_with_dates) %>%
  dplyr::left_join(special_epd_entities_with_samples) %>%
  dplyr::left_join(special_epd_entities_with_am) %>%
  dplyr::left_join(special_epd_entities_with_counts_0) %>%
  dplyr::left_join(special_epd_entities_with_counts_1) %>%
  dplyr::left_join(special_epd_entities_with_counts_2) %>%
  dplyr::mutate(has_DATES = ifelse(is.na(DATES), FALSE, DATES),
                has_SAMPLES = ifelse(is.na(SAMPLES), FALSE, SAMPLES),
                has_AM = ifelse(is.na(AM), FALSE, AM),
                COUNTS_0 = ifelse(is.na(COUNTS_0), FALSE, COUNTS_0),
                COUNTS_1 = ifelse(is.na(COUNTS_1), FALSE, COUNTS_1),
                COUNTS_2 = ifelse(is.na(COUNTS_2), FALSE, COUNTS_2),
                has_COUNTS = COUNTS_0 & COUNTS_1 & COUNTS_2) %>%
  dplyr::left_join(epd_age_models %>%
                     dplyr::select(-site_id, -site_name_clean)) %>%
  dplyr::relocate(n_dates, n_samples, n_am,
                  .before = has_DATES) %>%
  readr::write_excel_csv("~/Downloads/special-epd_summary.csv")
