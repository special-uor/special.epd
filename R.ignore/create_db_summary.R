`%>%` <- special.epd::`%>%`
# Create snapshot ----
special_epd_db_dump <- special.epd::snapshot() %>%
  special.epd::write_csvs(paste0("~/Downloads/special_epd_", Sys.Date()))
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


special_epd_entities_without_am <-
  "~/Downloads/special-epd_entities_without_age_models.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::select(-c(1:7, 9:10))

special_epd_summary <- special_epd_db_dump$entity %>%
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
  dplyr::left_join(special_epd_entities_without_am) %>%
  # dplyr::left_join(epd_age_models %>%
  #                    dplyr::select(-site_id, -site_name_clean)) %>%
  dplyr::mutate(n_counts = list(n_counts_0, n_counts_1, n_counts_2) %>%
                  purrr::pmap_dbl(function(n_counts_0, n_counts_1, n_counts_2) {
                    unique(c(n_counts_0, n_counts_1, n_counts_2))
                  })) %>%
  dplyr::relocate(n_dates, n_samples, n_am, n_counts,
                  .before = has_DATES) %>%
  dplyr::relocate(site_name, .before = entity_name) %>%
  # dplyr::filter(n_counts_0 != n_counts_1 | n_counts_1 != n_counts_2)
  dplyr::select(-DATES, -SAMPLES, -AM, -COUNTS_0, -COUNTS_1, -COUNTS_2) %>%
  dplyr::select(-n_counts_0, -n_counts_1, -n_counts_2)

# Store summary table ----
## CSV ----
special_epd_summary %>%
  readr::write_excel_csv(paste0("~/Downloads/special-epd_summary_",
                                Sys.Date(),
                                ".csv"),
                         na = "")

## Excel ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "summary")
openxlsx::writeData(wb, "summary", special_epd_summary)
false_cols <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
true_cols <- openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
openxlsx::conditionalFormatting(wb,
                                sheet = "summary",
                                cols = 16:19,
                                rows = seq_len(nrow(special_epd_summary) + 1),
                                type = "contains",
                                rule = "TRUE",
                                style = true_cols)
openxlsx::conditionalFormatting(wb,
                                sheet = "summary",
                                cols = 16:19,
                                rows = seq_len(nrow(special_epd_summary) + 1),
                                type = "contains",
                                rule = "FALSE",
                                style = false_cols)
openxlsx::saveWorkbook(wb, paste0("~/Downloads/special-epd_summary_",
                                  Sys.Date(),
                                  ".xlsx"))
