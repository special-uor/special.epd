## code to prepare `EPD_COUNTS` dataset goes here
`%>%` <- magrittr::`%>%`
epd_counts_id <- "1T11ObJCl70751RbB7ce8N32lf0STDG-7"
epd_counts_tmp_file <- tempfile(fileext = ".csv")
googledrive::as_id(epd_counts_id) %>%
  googledrive::drive_download(path = epd_counts_tmp_file, overwrite = TRUE)
EPD_COUNTS <- epd_counts_tmp_file %>%
  readr::read_csv()
unlink(epd_counts_tmp_file)
col_names <- colnames(EPD_COUNTS) %>%
  stringr::str_replace_all("\\?Fungi", "Fungi") %>%
  stringr::str_replace_all("\\?Micro", "Micro")
EPD_COUNTS_2 <- EPD_COUNTS %>%
  magrittr::set_names(col_names) %>%
  dplyr::select(c(1:16, (order(col_names[-c(1:16)]) + 16))) #%>%
  # dplyr::rename(entity_name = handle)
# # EPD_COUNTS_2[1, c(1:16, (order(col_names[-c(1:16)]) + 16))]
#   # dplyr::select(dplyr::starts_with("?"))
#   #   magrittr::set_names(colnames(.) %>%
#   #                       stringr::str_replace_all("\\?Fungi", "Fungi") %>%
#   #                       stringr::str_replace_all("\\?Micro", "Micro")) %>%
#   # dplyr::select(dplyr::starts_with("?|Fungi"))
# a <- EPD_COUNTS %>%
#   dplyr::slice(1:100) %>%
#   # smpds::rm_zero_taxa(cols = 1:16) %>%
#   smpds::rm_na_taxa(cols = 1:16)
#
#
# b <- EPD_COUNTS_2[1, c(1:16, (order(col_names[-c(1:16)]) + 16))]
# waldo::compare(EPD_COUNTS[1, ], b)
#
# b <- seq_len(nrow(EPD_COUNTS)) %>%
#   purrr::map_df(~EPD_COUNTS[.x, ] %>% smpds::rm_na_taxa(cols = 1:16))
epd_duplicated_counts_inspected <-
  "~/Downloads/epd_duplicated_cores_counts_2022-02-11.xlsx" %>%
  readxl::read_excel() %>%
  dplyr::mutate(keep = ifelse(dataset_id %in% c(1440, 24968, 52503),
                              FALSE,
                              keep))

# epd_duplicated_cores_processed %>%
#   dplyr::distinct(dataset_id, .keep_all = TRUE) %>%
#   dplyr::filter(keep > 0)
EPD_COUNTS_3 <- EPD_COUNTS_2 %>%
  dplyr::filter(!(dataset_id %in% (epd_duplicated_counts_inspected %>%
                                     dplyr::filter(!keep) %>%
                                     .$dataset_id))) %>%
  dplyr::mutate(entity_name = entity_name %>%
                  stringr::str_remove_all("-ALT$"))
  # dplyr::mutate(entity_name = ifelse(dataset_id %in% c(1440, 24968, 52503), #c(45320, 52503, 24968),
  #                                    paste0(entity_name, "-ALT"),
  #                                    entity_name))
EPD_COUNTS <- EPD_COUNTS_3

# Drop records without counts
EPD_COUNTS_4_1 <- smpds::rm_na_taxa(EPD_COUNTS[c(1:16, 17:517)], cols = 1:16)
EPD_COUNTS_4_2 <- smpds::rm_na_taxa(EPD_COUNTS[c(1:16, 518:1017)], cols = 1:16)
EPD_COUNTS_4_3 <- smpds::rm_na_taxa(EPD_COUNTS[c(1:16, 1018:1517)], cols = 1:16)
EPD_COUNTS_4_4 <- smpds::rm_na_taxa(EPD_COUNTS[c(1:16, 1518:2017)], cols = 1:16)
EPD_COUNTS_4_5 <- smpds::rm_na_taxa(EPD_COUNTS[c(1:16, 2018:2517)], cols = 1:16)
EPD_COUNTS_4_6 <- smpds::rm_na_taxa(EPD_COUNTS[c(1:16, 2518:3017)], cols = 1:16)
EPD_COUNTS_4_7 <- smpds::rm_na_taxa(EPD_COUNTS[c(1:16, 3018:3517)], cols = 1:16)
EPD_COUNTS_4_8 <- smpds::rm_na_taxa(EPD_COUNTS[c(1:16, 3518:4017)], cols = 1:16)
EPD_COUNTS_4_9 <- smpds::rm_na_taxa(EPD_COUNTS[c(1:16, 4018:4517)], cols = 1:16)
EPD_COUNTS_4_10 <- smpds::rm_na_taxa(EPD_COUNTS[c(1:16, 4518:4842)], cols = 1:16)

EPD_COUNTS_NA_cols <- EPD_COUNTS %>%
  dplyr::select(c(
    colnames(EPD_COUNTS[c(1:16, 17:517)])[!colnames(EPD_COUNTS[c(1:16, 17:517)]) %in% colnames(EPD_COUNTS_4_1)],
    colnames(EPD_COUNTS[c(1:16, 518:1017)])[!colnames(EPD_COUNTS[c(1:16, 518:1017)]) %in% colnames(EPD_COUNTS_4_2)],
    colnames(EPD_COUNTS[c(1:16, 1018:1517)])[!colnames(EPD_COUNTS[c(1:16, 1018:1517)]) %in% colnames(EPD_COUNTS_4_3)],
    colnames(EPD_COUNTS[c(1:16, 1518:2017)])[!colnames(EPD_COUNTS[c(1:16, 1518:2017)]) %in% colnames(EPD_COUNTS_4_4)],
    colnames(EPD_COUNTS[c(1:16, 2018:2517)])[!colnames(EPD_COUNTS[c(1:16, 2018:2517)]) %in% colnames(EPD_COUNTS_4_5)],
    colnames(EPD_COUNTS[c(1:16, 2518:3017)])[!colnames(EPD_COUNTS[c(1:16, 2518:3017)]) %in% colnames(EPD_COUNTS_4_6)],
    colnames(EPD_COUNTS[c(1:16, 3018:3517)])[!colnames(EPD_COUNTS[c(1:16, 3018:3517)]) %in% colnames(EPD_COUNTS_4_7)],
    colnames(EPD_COUNTS[c(1:16, 3518:4017)])[!colnames(EPD_COUNTS[c(1:16, 3518:4017)]) %in% colnames(EPD_COUNTS_4_8)],
    colnames(EPD_COUNTS[c(1:16, 4018:4517)])[!colnames(EPD_COUNTS[c(1:16, 4018:4517)]) %in% colnames(EPD_COUNTS_4_9)],
    colnames(EPD_COUNTS[c(1:16, 4518:4842)])[!colnames(EPD_COUNTS[c(1:16, 4518:4842)]) %in% colnames(EPD_COUNTS_4_10)]
  ))

EPD_COUNTS_NA_cols %>%
  purrr::map_df(~.x %>% sum(na.rm = TRUE)) %>%
  purrr::flatten_dbl() %>% sum()

EPD_COUNTS_5 <- EPD_COUNTS %>%
  dplyr::select(-colnames(EPD_COUNTS_NA_cols))

tibble::tibble(epd_taxa = colnames(EPD_COUNTS_5),
               clean_name = colnames(EPD_COUNTS_5),
               action = NA) %>%
  dplyr::slice(-c(1:16)) %>%
  dplyr::arrange(epd_taxa) %>%
  readr::write_excel_csv(file = "inst/extdata/epd_taxa_list.csv", na = "")

EPD_COUNTS_5$`Calluna vulgaris|clump|pollen|NISP|TRSH` %>% sum(na.rm = TRUE)
EPD_COUNTS_5$`Calluna vulgaris|NA|pollen|NISP|TRSH` %>% sum(na.rm = TRUE)
EPD_COUNTS <- EPD_COUNTS_5
usethis::use_data(EPD_COUNTS, overwrite = TRUE, compress = "xz")

# Unique counts
unique_counts <-
  readr::read_csv("inst/extdata/epd_taxa_list.csv")

# Extra counts ----
extra_counts <-
  readr::read_csv("inst/extdata/epd_missing_records_counts_2022-02-24.csv")

extra_counts_v2 <- smpds::rm_na_taxa(extra_counts, cols = 1:16)

aux <- unique_counts %>%
  dplyr::filter((epd_taxa %in% colnames(extra_counts_v2)[-c(1:16)]))

idx <- !(colnames(extra_counts_v2)[-c(1:16)] %in% aux$epd_taxa)

tibble::tibble(epd_taxa = colnames(extra_counts_v2)[-c(1:16)][idx],
               clean_name = epd_taxa,
               action = NA) %>%
  dplyr::arrange(epd_taxa) %>%
  readr::write_excel_csv(file = "inst/extdata/epd_taxa_list_additional_records.csv", na = "")

readr::read_csv("inst/extdata/epd_taxa_list.csv") %>%
  dplyr::bind_rows(readr::read_csv("inst/extdata/epd_taxa_list_additional_records.csv")) %>%
  dplyr::arrange(epd_taxa) %>%
  readr::write_excel_csv(file = "inst/extdata/epd_taxa_list_2022-02-26.csv", na = "")

