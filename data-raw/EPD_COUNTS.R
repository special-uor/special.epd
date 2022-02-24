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
  dplyr::select(c(1:16, (order(col_names[-c(1:16)]) + 16))) %>%
  dplyr::rename(entity_name = handle)
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
usethis::use_data(EPD_COUNTS, overwrite = TRUE, compress = "xz")
