EPD_COUNTS <-
  file.path(data_raw_path,
            "epd-records-extracted-from-neotoma_counts_2022-01-27.csv") %>%
  readr::read_csv()
EPD_COUNTS %>%
  magrittr::set_names(c(colnames(.)[1:15] %>%
                          stringr::str_replace_all("\\.", "\\_"),
                        colnames(.)[-c(1:15)])) %>%
  dplyr::mutate(site_name_clean = site_name %>%
                  purrr::map_chr(ageR:::cln_str),
                .after = site_name) %>%
  readr::write_csv("~/Downloads/raw data/epd-records-extracted-from-neotoma_counts_2022-02-08.csv", na = "")

EPD_DATES %>%
  dplyr::mutate(site_name_clean = site_name %>%
                  purrr::map_chr(ageR:::cln_str),
                .after = site_name) %>%
  readr::write_csv("~/Downloads/raw data/epd-records-extracted-from-neotoma_dates_2022-02-08.csv", na = "")
EPD_METADATA %>%
  dplyr::mutate(site_name_clean = site_name %>%
                  purrr::map_chr(ageR:::cln_str),
                .after = site_name) %>%
  readr::write_csv("~/Downloads/raw data/epd-records-extracted-from-neotoma_metadata_2022-02-08.csv", na = "")
