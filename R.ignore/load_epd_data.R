`%>%` <- magrittr::`%>%`
# Load EPD files
data_raw_path <- "~/Downloads/SPECIAL-EPD/raw data"
EPD_COUNTS <-
  file.path(data_raw_path,
            "epd-records-extracted-from-neotoma_counts_2022-02-08.csv") %>%
  readr::read_csv()

EPD_DATES <-
  file.path(data_raw_path,
            "epd-records-extracted-from-neotoma_dates_2022-02-08_SPH.xlsx") %>%
  readxl::read_excel() %>%
  janitor::clean_names()

EPD_DATES_coretops <-
  file.path(data_raw_path,
            "epd-records-extracted-from-neotoma_dates_2022-02-08_SPH.xlsx") %>%
  readxl::read_excel(sheet = 2) %>%
  janitor::clean_names() %>%
  dplyr::filter(!is.na(depth_cm))

EPD_METADATA <-
  file.path(data_raw_path,
            "epd-records-extracted-from-neotoma_metadata_2022-02-08.csv") %>%
  readr::read_csv() %>%
  janitor::clean_names()
