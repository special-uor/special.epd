## code to prepare `EPD_DATES` dataset goes here
`%>%` <- magrittr::`%>%`
epd_dates_id <- "1DqRuJ7hdPqrWYMzlW0ujs1P-tBVOQmBt"
epd_dates_tmp_file <- tempfile(fileext = ".xslx")
googledrive::as_id(epd_dates_id) %>%
  googledrive::drive_download(path = epd_dates_tmp_file, overwrite = TRUE)
EPD_DATES <- epd_dates_tmp_file %>%
  readxl::read_excel(sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::rename(depth = depth_cm,
                thickness = thickness_cm)
EPD_DATES_coretops <- epd_dates_tmp_file %>%
  readxl::read_excel(sheet = 2) %>%
  janitor::clean_names() %>%
  dplyr::filter(!is.na(depth_cm)) %>%
  dplyr::rename(depth = depth_cm,
                thickness = thickness_cm)
unlink(epd_dates_tmp_file)
EPD_METADATA %>%
  dplyr::select(ID_SITE, ID_ENTITY, site_id, entity_name) %>%
  dplyr::right_join(EPD_DATES,
                    by = c("site_id", "entity_name"))

EPD_DATES_coretops <- EPD_DATES_coretops %>%
  dplyr::mutate(date_type = dplyr::case_when(
    date_type %>% stringr::str_detect("core top estimated") ~
      "Top of core estimated",
    date_type %>% stringr::str_detect("core estimated") ~
      "Top of core estimated",
    date_type %>% stringr::str_detect("core top known") ~
      "Top of core known",
    date_type %>% stringr::str_detect("core known") ~
      "Top of core known",
    date_type %>% stringr::str_detect("isotopic correlation") ~
      "Isotopic correlation",
    date_type %>% stringr::str_detect("pollen correlation") ~
      "Pollen correlation",
    date_type %>% stringr::str_detect("stratigraphic correlation") ~
      "Stratigraphic correlation",
    TRUE ~ as.character(date_type)
  ))

EPD_DATES_2 <- EPD_DATES %>%
  dplyr::filter(!is.na(entity_name)) %>%
  dplyr::bind_rows(EPD_DATES_coretops) %>%
  dplyr::distinct() %>%
  dplyr::arrange(entity_name, depth, age_c14, age_cal)
  # dplyr::group_by(entity_name, lab_num, depth) %>%
  # dplyr::mutate(n = length(entity_name)) %>%
  # dplyr::filter(n > 1)

EPD_DATES_2 <- EPD_DATES
usethis::use_data(EPD_DATES, overwrite = TRUE, compress = "xz")
usethis::use_data(EPD_DATES_coretops, overwrite = TRUE, compress = "xz")
