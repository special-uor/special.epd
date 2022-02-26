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
usethis::use_data(EPD_DATES, overwrite = TRUE, compress = "xz")
usethis::use_data(EPD_DATES_coretops, overwrite = TRUE, compress = "xz")
