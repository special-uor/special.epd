# code to summarise list of categorical values
`%>%` <- magrittr::`%>%`
conn <- dabr::open_conn_mysql("special-epd",
                              password = rstudioapi::askForPassword())
#### date_info ----
date_info <- dabr::select_all(conn, "date_info") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("date_info", class(.)))
date_info_enumerates <- date_info %>%
  dplyr::select(date_type, material_dated, age_used, reason_age_not_used) %>%
  purrr::map_df(~tibble::tibble(current = .x %>% unique() %>% sort(),
                                new = current %>% stringr::str_squish()),
                .id = "variable") %>%
  dplyr::arrange(variable, current)
date_info_enumerates

##### Replace NAs ----
date_info %>%
  dplyr::filter(is.na(age_used) |
                  age_used %>% stringr::str_detect("unknown")) %>%
  dplyr::select(ID_ENTITY, ID_DATE_INFO, age_used) %>%
  dplyr::mutate(age_used = "not known") %>%
  rpd:::update_records(conn = conn, table = "date_info",
                       dry_run = TRUE, PK = 1:2)
# Results: No records were updated.

date_info %>%
  dplyr::filter(is.na(date_type) |
                  date_type %>% stringr::str_detect("unknown")) %>%
  dplyr::select(ID_ENTITY, ID_DATE_INFO, date_type) %>%
  dplyr::mutate(date_type = "not known") %>%
  rpd:::update_records(conn = conn, table = "date_info",
                       dry_run = TRUE, PK = 1:2)
# Results: 14 records were updated.

date_info %>%
  dplyr::filter(is.na(material_dated) |
                  material_dated %>% stringr::str_detect("unknown")) %>%
  dplyr::select(ID_ENTITY, ID_DATE_INFO, material_dated) %>%
  dplyr::mutate(material_dated = "not known") %>%
  rpd:::update_records(conn = conn, table = "date_info",
                       dry_run = TRUE, PK = 1:2)
# Results: 106 records were updated.

## PENDING
date_info %>%
  dplyr::filter(is.na(reason_age_not_used) |
                  reason_age_not_used %>% stringr::str_detect("unknown")) %>%
  dplyr::select(ID_ENTITY, ID_DATE_INFO, reason_age_not_used) %>%
  dplyr::mutate(reason_age_not_used = "not applicable") %>%
  rpd:::update_records(conn = conn, table = "date_info",
                       dry_run = TRUE, PK = 1:2)
# Results:  records were updated.

#### entity ----
entity <- dabr::select_all(conn, "entity") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("entity", class(.)))
entity_enumerates <- entity %>%
  dplyr::select(site_type, source) %>%
  purrr::map_df(~tibble::tibble(current = .x %>% unique() %>% sort(),
                                new = current %>% stringr::str_squish()),
                .id = "variable") %>%
  dplyr::arrange(variable, current)

##### Replace NAs ----
entity %>%
  dplyr::filter(is.na(site_type) |
                  site_type %>% stringr::str_detect("unknown")) %>%
  dplyr::select(ID_SITE, ID_ENTITY, site_type) %>%
  dplyr::mutate(site_type = "not known") %>%
  rpd:::update_records(conn = conn, table = "entity",
                       dry_run = TRUE, PK = 1:2)
# Results: 63 records were updated.

entity %>%
  dplyr::filter(is.na(source) |
                  source %>% stringr::str_detect("unknown")) %>%
  dplyr::select(ID_SITE, ID_ENTITY, source) %>%
  dplyr::mutate(source = "not known") %>%
  rpd:::update_records(conn = conn, table = "entity",
                       dry_run = TRUE, PK = 1:2)
# Results: No records were updated.

#### sample ----
sample <- conn %>%
  dabr::select_all("sample") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("sample", class(.))) %>%
  dplyr::select(-dplyr::starts_with("depth_"))
sample_enumerates <- sample %>%
  dplyr::select(age_type, count_type, sample_type) %>%
  purrr::map_df(~tibble::tibble(current = .x %>% unique() %>% sort(),
                                new = current %>% stringr::str_squish()),
                .id = "variable") %>%
  dplyr::arrange(variable, current)
sample_enumerates

##### Replace NAs ----
sample %>%
  dplyr::filter(is.na(age_type) |
                  age_type %>% stringr::str_detect("unknown")) %>%
  dplyr::select(ID_ENTITY, ID_SAMPLE, age_type) %>%
  dplyr::mutate(age_type = "not known") %>%
  rpd:::update_records(conn = conn, table = "sample",
                       dry_run = !TRUE, PK = 1:2)
# Results: 15381 records were updated.

#### Export workbook ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "date_info")
openxlsx::writeData(wb, "date_info", date_info_enumerates)
openxlsx::addWorksheet(wb, "entity")
openxlsx::writeData(wb, "entity", entity_enumerates)
openxlsx::addWorksheet(wb, "sample")
openxlsx::writeData(wb, "sample", sample_enumerates)
openxlsx::saveWorkbook(wb,
                       paste0("inst/extdata/epd_enumerates_list_",
                              Sys.Date(),
                              ".xlsx"))

# Load revised enumerates ----
## date_info ----
date_info_enumerates_sph <-
  readxl::read_excel("inst/extdata/epd_enumerates_list_2022-04-04_SPH.xlsx",
                     sheet = 1)

date_info_2 <- dabr::select_all(conn, "date_info") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("date_info", class(.)))

##### age_used ----
date_info_2 %>%
  dplyr::filter(stringr::str_detect(age_used, "No")) %>%
  dplyr::select(ID_ENTITY, ID_DATE_INFO, age_used) %>%
  dplyr::mutate(age_used = "no") %>%
  rpd:::update_records(conn = conn, table = "date_info",
                       dry_run = !TRUE, PK = 1:2)
# Results: 1 record was updated.

## entity ----
entity_enumerates_sph <-
  readxl::read_excel("inst/extdata/epd_enumerates_list_2022-04-04_SPH.xlsx",
                     sheet = 2)

# Close database connection
dabr::close_conn(conn)
