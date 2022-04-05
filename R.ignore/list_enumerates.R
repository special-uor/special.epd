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

date_info %>%
  dplyr::filter(is.na(reason_age_not_used) |
                  reason_age_not_used %>% stringr::str_detect("unknown")) %>%
  dplyr::select(ID_ENTITY, ID_DATE_INFO, age_used, reason_age_not_used) %>%
  dplyr::mutate(reason_age_not_used = ifelse(is.na(age_used) |
                                               age_used == "no",
                                             "not known",
                                             "not applicable")) %>%
  rpd:::update_records(conn = conn, table = "date_info",
                       dry_run = TRUE, PK = 1:2)
# Results: 8754 records were updated.

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
                       dry_run = TRUE, PK = 1:2)
# Results: 1 record was updated.

##### date_type ----
date_info_enumerates_sph_date_type <-
  date_info_enumerates_sph %>%
  dplyr::filter(variable == "date_type",
                current != new)
date_info_2 %>%
  dplyr::filter(date_type %in% date_info_enumerates_sph_date_type$current) %>%
  dplyr::select(ID_ENTITY, ID_DATE_INFO, date_type) %>%
  dplyr::left_join(date_info_enumerates_sph_date_type,
                   by = c("date_type" = "current")) %>%
  dplyr::rename(date_type_old = date_type,
                date_type = new) %>%
  dplyr::select(ID_ENTITY, ID_DATE_INFO, date_type) %>%
  rpd:::update_records(conn = conn, table = "date_info",
                       dry_run = TRUE, PK = 1:2)
# Results: 455 record was updated.

##### material_dated ----
date_info_enumerates_sph_material_dated <-
  date_info_enumerates_sph %>%
  dplyr::filter(variable == "material_dated",
                current != new)
date_info_2 %>%
  dplyr::filter(material_dated %in%
                  date_info_enumerates_sph_material_dated$current) %>%
  dplyr::select(ID_ENTITY, ID_DATE_INFO, material_dated) %>%
  dplyr::left_join(date_info_enumerates_sph_material_dated,
                   by = c("material_dated" = "current")) %>%
  # dplyr::filter(is.na(new))
  dplyr::rename(material_dated_old = material_dated,
                material_dated = new) %>%
  dplyr::select(ID_ENTITY, ID_DATE_INFO, material_dated) %>%
  rpd:::update_records(conn = conn, table = "date_info",
                       dry_run = TRUE, PK = 1:2)
# Results: 375 record was updated.

##### reason_age_not_used ----
date_info_enumerates_sph_reason_age_not_used <-
  date_info_enumerates_sph %>%
  dplyr::filter(variable == "reason_age_not_used",
                current != new | stringr::str_detect(SPH, "notes|reason")) %>%
  dplyr::mutate(new_notes = ifelse(stringr::str_detect(SPH, "notes"),
                                   new,
                                   NA),
                new_age_used = ifelse(stringr::str_detect(SPH, "reason"),
                                      "no",
                                      NA),
                new_reason = ifelse(is.na(new_notes),
                                    ifelse(is.na(new_age_used),
                                           new,
                                           NA),
                                    NA))
date_info_2 %>%
  dplyr::filter(reason_age_not_used %in%
                  date_info_enumerates_sph_reason_age_not_used$current) %>%
  dplyr::select(ID_ENTITY,
                ID_DATE_INFO,
                age_used_old = age_used,
                reason_age_not_used,
                notes_old = notes) %>%
  dplyr::left_join(date_info_enumerates_sph_reason_age_not_used,
                   by = c("reason_age_not_used" = "current")) %>%
  # dplyr::filter(is.na(new))
  dplyr::rename(reason_age_not_used_old = reason_age_not_used,
                reason_age_not_used = new_reason,
                notes = new_notes,
                age_used = new_age_used) %>%
  dplyr::select(ID_ENTITY,
                ID_DATE_INFO,
                # age_used_old,
                # age_used,
                reason_age_not_used_old,
                reason_age_not_used,
                notes_old,
                notes) %>%
  dplyr::mutate(notes = dplyr::coalesce(notes_old, notes)) %>%
  dplyr::select(-dplyr::contains("_old")) %>%
  rpd:::update_records(conn = conn, table = "date_info",
                       dry_run = !TRUE, PK = 1:2)
# Results: 289 record was updated.

## entity ----
entity_enumerates_sph <-
  readxl::read_excel("inst/extdata/epd_enumerates_list_2022-04-04_SPH.xlsx",
                     sheet = 2)
entity_2 <- dabr::select_all(conn, "entity") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("entity", class(.)))

##### site_type ----
entity_enumerates_sph_site_type <-
  entity_enumerates_sph %>%
  dplyr::filter(variable == "site_type",
                current != new)
entity_2 %>%
  dplyr::filter(site_type %in%
                  entity_enumerates_sph_site_type$current) %>%
  dplyr::select(ID_SITE, ID_ENTITY, site_type) %>%
  dplyr::left_join(entity_enumerates_sph_site_type,
                   by = c("site_type" = "current")) %>%
  # dplyr::filter(is.na(new))
  dplyr::rename(site_type_old = site_type,
                site_type = new) %>%
  dplyr::select(ID_SITE, ID_ENTITY, site_type) %>%
  rpd:::update_records(conn = conn, table = "entity",
                       dry_run = TRUE, PK = 1:2)
# Results: 66 record was updated.

##### source ----
entity_enumerates_sph_source <-
  entity_enumerates_sph %>%
  dplyr::filter(variable == "source",
                current != new)
entity_2 %>%
  dplyr::filter(source %in%
                  entity_enumerates_sph_source$current) %>%
  dplyr::select(ID_SITE, ID_ENTITY, source) %>%
  dplyr::left_join(entity_enumerates_sph_source,
                   by = c("source" = "current")) %>%
  # dplyr::filter(is.na(new))
  dplyr::rename(source_old = source,
                source = new) %>%
  dplyr::select(ID_SITE, ID_ENTITY, source) %>%
  rpd:::update_records(conn = conn, table = "entity",
                       dry_run = TRUE, PK = 1:2)
# Results: 15 record was updated.

## sample ----
sample_enumerates_sph <-
  readxl::read_excel("inst/extdata/epd_enumerates_list_2022-04-04_SPH.xlsx",
                     sheet = 3)
sample_2 <- dabr::select_all(conn, "sample") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("sample", class(.)))

##### sample_type ----
sample_enumerates_sph_sample_type <-
  sample_enumerates_sph %>%
  dplyr::filter(variable == "sample_type",
                current != new)
sample_2 %>%
  dplyr::filter(sample_type %in%
                  sample_enumerates_sph_sample_type$current) %>%
  dplyr::select(ID_ENTITY, ID_SAMPLE, sample_type) %>%
  dplyr::left_join(sample_enumerates_sph_sample_type,
                   by = c("sample_type" = "current")) %>%
  # dplyr::filter(is.na(new))
  dplyr::rename(sample_type_old = sample_type,
                sample_type = new) %>%
  dplyr::select(ID_ENTITY, ID_SAMPLE, sample_type) %>%
  rpd:::update_records(conn = conn, table = "sample",
                       dry_run = TRUE, PK = 1:2)
# Results: 1102 record was updated.

# Close database connection
dabr::close_conn(conn)
