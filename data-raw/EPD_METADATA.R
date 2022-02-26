## code to prepare `EPD_METADATA` dataset goes here
`%>%` <- magrittr::`%>%`
epd_metadata_id <- "1RyoM06ftmY5UjD6redWhBoCo1ZQt__fd"
epd_metadata_tmp_file <- tempfile(fileext = ".csv")
googledrive::as_id(epd_metadata_id) %>%
  googledrive::drive_download(path = epd_metadata_tmp_file, overwrite = TRUE)
EPD_METADATA <- epd_metadata_tmp_file %>%
  readr::read_csv()
# EPD_METADATA <- "~/Downloads/epd-records-extracted-from-neotoma_metadata_2022-02-08.csv" %>%
#   readr::read_csv()
unlink(epd_metadata_tmp_file)

EPD_METADATA %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(n = length(entity_name)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > 1)

EPD_METADATA_2 <- EPD_METADATA %>%
  dplyr::mutate(longitude = ifelse(entity_name == "STUPH-1",
                                   11.666833,
                                   longitude),
                latitude = ifelse(entity_name == "STUPH-1",
                                  78.960556,
                                  latitude),
                longitude = ifelse(entity_name == "STUPH-2",
                                   11.596944,
                                   longitude),
                latitude = ifelse(entity_name == "STUPH-2",
                                  78.960833,
                                  latitude)) %>%
  dplyr::mutate(latitude = round(latitude, digits = 6),
                longitude = round(longitude, digits = 6)) %>%
  dplyr::select(-dplyr::starts_with("dataset_type")) %>%
  dplyr::arrange(site_name, entity_name)

### Find duplicated cores ----
epd_duplicated_cores <- EPD_METADATA_2 %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(n = length(site_id)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > 1)
epd_duplicated_cores_processed <- "~/Downloads/epd_duplicated_cores_meta.xlsx" %>%
  readxl::read_excel() %>%
  dplyr::select(1:13)

# New subset without duplicated sites
EPD_METADATA_3 <- EPD_METADATA_2 %>%
  dplyr::filter(!(dataset_id %in% epd_duplicated_cores_processed$dataset_id)) %>%
  dplyr::bind_rows(
    epd_duplicated_cores_processed %>%
      dplyr::filter(keep > 0) %>%
      dplyr::select(-keep)
  ) %>%
  dplyr::arrange(site_name, entity_name)

# epd_duplicated_cores %>%
#   readr::write_excel_csv("~/Downloads/epd_duplicated_cores_meta.csv", na = "")
epd_duplicated_dates <- EPD_DATES %>%
  dplyr::filter(entity_name %in% epd_duplicated_cores$entity_name) %>%
  dplyr::arrange(entity_name, depth, lab_num)
# epd_duplicated_dates %>%
#   readr::write_excel_csv("~/Downloads/epd_duplicated_cores_dates.csv", na = "")
epd_duplicated_counts <- EPD_COUNTS_2 %>%
  dplyr::filter(entity_name %in% epd_duplicated_cores$entity_name) %>%
  dplyr::arrange(entity_name, depth) %>%
  smpds::rm_na_taxa(1:16)
# epd_duplicated_counts %>%
#   readr::write_excel_csv("~/Downloads/epd_duplicated_cores_counts.csv", na = "")
epd_duplicated_counts2 <- epd_duplicated_counts %>%
  dplyr::rowwise() %>%
  dplyr::mutate(total_taxa = sum(dplyr::c_across(-c(1:16)), na.rm = TRUE),
                .after = unit_name) %>%
  dplyr::group_by(entity_name, depth) %>%
  dplyr::mutate(same_taxa = length(unique(total_taxa)) == 1,
                .after = total_taxa) %>%
  dplyr::ungroup()

epd_duplicated_counts3 <- EPD_METADATA %>%
  dplyr::select(site_id, site_name, dataset_id, entity_name_new = entity_name) %>%
  dplyr::right_join(epd_duplicated_counts2) %>%
  dplyr::arrange(entity_name_new, entity_name, depth) %>%
  dplyr::mutate(keep = !is.na(entity_name_new),
                .before = 1) %>%
  dplyr::select(-total_taxa, -same_taxa)
epd_duplicated_counts3 %>%
  readr::write_excel_csv("~/Downloads/epd_duplicated_cores_counts_2022-02-15.csv", na = "")

#### Check dates ----
aux <- epd_duplicated_cores %>%
  dplyr::group_by(entity_name) %>%
  purrr::pmap(function(entity_name, ...) {
    all <- EPD_DATES %>%
      dplyr::filter(entity_name %in% !!entity_name)
    unique <- all %>%
      dplyr::distinct(entity_name, date_type, depth, thickness, lab_num, age_cal, age_c14, error, .keep_all = TRUE)
    list(all = all,
         unique = unique)
  })
aux[[1]]$all
aux[[2]]$unique

#### Check counts ----
aux <- epd_duplicated_cores %>%
  dplyr::group_by(entity_name) %>%
  purrr::pmap(function(entity_name, ...) {
    all <- EPD_COUNTS_2 %>%
      dplyr::filter(entity_name %in% !!entity_name)
    unique <- all %>%
      dplyr::distinct(entity_name, depth, thickness, .keep_all = TRUE)
    list(all = all,
         unique = unique)
  })
aux[[1]]$all
aux[[1]]$unique

aux <- EPD_COUNTS_2 %>%
  dplyr::filter(entity_name %in% "BELLFONT") %>%
  smpds::rm_na_taxa(1:16)

EPD_COUNTS_2 %>%
  dplyr::filter(dataset_id %in% epd_duplicated_cores$dataset_id) %>%
  dplyr::select(1:16) %>%
  dplyr::distinct(dataset_id, .keep_all = TRUE)

# Load EPD site types
epd_site_types <- "data-raw/epd-site types.xlsx" %>%
  readxl::read_excel() %>%
  dplyr::mutate(site_type = tolower(site_type)) %>%
  dplyr::distinct()
epd_unique_sites <- EPD_METADATA_3 %>%
  dplyr::distinct(site_id, site_name) %>%
  dplyr::mutate(ID_SITE = seq_along(site_id), .before = 1)
EPD_METADATA_4 <- EPD_METADATA_3 %>%
  dplyr::select(-dplyr::starts_with("ID_")) %>%
  dplyr::left_join(epd_unique_sites) %>%
  dplyr::relocate(ID_SITE, .before = 1) %>%
  dplyr::mutate(ID_ENTITY = seq_along(site_id), .before = 2) %>%
  dplyr::select(-dplyr::starts_with("site_type")) %>%
  dplyr::left_join(epd_site_types)

EPD_METADATA <- EPD_METADATA_4
usethis::use_data(EPD_METADATA, overwrite = TRUE, compress = "xz")

# EPD_METADATA %>%
#   readr::write_excel_csv("~/Downloads/epd-records-extracted-from-neotoma_metadata_2022-02-15.csv", na = "")
