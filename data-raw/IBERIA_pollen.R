## code to prepare `IBERIA_pollen` dataset goes here
`%>%` <- magrittr::`%>%`
iberia_pollen_url <- "https://researchdata.reading.ac.uk/343/19/Iberia_pollen_records_v2.csv"
IBERIA_pollen <- "inst/extdata/Iberia_pollen_records_v2.csv" %>%
  readr::read_csv() %>%
  dplyr::rename(publication = reference)
iberia_site_ids <- IBERIA_pollen %>%
  dplyr::arrange(site_name, entity_name) %>%
  dplyr::distinct(site_name) %>%
  dplyr::mutate(ID_SITE = seq_along(site_name), .before = 1)
iberia_entity_ids <- IBERIA_pollen %>%
  dplyr::arrange(site_name, entity_name) %>%
  dplyr::distinct(entity_name) %>%
  dplyr::mutate(ID_ENTITY = seq_along(entity_name), .before = 1)
iberia_ids <- IBERIA_pollen %>%
  dplyr::arrange(site_name, entity_name) %>%
  dplyr::distinct(site_name, entity_name) %>%
  dplyr::left_join(iberia_site_ids) %>%
  dplyr::left_join(iberia_entity_ids) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1)

IBERIA_pollen_2 <- iberia_ids %>%
  dplyr::right_join(IBERIA_pollen)
IBERIA_pollen <- IBERIA_pollen_2
usethis::use_data(IBERIA_pollen, overwrite = TRUE, compress = "xz")

IBERIA_pollen_dates <- "inst/extdata/Iberia_pollen_dates.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::rename(lab_num = date_code,
                depth = depth_cm,
                thickness = thickness_cm,
                age_c14 = radiocarbon_age,
                age_cal = calibrated_age,
                error_cal = calibrated_age_error) %>%
  dplyr::mutate(error = dplyr::coalesce(error, error_cal)) %>%
  dplyr::select(-error_cal)
IBERIA_pollen_dates <- iberia_ids %>%
  dplyr::right_join(IBERIA_pollen_dates)
usethis::use_data(IBERIA_pollen_dates, overwrite = TRUE, compress = "xz")

IBERIA_pollen_counts <- "inst/extdata/iberia_pollen_records_v2.csv" %>%
  readr::read_csv() %>%
  dplyr::rename(depth = `avg_depth..cm.`,
                IPE_age_cal = `IPE.age..cal.`)
IBERIA_pollen_counts

# OLD ----
## pollen counts ----
old_pollen_counts <-
  "~/OneDrive - University of Reading/UoR/2022/iberian-data/Iberia_all sites_clean_amal_v3_2Dec2020.xlsx" %>%
  readxl::read_excel(sheet = 1)
