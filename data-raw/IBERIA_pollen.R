## code to prepare `IBERIA_pollen` dataset goes here
`%>%` <- magrittr::`%>%`
iberia_pollen_url <- "https://researchdata.reading.ac.uk/343/19/Iberia_pollen_records_v2.csv"

# Metadata & Age models----
## v2 ----
IBERIA_pollen_v2 <-
  "inst/extdata/Iberia_pollen_records_v2.csv" %>%
  readr::read_csv() %>%
  dplyr::rename(publication = reference)
iberia_site_ids <- IBERIA_pollen_v2 %>%
  dplyr::arrange(site_name, entity_name) %>%
  dplyr::distinct(site_name) %>%
  dplyr::mutate(ID_SITE = seq_along(site_name), .before = 1)
iberia_entity_ids <- IBERIA_pollen_v2 %>%
  dplyr::arrange(site_name, entity_name) %>%
  dplyr::distinct(entity_name) %>%
  dplyr::mutate(ID_ENTITY = seq_along(entity_name), .before = 1)
iberia_ids <- IBERIA_pollen_v2 %>%
  dplyr::arrange(site_name, entity_name) %>%
  dplyr::distinct(site_name, entity_name) %>%
  dplyr::left_join(iberia_site_ids) %>%
  dplyr::left_join(iberia_entity_ids) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1)
IBERIA_pollen_v2 <- iberia_ids %>%
  dplyr::select(-site_name) %>%
  dplyr::right_join(IBERIA_pollen_v2 %>%
                      dplyr::select(-dplyr::starts_with("ID_SITE|ID_ENTITY")),
                    by = "entity_name") %>%
  dplyr::relocate(site_name, .before = entity_name)
## v3 ----
IBERIA_pollen_v2_ids <- IBERIA_pollen_v2 %>%
  dplyr::distinct(site_name, entity_name, .keep_all = TRUE) %>%
  dplyr::select(1:4) %>%
  dplyr::mutate(site_name = site_name %>%
                  stringr::str_replace_all("Eix", "Elx"),
                entity_name = entity_name %>%
                  stringr::str_replace_all("EIX", "ELX")) %>%
  dplyr::select(-site_name)

IBERIA_pollen_v3 <-
  "inst/extdata/Iberia_pollen_records_v3_0307.csv" %>%
  readr::read_csv() %>%
  dplyr::rename(publication = reference) %>%
  dplyr::left_join(IBERIA_pollen_v2_ids) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1) %>%
  dplyr::arrange(ID_SITE, ID_ENTITY)
IBERIA_pollen_v3_ids <- IBERIA_pollen_v3 %>%
  dplyr::distinct(site_name, entity_name, .keep_all = TRUE) %>%
  dplyr::select(1:4) %>%
  dplyr::select(-site_name)
waldo::compare(IBERIA_pollen_v2_ids, IBERIA_pollen_v3_ids)

data("IBERIA_pollen") # Load version 2 of the IBERIAN POLLEN data
waldo::compare(IBERIA_pollen, IBERIA_pollen_v2)
IBERIA_pollen_v2_summary <- IBERIA_pollen_v2 %>%
  dplyr::group_by(ID_SITE, ID_ENTITY) %>%
  dplyr::mutate(n_old = dplyr::n()) %>%
  dplyr::distinct(ID_SITE, ID_ENTITY, n_old)
IBERIA_pollen_v3_summary <- IBERIA_pollen_v3 %>%
  dplyr::group_by(ID_SITE, ID_ENTITY) %>%
  dplyr::mutate(n_new = dplyr::n()) %>%
  dplyr::distinct(ID_SITE, ID_ENTITY, n_new)
dplyr::left_join(IBERIA_pollen_v2_summary, IBERIA_pollen_v3_summary) %>%
  dplyr::filter(n_old != n_new)

usethis::use_data(IBERIA_pollen_v2, overwrite = TRUE, compress = "xz")
usethis::use_data(IBERIA_pollen_v3, overwrite = TRUE, compress = "xz")

# Dates ----
## v2 ----
IBERIA_pollen_dates_v2 <- "inst/extdata/Iberia_pollen_dates.xlsx" %>%
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
IBERIA_pollen_dates_v2 <- iberia_ids %>%
  dplyr::right_join(IBERIA_pollen_dates_v2 %>%
                      dplyr::select(-site_name),
                    by = "entity_name")

## v3 ----
IBERIA_pollen_dates_v3 <- "inst/extdata/Iberia_data_dates_v3.xlsx" %>%
  readxl::read_excel(sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::mutate(site_name = site_name %>%
                  stringr::str_replace_all("Eix", "Elx"),
                entity_name = entity_name %>%
                  stringr::str_replace_all("EIX", "ELX")) %>%
  dplyr::rename(lab_num = lab_number, #date_code,
                depth = avg_depth,
                # thickness = thickness_cm,
                # age_c14 = radiocarbon_age,
                age_cal = age_calib, #calibrated_age,
                error_cal = age_calib_error #calibrated_age_error
                ) %>%
  dplyr::mutate(error = dplyr::coalesce(error, error_cal)) %>%
  dplyr::select(-error_cal)
IBERIA_pollen_dates_v3 <- IBERIA_pollen_v2_ids %>%
  dplyr::right_join(IBERIA_pollen_dates_v3 %>%
                      dplyr::select(-site_name),
                    by = "entity_name")

usethis::use_data(IBERIA_pollen_dates_v2, overwrite = TRUE, compress = "xz")
usethis::use_data(IBERIA_pollen_dates_v3, overwrite = TRUE, compress = "xz")

waldo::compare(IBERIA_pollen_dates_v2,
               IBERIA_pollen_dates_v3,
               tolerance = 1E-9)

# Counts ----
# IBERIA_pollen_counts <- "inst/extdata/iberia_pollen_records_v2.csv" %>%
#   readr::read_csv() %>%
#   dplyr::rename(depth = `avg_depth..cm.`,
#                 IPE_age_cal = `IPE.age..cal.`)
# IBERIA_pollen_counts

# # OLD ---
# ## pollen counts ---
# old_pollen_counts <-
#   "~/OneDrive - University of Reading/UoR/2022/iberian-data/Iberia_all sites_clean_amal_v3_2Dec2020.xlsx" %>%
#   readxl::read_excel(sheet = 1)
