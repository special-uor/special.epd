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
## v3 - extra ----
iberia_extra_counts_filenames <-
  list.files(path = "~/Downloads/iberian_extra_sites/FOR DB",
             pattern = ".xls",
             full.names = TRUE) %>%
  tibble::tibble() %>%
  magrittr::set_names("path") %>%
  dplyr::mutate(is_list = basename(path) %>%
                  stringr::str_detect("_list|_LIST"),
                site_name = basename(path) %>%
                  stringr::str_remove_all(".xlsx|.xls") %>%
                  stringr::str_remove_all("_list|_LIST"),
                .before = 1)

IBERIA_extra_counts_v3 <- iberia_extra_counts_filenames %>%
  purrr::pmap_df(function(is_list, site_name, path, ...) {
    message("Processing: ", site_name)
    suppressMessages({
      ss <- path %>%
        readxl::read_excel(sheet = 1)
    })
    if (all(is.na(ss[1])) &
        (colnames(ss)[2] %>%
         stringr::str_detect("...2|Name|name|NAME"))) {
      ss <- ss[-1]
    }
    no_count_col <- colnames(ss) %>%
      stringr::str_detect("Count|count|COUNT") %>%
      any
    if (!is_list & !no_count_col) {
      ss <- ss[c(1:4)] %>%
        dplyr::bind_cols(purrr::map_dfc(ss[-c(1:4)], as.numeric)) %>%
        # ss %>%
        magrittr::set_names(
          c("taxon_name", "clean", "intermediate", "amalgamated", colnames(.)[-c(1:4)])
        ) %>%
        tidyr::pivot_longer(cols = -c(1:4),
                            names_to = "depth",
                            values_to = "count"
                            ) %>%
        dplyr::mutate(depth = as.numeric(depth)) %>%
        dplyr::arrange(depth, taxon_name) %>%
        dplyr::filter(!is.na(count))
    } else {
      ss <- ss %>%
        magrittr::set_names(
          c("taxon_name", "clean", "intermediate", "amalgamated", "depth", "count")
        ) %>%
        # dplyr::arrange(taxon_name) %>%
        dplyr::arrange(depth, taxon_name)
    }
    ss %>%
      dplyr::mutate(site_name, #= rpd:::cln_str(site_name),
                    .before = 1)
  })

IBERIA_extra_counts_v3_2 <- conn %>%
  dabr::select("SELECT * FROM entity WHERE site_name in (",
               IBERIA_extra_counts_v3 %>%
                 dplyr::distinct(site_name) %>%
                 purrr::map(dabr::quote) %>%
                 purrr::flatten_chr() %>%
                 stringr::str_c(collapse = ", "),
               ")") %>%
  dplyr::select(ID_SITE, ID_ENTITY, site_name, entity_name) %>%
  dplyr::mutate(cln_site_name = rpd:::cln_str(site_name)) %>%
  dplyr::right_join(IBERIA_extra_counts_v3 %>%
                      dplyr::mutate(cln_site_name = rpd:::cln_str(site_name)),
                    by = "cln_site_name") %>%
  dplyr::rename(site_name = site_name.y) %>%
  dplyr::select(-site_name.x, -cln_site_name)

waldo::compare(IBERIA_extra_counts_v3 %>%
                 dplyr::arrange(site_name, depth, taxon_name),
               IBERIA_extra_counts_v3_2 %>%
                 dplyr::arrange(site_name, depth, taxon_name) %>%
                 dplyr::select(-ID_SITE, -ID_ENTITY, -entity_name))

IBERIA_extra_counts_v3_2 %>%
  dplyr::filter(is.na(ID_SITE) | is.na(ID_ENTITY))
conn %>%
  dabr::select("SELECT * FROM entity WHERE site_name in (",
               IBERIA_extra_counts_v3_2 %>%
                 dplyr::distinct(site_name) %>%
                 purrr::map(dabr::quote) %>%
                 purrr::flatten_chr() %>%
                 stringr::str_c(collapse = ", "),
               ")")

IBERIA_extra_counts_v3 <- IBERIA_extra_counts_v3_2
usethis::use_data(IBERIA_extra_counts_v3, overwrite = TRUE, compress = "xz")

## new entities ----
iberia_new_counts_filenames <-
  list.files(path = "~/Downloads/iberian_extra_sites/FOR DB/NEW/",
             pattern = ".xls",
             full.names = TRUE) %>%
  tibble::tibble() %>%
  magrittr::set_names("path") %>%
  dplyr::mutate(is_list = basename(path) %>%
                  stringr::str_detect("_list|_LIST"),
                site_name = basename(path) %>%
                  stringr::str_remove_all(".xlsx|.xls") %>%
                  stringr::str_remove_all("_list|_LIST") %>%
                  stringr::str_remove_all("_new|_NEW"),
                .before = 1)

IBERIA_new_counts_v3 <- iberia_new_counts_filenames %>%
  purrr::pmap_df(function(is_list, site_name, path, ...) {
    message("Processing: ", site_name)
    suppressMessages({
      ss <- path %>%
        readxl::read_excel(sheet = 1)
    })
    if (all(is.na(ss[1])) &
        (colnames(ss)[2] %>%
         stringr::str_detect("...2|Name|name|NAME"))) {
      ss <- ss[-1]
    }
    no_count_col <- colnames(ss) %>%
      stringr::str_detect("Count|count|COUNT") %>%
      any
    if (!is_list & !no_count_col) {
      ss <- ss[c(1:4)] %>%
        dplyr::bind_cols(purrr::map_dfc(ss[-c(1:4)], as.numeric)) %>%
        # ss %>%
        magrittr::set_names(
          c("taxon_name", "clean", "intermediate", "amalgamated", colnames(.)[-c(1:4)])
        ) %>%
        tidyr::pivot_longer(cols = -c(1:4),
                            names_to = "depth",
                            values_to = "count"
        ) %>%
        dplyr::mutate(depth = as.numeric(depth)) %>%
        dplyr::arrange(depth, taxon_name) %>%
        dplyr::filter(!is.na(count))
    } else {
      ss <- ss %>%
        magrittr::set_names(
          c("taxon_name", "clean", "intermediate", "amalgamated", "depth", "count")
        ) %>%
        # dplyr::arrange(taxon_name) %>%
        dplyr::arrange(depth, taxon_name)
    }
    ss %>%
      dplyr::mutate(site_name, #= rpd:::cln_str(site_name),
                    .before = 1)
  })

usethis::use_data(IBERIA_new_counts_v3, overwrite = TRUE, compress = "xz")

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
