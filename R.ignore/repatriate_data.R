`%>%` <- magrittr::`%>%`
epd_repatriation_id <- "1knnWB6XQF_QAI9iumyUAdoFm37_7iTZh"
epd_repatriation_tmp_file <- tempfile(fileext = ".xslx")
googledrive::as_id(epd_repatriation_id) %>%
  googledrive::drive_download(path = epd_repatriation_tmp_file, overwrite = TRUE)

# Helper functions ----
to_bool <- function(x) {
  dplyr::case_when(is.na(x) ~ NA,
                   stringr::str_detect(x, "Y|y") ~ TRUE,
                   stringr::str_detect(x, "N|n") ~ FALSE,
                   TRUE ~ NA)
}

extract_embsecbio <- function(ID_ENTITY = NULL) {
  if (missing(ID_ENTITY))
    return(NULL)
  entity_tb <- embsecbio::entity %>%
    dplyr::filter(ID_ENTITY %in% !!ID_ENTITY) %>%
    dplyr::select(-mod_or_0ka_class, -comments)
  if (nrow(entity_tb) == 0)
    return(NULL)
  site_tb <- embsecbio::site %>%
    dplyr::filter(ID_SITE %in% entity_tb$ID_SITE) %>%
    dplyr::select(-basin_size, -catch_size)
  date_info_tb <- embsecbio::date_info %>%
    dplyr::filter(ID_ENTITY %in% entity_tb$ID_ENTITY) %>%
    dplyr::rename(depth = avg_depth,
                  age_c14 = dated_age,
                  material_dated = mat_dated) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(error = max(error_positive, error_negative)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-error_negative, -error_positive)
  sample_tb <- embsecbio::sample %>%
    dplyr::filter(ID_ENTITY %in% entity_tb$ID_ENTITY)
  pollen_tb <- embsecbio::pollen_data %>%
    dplyr::filter(ID_SAMPLE %in% sample_tb$ID_SAMPLE) %>%
    dplyr::select(-ID_SAMPLE_TAXA) #%>%
    # tidyr::pivot_wider(names_from = "taxon_clean", values_from = "taxon_count")
  age_model_tb <- embsecbio::age_model %>%
    dplyr::filter(ID_SAMPLE %in% sample_tb$ID_SAMPLE)
  list(site = site_tb,
       entity = entity_tb,
       date_info = date_info_tb,
       sample = sample_tb,
       pollen = pollen_tb,
       age_model = age_model_tb)
}

# Subsets ----
## RPD ----
rpd_repatriation <- epd_repatriation_tmp_file %>%
  readxl::read_excel(sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::filter(!is.na(rpd_id_entity)) %>%
  dplyr::mutate(dates_to_be_extracted_from_rpd =
                  to_bool(dates_to_be_extracted_from_rpd),
                age_model_to_be_extracted_from_rpd =
                  to_bool(age_model_to_be_extracted_from_rpd),
                matched_lab_no = to_bool(matched_lab_no)
                )
rpd_repatriation


## EMBSeCBIO ----
embsecbio_repatriation <- epd_repatriation_tmp_file %>%
  readxl::read_excel(sheet = 2) %>%
  janitor::clean_names() %>%
  dplyr::filter(!is.na(site_in_embsecbio)) %>%
  dplyr::mutate(dates_to_be_extracted_from_embsecbio =
                  to_bool(dates_to_be_extracted_from_embsecbio),
                age_model_to_be_extracted_from_embsecbio =
                  to_bool(age_model_to_be_extracted_from_embsecbio)) %>%
  dplyr::rename(EMBSeCBIO_ID_ENTITY = embsecbio_id_entity,
                entity_name = entity_name_3,
                EMBSeCBIO_entity_name = entity_name_8)
embsecbio_repatriation_dates <- embsecbio_repatriation %>%
  dplyr::filter(dates_to_be_extracted_from_embsecbio)

aux <- embsecbio_repatriation_dates$EMBSeCBIO_ID_ENTITY %>%
  extract_embsecbio()
groups <- aux$pollen %>%
  dplyr::group_by(ID_SAMPLE, taxon_clean) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

embsecbio_duplicated_taxon_counts <-
  # groups %>%
  # embsecbio::pollen_data %>%
  aux$pollen %>%
  dplyr::group_by(ID_SAMPLE, taxon_clean) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) %>%
  purrr::pmap_df(function(ID_SAMPLE, taxon_clean, ...) {
    embsecbio::pollen_data %>%
      dplyr::filter(ID_SAMPLE == !!ID_SAMPLE,
                    taxon_clean == !!taxon_clean)
  })

aux$pollen %>%
  dplyr::filter(ID_SAMPLE %in% groups$ID_SAMPLE,
                taxon_clean %in% groups$taxon_clean)
  dplyr::filter(ID_SAMPLE %in% c(6492))

## EMBSeCBIO (extra) ----
embsecbio_extra_repatriation <- epd_repatriation_tmp_file %>%
  readxl::read_excel(sheet = 3) %>%
  janitor::clean_names() %>%
  dplyr::mutate(dates_to_be_extracted_from_embsecbio =
                  to_bool(dates_to_be_extracted_from_embsecbio),
                age_model_to_be_extracted_from_embsecbio =
                  to_bool(age_model_to_be_extracted_from_embsecbio))


## IBERIA ----
iberia_repatriation <- epd_repatriation_tmp_file %>%
  readxl::read_excel(sheet = 4) %>%
  janitor::clean_names() %>%
  dplyr::mutate(site_in_iberia = to_bool(site_in_iberia),
                dates_to_be_extracted_from_iberia =
                  to_bool(dates_to_be_extracted_from_iberia),
                age_model_to_be_extracted_from_iberia =
                  to_bool(age_model_to_be_extracted_from_iberia))

## IBERIA (extra) ----
iberia_extra_repatriation <- epd_repatriation_tmp_file %>%
  readxl::read_excel(sheet = 5) %>%
  janitor::clean_names() %>%
  dplyr::mutate(site_in_iberia = to_bool(site_in_iberia),
                dates_to_be_extracted_from_iberia =
                  to_bool(dates_to_be_extracted_from_iberia),
                age_model_to_be_extracted_from_iberia =
                  to_bool(age_model_to_be_extracted_from_iberia))

all(iberia_extra_repatriation$site_in_iberia)
IBERIA_pollen %>%
  dplyr::filter(site_name %in% iberia_extra_repatriation$site_name)

# No dates in the RPD ----
no_dates_rpd <- readxl::read_excel("~/Downloads/No dates in RPD.xlsx")
rpd_site_entity_tbs <- rpdata::site[1:2] %>%
  dplyr::left_join(rpdata::entity)
rpd_site_entity_tbs %>%
  dplyr::filter(ID_ENTITY %in% no_dates_rpd$ID_ENTITY)
aux <- no_dates_rpd %>%
  dplyr::left_join(rpd_site_entity_tbs,
                   by = "ID_ENTITY")

aux <- rpd_site_entity_tbs[1:10] %>%
  dplyr::right_join(rpdata::date_info %>%
                      dplyr::filter(ID_ENTITY %in% no_dates_rpd$ID_ENTITY)) %>%
  dplyr::arrange(site_name, entity_name, avg_depth)
aux %>%
  readr::write_excel_csv("~/Downloads/rpd_records_for_LS_to_inspect_2022-02-10.csv", na = "")
