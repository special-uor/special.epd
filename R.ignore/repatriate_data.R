`%>%` <- magrittr::`%>%`
# Download file ----
epd_repatriation_id <- "1knnWB6XQF_QAI9iumyUAdoFm37_7iTZh"
epd_repatriation_tmp_file <- tempfile(fileext = ".xslx")
googledrive::as_id(epd_repatriation_id) %>%
  googledrive::drive_download(path = epd_repatriation_tmp_file, overwrite = TRUE)

# Open DB connection ----
conn <- dabr::open_conn_mysql("SPECIAL-EPD",
                              password = rstudioapi::askForPassword())

# Load datasets ----
data("EPD_COUNTS")
data("EPD_DATES")
data("EPD_DATES_coretops")
data("EPD_METADATA")

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
    dplyr::select(-mod_or_0ka_class, -comments) %>%
    dplyr::left_join(embsecbio::entity_pub) %>%
    dplyr::left_join(embsecbio::pub) %>%
    dplyr::group_by(ID_ENTITY) %>%
    dplyr::mutate(publication = citation %>%
                    stringr::str_c(collapse = ";\n")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ID_ENTITY_PUB, -ID_PUB, -citation) %>%
    dplyr::distinct()
  if (nrow(entity_tb) == 0)
    return(NULL)
  site_tb <- embsecbio::site %>%
    dplyr::filter(ID_SITE %in% entity_tb$ID_SITE) %>%
    dplyr::select(ID_SITE, site_name, site_type)
    # dplyr::select(-basin_size, -catch_size)
  metadata_tb <- site_tb %>%
    dplyr::right_join(entity_tb)
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
    dplyr::filter(ID_ENTITY %in% entity_tb$ID_ENTITY) %>%
    dplyr::rename(depth =  avg_depth) %>%
    dplyr::select(-sample_name) %>%
    dplyr::mutate(depth = depth * 100) %>%
    dplyr::rename(external_ID_ENTITY = ID_ENTITY)
  pollen_tb <- embsecbio::pollen_data %>%
    dplyr::filter(ID_SAMPLE %in% sample_tb$ID_SAMPLE) %>%
    dplyr::select(-ID_SAMPLE_TAXA) #%>%
    # tidyr::pivot_wider(names_from = "taxon_clean", values_from = "taxon_count")
  age_model_tb <- embsecbio::age_model %>%
    dplyr::filter(ID_SAMPLE %in% sample_tb$ID_SAMPLE) %>%
    dplyr::select(-dplyr::contains("intcal13")) %>%
    dplyr::rename(age = est_age_provided) %>%
    magrittr::set_names(colnames(.) %>%
                          stringr::str_remove_all("est_age_bacon_intcal20_") %>%
                          stringr::str_replace_all("uncert", "UNCERT")) #%>%
    # dplyr::left_join(sample_tb %>%
    #                    dplyr::select(ID_SAMPLE, external_ID_ENTITY, depth)) %>%
    # dplyr::relocate(external_ID_ENTITY, depth, .before = 1)
  sample_pollen_tb <- sample_tb %>%
    # dplyr::select(ID_SAMPLE, external_ID_ENTITY, depth) %>%
    dplyr::right_join(pollen_tb, by = "ID_SAMPLE")
  sample_age_model_tb <- sample_tb %>%
    # dplyr::select(ID_SAMPLE, external_ID_ENTITY, depth) %>%
    dplyr::right_join(age_model_tb, by = "ID_SAMPLE")
  list(metadata = metadata_tb,
       # site = site_tb,
       # entity = entity_tb,
       date_info = date_info_tb,
       sample = sample_tb,
       pollen = sample_pollen_tb,
       age_model = sample_age_model_tb)
}

extract_iberia <- function(entity_name = NULL) {
  if (missing(entity_name))
    return(NULL)
  data("IBERIA_pollen")
  data("IBERIA_pollen_dates")
  entity_age_model_tb <- IBERIA_pollen %>%
    dplyr::filter(entity_name %in% !!entity_name) %>%
    dplyr::select(1:17) %>%
    dplyr::rename(depth = `avg_depth..cm.`) %>%
    dplyr::select(-`IPE.age..cal.`)
  if (nrow(entity_age_model_tb) == 0)
    return(NULL)

  date_info_tb <- IBERIA_pollen_dates %>%
    dplyr::filter(entity_name %in% !!entity_name) %>%
    dplyr::select(-type)
  sample_tb <- IBERIA_pollen %>%
    dplyr::filter(entity_name %in% !!entity_name) %>%
    dplyr::select(1:17) %>%
    dplyr::rename(depth = `avg_depth..cm.`) %>%
    dplyr::select(-`IPE.age..cal.`)
  age_model_tb <- IBERIA_pollen %>%
    dplyr::filter(entity_name %in% !!entity_name) %>%
    dplyr::select(1:17) %>%
    dplyr::rename(depth = `avg_depth..cm.`,
                  age = `IPE.age..cal.`) %>%
    magrittr::set_names(colnames(.) %>%
                          stringr::str_remove_all("INTCAL2020_") %>%
                          stringr::str_replace_all("uncert", "UNCERT"))
  list(metadata = entity_age_model_tb %>%
         dplyr::select(1:9) %>%
         dplyr::distinct(),
       date_info = date_info_tb,
       sample = sample_tb,
       age_model = age_model_tb)
}

get_id_date_info <- function(conn) {
  suppressWarnings({
    ID_DATE_INFO <- dabr::select_all(conn, "date_info", quiet = TRUE) %>%
      .$ID_DATE_INFO %>%
      max()
  })
  if (is.na(ID_DATE_INFO) | is.infinite(ID_DATE_INFO))
    return(1)
  return(ID_DATE_INFO)
}

get_id_entity <- function(conn) {
  suppressWarnings({
    ID_ENTITY <- dabr::select_all(conn, "entity", quiet = TRUE) %>%
      .$ID_ENTITY %>%
      max()
  })
  if (is.na(ID_ENTITY) | is.infinite(ID_ENTITY))
    return(1)
  return(ID_ENTITY)
}

get_id_sample <- function(conn) {
  suppressWarnings({
    ID_SAMPLE <- dabr::select_all(conn, "sample", quiet = TRUE) %>%
      .$ID_SAMPLE %>%
      max()
  })
  if (is.na(ID_SAMPLE) | is.infinite(ID_SAMPLE))
    return(1)
  return(ID_SAMPLE)
}

get_id_site <- function(conn) {
  suppressWarnings({
    ID_SITE <- dabr::select_all(conn, "entity", quiet = TRUE) %>%
      .$ID_SITE %>%
      max()
  })
  if (is.na(ID_SITE) | is.infinite(ID_SITE))
    return(1)
  return(ID_SITE)
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
                entity_name = entity_name_5,
                EMBSeCBIO_entity_name = entity_name_10)

### EMBSeCBIO dates (12) ----
embsecbio_repatriation_dates <- embsecbio_repatriation %>%
  dplyr::filter(dates_to_be_extracted_from_embsecbio)

embsecbio_repatriated_dates_info <-
  embsecbio_repatriation_dates$EMBSeCBIO_ID_ENTITY %>%
  extract_embsecbio()
embsecbio_repatriated_dates_info_2 <- embsecbio_repatriation_dates %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                ID_ENTITY = EMBSeCBIO_ID_ENTITY) %>%
  dplyr::inner_join(embsecbio_repatriated_dates_info$metadata,
                    by = "ID_ENTITY") %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)
embsecbio_repatriated_dates_info_3 <- EPD_METADATA %>%
  dplyr::select(1:4, 6, 10) %>%
  dplyr::right_join(embsecbio_repatriated_dates_info_2 %>%
                      dplyr::select(1:6, 8),
                    by = c("entity_name" = "neotoma_entity_name"))

#### External links ----
meta_neo_res <- seq_len(nrow(embsecbio_repatriated_dates_info_3)) %>%
  purrr::map(function(i) {
    embsecbio_repatriated_dates_info_3[i, ] %>%
      dplyr::select(ID_SITE,
                    ID_ENTITY,
                    external_ID_SITE,
                    external_ID_ENTITY,
                    external_site_name,
                    external_entity_name) %>%
      dplyr::mutate(external_source = "EMBSECBIO") %>%
      rpd:::add_records(conn = conn, table = "external_link", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_METADATA_NEO_DB <- dabr::select_all(conn, "external_link") %>%
  dplyr::filter(external_ID_ENTITY %in%
                  embsecbio_repatriated_dates_info_3$external_ID_ENTITY,
                external_source == "EMBSECBIO") %>%
  dplyr::select(ID_SITE,
                ID_ENTITY,
                external_ID_SITE,
                external_ID_ENTITY,
                external_site_name,
                external_entity_name)
waldo::compare(embsecbio_repatriated_dates_info_3 %>%
                 dplyr::select(ID_SITE,
                               ID_ENTITY,
                               external_ID_SITE,
                               external_ID_ENTITY,
                               external_site_name,
                               external_entity_name) %>%
                 dplyr::mutate(external_ID_SITE = as.integer(external_ID_SITE),
                               external_ID_ENTITY = as.integer(external_ID_ENTITY)),
               EPD_METADATA_NEO_DB)

#### Dates ----
embsecbio_repatriated_dates_info_4 <-
  embsecbio_repatriated_dates_info$date_info %>%
  dplyr::select(-ID_DATE_INFO) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                reason_age_not_used = date_comments) %>%
  dplyr::mutate(age_used = ifelse(is.na(reason_age_not_used), "yes", "no")) %>%
  dplyr::left_join(embsecbio_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  dplyr::select(-external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1)
meta_neo_res <- seq_len(nrow(embsecbio_repatriated_dates_info_4)) %>%
  purrr::map(function(i) {
    embsecbio_repatriated_dates_info_4[i, ] %>%
      rpd:::add_records(conn = conn, table = "date_info", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_repatriated_dates_info_4$ID_ENTITY)
waldo::compare(embsecbio_repatriated_dates_info_4 %>%
                 .[order(colnames(.))],
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-age_calib, -ID_DATE_INFO, -notes))

# #### Samples ---
# aux <- EPD_COUNTS %>%
#   dplyr::filter(dataset_id %in% embsecbio_repatriated_dates_info_3$dataset_id) %>%
#   smpds::rm_na_taxa(1:16) %>%
#   dplyr::select(-chronology_id, -chronology_name) %>%
#   dplyr::left_join(EPD_METADATA %>%
#                      dplyr::select(1:6, 10),
#                    by = c("site_id", "site_name", "site_name_clean", "dataset_id", "entity_name")) %>%
#   dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1)
#
# embsecbio_repatriated_dates_info$age_model %>%
#   dplyr::select(-est_age_original, -comment) %>%
#   dplyr::left_join(embsecbio_repatriated_dates_info_3 %>%
#                      dplyr::select(ID_ENTITY, external_ID_ENTITY))
# embsecbio_repatriated_dates_info$pollen %>%
#   dplyr::left_join(embsecbio_repatriated_dates_info_3 %>%
#                      dplyr::select(ID_ENTITY, external_ID_ENTITY))


### EPD dates (106) ----
epd_embsecbio_repatriation_dates <- embsecbio_repatriation %>%
  dplyr::filter(!dates_to_be_extracted_from_embsecbio)
epd_embsecbio_repatriated_dates_info <-
  epd_embsecbio_repatriation_dates$EMBSeCBIO_ID_ENTITY %>%
  extract_embsecbio()
epd_embsecbio_repatriated_dates_info_2 <- epd_embsecbio_repatriation_dates %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                ID_ENTITY = EMBSeCBIO_ID_ENTITY) %>%
  dplyr::inner_join(epd_embsecbio_repatriated_dates_info$metadata,
                    by = "ID_ENTITY") %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)
epd_embsecbio_repatriated_dates_info_3 <- EPD_METADATA %>%
  dplyr::select(1:4, 6, 10) %>%
  dplyr::right_join(epd_embsecbio_repatriated_dates_info_2 %>%
                      dplyr::select(1:6, 8),
                    by = c("entity_name" = "neotoma_entity_name"))

#### External links ----
meta_neo_res <- seq_len(nrow(epd_embsecbio_repatriated_dates_info_3)) %>%
  purrr::map(function(i) {
    epd_embsecbio_repatriated_dates_info_3[i, ] %>%
      dplyr::select(ID_SITE,
                    ID_ENTITY,
                    external_ID_SITE,
                    external_ID_ENTITY,
                    external_site_name,
                    external_entity_name) %>%
      dplyr::mutate(external_source = "EMBSECBIO") %>%
      rpd:::add_records(conn = conn, table = "external_link", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_METADATA_NEO_DB <- dabr::select_all(conn, "external_link") %>%
  dplyr::filter(external_ID_ENTITY %in%
                  epd_embsecbio_repatriated_dates_info_3$external_ID_ENTITY,
                external_source == "EMBSECBIO") %>%
  dplyr::select(ID_SITE,
                ID_ENTITY,
                external_ID_SITE,
                external_ID_ENTITY,
                external_site_name,
                external_entity_name)
waldo::compare(epd_embsecbio_repatriated_dates_info_3 %>%
                 dplyr::select(ID_SITE,
                               ID_ENTITY,
                               external_ID_SITE,
                               external_ID_ENTITY,
                               external_site_name,
                               external_entity_name) %>%
                 dplyr::mutate(external_ID_SITE = as.integer(external_ID_SITE),
                               external_ID_ENTITY = as.integer(external_ID_ENTITY)),
               EPD_METADATA_NEO_DB)

#### Dates ----
epd_embsecbio_repatriated_dates_info_4 <-
  epd_embsecbio_repatriated_dates_info$date_info %>%
  dplyr::select(-ID_DATE_INFO) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                reason_age_not_used = date_comments) %>%
  dplyr::mutate(age_used = ifelse(is.na(reason_age_not_used), "yes", "no")) %>%
  dplyr::left_join(epd_embsecbio_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  dplyr::select(-external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1)

epd_embsecbio_repatriated_dates_info_4_EPD <- EPD_DATES %>%
  dplyr::filter(entity_name %in% epd_embsecbio_repatriated_dates_info_3$entity_name,
                is.na(ages_already) | ages_already == "EMBSECBIO") %>%
  dplyr::left_join(EPD_METADATA %>%
                     dplyr::select(1:6),
                   by = c("site_id", "site_name", "site_name_clean", "entity_name")) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1) %>%
  dplyr::arrange(ID_ENTITY, depth) %>%
  dplyr::select(-ID_SITE, -ages_already, -site_id, -site_name, -site_name_clean, -entity_name) %>%
  dplyr::rename(age_calib = age_cal)

dplyr::bind_rows(epd_embsecbio_repatriated_dates_info_4,
                 epd_embsecbio_repatriated_dates_info_4_EPD) %>%
  dplyr::mutate(lab_num = stringr::str_squish(lab_num) %>%
                  stringr::str_remove_all("\\s|-")) %>%
  dplyr::group_by(ID_ENTITY, lab_num) %>%
  dplyr::mutate(n = length(lab_num)) %>%
  dplyr::arrange(ID_ENTITY, depth) %>%
  dplyr::filter(n != 2)

dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in% epd_embsecbio_repatriated_dates_info_4_EPD$ID_ENTITY)

meta_neo_res <- seq_len(nrow(epd_embsecbio_repatriated_dates_info_4_EPD)) %>%
  purrr::map(function(i) {
    epd_embsecbio_repatriated_dates_info_4_EPD[i, ] %>%
      rpd:::add_records(conn = conn, table = "date_info", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in% epd_embsecbio_repatriated_dates_info_4$ID_ENTITY)
waldo::compare(epd_embsecbio_repatriated_dates_info_4_EPD %>%
                 .[order(colnames(.))],
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-ID_DATE_INFO))


### EMBSeCBIO age models (106) ----
embsecbio_repatriation_am <- embsecbio_repatriation %>%
  dplyr::filter(age_model_to_be_extracted_from_embsecbio)

embsecbio_repatriated_am_info <-
  embsecbio_repatriation_am$EMBSeCBIO_ID_ENTITY %>%
  extract_embsecbio()

embsecbio_repatriated_am_info_2 <- embsecbio_repatriation_am %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                ID_ENTITY = EMBSeCBIO_ID_ENTITY) %>%
  dplyr::inner_join(embsecbio_repatriated_am_info$metadata,
                    by = "ID_ENTITY") %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)

embsecbio_repatriated_am_info_3 <- EPD_METADATA %>%
  dplyr::select(1:4, 6, 10) %>%
  dplyr::right_join(embsecbio_repatriated_am_info_2 %>%
                      dplyr::select(1:6, 8),
                    by = c("entity_name" = "neotoma_entity_name"))

#### Samples ----
embsecbio_repatriated_am_info_EPD_COUNTS <- EPD_COUNTS %>%
  dplyr::filter(dataset_id %in% embsecbio_repatriated_am_info_3$dataset_id) %>%
  smpds::rm_na_taxa(1:16) %>%
  dplyr::select(-chronology_id) %>%
  dplyr::left_join(EPD_METADATA %>%
                     dplyr::select(1:6, 10),
                   by = c("site_id", "site_name", "site_name_clean", "dataset_id", "entity_name")) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1)

embsecbio_repatriated_am_info_4 <- embsecbio_repatriated_am_info$age_model %>%
  dplyr::mutate(age = dplyr::coalesce(age, est_age_original),
                age_original = est_age_original) %>%
  dplyr::select(-est_age_original, -comment) %>%
  dplyr::left_join(embsecbio_repatriated_am_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  # dplyr::filter(ID_ENTITY != 18) %>%
  dplyr::select(-ID_SAMPLE, -external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1)

# Extract thickness from the EPD data
embsecbio_repatriated_am_info_5 <- embsecbio_repatriated_am_info_4 %>%
  dplyr::mutate(depth2 = round(depth, 3)) %>%
  dplyr::left_join(embsecbio_repatriated_am_info_EPD_COUNTS %>%
                     dplyr::select(ID_ENTITY, depth, thickness, chronology_name,
                                   age_EPD = age, age_younger, age_older, age_type) %>%
                     dplyr::mutate(depth = round(depth, 3)),
                   by = c("ID_ENTITY",  "depth2" = "depth")) %>%
  dplyr::select(-depth2) %>%
  dplyr::relocate(thickness,
                  # age_original,
                  age_EPD,
                  chronology_name,
                  age_younger,
                  age_older,
                  age_type,
                  .before = mean) %>%
  # dplyr::filter(age != age_EPD)
  # dplyr::group_by(ID_ENTITY) %>%
  dplyr::mutate(lower = round(age_EPD) * 0.99,
                upper = round(age_EPD) * 1.01,
                same_age = (round(age) >= lower & round(age) <= upper) | (is.na(age) & is.na(age_EPD)),
                chronology_name = ifelse(same_age, chronology_name, NA),
                age_younger = ifelse(same_age, age_younger, NA),
                age_older = ifelse(same_age, age_older, NA),
                age_type = ifelse(same_age, age_type, NA)) %>%
  dplyr::select(-age_original, -age_EPD, -lower, -upper, -same_age) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(ID_ENTITY), .before = 1)
  # dplyr::mutate(ID_SAMPLE = get_id_sample(conn))

# Verify if the EPD has more samples than the EMBSeCBIO
tibble::tibble(
  ID_ENTITY = names(table(embsecbio_repatriated_am_info_EPD_COUNTS$ID_ENTITY)),
  EPD = table(embsecbio_repatriated_am_info_EPD_COUNTS$ID_ENTITY),
  EMB = table(embsecbio_repatriated_am_info_4$ID_ENTITY)
) %>%
  dplyr::filter(EPD > EMB)

meta_neo_res <- seq_len(nrow(embsecbio_repatriated_am_info_5)) %>%
  purrr::map(function(i) {
    embsecbio_repatriated_am_info_5[i, ] %>%
      dplyr::select(1:11) %>%
      rpd:::add_records(conn = conn, table = "sample", dry_run = FALSE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_repatriated_am_info_5$ID_ENTITY)
waldo::compare(embsecbio_repatriated_am_info_5 %>%
                 dplyr::select(1:11) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)), tolerance = 2)

#### Age models ----
meta_neo_res <- seq_len(nrow(embsecbio_repatriated_am_info_5)) %>%
  purrr::map(function(i) {
    embsecbio_repatriated_am_info_5[i, ] %>%
      dplyr::select(1, 12:17) %>%
      dplyr::mutate(ID_MODEL = 8, .before = 2) %>% # Bacon IntCal20
      rpd:::add_records(conn = conn, table = "age_model", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate ----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "age_model") %>%
  dplyr::filter(ID_SAMPLE %in% embsecbio_repatriated_am_info_5$ID_SAMPLE)
waldo::compare(embsecbio_repatriated_am_info_5 %>%
                 dplyr::select(1, 12:17) %>%
                 dplyr::mutate(ID_MODEL = 8, .before = 2) %>%
                 .[order(colnames(.))],
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))], tolerance = 2)

### EPD age models (106) ----
# This will be re-generated with the latest version of ageR
embsecbio_repatriation_am <- embsecbio_repatriation %>%
  dplyr::filter(is.na(age_model_to_be_extracted_from_embsecbio))

embsecbio_repatriated_am_info <-
  embsecbio_repatriation_am$EMBSeCBIO_ID_ENTITY %>%
  extract_embsecbio()

embsecbio_repatriated_am_info_2 <- embsecbio_repatriation_am %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                ID_ENTITY = EMBSeCBIO_ID_ENTITY) %>%
  dplyr::inner_join(embsecbio_repatriated_am_info$metadata,
                    by = "ID_ENTITY") %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)

embsecbio_repatriated_am_info_3 <- EPD_METADATA %>%
  dplyr::select(1:4, 6, 10) %>%
  dplyr::right_join(embsecbio_repatriated_am_info_2 %>%
                      dplyr::select(1:6, 8),
                    by = c("entity_name" = "neotoma_entity_name"))

# embsecbio_repatriated_am_info$pollen %>%
#   dplyr::left_join(embsecbio_repatriated_am_info_3 %>%
#                      dplyr::select(ID_ENTITY, external_ID_ENTITY))


# groups <- embsecbio_repatriated_dates_info$pollen %>%
#   dplyr::group_by(ID_SAMPLE, taxon_clean) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
#
# embsecbio_duplicated_taxon_counts <-
#   # groups %>%
#   # embsecbio::pollen_data %>%
#   embsecbio_repatriated_dates_info$pollen %>%
#   dplyr::group_by(ID_SAMPLE, taxon_clean) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) %>%
#   purrr::pmap_df(function(ID_SAMPLE, taxon_clean, ...) {
#     embsecbio::pollen_data %>%
#       dplyr::filter(ID_SAMPLE == !!ID_SAMPLE,
#                     taxon_clean == !!taxon_clean)
#   })
#
# embsecbio_repatriated_dates_info$pollen %>%
#   dplyr::filter(ID_SAMPLE %in% groups$ID_SAMPLE,
#                 taxon_clean %in% groups$taxon_clean)
#   dplyr::filter(ID_SAMPLE %in% c(6492))

## EMBSeCBIO (extra) ----
embsecbio_extra_repatriation <- epd_repatriation_tmp_file %>%
  readxl::read_excel(sheet = 3) %>%
  janitor::clean_names() %>%
  dplyr::mutate(dates_to_be_extracted_from_embsecbio =
                  to_bool(dates_to_be_extracted_from_embsecbio),
                age_model_to_be_extracted_from_embsecbio =
                  to_bool(age_model_to_be_extracted_from_embsecbio)) %>%
  dplyr::rename(EMBSeCBIO_ID_ENTITY = embsecbio_id_entity,
                entity_name = entity_name_5,
                EMBSeCBIO_entity_name = entity_name_10)

### EMBSeCBIO dates (9) ----
embsecbio_extra_repatriation_dates <- embsecbio_extra_repatriation %>%
  dplyr::filter(dates_to_be_extracted_from_embsecbio)

embsecbio_extra_repatriated_dates_info <-
  embsecbio_extra_repatriation_dates$EMBSeCBIO_ID_ENTITY %>%
  extract_embsecbio()

#### Entities ----
embsecbio_extra_repatriated_dates_info_entities <-
  embsecbio_extra_repatriated_dates_info$metadata %>%
  dplyr::arrange(site_name, entity_name) %>%
  dplyr::mutate(ID_SITE = c(1448, 1449, 1450, 1451, 1451, 1451, 1451, 1452, 1453),
                ID_ENTITY = 1619:1627) %>%
  dplyr::select(-entity_type)
  # dplyr::mutate(ID_SITE = c(1, 2, 3, 4, 4, 4, 4, 5, 6) + get_id_site(conn),
  #               ID_ENTITY = seq_along(ID_ENTITY) + get_id_entity(conn))

meta_neo_res <- seq_len(nrow(embsecbio_extra_repatriated_dates_info_entities)) %>%
  purrr::map(function(i) {
    embsecbio_extra_repatriated_dates_info_entities[i, ] %>%
      rpd:::add_records(conn = conn, table = "entity", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_NEO_DB <- dabr::select_all(conn, "entity") %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_extra_repatriated_dates_info_entities$ID_ENTITY)
waldo::compare(embsecbio_extra_repatriated_dates_info_entities %>%
                 .[order(colnames(.))],
               EPD_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-doi))


embsecbio_extra_repatriated_dates_info_2 <- embsecbio_extra_repatriation_dates %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = site_name,
                # neotoma_entity_name = entity_name,
                ID_ENTITY = EMBSeCBIO_ID_ENTITY) %>%
  dplyr::inner_join(embsecbio_extra_repatriated_dates_info$metadata,
                    by = "ID_ENTITY") %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)
embsecbio_extra_repatriated_dates_info_3 <- embsecbio_extra_repatriated_dates_info_entities %>%
  dplyr::select(ID_SITE, ID_ENTITY, site_name, entity_name) %>%
  dplyr::right_join(embsecbio_extra_repatriated_dates_info_2 %>%
                      dplyr::select(1:6, 8),
                    by = c("entity_name" = "neotoma_entity_name"))

#### External links ----
meta_neo_res <- seq_len(nrow(embsecbio_extra_repatriated_dates_info_3))[-1] %>%
  purrr::map(function(i) {
    embsecbio_extra_repatriated_dates_info_3[i, ] %>%
      dplyr::select(ID_SITE,
                    ID_ENTITY,
                    external_ID_SITE,
                    external_ID_ENTITY,
                    external_site_name,
                    external_entity_name) %>%
      dplyr::mutate(external_source = "EMBSECBIO") %>%
      rpd:::add_records(conn = conn, table = "external_link", dry_run = FALSE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_NEO_DB <- dabr::select_all(conn, "external_link") %>%
  dplyr::filter(external_ID_ENTITY %in%
                  embsecbio_extra_repatriated_dates_info_3$external_ID_ENTITY,
                external_source == "EMBSECBIO") %>%
  dplyr::select(ID_SITE,
                ID_ENTITY,
                external_ID_SITE,
                external_ID_ENTITY,
                external_site_name,
                external_entity_name)
waldo::compare(embsecbio_extra_repatriated_dates_info_3 %>%
                 dplyr::select(ID_SITE,
                               ID_ENTITY,
                               external_ID_SITE,
                               external_ID_ENTITY,
                               external_site_name,
                               external_entity_name) %>%
                 dplyr::mutate(external_ID_SITE = as.integer(external_ID_SITE),
                               external_ID_ENTITY = as.integer(external_ID_ENTITY)),
               EPD_NEO_DB %>%
                 dplyr::slice(-1))

#### Dates ----
embsecbio_extra_repatriated_dates_info_4 <-
  embsecbio_extra_repatriated_dates_info$date_info %>%
  dplyr::select(-ID_DATE_INFO) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                reason_age_not_used = date_comments) %>%
  dplyr::mutate(age_used = ifelse(is.na(reason_age_not_used), "yes", "no")) %>%
  dplyr::left_join(embsecbio_extra_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  dplyr::select(-external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1)
meta_neo_res <- seq_len(nrow(embsecbio_extra_repatriated_dates_info_4)) %>%
  purrr::map(function(i) {
    embsecbio_extra_repatriated_dates_info_4[i, ] %>%
      rpd:::add_records(conn = conn, table = "date_info", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_NEO_DB <- dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_extra_repatriated_dates_info_4$ID_ENTITY)
waldo::compare(embsecbio_extra_repatriated_dates_info_4 %>%
                 .[order(colnames(.))],
               EPD_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-age_calib, -ID_DATE_INFO, -notes))

#### Samples ----
embsecbio_extra_repatriated_am_info_4 <- embsecbio_extra_repatriated_dates_info$age_model %>%
  dplyr::mutate(age = dplyr::coalesce(age, est_age_original),
                age_original = est_age_original) %>%
  dplyr::select(-est_age_original, -comment) %>%
  dplyr::left_join(embsecbio_extra_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  # dplyr::filter(ID_ENTITY != 18) %>%
  dplyr::select(-ID_SAMPLE, -external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1)


meta_neo_res <- seq_len(nrow(embsecbio_extra_repatriated_am_info_4)) %>%
  purrr::map(function(i) {
    embsecbio_extra_repatriated_am_info_4[i, ] %>%
      dplyr::select(1:5) %>%
      rpd:::add_records(conn = conn, table = "sample", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_extra_repatriated_am_info_4$ID_ENTITY)
waldo::compare(embsecbio_extra_repatriated_am_info_4 %>%
                 dplyr::select(1:5) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               EPD_NEO_DB %>%
                 dplyr::select(-age_older, -age_younger, -age_type, -ID_SAMPLE, -chronology_name, -thickness) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)), tolerance = 2)

#### Age models ----
EPD_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_extra_repatriated_am_info_4$ID_ENTITY)
embsecbio_extra_repatriated_am_info_5 <- embsecbio_extra_repatriated_am_info_4 %>%
  dplyr::mutate(ID_SAMPLE = EPD_NEO_DB$ID_SAMPLE, .before = 1) %>%
  # dplyr::filter(!(ID_ENTITY %in% c(1626, 1627))) %>% # Need further analysis
  dplyr::select(-c(2:6)) %>%
  dplyr::select(-age_original)
meta_neo_res <- seq_len(nrow(embsecbio_extra_repatriated_am_info_5)) %>%
  purrr::map(function(i) {
    embsecbio_extra_repatriated_am_info_5[i, ] %>%
      dplyr::mutate(ID_MODEL = 8, .before = 2) %>% # Bacon IntCal20
      rpd:::add_records(conn = conn, table = "age_model", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate ----
EPD_NEO_DB <- dabr::select_all(conn, "age_model") %>%
  dplyr::filter(ID_SAMPLE %in% embsecbio_extra_repatriated_am_info_5$ID_SAMPLE)
waldo::compare(embsecbio_extra_repatriated_am_info_5 %>%
                 dplyr::mutate(ID_MODEL = 8, .before = 2) %>%
                 .[order(colnames(.))],
               EPD_NEO_DB %>%
                 .[order(colnames(.))], tolerance = 2)

#### HERE (PENDING) ----
"The samples linked to the entities 1626 and 1627 might need to be deleted (2022-02-17)"
dabr::select_all(conn, "entity") %>%
  dplyr::filter(ID_ENTITY %in% c(1626, 1627))
dabr::select_all(conn, "external_link") %>%
  dplyr::filter(ID_ENTITY %in% c(1626, 1627))
dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% c(1626, 1627))
extract_embsecbio(c(1341, 1342))
dabr::select(conn, "SELECT * FROM age_model WHERE ID_SAMPLE BETWEEN 5813 AND 5870")


## IBERIA ----
iberia_repatriation <- epd_repatriation_tmp_file %>%
  readxl::read_excel(sheet = 4) %>%
  janitor::clean_names() %>%
  dplyr::mutate(site_in_iberia = to_bool(site_in_iberia),
                dates_to_be_extracted_from_iberia =
                  to_bool(dates_to_be_extracted_from_iberia),
                age_model_to_be_extracted_from_iberia =
                  to_bool(age_model_to_be_extracted_from_iberia)) %>%
  dplyr::rename(IBERIA_ID_ENTITY = iberia_id_entity,
                IBERIA_site_name = site_name_in_iberia,
                IBERIA_entity_name = entity_name_in_iberia)

# iberia_repatriation %>%
#   dplyr::arrange(IBERIA_site_name, IBERIA_entity_name) %>%
#   dplyr::distinct(IBERIA_site_name) %>%
#   dplyr::mutate(IBERIA_ID_SITE = seq_along(IBERIA_site_name))
  # dplyr::mutate(IBERIA_ID_ENTITY = seq_along(IBERIA_entity_name)) %>%

IBERIA_pollen %>%
  dplyr::select(1:5) %>%
  dplyr::distinct() %>%
  dplyr::filter(entity_name %in% iberia_repatriation_dates$IBERIA_entity_name)

### IBERIA dates (83) ----
iberia_repatriation_dates <- iberia_repatriation %>%
  dplyr::filter(dates_to_be_extracted_from_iberia)

iberia_repatriated_dates_info <-
  iberia_repatriation_dates$IBERIA_entity_name %>%
  extract_iberia()

iberia_repatriated_dates_info_2 <- iberia_repatriation_dates %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                entity_name = IBERIA_entity_name) %>%
                # ID_ENTITY = IBERIA_ID_ENTITY) %>%
  dplyr::inner_join(iberia_repatriated_dates_info$metadata,
                    by = "entity_name") %>%
  # dplyr::mutate(external_ID_ENTITY = NA,
  #               external_ID_SITE =  NA) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)
iberia_repatriated_dates_info_3 <- EPD_METADATA %>%
  dplyr::select(1:4, 6, 10) %>%
  dplyr::right_join(iberia_repatriated_dates_info_2 %>%
                      dplyr::select(1:7),
                    by = c("entity_name" = "neotoma_entity_name"))

#### External links ----
meta_neo_res <- seq_len(nrow(iberia_repatriated_dates_info_3)) %>%
  purrr::map(function(i) {
    iberia_repatriated_dates_info_3[i, ] %>%
      dplyr::select(ID_SITE,
                    ID_ENTITY,
                    external_ID_SITE,
                    external_ID_ENTITY,
                    external_site_name,
                    external_entity_name) %>%
      dplyr::mutate(external_source = "IBERIA") %>%
      rpd:::add_records(conn = conn, table = "external_link", dry_run = FALSE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_NEO_DB <- dabr::select_all(conn, "external_link") %>%
  dplyr::filter(external_ID_ENTITY %in%
                  iberia_repatriated_dates_info_3$external_ID_ENTITY,
                external_source == "IBERIA") %>%
  dplyr::select(ID_SITE,
                ID_ENTITY,
                external_ID_SITE,
                external_ID_ENTITY,
                external_site_name,
                external_entity_name)
waldo::compare(iberia_repatriated_dates_info_3 %>%
                 dplyr::select(ID_SITE,
                               ID_ENTITY,
                               external_ID_SITE,
                               external_ID_ENTITY,
                               external_site_name,
                               external_entity_name) %>%
                 dplyr::mutate(external_ID_SITE = as.integer(external_ID_SITE),
                               external_ID_ENTITY = as.integer(external_ID_ENTITY)),
               EPD_NEO_DB)

#### Dates ----
iberia_repatriated_dates_info_4 <-
  iberia_repatriated_dates_info$date_info %>%
  dplyr::select(-ID_SITE) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                reason_age_not_used = notes,
                age_calib = age_cal) %>%
  dplyr::mutate(age_used = ifelse(is.na(reason_age_not_used), "yes", "no")) %>%
  dplyr::left_join(iberia_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  dplyr::select(-external_ID_ENTITY, -site_name, -entity_name) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::mutate(ID_DATE_INFO = 657:1272)

meta_neo_res <- seq_len(nrow(iberia_repatriated_dates_info_4)) %>%
  purrr::map(function(i) {
    iberia_repatriated_dates_info_4[i, ] %>%
      rpd:::add_records(conn = conn, table = "date_info", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in% iberia_repatriated_dates_info_4$ID_ENTITY)
waldo::compare(iberia_repatriated_dates_info_4 %>%
                 .[order(colnames(.))],
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-notes),
               tolerance = 1)

### IBERIA age models (106) ----
iberia_repatriation_am <- iberia_repatriation %>%
  dplyr::filter(age_model_to_be_extracted_from_iberia)

iberia_repatriated_am_info <-
  iberia_repatriation_am$IBERIA_entity_name %>%
  extract_iberia()

iberia_repatriated_am_info_2 <- iberia_repatriation_am %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                entity_name = IBERIA_entity_name) %>%
  # ID_ENTITY = IBERIA_ID_ENTITY) %>%
  dplyr::inner_join(iberia_repatriated_am_info$metadata,
                    by = "entity_name") %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)

iberia_repatriated_am_info_3 <- EPD_METADATA %>%
  dplyr::select(1:4, 6, 10) %>%
  dplyr::right_join(iberia_repatriated_am_info_2 %>%
                      dplyr::select(1:7),
                    by = c("entity_name" = "neotoma_entity_name"))

#### Samples ----
iberia_repatriated_am_info_EPD_COUNTS <- EPD_COUNTS %>%
  dplyr::filter(dataset_id %in% iberia_repatriated_am_info_3$dataset_id) %>%
  smpds::rm_na_taxa(1:16) %>%
  dplyr::select(-chronology_id) %>%
  dplyr::left_join(EPD_METADATA %>%
                     dplyr::select(1:6, 10),
                   by = c("site_id", "site_name", "site_name_clean", "dataset_id", "entity_name")) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1)

iberia_repatriated_am_info_4 <- iberia_repatriated_am_info$age_model %>%
  dplyr::select(-ID_SITE) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY) %>%
  dplyr::left_join(iberia_repatriated_am_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  dplyr::select(-external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::select(-site_name, -entity_name, -latitude, -longitude, -elevation, -source, -publication)

# Extract thickness from the EPD data
iberia_repatriated_am_info_5 <- iberia_repatriated_am_info_4 %>%
  dplyr::mutate(depth2 = round(depth, 3)) %>%
  dplyr::left_join(iberia_repatriated_am_info_EPD_COUNTS %>%
                     dplyr::select(ID_ENTITY, depth, thickness, chronology_name,
                                   age_EPD = age, age_younger, age_older, age_type) %>%
                     dplyr::mutate(depth = round(depth, 3)),
                   by = c("ID_ENTITY",  "depth2" = "depth")) %>%
  dplyr::select(-depth2) %>%
  dplyr::relocate(thickness,
                  # age_original,
                  age_EPD,
                  chronology_name,
                  age_younger,
                  age_older,
                  age_type,
                  .before = mean) %>%
  # dplyr::filter(age != age_EPD)
  # dplyr::group_by(ID_ENTITY) %>%
  dplyr::mutate(age = ifelse(is.na(age), round(age_EPD), age)) %>%
  dplyr::mutate(lower = round(age_EPD) * 0.99,
                upper = round(age_EPD) * 1.01,
                same_age = (round(age) >= lower & round(age) <= upper) | (is.na(age) & is.na(age_EPD)),
                chronology_name = ifelse(same_age, chronology_name, NA),
                age_younger = ifelse(same_age, age_younger, NA),
                age_older = ifelse(same_age, age_older, NA),
                age_type = ifelse(same_age, age_type, NA)) %>%
  dplyr::select(-age_EPD, -lower, -upper, -same_age) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(ID_ENTITY) + get_id_sample(conn),
                .before = 1)

# Verify if the EPD has more samples than the IBERIA
tibble::tibble(
  ID_ENTITY = names(table(iberia_repatriated_am_info_EPD_COUNTS$ID_ENTITY)),
  EPD = table(iberia_repatriated_am_info_EPD_COUNTS$ID_ENTITY),
  EMB = table(iberia_repatriated_am_info_4$ID_ENTITY)
) %>%
  dplyr::filter(EPD > EMB)

# Verify if the "new" samples exist in the DB
dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_SAMPLE %in% iberia_repatriated_am_info_5$ID_SAMPLE)

meta_neo_res <- seq_len(nrow(iberia_repatriated_am_info_5))[-1] %>%
  purrr::map(function(i) {
    iberia_repatriated_am_info_5[i, ] %>%
      dplyr::select(1:9) %>%
      rpd:::add_records(conn = conn, table = "sample", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% iberia_repatriated_am_info_5$ID_ENTITY)
waldo::compare(iberia_repatriated_am_info_5 %>%
                 dplyr::select(1:9) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               EPD_DATES_NEO_DB %>%
                 dplyr::select(-count_type, -sample_type) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)), tolerance = 2)

#### Age models ----
meta_neo_res <- seq_len(nrow(iberia_repatriated_am_info_5))[-1] %>%
  purrr::map(function(i) {
    iberia_repatriated_am_info_5[i, ] %>%
      dplyr::select(1, 10:15) %>%
      dplyr::mutate(ID_MODEL = 8, .before = 2) %>% # Bacon IntCal20
      rpd:::add_records(conn = conn, table = "age_model", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate ----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "age_model") %>%
  dplyr::filter(ID_SAMPLE %in% iberia_repatriated_am_info_5$ID_SAMPLE)
waldo::compare(iberia_repatriated_am_info_5 %>%
                 dplyr::select(1, 10:15) %>%
                 dplyr::mutate(ID_MODEL = 8, .before = 2) %>%
                 .[order(colnames(.))],
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))], tolerance = 2)

#### HERE: COUNTS ----

## IBERIA (extra) ----
iberia_extra_repatriation <- epd_repatriation_tmp_file %>%
  readxl::read_excel(sheet = 5) %>%
  janitor::clean_names() %>%
  dplyr::mutate(site_in_iberia = to_bool(site_in_iberia),
                dates_to_be_extracted_from_iberia =
                  to_bool(dates_to_be_extracted_from_iberia),
                age_model_to_be_extracted_from_iberia =
                  to_bool(age_model_to_be_extracted_from_iberia)) %>%
  dplyr::rename(entity_name = entity_name_5) %>%
  dplyr::select(-site_id, -iberia_id_entity, -entity_name_10)

### IBERIA dates (39) ----
iberia_extra_repatriation_dates <- iberia_extra_repatriation %>%
  dplyr::filter(dates_to_be_extracted_from_iberia)

iberia_extra_repatriated_dates_info <-
  iberia_extra_repatriation_dates$entity_name %>%
  extract_iberia()

#### HERE Entities ----
iberia_extra_repatriated_dates_info_entities <-
  iberia_extra_repatriated_dates_info$metadata %>%
  dplyr::arrange(site_name, entity_name) %>%
  # dplyr::mutate(ID_SITE = seq_along(ID_SITE) + get_id_site(conn),
  #               ID_ENTITY = seq_along(ID_ENTITY) + get_id_entity(conn))
  dplyr::mutate(ID_SITE = 1454:1492,
                ID_ENTITY = 1628:1666)

##### Verify if the "new" entities exist in the DB
dabr::select_all(conn, "entity") %>%
  dplyr::filter(ID_SITE %in% iberia_extra_repatriated_dates_info_entities$ID_SITE)
dabr::select_all(conn, "entity") %>%
  dplyr::filter(ID_ENTITY %in% iberia_extra_repatriated_dates_info_entities$ID_ENTITY)

meta_neo_res <- seq_len(nrow(iberia_extra_repatriated_dates_info_entities)) %>%
  purrr::map(function(i) {
    iberia_extra_repatriated_dates_info_entities[i, ] %>%
      rpd:::add_records(conn = conn, table = "entity", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_NEO_DB <- dabr::select_all(conn, "entity") %>%
  dplyr::filter(ID_ENTITY %in% iberia_extra_repatriated_dates_info_entities$ID_ENTITY)
waldo::compare(iberia_extra_repatriated_dates_info_entities %>%
                 .[order(colnames(.))],
               EPD_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-doi, -site_type))


iberia_extra_repatriated_dates_info_2 <-
  iberia_extra_repatriated_dates_info$metadata %>%
  dplyr::select(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)
  # iberia_extra_repatriation_dates %>%
  # dplyr::select(neotoma_ID_SITE = site_id,
  #               neotoma_site_name = site_name,
  #               neotoma_entity_name = site_name,
  #               # neotoma_entity_name = entity_name,
  #               ID_ENTITY = IBERIA_ID_ENTITY) %>%
  # dplyr::inner_join(iberia_extra_repatriated_dates_info$metadata,
  #                   by = "ID_ENTITY") %>%
  # dplyr::rename(external_ID_ENTITY = ID_ENTITY,
  #               external_ID_SITE =  ID_SITE,
  #               external_site_name = site_name,
  #               external_entity_name = entity_name)
iberia_extra_repatriated_dates_info_3 <- iberia_extra_repatriated_dates_info_entities %>%
  dplyr::select(ID_SITE, ID_ENTITY, external_entity_name = entity_name) %>%
  dplyr::right_join(iberia_extra_repatriated_dates_info_2,
                    by = c("external_entity_name"))
iberia_extra_repatriated_dates_info_3


#### External links ----
meta_neo_res <- seq_len(nrow(iberia_extra_repatriated_dates_info_3)) %>%
  purrr::map(function(i) {
    iberia_extra_repatriated_dates_info_3[i, ] %>%
      dplyr::select(ID_SITE,
                    ID_ENTITY,
                    external_ID_SITE,
                    external_ID_ENTITY,
                    external_site_name,
                    external_entity_name) %>%
      dplyr::mutate(external_source = "IBERIA") %>%
      # rpd:::update_records(conn = conn, table = "external_link", PK = c(1, 2, 3, 4))
      rpd:::add_records(conn = conn, table = "external_link", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_NEO_DB <- dabr::select_all(conn, "external_link") %>%
  dplyr::filter(external_ID_ENTITY %in%
                  iberia_extra_repatriated_dates_info_3$external_ID_ENTITY,
                external_source == "IBERIA") %>%
  dplyr::select(ID_SITE,
                ID_ENTITY,
                external_ID_SITE,
                external_ID_ENTITY,
                external_site_name,
                external_entity_name)
waldo::compare(iberia_extra_repatriated_dates_info_3 %>%
                 dplyr::select(ID_SITE,
                               ID_ENTITY,
                               external_ID_SITE,
                               external_ID_ENTITY,
                               external_site_name,
                               external_entity_name) %>%
                 dplyr::mutate(external_ID_SITE = as.integer(external_ID_SITE),
                               external_ID_ENTITY = as.integer(external_ID_ENTITY)),
               EPD_NEO_DB)

#### Dates ----
# iberia_repatriated_dates_info_4 <-
#   iberia_repatriated_dates_info$date_info %>%
#   dplyr::select(-ID_SITE) %>%
#   dplyr::rename(external_ID_ENTITY = ID_ENTITY,
#                 reason_age_not_used = notes,
#                 age_calib = age_cal) %>%
#   dplyr::mutate(age_used = ifelse(is.na(reason_age_not_used), "yes", "no")) %>%
#   dplyr::left_join(iberia_repatriated_dates_info_3 %>%
#                      dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
#   dplyr::select(-external_ID_ENTITY, -site_name, -entity_name) %>%
#   dplyr::relocate(ID_ENTITY, .before = 1) %>%
#   dplyr::mutate(ID_DATE_INFO = 657:1272)

iberia_extra_repatriated_dates_info_4 <-
  iberia_extra_repatriated_dates_info$date_info %>%
  dplyr::select(-ID_SITE) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                reason_age_not_used = notes,
                age_calib = age_cal) %>%
  dplyr::mutate(age_used = ifelse(is.na(reason_age_not_used), "yes", "no")) %>%
  dplyr::left_join(iberia_extra_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  dplyr::select(-external_ID_ENTITY, -site_name, -entity_name) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::mutate(ID_DATE_INFO = 1273:1606)
  # dplyr::mutate(ID_DATE_INFO = seq_along(ID_ENTITY) + get_id_date_info(conn))

meta_neo_res <- seq_len(nrow(iberia_extra_repatriated_dates_info_4)) %>%
  purrr::map(function(i) {
    iberia_extra_repatriated_dates_info_4[i, ] %>%
      rpd:::add_records(conn = conn, table = "date_info", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_NEO_DB <- dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in% iberia_extra_repatriated_dates_info_4$ID_ENTITY)
waldo::compare(iberia_extra_repatriated_dates_info_4 %>%
                 .[order(colnames(.))],
               EPD_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-notes),
               tolerance = 1)

#### Samples ----
iberia_extra_repatriated_am_info_4 <- iberia_extra_repatriated_dates_info$age_model %>%
  dplyr::select(-ID_SITE) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY) %>%
  dplyr::left_join(iberia_extra_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  dplyr::select(-external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::select(-site_name, -entity_name, -latitude, -longitude, -elevation, -source, -publication) %>%
  dplyr::mutate(ID_SAMPLE = seq_along(ID_ENTITY) + get_id_sample(conn),
                .before = 2)

meta_neo_res <- seq_len(nrow(iberia_extra_repatriated_am_info_4)) %>%
  purrr::map(function(i) {
    iberia_extra_repatriated_am_info_4[i, ] %>%
      dplyr::select(1:4) %>%
      rpd:::add_records(conn = conn, table = "sample", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% iberia_extra_repatriated_am_info_4$ID_ENTITY)
waldo::compare(iberia_extra_repatriated_am_info_4 %>%
                 dplyr::select(1:4) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               EPD_NEO_DB %>%
                 dplyr::select(-count_type, -age_older, -age_younger, -age_type, -chronology_name, -thickness, -sample_type) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               tolerance = 1)

#### Age models ----
# EPD_NEO_DB <- dabr::select_all(conn, "sample") %>%
#   dplyr::filter(ID_ENTITY %in% iberia_extra_repatriated_am_info_4$ID_ENTITY)
iberia_extra_repatriated_am_info_5 <- iberia_extra_repatriated_am_info_4 %>%
  # dplyr::mutate(ID_SAMPLE = EPD_NEO_DB$ID_SAMPLE, .before = 1) %>%
  dplyr::select(-c(1, 3:4))
meta_neo_res <- seq_len(nrow(iberia_extra_repatriated_am_info_5))[-1] %>%
  purrr::map(function(i) {
    iberia_extra_repatriated_am_info_5[i, ] %>%
      dplyr::mutate(ID_MODEL = 8, .before = 2) %>% # Bacon IntCal20
      rpd:::add_records(conn = conn, table = "age_model", dry_run = FALSE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate ----
EPD_NEO_DB <- dabr::select_all(conn, "age_model") %>%
  dplyr::filter(ID_SAMPLE %in% iberia_extra_repatriated_am_info_5$ID_SAMPLE)
waldo::compare(iberia_extra_repatriated_am_info_5 %>%
                 dplyr::mutate(ID_MODEL = 8, .before = 2) %>%
                 .[order(colnames(.))],
               EPD_NEO_DB %>%
                 .[order(colnames(.))],
               tolerance = 1)

#### HERE: COUNTS ----

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
