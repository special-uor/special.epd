`%>%` <- magrittr::`%>%`
# Download file ----
epd_repatriation_id <- "1knnWB6XQF_QAI9iumyUAdoFm37_7iTZh"
epd_repatriation_tmp_file <- tempfile(fileext = ".xslx")
googledrive::as_id(epd_repatriation_id) %>%
  googledrive::drive_download(path = epd_repatriation_tmp_file, overwrite = TRUE)

# Open DB connection ----
conn <- dabr::open_conn_mysql("SPECIAL-EPD",
                              password = rstudioapi::askForPassword())
conn_rpd <- dabr::open_conn_mysql("RPD-latest",
                                  password = rstudioapi::askForPassword())

# Load datasets ----
data("EPD_COUNTS")
data("EPD_DATES")
data("EPD_DATES_coretops")
data("EPD_METADATA")

a <- dplyr::bind_rows(
  EPD_DATES,
  EPD_DATES_coretops
)

a %>%
  dplyr::filter(!is.na(depth)) %>%
  dplyr::arrange(entity_name, depth) %>%
  dplyr::group_by(entity_name) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::filter(n < 2)

# Helper functions ----
to_bool <- function(x) {
  dplyr::case_when(is.na(x) ~ NA,
                   stringr::str_starts(x, "Y|y") ~ TRUE,
                   stringr::str_starts(x, "N|n") ~ FALSE,
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

extract_rpd <- function(ID_ENTITY = NULL) {
  if (missing(ID_ENTITY))
    return(NULL)
  entity_tb <- rpdata::entity %>%
    dplyr::filter(ID_ENTITY %in% !!ID_ENTITY) %>%
    dplyr::select(-core_location, -measurement_method, -last_updated) %>%
    dplyr::rename(entity_type = TYPE) %>%
    dplyr::left_join(rpdata::entity_link_publication) %>%
    dplyr::left_join(rpdata::publication) %>%
    dplyr::group_by(ID_ENTITY) %>%
    dplyr::mutate(publication = citation %>%
                    stringr::str_c(collapse = ";\n")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ID_PUB, -citation, -pub_DOI_URL, -bibentry, -ID_UNIT) %>%
    dplyr::distinct()
  if (nrow(entity_tb) == 0)
    return(NULL)
  site_tb <- rpdata::site %>%
    dplyr::filter(ID_SITE %in% entity_tb$ID_SITE) %>%
    dplyr::select(ID_SITE, site_name, site_type) # %>%
    # dplyr::select(-water_depth, -basin_size_class, -catch_size_class, -flow_type, -basin_size_km2, -catch_size_km2)
  metadata_tb <- site_tb %>%
    dplyr::right_join(entity_tb)
  date_info_tb <- rpdata::date_info %>%
    dplyr::filter(ID_ENTITY %in% entity_tb$ID_ENTITY) %>%
    dplyr::rename(depth = avg_depth,
                  age_c14 = age_C14) %>%
    dplyr::select(-correlation_info)
  sample_tb <- rpdata::sample %>%
    dplyr::filter(ID_ENTITY %in% entity_tb$ID_ENTITY) %>%
    dplyr::rename(depth =  avg_depth,
                  thickness = sample_thickness) %>%
    dplyr::select(-analytical_sample_size, -analytical_sample_size_unit) %>%
    dplyr::mutate(depth = depth * 100) %>%
    dplyr::rename(external_ID_ENTITY = ID_ENTITY) %>%
    dplyr::left_join(rpdata::chronology,
                     by = "ID_SAMPLE") %>%
    dplyr::left_join(rpdata::model_name,
                     by = "ID_MODEL") %>%
    dplyr::rename(age = original_est_age,
                  model_name_original = model_name) %>%
    dplyr::select(-ID_MODEL)
  age_model_tb <- rpdata::age_model %>%
    dplyr::filter(ID_SAMPLE %in% sample_tb$ID_SAMPLE) %>%
    dplyr::left_join(rpdata::model_name,
                     by = "ID_MODEL") %>%
    dplyr::select(-ID_MODEL) %>%
    dplyr::relocate(model_name, .before = 1)
  sample_age_model_tb <- sample_tb %>%
    dplyr::right_join(age_model_tb, by = "ID_SAMPLE")
  list(metadata = metadata_tb,
       date_info = date_info_tb,
       sample = sample_tb,
       age_model = sample_age_model_tb)
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

find_age_models <- function(path, entity_name) {
  outputs <- path %>%
    list.files(pattern = "\\.csv", recursive = TRUE, full.names = TRUE) %>%
    stringr::str_subset("bacon")
  outputs_tb <- tibble::tibble(path = outputs) %>%
    dplyr::mutate(entity_name = path %>%
                    purrr::map_chr(~.x %>% dirname %>% basename),
                  subfolder = path %>%
                    purrr::map_chr(~.x %>% dirname %>% dirname %>% basename),
                  .before = 1)
  if (missing(entity_name))
    return(outputs_tb)
  outputs_tb %>%
    dplyr::filter(entity_name %in% !!entity_name) %>%
    dplyr::mutate(am = path %>%
                    purrr::map(~suppressMessages(readr::read_csv(.x))))
}

upload_age_model <- function(conn, entity_name, am, ...) {
  rpd:::msg(entity_name, limit = 78, nl = FALSE)
  # Select records from the sample-charcoal table
  sample_tb <- dabr::select(conn,
                            "SELECT * FROM sample INNER JOIN entity ON",
                            "sample.ID_ENTITY = entity.ID_ENTITY WHERE",
                            "entity_name = ",
                            dabr::quote(entity_name),
                            quiet = TRUE)
  # Verify all the samples exist in the DB
  if (nrow(sample_tb) == nrow(am)) {
    # Check depths
    depth_diff <- abs(am$depths - sample_tb$depth) > 0.001
    if (sum(depth_diff) == nrow(sample_tb)) { # DB has the wrong units
      depth_diff <- abs(am$depths - sample_tb$depth) > 0.001
      if (sum(depth_diff) != nrow(sample_tb)) {
        rpd:::msg("The records in the DB have the wrong units, `cm`.")
        return(tibble::tibble(entity_name,
                              am = list(am),
                              status = FALSE,
                              reason = "The records in the DB have the wrong units, `cm`.",
                              ...))
      }
    }
    if (sum(depth_diff) > 0) {
      rpd:::msg("Mistmatch between depths detected.")
      return(tibble::tibble(entity_name,
                            am = list(am),
                            status = FALSE,
                            reason = "Mistmach in the depths.",
                            ...))
    }
    # Prepare raw data for insertion
    am <- am %>%
      dplyr::mutate(ID_MODEL = 8, # BACON INTCAL20
                    ID_SAMPLE = sample_tb$ID_SAMPLE) %>%
      dplyr::select(-c(sample_ids, depths)) %>%
      purrr::map_df(as.integer) # Convert columns to integer
    # Delete any existing age models
    all_samples_tb <- dabr::select(conn,
                                   "SELECT * FROM sample",
                                   "LEFT JOIN entity ON entity.ID_ENTITY = sample.ID_ENTITY",
                                   "WHERE entity_name = ",
                                   dabr::quote(entity_name),
                                   quiet = TRUE) %>%
      dplyr::arrange(ID_ENTITY)
    if (nrow(all_samples_tb) == 0) {
      print("NO SAMPLES WHERE FOUND")
    }
    if (nrow(all_samples_tb) > 0) {
      dabr::delete(conn,
                   "DELETE FROM age_model WHERE ID_SAMPLE IN (",
                   paste0(all_samples_tb$ID_SAMPLE, collapse = ", "),
                   ") AND ID_MODEL = 8")
    }
    rpd:::update_records(conn, "age_model", am, PK = c(7:8), quiet = TRUE)

    # Check inserted records
    age_model_tb <-
      dabr::select(conn,
                   "SELECT * FROM age_model WHERE ID_SAMPLE IN (",
                   paste0(am$ID_SAMPLE, collapse = ", "),
                   ") AND ID_MODEL = 8",
                   quiet = TRUE) %>%
      tibble::as_tibble()
    # Change case of columns
    colnames(age_model_tb) <- tolower(colnames(age_model_tb))
    colnames(am) <- tolower(colnames(am))
    # Change column order
    am <- am[, colnames(age_model_tb)]
    print(waldo::compare(am, age_model_tb))#, tolerance = 0.01))
  } else {
    rpd:::msg("Mistmatch in the number of samples: ", entity_name)
    return(tibble::tibble(am = list(am), status = FALSE, reason = "Mistmatch in the number of samples."))
  }

  return(tibble::tibble(entity_name,
                        am = list(am),
                        status = TRUE,
                        reason = NA_character_,
                        ...))
  # return(tibble::tibble(am = list(am), status = TRUE, reason = NA_character_))
}

path <- "~/Downloads/special_epd_am"
find_age_models(path)

# Age models ----
epd_age_models_id <- "1wzGY-qFOsrll5MbNipgNjjaKklHldZxO"
epd_age_models_tmp_file <- tempfile(fileext = ".xslx")
googledrive::as_id(epd_age_models_id) %>%
  googledrive::drive_download(path = epd_age_models_tmp_file, overwrite = TRUE)
epd_age_models <- epd_age_models_tmp_file %>%
  readxl::read_excel(sheet = 1) %>%
  janitor::clean_names() %>%
  dplyr::mutate(site_id = as.integer(site_id %>%
                                       stringr::str_squish()),
                age_model_run_successfully_ready_to_check =
                  to_bool(age_model_run_successfully_ready_to_check),
                age_model_checked =
                  to_bool(age_model_checked),
                ready_to_upload =
                  to_bool(ready_to_upload))

epd_age_models_ready_to_upload <- epd_age_models %>%
  dplyr::filter(ready_to_upload)

find_age_models(path) %>%
  dplyr::filter(entity_name %in% epd_age_models_ready_to_upload$entity_name)

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
                matched_lab_no = to_bool(matched_lab_no)) %>%
  dplyr::rename(entity_name = entity_name_7,
                RPD_entity_name = entity_name_12,
                RPD_ID_ENTITY = rpd_id_entity)
rpd_repatriation

### RPD dates (113) ----
rpd_repatriation_dates <- rpd_repatriation %>%
  dplyr::filter(dates_to_be_extracted_from_rpd)

rpd_repatriated_dates_info <-
  rpd_repatriation_dates$RPD_ID_ENTITY %>%
  extract_rpd()
rpd_repatriated_dates_info_2 <- rpd_repatriation_dates %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                ID_ENTITY = RPD_ID_ENTITY) %>%
  dplyr::inner_join(rpd_repatriated_dates_info$metadata,
                    by = "ID_ENTITY") %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)
rpd_repatriated_dates_info_3 <- EPD_METADATA %>%
  dplyr::select(1:4, 6, 10) %>%
  dplyr::right_join(rpd_repatriated_dates_info_2 %>%
                      dplyr::select(1:6, 8),
                    by = c("entity_name" = "neotoma_entity_name"))

#### External links ----
meta_neo_res <- rpd_repatriated_dates_info_3 %>%
  dplyr::filter(external_entity_name %>% stringr::str_detect("Solso")) %>%
  nrow() %>%
  seq_len() %>%
  purrr::map(function(i) {
    rpd_repatriated_dates_info_3 %>%
      dplyr::filter(external_entity_name %>% stringr::str_detect("Solso")) %>%
      dplyr::slice(i) %>%
      dplyr::select(ID_SITE,
                    ID_ENTITY,
                    external_ID_SITE,
                    external_ID_ENTITY,
                    external_site_name,
                    external_entity_name) %>%
      dplyr::mutate(external_source = "RPD") %>%
      rpd:::add_records(conn = conn, table = "external_link", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_METADATA_NEO_DB <- dabr::select_all(conn, "external_link") %>%
  dplyr::filter(external_ID_ENTITY %in%
                  rpd_repatriated_dates_info_3$external_ID_ENTITY,
                external_source == "RPD") %>%
  dplyr::select(ID_SITE,
                ID_ENTITY,
                external_ID_SITE,
                external_ID_ENTITY,
                external_site_name,
                external_entity_name)
waldo::compare(rpd_repatriated_dates_info_3 %>%
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
rpd_repatriated_dates_info_4 <-
  rpd_repatriated_dates_info$date_info %>%
  dplyr::select(-ID_DATE_INFO) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY) %>%
  # dplyr::mutate(age_used = ifelse(is.na(reason_age_not_used), "yes", "no")) %>%
  dplyr::left_join(rpd_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  dplyr::select(-external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::rename(lab_num = lab_number) #%>%
  #dplyr::filter(ID_ENTITY %in% c(838)) # New records

special.epd::dump_all(conn, ID_ENTITY = 838)
meta_neo_res <- seq_len(nrow(rpd_repatriated_dates_info_4)) %>%
  purrr::map(function(i) {
    rpd_repatriated_dates_info_4[i, ] %>%
      rpd:::add_records(conn = conn, table = "date_info", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in% rpd_repatriated_dates_info_4$ID_ENTITY)
waldo::compare(rpd_repatriated_dates_info_4 %>%
                 .[order(colnames(.))] %>%
                 dplyr::arrange(ID_ENTITY, depth),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-ID_DATE_INFO) %>%
                 dplyr::arrange(ID_ENTITY, depth),
               max_diffs = Inf)

#### DURANK3 ----
"The entity called 'DURANK3' was identified in both the EMBSeCBIO and RPD."
"The age models will be inspected to determine which version to keep."
a <- special.epd::get_entity(conn, 780)
special.epd::dump_all(conn, ID_ENTITY = 780)
dabr::select(conn,
             "SELECT * FROM external_link WHERE",
             "external_ID_SITE = 620 AND",
             "external_ID_ENTITY = 712 AND",
             "external_source = 'RPD'")
dabr::delete(conn,
             "DELETE FROM external_link WHERE",
             "external_ID_SITE = 620 AND",
             "external_ID_ENTITY = 712 AND",
             "external_source = 'RPD'")
# Results: 1 record was deleted.
dabr::select(conn,
             "SELECT * FROM date_info WHERE",
             "ID_ENTITY = 780")
dabr::update(conn,
             "UPDATE date_info SET material_dated = 'charcoal' WHERE",
             "ID_ENTITY = 780 AND",
             "ID_DATE_INFO IN (30, 31, 32, 33)")
# Results: 4 records were updated.
dabr::update(conn,
             "UPDATE date_info SET material_dated = 'undifferentiated plant macrofossil' WHERE",
             "ID_ENTITY = 780 AND",
             "ID_DATE_INFO IN (34)")
# Results: 1 record was updated.
dabr::update(conn,
             "UPDATE date_info SET date_type = 'Top of core known' WHERE",
             "ID_ENTITY = 780 AND",
             "ID_DATE_INFO = 1623") # Originally a 'Pollen correlation'
# Results: 1 record was updated.

# Delete duplicated dates
dabr::select(conn,
             "SELECT * FROM date_info WHERE",
             "ID_ENTITY = 780 AND",
             "ID_DATE_INFO IN (1621, 1622, 1624, 1625, 1626)")
dabr::delete(conn,
             "DELETE FROM date_info WHERE",
             "ID_ENTITY = 780 AND",
             "ID_DATE_INFO IN (1621, 1622, 1624, 1625, 1626)")
# Results: 5 records were deleted.

### non-RPD dates (7) ----
non_rpd_repatriation_dates <- rpd_repatriation %>%
  dplyr::filter(!dates_to_be_extracted_from_rpd) %>%
  dplyr::filter(entity_name != "DURANK3")

non_rpd_repatriated_dates_info <-
  non_rpd_repatriation_dates$RPD_ID_ENTITY %>%
  extract_rpd()
non_rpd_repatriated_dates_info_2 <- non_rpd_repatriation_dates %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                ID_ENTITY = RPD_ID_ENTITY) %>%
  dplyr::inner_join(non_rpd_repatriated_dates_info$metadata,
                    by = "ID_ENTITY") %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)
non_rpd_repatriated_dates_info_3 <- EPD_METADATA %>%
  dplyr::select(1:4, 6, 10) %>%
  dplyr::right_join(non_rpd_repatriated_dates_info_2 %>%
                      dplyr::select(1:6, 8),
                    by = c("entity_name" = "neotoma_entity_name"))

#### Dates ----
non_rpd_repatriated_dates_info_4 <- EPD_DATES %>%
  dplyr::filter(entity_name %in% non_rpd_repatriated_dates_info_3$entity_name) %>%
  # dplyr::rename(external_ID_SITE = site_id) %>%
  dplyr::left_join(non_rpd_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, site_id, entity_name)) %>%
  dplyr::select(-ages_already, -site_id, -site_name, -site_name_clean, -entity_name) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::rename(age_calib = age_cal)

# Check for existing dates
special.epd::dump_all(conn, ID_ENTITY = non_rpd_repatriated_dates_info_4$ID_ENTITY)
meta_neo_res <- seq_len(nrow(non_rpd_repatriated_dates_info_4)) %>%
  purrr::map(function(i) {
    non_rpd_repatriated_dates_info_4[i, ] %>%
      rpd:::add_records(conn = conn, table = "date_info", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in% non_rpd_repatriated_dates_info_4$ID_ENTITY)
waldo::compare(non_rpd_repatriated_dates_info_4 %>%
                 .[order(colnames(.))] %>%
                 dplyr::arrange(ID_ENTITY, depth),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-ID_DATE_INFO) %>%
                 dplyr::arrange(ID_ENTITY, depth),
               max_diffs = Inf)

### RPD age models (47) ----
rpd_repatriation_am <- rpd_repatriation %>%
  dplyr::filter(age_model_to_be_extracted_from_rpd)

rpd_repatriated_am_info <-
  rpd_repatriation_am$RPD_ID_ENTITY %>%
  extract_rpd()

rpd_repatriated_am_info_2 <- rpd_repatriation_am %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                ID_ENTITY = RPD_ID_ENTITY) %>%
  dplyr::inner_join(rpd_repatriated_am_info$metadata,
                    by = "ID_ENTITY") %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)

rpd_repatriated_am_info_3 <- EPD_METADATA %>%
  dplyr::select(1:4, 6, 10) %>%
  dplyr::right_join(rpd_repatriated_am_info_2 %>%
                      dplyr::select(1:6, 8),
                    by = c("entity_name" = "neotoma_entity_name"))

#### Samples ----
rpd_repatriated_am_info_EPD_COUNTS <- EPD_COUNTS %>%
  dplyr::filter(dataset_id %in% rpd_repatriated_am_info_3$dataset_id) %>%
  smpds::rm_na_taxa(1:16) %>%
  dplyr::select(-chronology_id) %>%
  dplyr::left_join(EPD_METADATA %>%
                     dplyr::select(1:6, 10),
                   by = c("site_id", "site_name", "site_name_clean", "dataset_id", "entity_name")) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1)

rpd_repatriated_am_info_4 <- rpd_repatriated_am_info$sample %>%
  dplyr::left_join(rpd_repatriated_am_info$age_model) %>%
  # dplyr::mutate(age = dplyr::coalesce(age, est_age_original),
  #               age_original = est_age_original) %>%
  # dplyr::select(-est_age_original, -comment) %>%
  dplyr::left_join(rpd_repatriated_am_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY),
                   by = "external_ID_ENTITY") %>%
  # dplyr::filter(ID_ENTITY != 18) %>%
  dplyr::select(-ID_SAMPLE, -external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1)

# # Extract thickness from the EPD data
# rpd_repatriated_am_info_5 <- rpd_repatriated_am_info_4 %>%
#   dplyr::mutate(depth2 = round(depth, 3)) %>%
#   dplyr::left_join(rpd_repatriated_am_info_EPD_COUNTS %>%
#                      dplyr::select(ID_ENTITY, depth, thickness, chronology_name,
#                                    age_EPD = age, age_younger, age_older, age_type) %>%
#                      dplyr::mutate(depth = round(depth, 3)),
#                    by = c("ID_ENTITY",  "depth2" = "depth")) %>%
#   dplyr::select(-depth2) %>%
#   dplyr::relocate(#thickness,
#                   # age_original,
#                   age_EPD,
#                   chronology_name,
#                   age_younger,
#                   age_older,
#                   age_type,
#                   .before = mean) %>%
#   # dplyr::filter(age != age_EPD)
#   # dplyr::group_by(ID_ENTITY) %>%
#   dplyr::mutate(lower = round(age_EPD) * 0.99,
#                 upper = round(age_EPD) * 1.01,
#                 same_age = (round(age) >= lower & round(age) <= upper) | (is.na(age) & is.na(age_EPD)),
#                 chronology_name = ifelse(same_age, chronology_name, NA),
#                 age_younger = ifelse(same_age, age_younger, NA),
#                 age_older = ifelse(same_age, age_older, NA),
#                 age_type = ifelse(same_age, age_type, NA)) %>%
#   dplyr::select(-age_original, -age_EPD, -lower, -upper, -same_age) %>%
#   dplyr::mutate(ID_SAMPLE = seq_along(ID_ENTITY), .before = 1)
# # dplyr::mutate(ID_SAMPLE = get_id_sample(conn))
#
# # Verify if the EPD has more samples than the RPD
# tibble::tibble(
#   ID_ENTITY = names(table(rpd_repatriated_am_info_EPD_COUNTS$ID_ENTITY)),
#   EPD = table(rpd_repatriated_am_info_EPD_COUNTS$ID_ENTITY),
#   EMB = table(rpd_repatriated_am_info_4$ID_ENTITY)
# ) %>%
#   dplyr::filter(EPD > EMB)

rpd_repatriated_am_info_3 %>%
  dplyr::filter(!(ID_ENTITY %in% rpd_repatriated_am_info_4$ID_ENTITY))

external_links <- dabr::select_all(conn, "external_link") %>%
  dplyr::filter(ID_ENTITY %in% rpd_repatriated_am_info_3$ID_ENTITY)

aux <- conn %>%
  special.epd::dump_all(ID_ENTITY = rpd_repatriated_am_info_4$ID_ENTITY)
rpd_repatriated_am_info_3 %>%
  dplyr::filter(!(ID_ENTITY %in% aux$date_info$ID_ENTITY))

meta_neo_res <- seq_len(nrow(rpd_repatriated_am_info_4)) %>%
  purrr::map(function(i) {
    rpd_repatriated_am_info_4[i, ] %>%
      dplyr::mutate(chronology_name =
                      stringr::str_c("Original age model: ",
                                     model_name_original)) %>%
      dplyr::select(ID_ENTITY, depth, thickness, chronology_name, age) %>%
      rpd:::add_records(conn = conn, table = "sample", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% rpd_repatriated_am_info_4$ID_ENTITY)
waldo::compare(rpd_repatriated_am_info_4 %>%
                 dplyr::mutate(chronology_name =
                                 stringr::str_c("Original age model: ",
                                                model_name_original)) %>%
                 dplyr::select(ID_ENTITY, depth, thickness, chronology_name, age) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)) %>%
                 dplyr::select(-age_older, -age_type, -age_younger,
                               -ID_SAMPLE, -sample_type, -count_type),
               tolerance = 2)

#### Age models (PENDING 17 AM) ----
# NOTE: There are 17 entities with pending AM
rpd_records_wo_am <- tibble::tribble(
  ~ID_SITE, ~ID_ENTITY,                ~site_name, ~entity_name, ~external_ID_SITE, ~external_ID_ENTITY,           ~external_site_name,      ~external_entity_name, ~external_source,
  196L,       217L,              "Bruckmisse",       "BRM1",              814L,               1340L,                  "Bruckmisse",          "Bruckmisse_BRM1",            "RPD",
  196L,       218L,              "Bruckmisse",       "BRM3",              814L,               1341L,                  "Bruckmisse",          "Bruckmisse_BRM3",            "RPD",
  236L,       265L,             "Champ Gazon",   "CHAGAZON",             1922L,               2381L,                 "Champ Gazon",         "Champ Gazon core",            "RPD",
  365L,       405L,      "Etang de la Gruere",      "EGR2A",            15746L,              40452L,          "Etang de la Gruère",                    "EGR2A",        "NEOTOMA",
  365L,       406L,      "Etang de la Gruere",      "EGR2G",            15746L,              40484L,          "Etang de la Gruère",                    "EGR2G",        "NEOTOMA",
  422L,       469L,             "Glaswaldsee",       "GWA1",              820L,               1354L,                 "Glaswaldsee",         "Glaswaldsee core",            "RPD",
  430L,       478L,             "Gorgo Basso",   "GORGOBAS",              609L,                701L,                 "Gorgo Basso",     "Gorgo Basso core GL3",            "RPD",
  433L,       481L,            "Gourte Mires",     "GOURTE",             1636L,               2075L,                "Gourte Mires",        "Gourte Mires core",            "RPD",
  690L,       776L,               "Lake Bled",      "BLEDC",             1332L,               1717L,                   "Lake Bled",           "Lake Bled core",            "RPD",
  786L,       890L,    "Le Verny des Brulons",   "VERNYBRU",             1836L,               2291L,           "Verny des Brulons",   "Verny des Brulons core",            "RPD",
  852L,       962L,             "Long Breach",   "LONGBREA",              547L,               1437L,                 "Long Breach",         "Long Breach Core",            "RPD",
  1086L,      1206L,         "Pryskyricny dul",   "PRYSKYRI",             1985L,               2458L,             "Pryskyricny dul",            "PRYSKYRI core",            "RPD",
  1301L,      1443L,                "Torveraz",   "TORVERA2",              808L,                905L,                    "Torveraz",          "Torveraz core 2",            "RPD",
  1354L,      1506L,             "Vaike Juusa",      "VJUUS",             1355L,               2466L,                 "Vaike Juusa",                      "J20",            "RPD",
  1374L,      1530L,    "Vestre Oykjamyrtjorn",      "OYKJA",             1840L,               2295L,         "Vestre Oykjamyrtorn", "Vestre Oykjamyrtorn core",            "RPD",
  1410L,      1576L, "Wilder See am Ruhestein",       "WILA",              802L,               1414L,     "Wilder See am Ruhestein",          "Wilder See core",            "RPD",
  1412L,      1579L, "Wildseemoor-Kaltenbronn",       "KWI2",              801L,               1415L, "Wildseemoor bei Kaltenbronn",    "Wildseemoor core KWI2",            "RPD"
)

# aux2 <- rpd_repatriated_am_info_5 %>%
#   dplyr::mutate(ID_MODEL = 8, .before = 2) %>% # Bacon IntCal20
#   dplyr::select(ID_MODEL, ID_SAMPLE, mean, median,
#                 UNCERT_5, UNCERT_25, UNCERT_75, UNCERT_95) %>%
#   dplyr::filter(is.na(mean), is.na(median))
# rpd_records_wo_am <- dabr::select_all(conn, "entity")  %>%
#   dplyr::filter(ID_ENTITY %in% (dabr::select_all(conn, "sample") %>%
#                   dplyr::filter(ID_SAMPLE %in% aux2$ID_SAMPLE) %>%
#                   .$ID_ENTITY %>% unique())) %>%
#   dplyr::select(1:4) %>%
#   dplyr::left_join(dabr::select_all(conn, "external_link")) %>%
#   dplyr::filter(entity_name %>% stringr::str_detect("EGR2") |
#                   external_source == "RPD")
rpd_records_wo_am #%>%
  # readr::write_excel_csv("~/Downloads/rpd_records_without_age_models.csv")

am <- dabr::select_all(conn_rpd, "age_model") %>%
  dplyr::left_join(dabr::select_all(conn_rpd, "sample") %>%
                     dplyr::select(ID_SAMPLE, ID_ENTITY)) %>%
  dplyr::filter(ID_ENTITY %in% rpd_records_wo_am$external_ID_ENTITY)

# Extract ID_SAMPLE
aux <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% rpd_repatriated_am_info_4$ID_ENTITY)
rpd_repatriated_am_info_5 <- rpd_repatriated_am_info_4 %>%
  # dplyr::left_join(aux[1:3]) %>%
  dplyr::bind_cols(aux[2]) %>%
  dplyr::relocate(ID_SAMPLE, .before = 2)
# Verify that the ID_SAMPLEs have been assigned to the correct records
waldo::compare(aux[1:4], rpd_repatriated_am_info_5[1:4], tolerance = 0.001)
# Create subset of entities with IntCal20 age models
rpd_repatriated_am_info_6 <- rpd_repatriated_am_info_5 %>%
  dplyr::filter(model_name %>%
                  stringr::str_to_lower() %>%
                  stringr::str_detect("bacon_intcal20"))
# Check if any entries in the mean and median columns are missing
rpd_repatriated_am_info_6 %>%
  dplyr::filter(is.na(mean) | is.na(median))
meta_neo_res <- seq_len(nrow(rpd_repatriated_am_info_6))[-1] %>%
  purrr::map(function(i) {
    rpd_repatriated_am_info_6[i, ] %>%
      dplyr::mutate(ID_MODEL = 8, .before = 2) %>% # Bacon IntCal20
      dplyr::select(ID_MODEL, ID_SAMPLE, mean, median,
                    UNCERT_5, UNCERT_25, UNCERT_75, UNCERT_95) %>%
      rpd:::add_records(conn = conn, table = "age_model", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate ----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "age_model") %>%
  dplyr::filter(ID_SAMPLE %in% rpd_repatriated_am_info_6$ID_SAMPLE)
waldo::compare(rpd_repatriated_am_info_6 %>%
                 dplyr::select(2, 9:14) %>%
                 dplyr::mutate(ID_MODEL = 8, .before = 2) %>%
                 .[order(colnames(.))],
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))],
               tolerance = 2)


### non-RPD age models (89) ----
non_rpd_repatriation_am <- rpd_repatriation %>%
  dplyr::filter(!age_model_to_be_extracted_from_rpd) %>%
  dplyr::filter(entity_name != "DURANK3") # Repatriated from the EMBSeCBIO

non_rpd_repatriated_am_info <- non_rpd_repatriation_am %>%
  dplyr::select(site_id, site_name, entity_name) %>%
  dplyr::left_join(dabr::select_all(conn, "external_link"),
                   by = c("site_id" = "external_ID_SITE",
                          "entity_name" = "external_entity_name"))
# non_rpd_repatriated_am_info <-
#   non_rpd_repatriation_am$RPD_ID_ENTITY %>%
#   extract_rpd()
#
# non_rpd_repatriated_am_info_2 <- non_rpd_repatriation_am %>%
#   dplyr::select(neotoma_ID_SITE = site_id,
#                 neotoma_site_name = site_name,
#                 neotoma_entity_name = entity_name,
#                 ID_ENTITY = RPD_ID_ENTITY) %>%
#   dplyr::inner_join(non_rpd_repatriated_am_info$metadata,
#                     by = "ID_ENTITY") %>%
#   dplyr::rename(external_ID_ENTITY = ID_ENTITY,
#                 external_ID_SITE =  ID_SITE,
#                 external_site_name = site_name,
#                 external_entity_name = entity_name)
#
# non_rpd_repatriated_am_info_3 <- EPD_METADATA %>%
#   dplyr::select(1:4, 6, 10) %>%
#   dplyr::right_join(non_rpd_repatriated_am_info_2 %>%
#                       dplyr::select(1:6, 8),
#                     by = c("entity_name" = "neotoma_entity_name"))

#### Samples ----
non_rpd_repatriated_am_info_EPD_COUNTS <- EPD_COUNTS %>%
  dplyr::filter(dataset_id %in% non_rpd_repatriated_am_info$external_ID_ENTITY) %>%
  # dplyr::filter(dataset_id %in% non_rpd_repatriated_am_info_3$dataset_id) %>%
  smpds::rm_na_taxa(1:16) %>%
  dplyr::select(-chronology_id) %>%
  dplyr::left_join(EPD_METADATA %>%
                     dplyr::select(1:3, 6, 10),
                   by = c("site_id", "dataset_id", "entity_name")) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1) %>%
  dplyr::mutate(depth = ifelse(ID_ENTITY == 365 & depth == 0, 0.5, depth)) # Update depth GDU: 0 -> 0.5
non_rpd_repatriated_am_info_EPD_COUNTS

# Note: remove additional sample linked to GDU (ID_ENTITY = 365, depth = 0)
# ID_SAMPLE = 21612
special.epd::dump_all(conn, ID_ENTITY = 365)
# dabr::select(conn, "SELECT * FROM SAMPLE WHERE ID_ENTITY = 365 AND depth = 0")
# dabr::delete(conn, "DELETE FROM SAMPLE WHERE ID_ENTITY = 365 AND depth = 0")


# non_rpd_repatriated_am_info_EPD_COUNTS %>%
#   dplyr::distinct(ID_SITE, ID_ENTITY, site_id, site_name, dataset_id, entity_name) %>%
#   dplyr::select(site_id, site_name, entity_name, ID_SITE, ID_ENTITY, external_ID_ENTITY = dataset_id) %>%
#   dplyr::arrange(ID_SITE, ID_ENTITY) %>%
#   waldo::compare(non_rpd_repatriated_am_info %>%
#                    dplyr::arrange(ID_SITE, ID_ENTITY))

# non_rpd_repatriated_am_info_4 <- non_rpd_repatriated_am_info$sample %>%
#   dplyr::left_join(non_rpd_repatriated_am_info$age_model) %>%
#   # dplyr::mutate(age = dplyr::coalesce(age, est_age_original),
#   #               age_original = est_age_original) %>%
#   # dplyr::select(-est_age_original, -comment) %>%
#   dplyr::left_join(non_rpd_repatriated_am_info_3 %>%
#                      dplyr::select(ID_ENTITY, external_ID_ENTITY),
#                    by = "external_ID_ENTITY") %>%
#   # dplyr::filter(ID_ENTITY != 18) %>%
#   dplyr::select(-ID_SAMPLE, -external_ID_ENTITY) %>%
#   dplyr::relocate(ID_ENTITY, .before = 1)


non_rpd_repatriated_am_info_2 <- non_rpd_repatriated_am_info_EPD_COUNTS %>%
  dplyr::mutate(ID_SAMPLE = 18260 + seq_along(ID_ENTITY), # get_id_sample(conn)
                .after = ID_ENTITY) %>%
  dplyr::select(-ID_SITE, -site_id, -site_name, -site_name_clean, -dataset_id, -dataset_name, -entity_name, -sample_id, -unit_name)

# Check if the "new" records are already in th DB:
special.epd::dump_all(conn, ID_ENTITY = non_rpd_repatriated_am_info_2$ID_ENTITY)
dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_SAMPLE %in% non_rpd_repatriated_am_info_2$ID_SAMPLE |
                  ID_ENTITY %in% non_rpd_repatriated_am_info_2$ID_ENTITY)
meta_neo_res <- seq_len(nrow(non_rpd_repatriated_am_info_2)) %>%
  purrr::map(function(i) {
    non_rpd_repatriated_am_info_2[i, ] %>%
      dplyr::select(1:9) %>%
      rpd:::add_records(conn = conn, table = "sample", dry_run = TRUE)
  })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% non_rpd_repatriated_am_info_2$ID_ENTITY)
waldo::compare(non_rpd_repatriated_am_info_2 %>%
                 dplyr::select(1:9) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)) %>%
                 dplyr::select(-count_type, -sample_type),
               tolerance = 2)

#### (HERE) Age models (67) ----
non_rpd_repatriated_am_new <-
  find_age_models(path, non_rpd_repatriated_am_info$entity_name)
non_rpd_repatriated_am_new2 <- non_rpd_repatriated_am_new %>%
  purrr::pmap_df(upload_age_model, conn = conn)

all(non_rpd_repatriated_am_new2$status)
non_rpd_repatriated_am_new2
non_rpd_repatriated_am_new3 <-
  non_rpd_repatriated_am_new2$am %>%
  purrr::map_df(~.x) %>%
  magrittr::set_names(colnames(.) %>% stringr::str_to_upper())

EPD_AM <- dabr::select(conn,
                       "SELECT * FROM age_model WHERE ID_SAMPLE IN (",
                       paste0(non_rpd_repatriated_am_new3$ID_SAMPLE, collapse = ", "),
                       ") AND ID_MODEL = 8")
waldo::compare(non_rpd_repatriated_am_new3 %>%
                 dplyr::arrange(ID_SAMPLE),
               EPD_AM %>%
                 magrittr::set_names(colnames(.) %>% stringr::str_to_upper()) %>%
                 dplyr::arrange(ID_SAMPLE))

special.epd::dump_all(conn, entity_name = non_rpd_repatriated_am_new$entity_name)

# NOTE: Pending age models
non_rpd_repatriated_am_info %>%
  dplyr::filter(!(entity_name %in% non_rpd_repatriated_am_new2$entity_name))

tibble::tribble(
  ~site_id,                  ~site_name, ~entity_name, ~ID_SITE, ~ID_ENTITY, ~external_ID_ENTITY,         ~external_site_name, ~external_source,
  3234,           "Lilla Gloppsjön",   "LGLOPPSJ",     804L,       910L,               4200L,           "Lilla Gloppsjön",        "NEOTOMA",
  3245,               "Lobsigensee",    "LOBHOLO",     825L,       935L,               4213L,               "Lobsigensee",        "NEOTOMA",
  3265,               "Lake Maardu",      "MAASV",     712L,       802L,              45381L,               "Lake Maardu",        "NEOTOMA",
  13390,              "Trettetj√∏rn",     "TRETTE",    1333L,      1477L,              20042L,               "Trettetjørn",        "NEOTOMA",
  14551,      "Lac du Verney-Dessus",   "DESVERNE",     647L,       727L,              22810L,      "Lac du Verney-Dessus",        "NEOTOMA",
  14613, "Lago Piccolo di Avigliana",   "LAVPICCO",     667L,       751L,              22908L, "Lago Piccolo di Avigliana",        "NEOTOMA",
  16133,   "Paleochenal de Neublans",   "NEUBLANS",    1025L,      1143L,              24927L,   "Paleochenal de Neublans",        "NEOTOMA",
  16267,            "Peyre peat-bog",    "PEYREII",    1048L,      1166L,              25213L,            "Peyre peat-bog",        "NEOTOMA",
  23495,                  "Bibersee",      "BIBER",     143L,       162L,              40569L,                  "Bibersee",        "NEOTOMA",
  23895,             "Bay of Biscay",   "MD042845",     115L,       132L,              41446L,             "Bay of Biscay",        "NEOTOMA",
  26125,               "Svartvatnet",     "OTTSVA",    1256L,      1396L,              45343L,               "Svartvatnet",        "NEOTOMA",
  26143,                "Lake Ruila",        "RUI",     731L,       826L,              45379L,                "Lake Ruila",        "NEOTOMA",
  26144,               "Lake Kahala",       "KAHA",     703L,       791L,              45383L,               "Lake Kahala",        "NEOTOMA",
  26145,          "Viitna Linaj√§rv",       "VIPI",    1375L,      1532L,              45385L,           "Viitna Linajärv",        "NEOTOMA",
  26150,    "Lake Plaani K√ºlaj√§rv",        "PLA",     728L,       823L,              45398L,      "Lake Plaani Külajärv",        "NEOTOMA",
  26156,            "Lake Jarveotsa",   "JARVEOTS",     702L,       790L,              45410L,            "Lake Jarveotsa",        "NEOTOMA",
  26217,                "Grostjørna",    "SETESGR",     450L,       503L,              45719L,                "Grostjørna",        "NEOTOMA",
  26991,       "Pr√°≈°ilsk√© jezero",     "PRA-15",    1078L,      1198L,              47520L,          "Prášilské jezero",        "NEOTOMA",
  27384,                  "Yaksha 2",      "YAK2A",    1426L,      1595L,              49139L,                  "Yaksha 2",        "NEOTOMA",
  27385,                  "Yaksha 3",      "YAK3A",    1427L,      1596L,              49141L,                  "Yaksha 3",        "NEOTOMA",
  27526,                "Zeller See",   "ZELLERSE",    1440L,      1610L,              49444L,                "Zeller See",        "NEOTOMA",
  27563,             "Lake Sidi Ali",   "SALI0110",     738L,       833L,              49519L,             "Lake Sidi Ali",        "NEOTOMA"
)

aux <- non_rpd_repatriated_am_new2 %>%
  dplyr::left_join(EPD_METADATA %>%
                     dplyr::select(site_id, entity_name)) %>%
  dplyr::arrange(site_id)


# non_rpd_repatriated_am_new2 %>%
#   dplyr::filter(entity_name %in%
#                   (epd_age_models %>%
#                      dplyr::filter(ready_to_upload) %>%
#                      .$entity_name))

# ##### Export data ----
# conn %>%
#   special.epd::dump_all(entity_name = non_rpd_repatriated_am_new2$entity_name) %>%
#   special.epd::write_csvs(prefix = "~/Downloads/special_epd_snapshot_2022-03-09_")
# entity_tb <- readr::read_csv("special_epd_snapshot_2022-03-09__metadata.csv")
# dates_tb <- readr::read_csv("special_epd_snapshot_2022-03-09__dates.csv")
# samples_tb <- readr::read_csv("special_epd_snapshot_2022-03-09__samples.csv")
# age_model_tb <- readr::read_csv("special_epd_snapshot_2022-03-09__age_model.csv")
# pollen_counts_clean_tb <- readr::read_csv("special_epd_snapshot_2022-03-09__pollen_counts_clean.csv")
# pollen_counts_intermediate_tb <- readr::read_csv("special_epd_snapshot_2022-03-09__pollen_counts_intermediate.csv")
# pollen_counts_amalgamated_tb <- readr::read_csv("special_epd_snapshot_2022-03-09__pollen_counts_amalgamated.csv")

#### Counts ----
non_rpd_repatriated_am_info_3 <- non_rpd_repatriated_am_info_2 %>%
  # dplyr::filter(ID_SAMPLE == 21612) %>%
  dplyr::select(-ID_ENTITY, -depth, -thickness, -chronology_name, -age_type, -age, -age_younger, -age_older)
taxon_name_tb <- dabr::select_all(conn, "taxon_name")
data("EPD_taxa_amalgamation")
##### Clean ----
non_rpd_repatriated_am_info_4 <-
  seq_len(nrow(non_rpd_repatriated_am_info_3)) %>%
  purrr::map_df(function(i) {
    non_rpd_repatriated_am_info_3[i, ] %>%
      tidyr::pivot_longer(-1, names_to = "epd_taxa", values_to = "count") %>%
      dplyr::filter(!is.na(count)) %>%
      dplyr::left_join(EPD_taxa_amalgamation,
                       by = "epd_taxa") %>%
      # dplyr::select(ID_SAMPLE, taxon_name = clean_name, count, epd_taxa) %>%
      dplyr::left_join(taxon_name_tb,
                       by = c("clean_name" = "taxon_name")) %>%
      dplyr::group_by(ID_SAMPLE, ID_TAXON) %>%
      dplyr::mutate(count = sum(count, na.rm = TRUE),
                    n = length(count)) %>%
      # dplyr::filter(n > 1)
      dplyr::distinct(ID_SAMPLE, ID_TAXON, .keep_all = TRUE) %>%
      dplyr::select(ID_SAMPLE, ID_TAXON, count) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(amalgamation_level = 0, .before = count) # Clean names only

  })

non_rpd_repatriated_am_info_5 <- non_rpd_repatriated_am_info_4
meta_neo_res <- seq_len(nrow(non_rpd_repatriated_am_info_5)) %>%
  purrr::map(function(i) {
    if (i %% 10000 == 0)
      print(i)
    non_rpd_repatriated_am_info_5[i, ] %>%

      rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
  })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% non_rpd_repatriated_am_info_3$ID_SAMPLE,
                amalgamation_level == 0)
waldo::compare(non_rpd_repatriated_am_info_5 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 2)

##### Intermediate ----
EPD_taxa_amalgamation_stage2 <- EPD_taxa_amalgamation %>%
  dplyr::select(-epd_taxa) %>%
  dplyr::distinct(clean_name, intermediate, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(intermediate))

EPD_taxa_amalgamation_stage2 %>%
  dplyr::filter(intermediate != intermediate2)

non_rpd_repatriated_am_info_6 <-
  unique(non_rpd_repatriated_am_info_5$ID_SAMPLE) %>%
  purrr::map_df(function(ID_SAMPLE) {
    non_rpd_repatriated_am_info_5 %>%
      dplyr::filter(ID_SAMPLE == !!ID_SAMPLE) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "ID_TAXON") %>%
      dplyr::left_join(EPD_taxa_amalgamation_stage2,
                       by = c("taxon_name" = "clean_name")) %>%
      dplyr::mutate(amalgamation_level = 1) %>%
      dplyr::select(-ID_TAXON, -dplyr::starts_with("action")) %>%
      dplyr::rename(clean_taxon_name = taxon_name,
                    taxon_name = intermediate) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "taxon_name") %>%
      dplyr::group_by(ID_SAMPLE, ID_TAXON) %>%
      dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
      dplyr::distinct(ID_SAMPLE, ID_TAXON, .keep_all = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::select(ID_SAMPLE, ID_TAXON, amalgamation_level, count)
  })

meta_neo_res <- seq_len(nrow(non_rpd_repatriated_am_info_6)) %>%
  purrr::map(function(i) {
    if (i %% 10000 == 0)
      print(i)
    non_rpd_repatriated_am_info_6[i, ] %>%
      rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
  })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% non_rpd_repatriated_am_info_3$ID_SAMPLE,
                amalgamation_level == 1)
waldo::compare(non_rpd_repatriated_am_info_6 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 2)

##### Amalgamated ----
EPD_taxa_amalgamation_stage3 <- EPD_taxa_amalgamation_stage2 %>%
  dplyr::select(-clean_name, -dplyr::starts_with("action")) %>%
  dplyr::distinct(intermediate, amalgamated, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(amalgamated))

EPD_taxa_amalgamation_stage3 %>%
  dplyr::filter(amalgamated != amalgamated2)

non_rpd_repatriated_am_info_7 <-
  unique(non_rpd_repatriated_am_info_6$ID_SAMPLE) %>%
  purrr::map_df(function(ID_SAMPLE) {
    non_rpd_repatriated_am_info_6 %>%
      dplyr::filter(ID_SAMPLE == !!ID_SAMPLE) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "ID_TAXON") %>%
      dplyr::left_join(EPD_taxa_amalgamation_stage3,
                       by = c("taxon_name" = "intermediate")) %>%
      dplyr::mutate(amalgamation_level = 2) %>%
      dplyr::select(-ID_TAXON) %>%
      dplyr::rename(intermediate_taxon_name = taxon_name,
                    taxon_name = amalgamated) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "taxon_name") %>%
      dplyr::group_by(ID_SAMPLE, ID_TAXON) %>%
      dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
      dplyr::distinct(ID_SAMPLE, ID_TAXON, .keep_all = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::select(ID_SAMPLE, ID_TAXON, amalgamation_level, count)
  })

meta_neo_res <- seq_len(nrow(non_rpd_repatriated_am_info_7)) %>%
  purrr::map(function(i) {
    if (i %% 10000 == 0)
      print(i)
    non_rpd_repatriated_am_info_7[i, ] %>%
      rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
  })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% non_rpd_repatriated_am_info_3$ID_SAMPLE,
                amalgamation_level == 2)
waldo::compare(non_rpd_repatriated_am_info_7 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 2)

aux <- special.epd::dump_all(conn, ID_ENTITY = non_rpd_repatriated_am_info_2$ID_ENTITY)
aux2 <- aux$entity %>%
  dplyr::filter(ID_ENTITY %in% aux$sample$ID_ENTITY)

##### Inspection ----
tmp <- non_rpd_repatriated_am_info_5 %>%
  dplyr::group_by(ID_SAMPLE) %>%
  dplyr::summarise(n_clean = dplyr::n()) %>%
  dplyr::left_join(non_rpd_repatriated_am_info_6 %>%
                     dplyr::group_by(ID_SAMPLE) %>%
                     dplyr::summarise(n_intermediate = dplyr::n()),
                   by = "ID_SAMPLE") %>%
  dplyr::left_join(non_rpd_repatriated_am_info_7 %>%
                     dplyr::group_by(ID_SAMPLE) %>%
                     dplyr::summarise(n_amalgamated = dplyr::n()),
                   by = "ID_SAMPLE") %>%
  dplyr::mutate(diff_1 = n_clean - n_intermediate,
                diff_2 = n_clean - n_amalgamated) %>%
  dplyr::arrange(dplyr::desc(diff_2), dplyr::desc(diff_1))
tmp

# Manual inspection of a record
r0 <- non_rpd_repatriated_am_info_3 %>%
  dplyr::filter(ID_SAMPLE %in% 24032) %>%
  smpds::rm_na_taxa()
r1 <- non_rpd_repatriated_am_info_5 %>%
  dplyr::filter(ID_SAMPLE %in% 24032) %>%
  dplyr::left_join(taxon_name_tb,
                   by = "ID_TAXON") %>%
  dplyr::select(-ID_TAXON, -amalgamation_level) %>%
  tidyr::pivot_wider(ID_SAMPLE, names_from = taxon_name, values_from = count)

r2 <- non_rpd_repatriated_am_info_6 %>%
  dplyr::filter(ID_SAMPLE %in% 24032) %>%
  dplyr::left_join(taxon_name_tb,
                   by = "ID_TAXON") %>%
  dplyr::select(-ID_TAXON, -amalgamation_level) %>%
  tidyr::pivot_wider(ID_SAMPLE, names_from = taxon_name, values_from = count)

r3 <- non_rpd_repatriated_am_info_7 %>%
  dplyr::filter(ID_SAMPLE %in% 24032) %>%
  dplyr::left_join(taxon_name_tb,
                   by = "ID_TAXON") %>%
  dplyr::select(-ID_TAXON, -amalgamation_level) %>%
  tidyr::pivot_wider(ID_SAMPLE, names_from = taxon_name, values_from = count)

tmp2 <- dplyr::bind_rows(r0, r1, r2, r3) %>%
  dplyr::select(-ID_SAMPLE)
tmp3 <- tmp2[order(colnames(tmp2))]


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
                entity_name = entity_name_7,
                EMBSeCBIO_entity_name = entity_name_12)

### EMBSeCBIO dates (12) ----
embsecbio_repatriation_dates <- embsecbio_repatriation %>%
  dplyr::filter(dates_to_be_extracted_from_embsecbio)

bkg <- conn %>%
  special.epd::dump_all(entity_name = embsecbio_repatriation_dates$entity_name)
bkg$entity
bkg$date_info %>%
  split(.$ID_ENTITY) %>%
  names()
bkg$sample %>%
  split(.$ID_ENTITY) %>%
  names()
bkg$entity %>%
  dplyr::filter(!(ID_ENTITY %in% c(2L, 31L, 64L, 266L, 476L, 583L, 779L, 780L, 810L, 821L)))
special.epd::dump_all(conn, entity_name = c("KARAMIK", "BH2"))

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


### non-EMBSeCBIO / EPD dates (106) ----
epd_embsecbio_repatriation_dates <- embsecbio_repatriation %>%
  dplyr::filter(!dates_to_be_extracted_from_embsecbio)
epd_embsecbio_repatriated_dates_info <- EPD_DATES %>%
  dplyr::filter(entity_name %in% epd_embsecbio_repatriation_dates$entity_name) %>%
  dplyr::filter(!is.na(site_id), !is.na(depth))
  # epd_embsecbio_repatriation_dates$EMBSeCBIO_ID_ENTITY %>%
  # extract_embsecbio()

epd_embsecbio_external_links <- dabr::select_all(conn, "external_link") %>%
  dplyr::filter(external_entity_name %in%
                  epd_embsecbio_repatriated_dates_info$entity_name)

epd_embsecbio_repatriated_dates_info_2 <- epd_embsecbio_repatriation_dates %>%
  dplyr::select(external_ID_SITE = site_id,
                external_site_name = site_name,
                external_entity_name = entity_name) %>%
  dplyr::left_join(epd_embsecbio_external_links,
                   by = c("external_ID_SITE",
                          "external_site_name",
                          "external_entity_name")) %>%
  dplyr::right_join(epd_embsecbio_repatriated_dates_info,
                    by = c(
                      "external_ID_SITE" = "site_id",
                      "external_site_name" = "site_name",
                      "external_entity_name" = "entity_name"
                    ))
  # dplyr::select(neotoma_ID_SITE = site_id,
  #               neotoma_site_name = site_name,
  #               neotoma_entity_name = entity_name) %>%
                # ID_ENTITY = EMBSeCBIO_ID_ENTITY) %>%
  # dplyr::inner_join(epd_embsecbio_repatriated_dates_info$metadata,
  #                   by = "ID_ENTITY") %>%
  # dplyr::rename(external_ID_ENTITY = ID_ENTITY,
  #               external_ID_SITE =  ID_SITE,
  #               external_site_name = site_name,
  #               external_entity_name = entity_name)
# epd_embsecbio_repatriated_dates_info_3 <- EPD_METADATA %>%
#   dplyr::select(1:4, 6, 10) %>%
#   dplyr::right_join(epd_embsecbio_repatriated_dates_info_2 %>%
#                       dplyr::select(1:6, 8),
#                     by = c("entity_name" = "neotoma_entity_name"))

epd_embsecbio_repatriated_dates_info_3 <- epd_embsecbio_repatriated_dates_info_2 #%>%
  # dplyr::select(-dplyr::starts_with("external_"), -ages_already)

#### Dates ----
epd_embsecbio_repatriated_dates_info_4 <-
  epd_embsecbio_repatriated_dates_info_3 %>%
  dplyr::select(-c(1:3, 6:7)) %>%
  dplyr::arrange(ID_ENTITY, depth) %>%
  dplyr::select(-ID_SITE, -ages_already, -site_name_clean) %>%
  dplyr::rename(age_calib = age_cal)
  # epd_embsecbio_repatriated_dates_info$date_info %>%
  # dplyr::select(-ID_DATE_INFO) %>%
  # dplyr::rename(external_ID_ENTITY = ID_ENTITY,
  #               reason_age_not_used = date_comments) %>%
  # dplyr::mutate(age_used = ifelse(is.na(reason_age_not_used), "yes", "no")) %>%
  # dplyr::left_join(epd_embsecbio_repatriated_dates_info_3 %>%
  #                    dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  # dplyr::select(-external_ID_ENTITY) %>%
  # dplyr::relocate(ID_ENTITY, .before = 1)

epd_embsecbio_repatriated_dates_info_4_EPD <- EPD_DATES %>%
  dplyr::filter(entity_name %in%
                  epd_embsecbio_repatriated_dates_info_3$external_entity_name) %>% #,
                # is.na(ages_already) | ages_already == "EMBSECBIO") %>%
  dplyr::filter(!is.na(site_id), !is.na(depth)) %>%
  dplyr::left_join(EPD_METADATA %>%
                     dplyr::select(1:6),
                   by = c("site_id", "site_name", "site_name_clean", "entity_name")) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1) %>%
  dplyr::arrange(ID_ENTITY, depth) %>%
  dplyr::select(-ID_SITE, -ages_already, -site_id, -site_name, -site_name_clean, -entity_name) %>%
  dplyr::rename(age_calib = age_cal)
#
# waldo::compare(epd_embsecbio_repatriated_dates_info_4, epd_embsecbio_repatriated_dates_info_4_EPD)
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

waldo::compare(epd_embsecbio_repatriated_dates_info_4_EPD %>%
                 dplyr::arrange(ID_ENTITY, depth),
               epd_embsecbio_repatriated_dates_info_4 %>%
                 dplyr::arrange(ID_ENTITY, depth))
meta_neo_res <- seq_len(nrow(epd_embsecbio_repatriated_dates_info_4_EPD)) %>%
  purrr::map(function(i) {
    epd_embsecbio_repatriated_dates_info_4_EPD[i, ] %>%
      rpd:::add_records(conn = conn, table = "date_info", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in% epd_embsecbio_repatriated_dates_info_4_EPD$ID_ENTITY)
waldo::compare(epd_embsecbio_repatriated_dates_info_4_EPD %>%
                 .[order(colnames(.))] %>%
                 dplyr::arrange(ID_ENTITY, depth),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-ID_DATE_INFO) %>%
                 dplyr::arrange(ID_ENTITY, depth), max_diffs = Inf)


special.epd::dump_all(conn,
                      ID_ENTITY = epd_embsecbio_repatriated_dates_info_4_EPD$ID_ENTITY)
# epd_embsecbio_repatriated_dates_info_4_EPD %>%
#   dplyr::filter(is.na(ID_ENTITY))
# epd_embsecbio_repatriated_dates_info_4_EPD %>%
#   dplyr::filter((lab_num %in% EPD_DATES_NEO_DB$lab_num))
#   dplyr::filter(!(ID_ENTITY %in% EPD_DATES_NEO_DB$ID_ENTITY &
#                     lab_num %in% EPD_DATES_NEO_DB$lab_num))
#   # dplyr::filter(!(ID_ENTITY %in% EPD_DATES_NEO_DB$ID_ENTITY &
#   #                   depth %in% EPD_DATES_NEO_DB$depth))


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
      rpd:::add_records(conn = conn, table = "sample", dry_run = TRUE)
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

### non-EMBSeCBIO / EPD age models (12) ----
non_embsecbio_repatriation_am <- embsecbio_repatriation %>%
  dplyr::filter(!age_model_to_be_extracted_from_embsecbio)

non_embsecbio_repatriated_am_info <- non_embsecbio_repatriation_am %>%
  dplyr::select(site_id, site_name, entity_name) %>%
  dplyr::left_join(dabr::select_all(conn, "external_link"),
                   by = c("site_id" = "external_ID_SITE",
                          "entity_name" = "external_entity_name"))

#### Samples ----
non_embsecbio_repatriated_am_info_EPD_COUNTS <- EPD_COUNTS %>%
  dplyr::filter(dataset_id %in% non_embsecbio_repatriated_am_info$external_ID_ENTITY) %>%
  # dplyr::filter(dataset_id %in% non_embsecbio_repatriated_am_info_3$dataset_id) %>%
  smpds::rm_na_taxa(1:16) %>%
  dplyr::select(-chronology_id) %>%
  dplyr::left_join(EPD_METADATA %>%
                     dplyr::select(1:3, 6, 10),
                   by = c("site_id", "dataset_id", "entity_name")) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1) #%>%
  # dplyr::mutate(depth = ifelse(ID_ENTITY == 365 & depth == 0, 0.5, depth)) # Update depth GDU: 0 -> 0.5
non_embsecbio_repatriated_am_info_EPD_COUNTS

# # Note: remove additional sample linked to GDU (ID_ENTITY = 365, depth = 0)
# # ID_SAMPLE = 21612
# special.epd::dump_all(conn, ID_ENTITY = 365)


non_embsecbio_repatriated_am_info_2 <- non_embsecbio_repatriated_am_info_EPD_COUNTS %>%
  dplyr::mutate(ID_SAMPLE = 28481 + seq_along(ID_ENTITY), # get_id_sample(conn)
                .after = ID_ENTITY) %>%
  dplyr::select(-ID_SITE, -site_id, -site_name, -site_name_clean, -dataset_id, -dataset_name, -entity_name, -sample_id, -unit_name)

# Check if the "new" records are already in th DB:
special.epd::dump_all(conn, ID_ENTITY = non_embsecbio_repatriated_am_info_2$ID_ENTITY)
dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_SAMPLE %in% non_embsecbio_repatriated_am_info_2$ID_SAMPLE |
                  ID_ENTITY %in% non_embsecbio_repatriated_am_info_2$ID_ENTITY)
meta_neo_res <- seq_len(nrow(non_embsecbio_repatriated_am_info_2)) %>%
  purrr::map(function(i) {
    non_embsecbio_repatriated_am_info_2[i, ] %>%
      dplyr::select(1:9) %>%
      rpd:::add_records(conn = conn, table = "sample", dry_run = TRUE)
  })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% non_embsecbio_repatriated_am_info_2$ID_ENTITY)
waldo::compare(non_embsecbio_repatriated_am_info_2 %>%
                 dplyr::select(1:9) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)) %>%
                 dplyr::select(-count_type, -sample_type),
               tolerance = 2)

#### (HERE) Age models (11) ----
non_embsecbio_repatriated_am_new <-
  find_age_models(path, non_embsecbio_repatriated_am_info$entity_name)

# Check if the AM are ready to be uploaded
non_embsecbio_repatriated_am_new %>%
  dplyr::filter(entity_name %in% epd_age_models_ready_to_upload$entity_name)

# Check if the age models already exist in the DB
special.epd::dump_all(conn, entity_name = non_embsecbio_repatriated_am_new$entity_name)

non_embsecbio_repatriated_am_new2 <- non_embsecbio_repatriated_am_new %>%
  purrr::pmap_df(upload_age_model, conn = conn)
# Check if all the age models were uploaded
all(non_embsecbio_repatriated_am_new2$status)
non_embsecbio_repatriated_am_new2
non_embsecbio_repatriated_am_new3 <-
  non_embsecbio_repatriated_am_new2$am %>%
  purrr::map_df(~.x) %>%
  magrittr::set_names(colnames(.) %>% stringr::str_to_upper())

EPD_AM <- dabr::select(conn,
                       "SELECT * FROM age_model WHERE ID_SAMPLE IN (",
                       paste0(non_embsecbio_repatriated_am_new3$ID_SAMPLE, collapse = ", "),
                       ") AND ID_MODEL = 8")
waldo::compare(non_embsecbio_repatriated_am_new3 %>%
                 dplyr::arrange(ID_SAMPLE),
               EPD_AM %>%
                 magrittr::set_names(colnames(.) %>% stringr::str_to_upper()) %>%
                 dplyr::arrange(ID_SAMPLE))

special.epd::dump_all(conn, entity_name = non_embsecbio_repatriated_am_new$entity_name)

# NOTE: Pending age models
non_embsecbio_repatriated_am_info %>%
  dplyr::filter(!(entity_name %in% non_embsecbio_repatriated_am_new2$entity_name))

# This entity only has one date, so no age model was created
tibble::tribble(
  ~site_id,    ~site_name, ~entity_name, ~ID_SITE, ~ID_ENTITY, ~external_ID_ENTITY, ~external_site_name, ~external_source,
  3033, "Bozova Lake",     "BOZOVA",     185L,       206L,               3950L,       "Bozova Lake",        "NEOTOMA"
)


aux <- non_embsecbio_repatriated_am_new2 %>%
  dplyr::left_join(EPD_METADATA %>%
                     dplyr::select(site_id, entity_name)) %>%
  dplyr::arrange(site_id)

#### Counts ----
non_embsecbio_repatriated_am_info_3 <- non_embsecbio_repatriated_am_info_2 %>%
  # dplyr::filter(ID_SAMPLE == 21612) %>%
  dplyr::select(-ID_ENTITY, -depth, -thickness, -chronology_name, -age_type, -age, -age_younger, -age_older)
taxon_name_tb <- dabr::select_all(conn, "taxon_name")
data("EPD_taxa_amalgamation")
##### Clean ----
non_embsecbio_repatriated_am_info_4 <-
  seq_len(nrow(non_embsecbio_repatriated_am_info_3)) %>%
  purrr::map_df(function(i) {
    non_embsecbio_repatriated_am_info_3[i, ] %>%
      tidyr::pivot_longer(-1, names_to = "epd_taxa", values_to = "count") %>%
      dplyr::filter(!is.na(count)) %>%
      dplyr::left_join(EPD_taxa_amalgamation,
                       by = "epd_taxa") %>%
      # dplyr::select(ID_SAMPLE, taxon_name = clean_name, count, epd_taxa) %>%
      dplyr::left_join(taxon_name_tb,
                       by = c("clean_name" = "taxon_name")) %>%
      dplyr::group_by(ID_SAMPLE, ID_TAXON) %>%
      dplyr::mutate(count = sum(count, na.rm = TRUE),
                    n = length(count)) %>%
      # dplyr::filter(n > 1)
      dplyr::distinct(ID_SAMPLE, ID_TAXON, .keep_all = TRUE) %>%
      dplyr::select(ID_SAMPLE, ID_TAXON, count) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(amalgamation_level = 0, .before = count) # Clean names only
  }) %>%
  dplyr::arrange(ID_SAMPLE, ID_TAXON)

meta_neo_res <- seq_len(nrow(non_embsecbio_repatriated_am_info_4)) %>%
  purrr::map(function(i) {
    if (i %% 10000 == 0)
      print(i)
    non_embsecbio_repatriated_am_info_4[i, ] %>%

      rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
  })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% non_embsecbio_repatriated_am_info_4$ID_SAMPLE,
                amalgamation_level == 0)
waldo::compare(non_embsecbio_repatriated_am_info_4 %>%
                 # dplyr::arrange(ID_SAMPLE, ID_TAXON) %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)

##### Intermediate ----
EPD_taxa_amalgamation_stage2 <- EPD_taxa_amalgamation %>%
  dplyr::select(-epd_taxa) %>%
  dplyr::distinct(clean_name, intermediate, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(intermediate))

EPD_taxa_amalgamation_stage2 %>%
  dplyr::filter(intermediate != intermediate2)

non_embsecbio_repatriated_am_info_5 <-
  unique(non_embsecbio_repatriated_am_info_4$ID_SAMPLE) %>%
  purrr::map_df(function(ID_SAMPLE) {
    non_embsecbio_repatriated_am_info_4 %>%
      dplyr::filter(ID_SAMPLE == !!ID_SAMPLE) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "ID_TAXON") %>%
      dplyr::left_join(EPD_taxa_amalgamation_stage2,
                       by = c("taxon_name" = "clean_name")) %>%
      dplyr::mutate(amalgamation_level = 1) %>%
      dplyr::select(-ID_TAXON, -dplyr::starts_with("action")) %>%
      dplyr::rename(clean_taxon_name = taxon_name,
                    taxon_name = intermediate) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "taxon_name") %>%
      dplyr::group_by(ID_SAMPLE, ID_TAXON) %>%
      dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
      dplyr::distinct(ID_SAMPLE, ID_TAXON, .keep_all = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::select(ID_SAMPLE, ID_TAXON, amalgamation_level, count)
  }) %>%
  dplyr::arrange(ID_SAMPLE, ID_TAXON)

meta_neo_res <- seq_len(nrow(non_embsecbio_repatriated_am_info_5)) %>%
  purrr::map(function(i) {
    if (i %% 10000 == 0)
      print(i)
    non_embsecbio_repatriated_am_info_5[i, ] %>%
      rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
  })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% non_embsecbio_repatriated_am_info_5$ID_SAMPLE,
                amalgamation_level == 1)
waldo::compare(non_embsecbio_repatriated_am_info_5 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)

##### Amalgamated ----
EPD_taxa_amalgamation_stage3 <- EPD_taxa_amalgamation_stage2 %>%
  dplyr::select(-clean_name, -dplyr::starts_with("action")) %>%
  dplyr::distinct(intermediate, amalgamated, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(amalgamated))

EPD_taxa_amalgamation_stage3 %>%
  dplyr::filter(amalgamated != amalgamated2)

non_embsecbio_repatriated_am_info_6 <-
  unique(non_embsecbio_repatriated_am_info_5$ID_SAMPLE) %>%
  purrr::map_df(function(ID_SAMPLE) {
    non_embsecbio_repatriated_am_info_5 %>%
      dplyr::filter(ID_SAMPLE == !!ID_SAMPLE) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "ID_TAXON") %>%
      dplyr::left_join(EPD_taxa_amalgamation_stage3,
                       by = c("taxon_name" = "intermediate")) %>%
      dplyr::mutate(amalgamation_level = 2) %>%
      dplyr::select(-ID_TAXON) %>%
      dplyr::rename(intermediate_taxon_name = taxon_name,
                    taxon_name = amalgamated) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "taxon_name") %>%
      dplyr::group_by(ID_SAMPLE, ID_TAXON) %>%
      dplyr::mutate(count = sum(count, na.rm = TRUE)) %>%
      dplyr::distinct(ID_SAMPLE, ID_TAXON, .keep_all = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::select(ID_SAMPLE, ID_TAXON, amalgamation_level, count)
  }) %>%
  dplyr::arrange(ID_SAMPLE, ID_TAXON)

meta_neo_res <- seq_len(nrow(non_embsecbio_repatriated_am_info_6)) %>%
  purrr::map(function(i) {
    if (i %% 10000 == 0)
      print(i)
    non_embsecbio_repatriated_am_info_6[i, ] %>%
      rpd:::add_records(conn = conn, table = "pollen_count", dry_run = F, quiet = TRUE)
  })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% non_embsecbio_repatriated_am_info_6$ID_SAMPLE,
                amalgamation_level == 2)
waldo::compare(non_embsecbio_repatriated_am_info_6 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)

aux <- special.epd::dump_all(conn,
                             ID_ENTITY = non_embsecbio_repatriated_am_info_2$ID_ENTITY)
aux$entity %>%
  dplyr::filter(ID_ENTITY %in% aux$sample$ID_ENTITY)
non_embsecbio_repatriated_am_info %>%
  dplyr::filter(ID_ENTITY %in% non_embsecbio_repatriated_am_info_2$ID_ENTITY)
aux2

## EMBSeCBIO (extra) ----
embsecbio_extra_repatriation <- epd_repatriation_tmp_file %>%
  readxl::read_excel(sheet = 3) %>%
  janitor::clean_names() %>%
  dplyr::mutate(dates_to_be_extracted_from_embsecbio =
                  to_bool(dates_to_be_extracted_from_embsecbio),
                age_model_to_be_extracted_from_embsecbio =
                  to_bool(age_model_to_be_extracted_from_embsecbio)) %>%
  dplyr::rename(EMBSeCBIO_ID_ENTITY = embsecbio_id_entity,
                entity_name = entity_name_7,
                EMBSeCBIO_entity_name = entity_name_12)

### EMBSeCBIO dates (8) ----
embsecbio_extra_repatriation_dates <- embsecbio_extra_repatriation %>%
  dplyr::filter(dates_to_be_extracted_from_embsecbio)

embsecbio_extra_repatriated_dates_info <-
  embsecbio_extra_repatriation_dates$EMBSeCBIO_ID_ENTITY %>%
  extract_embsecbio()

#### Entities ----
embsecbio_extra_repatriated_dates_info_entities <-
  embsecbio_extra_repatriated_dates_info$metadata %>%
  dplyr::arrange(site_name, entity_name) %>%
  dplyr::filter(entity_name %>% stringr::str_detect("GS05", TRUE)) %>%
  dplyr::mutate(ID_SITE = c(1449, 1450, 1451, 1451, 1451, 1451, 1452, 1453),
                ID_ENTITY = 1620:1627) %>%
  # dplyr::mutate(ID_SITE = c(1448, 1449, 1450, 1451, 1451, 1451, 1451, 1452, 1453),
  #               ID_ENTITY = 1619:1627) %>%
  dplyr::select(-entity_type)
  # dplyr::mutate(ID_SITE = c(1, 2, 3, 4, 4, 4, 4, 5, 6) + get_id_site(conn),
  #               ID_ENTITY = seq_along(ID_ENTITY) + get_id_entity(conn))

# (2022-03-10): ID_ENTITY = 1619 This is a duplicated entry of GS05
# Change depth of GS05 (ID_ENTITY = 509) from -518 to -26
special.epd::dump_all(conn, ID_ENTITY = c(509, 1619))
# special.epd::dump_all(conn, entity_name = "GS05")
dabr::select(conn,
             "SELECT * FROM entity WHERE ID_ENTITY = 1619 AND entity_name LIKE '%Caspian%GS05%'")
dabr::delete(conn,
             "DELETE FROM entity WHERE ID_ENTITY = 1619 AND entity_name LIKE '%Caspian%GS05%'")
# Results: 1 record was deleted.
dabr::select(conn,
             "SELECT * FROM external_link WHERE external_ID_ENTITY = 1830 AND external_entity_name LIKE '%Caspian%GS05%'")
dabr::delete(conn,
             "DELETE FROM external_link WHERE external_ID_ENTITY = 1830 AND external_entity_name LIKE '%Caspian%GS05%'")
# Results: 1 record was deleted.

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
                 dplyr::select(-doi),
               tolerance = 1e-9)

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
                external_entity_name = entity_name) %>%
  dplyr::filter(external_entity_name %>% stringr::str_detect("GS05", TRUE))
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
      rpd:::add_records(conn = conn, table = "external_link", dry_run = TRUE)
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
               EPD_NEO_DB,
               tolerance = 1e-9)

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
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::filter(!is.na(ID_ENTITY))
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
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::filter(!is.na(ID_ENTITY))

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

dabr::select_all(conn, "external_link") %>%
  dplyr::filter(external_entity_name %in% iberia_extra_repatriated_dates_info_3$external_entity_name) %>%
  dplyr::filter(external_source %in% c("EMBSECBIO")) %>%
  purrr::pwalk(function(ID_SITE,
                        ID_ENTITY,
                        external_ID_SITE,
                        external_ID_ENTITY,
                        external_site_name,
                        external_entity_name,
                        external_source) {
    dabr::delete(conn,
                 "DELETE FROM external_link WHERE",
                 "ID_SITE = ", ID_SITE,
                 "AND ID_ENTITY = ", ID_ENTITY,
                 "AND external_ID_SITE = ", external_ID_SITE,
                 "AND external_ID_ENTITY = ", external_ID_ENTITY,
                 "AND external_site_name = ", dabr::quote(external_site_name),
                 "AND external_entity_name = ", dabr::quote(external_entity_name),
                 "AND external_source = ", dabr::quote(external_source))
  })

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
