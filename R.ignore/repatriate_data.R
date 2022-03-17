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

extract_iberia <- function(entity_name = NULL, v2 = FALSE) {
  if (missing(entity_name))
    return(NULL)
  if (v2) {
    message("Loading version 3...")
    data("IBERIA_pollen_v2")
    data("IBERIA_pollen_dates_v2")
    IBERIA_pollen <- IBERIA_pollen_v2
    IBERIA_pollen_dates <- IBERIA_pollen_dates_v2
    rm("IBERIA_pollen_v2", "IBERIA_pollen_dates_v2")
  } else {
    message("Loading version 3...")
    data("IBERIA_pollen_v3")
    data("IBERIA_pollen_dates_v3")
    IBERIA_pollen <- IBERIA_pollen_v3
    IBERIA_pollen_dates <- IBERIA_pollen_dates_v3
    rm("IBERIA_pollen_v3", "IBERIA_pollen_dates_v3")
  }
  # data("IBERIA_pollen")
  # data("IBERIA_pollen_dates")
  entity_age_model_tb <- IBERIA_pollen %>%
    dplyr::filter(entity_name %in% !!entity_name) %>%
    dplyr::select(1:17) %>%
    dplyr::rename(depth = `avg_depth..cm.`) %>%
    dplyr::select(-`IPE.age..cal.`)
  if (nrow(entity_age_model_tb) == 0)
    return(NULL)

  date_info_tb <- IBERIA_pollen_dates %>%
    dplyr::filter(entity_name %in% !!entity_name) %>%
    dplyr::select(-dplyr::starts_with("type"))
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

idx_pairs <- function(max, step) {
  tibble::tibble(x = seq(1, max, step), y = c(x[-1] - 1, max))
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
  # if (nrow(am) %% nrow(sample_tb) == 0) { # In case there are duplicated samples
  #   am <- am %>%
  #     dplyr::distinct(depths, mean, median, uncert_5, uncert_95, uncert_25, uncert_75, .keep_all = TRUE)
  # }
  am <- am %>%
    dplyr::distinct(depths, mean, median, uncert_5, uncert_95, uncert_25, uncert_75, .keep_all = TRUE) %>%
    dplyr::left_join(sample_tb %>%
                       dplyr::select(ID_SAMPLE, depth),
                     by = c("depths" = "depth"))
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
    rpd:::msg(paste0("Mistmatch in the number of samples: ", entity_name,". "))
    return(tibble::tibble(entity_name,
                          am = list(am),
                          status = FALSE,
                          reason = "Mistmatch in the number of samples.",
                          ...))
  }

  return(tibble::tibble(entity_name,
                        am = list(am),
                        status = TRUE,
                        reason = NA_character_,
                        ...))
  # return(tibble::tibble(am = list(am), status = TRUE, reason = NA_character_))
}

upload_age_model2 <- function(conn, entity_name, am, ...) {
  rpd:::msg(entity_name, limit = 78, nl = FALSE)
  # Select records from the sample-charcoal table
  sample_tb <- dabr::select(conn,
                            "SELECT * FROM sample INNER JOIN entity ON",
                            "sample.ID_ENTITY = entity.ID_ENTITY WHERE",
                            "entity_name = ",
                            dabr::quote(entity_name),
                            quiet = TRUE)
  # browser()
  am <- am %>%
    dplyr::distinct(depths, mean, median, uncert_5, uncert_95, uncert_25, uncert_75, .keep_all = TRUE) %>%
    dplyr::left_join(sample_tb %>%
                       dplyr::select(ID_SAMPLE, depth),
                     by = c("depths" = "depth"))
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
    rpd:::msg(paste0("Mistmatch in the number of samples: ", entity_name,". "))
    return(tibble::tibble(entity_name,
                          am = list(am),
                          status = FALSE,
                          reason = "Mistmatch in the number of samples.",
                          ...))
  }

  return(tibble::tibble(entity_name,
                        am = list(am),
                        status = TRUE,
                        reason = NA_character_,
                        ...))
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
                age_model_run_successfully_ready_to_check_bool =
                  to_bool(age_model_run_successfully_ready_to_check),
                age_model_checked_bool =
                  to_bool(age_model_checked),
                ready_to_upload_bool =
                  to_bool(ready_to_upload))

epd_age_models_ready_to_upload <- epd_age_models %>%
  dplyr::filter(ready_to_upload_bool)
epd_age_models_ignored_entities <- epd_age_models %>%
  dplyr::filter(ready_to_upload %>%
                  stringr::str_to_lower() %>%
                  stringr::str_detect("ignore"))

epd_age_models_extracted_from_embsecbio <- epd_age_models %>%
  dplyr::filter(age_model_run_successfully_ready_to_check %>%
                  stringr::str_detect("can be extracted from EMBSECBIO|EMBESCBIO"))

epd_age_models_not_uploaded <- epd_age_models %>%
  dplyr::filter(ready_to_upload %>%
                  stringr::str_to_lower() %>%
                  stringr::str_detect("ignore", negate = TRUE)) %>%
  dplyr::filter(is.na(ready_to_upload_bool)) %>%
  dplyr::filter(age_model_run_successfully_ready_to_check %>%
                  stringr::str_detect("can be extracted from EMBSECBIO|EMBESCBIO",
                                      negate = TRUE)) %>%
  dplyr::filter(!(entity_name %in% c("DURANK3", "KILOMYR"))) %>%
  dplyr::filter(age_model_run_successfully_ready_to_check %>%
                  stringr::str_detect("one date", negate = TRUE))

# epd_age_models_not_uploaded %>%
#   readr::write_excel_csv("~/Downloads/epd_age_models_not_uploaded.csv")

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

special.epd::snapshot(conn, ID_ENTITY = 838)
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
special.epd::snapshot(conn, ID_ENTITY = 780)
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
special.epd::snapshot(conn, ID_ENTITY = non_rpd_repatriated_dates_info_4$ID_ENTITY)
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
  special.epd::snapshot(ID_ENTITY = rpd_repatriated_am_info_4$ID_ENTITY)
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
special.epd::snapshot(conn, ID_ENTITY = 365)
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
special.epd::snapshot(conn, ID_ENTITY = non_rpd_repatriated_am_info_2$ID_ENTITY)
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

special.epd::snapshot(conn, entity_name = non_rpd_repatriated_am_new$entity_name)

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
#   special.epd::snapshot(entity_name = non_rpd_repatriated_am_new2$entity_name) %>%
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

aux <- special.epd::snapshot(conn, ID_ENTITY = non_rpd_repatriated_am_info_2$ID_ENTITY)
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
# Import the taxa amalgamation table
embsecbio_taxa_amalgamation <-
  readxl::read_excel("data-raw/embsecbio_taxon_list_clean_names_SPH.xlsx",
                     skip = 1) %>%
  janitor::clean_names() %>%
  dplyr::rename(taxon_name = taxon_clean,
                clean_name = epd_clean_names) %>%
  dplyr::filter(clean_name %>%
                  stringr::str_to_lower() %>%
                  stringr::str_squish() %>%
                  stringr::str_detect("exclude", negate = TRUE)) %>%
  dplyr::mutate(clean_name = clean_name %>%
                  stringr::str_squish())

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
  special.epd::snapshot(entity_name = embsecbio_repatriation_dates$entity_name)
bkg$entity
bkg$date_info %>%
  split(.$ID_ENTITY) %>%
  names()
bkg$sample %>%
  split(.$ID_ENTITY) %>%
  names()
bkg$entity %>%
  dplyr::filter(!(ID_ENTITY %in% c(2L, 31L, 64L, 266L, 476L, 583L, 779L, 780L, 810L, 821L)))
special.epd::snapshot(conn, entity_name = c("KARAMIK", "BH2"))

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
               EPD_METADATA_NEO_DB,
               tolerance = 1E-9)

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
                 dplyr::select(-age_calib, -ID_DATE_INFO, -notes),
               tolerance = 1E-9)

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

epd_embsecbio_repatriated_dates_info_3 <- epd_embsecbio_repatriated_dates_info_2

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
                 dplyr::arrange(ID_ENTITY, depth),
               tolerance = 1E-9)
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
                 dplyr::arrange(ID_ENTITY, depth),
               max_diffs = Inf,
               tolerance = 1E-9)


special.epd::snapshot(conn,
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

embsecbio_taxon_names <- embsecbio_repatriated_am_info$pollen$taxon_clean %>%
  unique()
# tibble::tibble(
#   taxon_clean = embsecbio_repatriated_am_info$pollen$taxon_clean %>% unique() %>% sort()
# ) %>% readr::write_excel_csv("~/Downloads/embsecbio_taxon_list_clean_names.csv")

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

# Missing samples (the entity 'GALINI' does not have an age model and only
# original_est_age at 2575 cm).
embsecbio_missing_samples <- embsecbio_repatriated_am_info$sample %>%
  dplyr::filter(!(ID_SAMPLE %in% embsecbio_repatriated_am_info$age_model$ID_SAMPLE)) %>%
  dplyr::left_join(embsecbio_repatriated_am_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY),
                   by = "external_ID_ENTITY") %>%
  dplyr::select(-ID_SAMPLE, -external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::mutate(depth2 = round(depth, 3)) %>%
  dplyr::left_join(embsecbio_repatriated_am_info_EPD_COUNTS %>%
                     dplyr::select(ID_ENTITY, depth, thickness, chronology_name,
                                   age_EPD = age, age_younger, age_older, age_type) %>%
                     dplyr::mutate(depth = round(depth, 3)),
                   by = c("ID_ENTITY",  "depth2" = "depth")) %>%
  dplyr::select(-depth2) %>%
  dplyr::relocate(thickness,
                  age_EPD,
                  chronology_name,
                  age_younger,
                  age_older,
                  age_type,
                  .after = depth) %>%
  dplyr::mutate(ID_SAMPLE = 101316:101360)

embsecbio_missing_samples %>%
  rpd:::add_records(conn = conn, table = "sample", dry_run = TRUE)
# Results: 45 records were inserted.

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
                 dplyr::bind_rows(embsecbio_missing_samples) %>%
                 dplyr::arrange(ID_ENTITY, depth) %>%
                 dplyr::select(-ID_SAMPLE) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)) %>%
                 magrittr::set_class(class(.)[-1]),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::arrange(ID_ENTITY, depth) %>%
                 dplyr::select(-ID_SAMPLE) %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               tolerance = 1)

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
                 .[order(colnames(.))] %>%
                 magrittr::set_class(class(.)[-1]),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))],
               tolerance = 1E-9)

# New age model: 'BOZOVA' - NOT AVAILABLE
find_age_models("~/Downloads/special_epd_am/", entity_name = "BOZOVA")

#### Counts ----
EPD_SAMPLES_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_repatriated_am_info_5$ID_ENTITY) %>%
  dplyr::select(ID_ENTITY,
                ID_SAMPLE,
                depth)
embsecbio_pollen_counts <- embsecbio_repatriated_am_info$pollen %>%
  dplyr::select(-ID_SAMPLE) %>%
  dplyr::left_join(embsecbio_repatriated_am_info_3 %>%
                     dplyr::select(ID_ENTITY, entity_name, external_ID_ENTITY),
                   by = "external_ID_ENTITY") %>%
  dplyr::mutate(depth_rnd = round(depth, 3)) %>%
  dplyr::left_join(EPD_SAMPLES_NEO_DB %>%
                     dplyr::mutate(depth_rnd = round(depth, 3)),
                   by = c("ID_ENTITY", "depth_rnd"))
embsecbio_pollen_counts %>% dplyr::filter(is.na(ID_SAMPLE))
# embsecbio_pollen_counts %>%
#   dplyr::group_by(ID_SAMPLE, ID_ENTITY, taxon_clean, depth.x)
# extract_embsecbio(1132)

aux <- conn %>%
  special.epd::snapshot(ID_ENTITY = embsecbio_pollen_counts$ID_ENTITY)
aux$date_info$ID_ENTITY %>% unique() %>% length()
aux$sample$ID_ENTITY %>% unique() %>% length()
aux$age_model$ID_SAMPLE %>% unique() %>% length()
embsecbio_pollen_counts$ID_SAMPLE %>% unique() %>% length()
tibble::tibble(
  ID_SAMPLE = embsecbio_pollen_counts$ID_SAMPLE %>% unique(),
) %>%
  dplyr::anti_join(
    tibble::tibble(
      ID_SAMPLE = aux$sample$ID_SAMPLE %>% unique()
    )
  )

# special.epd::snapshot(ID_ENTITY = epd_repatriated_samples_2$ID_ENTITY)
existing_pollen_counts <-
  aux$pollen_count %>%
  purrr::map_df(~.x) %>% # Combine the three pollen count tables
  dplyr::select(ID_SAMPLE) %>%
  dplyr::group_by(ID_SAMPLE) %>%
  dplyr::summarise(n = dplyr::n())

existing_pollen_counts %>%
  dplyr::filter(n != 3) # Verify if sample does not have the 3 tables

# epd_repatriated_samples_3 <- epd_repatriated_samples_2 %>%
#   dplyr::select(-ID_ENTITY, -depth, -thickness, -chronology_name, -age_type, -age, -age_younger, -age_older)
#
# epd_repatriated_samples_3 %>%
#   dplyr::filter(ID_SAMPLE %in% existing_pollen_counts$ID_SAMPLE)
taxon_name_tb <- dabr::select_all(conn, "taxon_name")
# Check if all the EMBSeCBIO taxons are in the `taxon_name` table
new_taxon_names <-
  c(embsecbio_taxa_amalgamation$clean_name,
  embsecbio_taxa_amalgamation$intermediate,
  embsecbio_taxa_amalgamation$amalgamated) %>%
  unique() %>%
  sort() %>%
  tibble::tibble() %>%
  magrittr::set_names("taxon_name") %>%
  dplyr::left_join(taxon_name_tb) %>%
  dplyr::filter(is.na(ID_TAXON))
new_taxon_names %>%
  rpd:::add_records(conn = conn, table = "taxon_name", dry_run = TRUE)
# Results: 47 records were inserted.

# Remove taxons not in the `embsecbio_taxa_amalgamation` table
embsecbio_pollen_counts_excluded <- embsecbio_pollen_counts %>%
  dplyr::filter(!(taxon_clean %in% embsecbio_taxa_amalgamation$taxon_name))
embsecbio_pollen_counts_2 <- embsecbio_pollen_counts %>%
  dplyr::filter(taxon_clean %in% embsecbio_taxa_amalgamation$taxon_name)
##### Clean ----
# oplan <- future::plan(future::multisession, workers = 8)
# options(future.globals.maxSize = 2000*1024^2)
embsecbio_pollen_counts_3 <-
  sort(unique(embsecbio_pollen_counts$ID_SAMPLE)) %>%
  # furrr::future_map_dfr(function(i) {
  purrr::map_df(function(i) {
    embsecbio_pollen_counts_2 %>%
      dplyr::filter(ID_SAMPLE == i) %>%
      dplyr::select(ID_SAMPLE, taxon_name = taxon_clean, count = taxon_count) %>%
      dplyr::filter(!is.na(count)) %>%
      dplyr::left_join(embsecbio_taxa_amalgamation,
                       by = "taxon_name") %>%
      dplyr::left_join(taxon_name_tb,
                       by = c("clean_name" = "taxon_name")) %>%
      dplyr::group_by(ID_SAMPLE, ID_TAXON) %>%
      dplyr::mutate(count = sum(count, na.rm = TRUE),
                    n = length(count)) %>%
      dplyr::distinct(ID_SAMPLE, ID_TAXON, .keep_all = TRUE) %>%
      dplyr::select(ID_SAMPLE, ID_TAXON, count) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(amalgamation_level = 0, .before = count) # Clean names only
  }) %>%
  dplyr::arrange(ID_SAMPLE, ID_TAXON)
# future::plan(oplan)

embsecbio_pollen_counts_3 %>%
  dplyr::filter(is.na(ID_TAXON))
dim(embsecbio_pollen_counts_3)


embsecbio_pollen_counts_3 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()
idx <- idx_pairs(nrow(embsecbio_pollen_counts_3), 2000)
pb <- progress::progress_bar$new(total = nrow(idx))
# meta_neo_res <- seq_len(nrow(embsecbio_pollen_counts)) %>%
meta_neo_res <-
  purrr::map2(idx$x,
              idx$y,
              ~ {
                pb$tick()
                embsecbio_pollen_counts_3[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% embsecbio_pollen_counts_3$ID_SAMPLE,
                amalgamation_level == 0)
waldo::compare(embsecbio_pollen_counts_3 %>%
                 # dplyr::arrange(ID_SAMPLE, ID_TAXON) %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)
# 165980
##### Intermediate ----
embsecbio_taxa_amalgamation_stage2 <- embsecbio_taxa_amalgamation %>%
  dplyr::select(-taxon_name) %>%
  dplyr::distinct(clean_name, intermediate, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(intermediate))

embsecbio_pollen_counts_4 <-
  sort(unique(embsecbio_pollen_counts_3$ID_SAMPLE)) %>%
  purrr::map_df(function(i) {
    embsecbio_pollen_counts_3 %>%
      dplyr::filter(ID_SAMPLE == i) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "ID_TAXON") %>%
      dplyr::left_join(embsecbio_taxa_amalgamation_stage2,
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

embsecbio_pollen_counts_4 %>%
  dplyr::filter(is.na(ID_TAXON))

embsecbio_pollen_counts_4 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()
idx_stage2 <- idx_pairs(nrow(embsecbio_pollen_counts_4), 2000)
pb <- progress::progress_bar$new(total = nrow(idx_stage2))
meta_neo_res <-
  purrr::map2(idx_stage2$x,
              idx_stage2$y,
              ~ {
                pb$tick()
                embsecbio_pollen_counts_4[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% embsecbio_pollen_counts_4$ID_SAMPLE,
                amalgamation_level == 1)
waldo::compare(embsecbio_pollen_counts_4 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)
# 162962

##### Amalgamated ----
embsecbio_taxa_amalgamation_stage3 <- embsecbio_taxa_amalgamation_stage2 %>%
  dplyr::select(-clean_name, -dplyr::starts_with("action")) %>%
  dplyr::distinct(intermediate, amalgamated, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(amalgamated))

embsecbio_pollen_counts_5 <-
  unique(embsecbio_pollen_counts_4$ID_SAMPLE) %>%
  purrr::map_df(function(ID_SAMPLE) {
    embsecbio_pollen_counts_4 %>%
      dplyr::filter(ID_SAMPLE == !!ID_SAMPLE) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "ID_TAXON") %>%
      dplyr::left_join(embsecbio_taxa_amalgamation_stage3,
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

embsecbio_pollen_counts_5 %>%
  dplyr::filter(is.na(ID_TAXON)|is.na(count))

embsecbio_pollen_counts_5 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()
idx_stage3 <- idx_pairs(nrow(embsecbio_pollen_counts_5), 2000)
pb <- progress::progress_bar$new(total = nrow(idx_stage3))
meta_neo_res <-
  purrr::map2(idx_stage3$x,
              idx_stage3$y,
              ~ {
                pb$tick()
                embsecbio_pollen_counts_5[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
# 152998
###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% embsecbio_pollen_counts_5$ID_SAMPLE,
                amalgamation_level == 2)
waldo::compare(embsecbio_pollen_counts_5 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)

embsecbio_pollen_counts %>%
  dplyr::distinct(entity_name, .keep_all = TRUE)


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
# special.epd::snapshot(conn, ID_ENTITY = 365)


non_embsecbio_repatriated_am_info_2 <- non_embsecbio_repatriated_am_info_EPD_COUNTS %>%
  dplyr::mutate(ID_SAMPLE = 28481 + seq_along(ID_ENTITY), # get_id_sample(conn)
                .after = ID_ENTITY) %>%
  dplyr::select(-ID_SITE, -site_id, -site_name, -site_name_clean, -dataset_id, -dataset_name, -entity_name, -sample_id, -unit_name)

# Check if the "new" records are already in th DB:
special.epd::snapshot(conn, ID_ENTITY = non_embsecbio_repatriated_am_info_2$ID_ENTITY)
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

#### Age models (11) ----
non_embsecbio_repatriated_am_new <-
  find_age_models(path, non_embsecbio_repatriated_am_info$entity_name)

# Check if the AM are ready to be uploaded
non_embsecbio_repatriated_am_new %>%
  dplyr::filter(entity_name %in% epd_age_models_ready_to_upload$entity_name)

# Check if the age models already exist in the DB
special.epd::snapshot(conn, entity_name = non_embsecbio_repatriated_am_new$entity_name)

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

special.epd::snapshot(conn, entity_name = non_embsecbio_repatriated_am_new$entity_name)

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

aux <- special.epd::snapshot(conn,
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

embsecbio_taxon_names <- embsecbio_repatriated_am_info$pollen$taxon_clean %>%
  unique()
embsecbio_extra_taxon_names <- embsecbio_extra_repatriated_dates_info$pollen$taxon_clean %>%
  unique()
# tibble::tibble(
#   taxon_clean = c(embsecbio_taxon_names,
#                   embsecbio_extra_taxon_names) %>%
#     unique() %>%
#     sort()
# ) %>% readr::write_excel_csv("~/Downloads/embsecbio_taxon_list_clean_names.csv")

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
special.epd::snapshot(conn, ID_ENTITY = c(509, 1619))
# special.epd::snapshot(conn, entity_name = "GS05")
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
                 .[order(colnames(.))] %>%
                 magrittr::set_class(class(.)[-1]),
               EPD_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-doi),
               tolerance = 1E-9)

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
meta_neo_res <- seq_len(nrow(embsecbio_extra_repatriated_dates_info_3)) %>%
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
                 dplyr::mutate(external_ID_SITE =
                                 as.integer(external_ID_SITE),
                               external_ID_ENTITY =
                                 as.integer(external_ID_ENTITY)) %>%
                 magrittr::set_class(class(.)[-1]),
               EPD_NEO_DB,
               tolerance = 1E-9)

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
  dplyr::filter(ID_ENTITY %in%
                  embsecbio_extra_repatriated_dates_info_4$ID_ENTITY)
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
                               age = round(age)) %>%
                 magrittr::set_class(class(.)[-1]),
               EPD_NEO_DB %>%
                 dplyr::select(-age_older, -age_younger, -age_type, -ID_SAMPLE, -chronology_name, -thickness) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               tolerance = 1)

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
                 .[order(colnames(.))] %>%
                 magrittr::set_class(class(.)[-1]),
               EPD_NEO_DB %>%
                 .[order(colnames(.))],
               tolerance = 1E-9)

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

#### Counts ----
EPD_SAMPLES_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% embsecbio_extra_repatriated_am_info_4$ID_ENTITY) %>%
  dplyr::select(ID_ENTITY,
                ID_SAMPLE,
                depth)

embsecbio_extra_pollen_counts <- embsecbio_extra_repatriated_dates_info$pollen %>%
  dplyr::select(-ID_SAMPLE) %>%
  dplyr::left_join(embsecbio_extra_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, entity_name, external_ID_ENTITY),
                   by = "external_ID_ENTITY") %>%
  dplyr::mutate(depth_rnd = round(depth, 3)) %>%
  dplyr::left_join(EPD_SAMPLES_NEO_DB %>%
                     dplyr::mutate(depth_rnd = round(depth, 3)),
                   by = c("ID_ENTITY", "depth_rnd"))
embsecbio_extra_pollen_counts %>% dplyr::filter(is.na(ID_SAMPLE))

aux <- conn %>%
  special.epd::snapshot(ID_ENTITY = embsecbio_extra_pollen_counts$ID_ENTITY)
aux$date_info$ID_ENTITY %>% unique() %>% length()
aux$sample$ID_ENTITY %>% unique() %>% length()
aux$age_model$ID_SAMPLE %>% unique() %>% length()
embsecbio_extra_pollen_counts$ID_SAMPLE %>% unique() %>% length()
tibble::tibble(
  ID_SAMPLE = embsecbio_extra_pollen_counts$ID_SAMPLE %>% unique(),
) %>%
  dplyr::anti_join(
    tibble::tibble(
      ID_SAMPLE = aux$sample$ID_SAMPLE %>% unique()
    )
  )

# special.epd::snapshot(ID_ENTITY = epd_repatriated_samples_2$ID_ENTITY)
existing_pollen_counts <-
  aux$pollen_count %>%
  purrr::map_df(~.x) %>% # Combine the three pollen count tables
  dplyr::select(ID_SAMPLE) %>%
  dplyr::group_by(ID_SAMPLE) %>%
  dplyr::summarise(n = dplyr::n())

existing_pollen_counts %>%
  dplyr::filter(n != 3) # Verify if sample does not have the 3 tables

taxon_name_tb <- dabr::select_all(conn, "taxon_name")
# Check if all the EMBSeCBIO taxons are in the `taxon_name` table
new_taxon_names <-
  c(embsecbio_taxa_amalgamation$clean_name,
    embsecbio_taxa_amalgamation$intermediate,
    embsecbio_taxa_amalgamation$amalgamated) %>%
  unique() %>%
  sort() %>%
  tibble::tibble() %>%
  magrittr::set_names("taxon_name") %>%
  dplyr::left_join(taxon_name_tb) %>%
  dplyr::filter(is.na(ID_TAXON))
new_taxon_names %>%
  rpd:::add_records(conn = conn, table = "taxon_name", dry_run = TRUE)

# Remove taxons not in the `embsecbio_taxa_amalgamation` table
embsecbio_extra_pollen_counts_excluded <- embsecbio_extra_pollen_counts %>%
  dplyr::filter(!(taxon_clean %in% embsecbio_taxa_amalgamation$taxon_name))
embsecbio_extra_pollen_counts_2 <- embsecbio_extra_pollen_counts %>%
  dplyr::filter(taxon_clean %in% embsecbio_taxa_amalgamation$taxon_name)
##### Clean ----
embsecbio_extra_pollen_counts_3 <-
  sort(unique(embsecbio_extra_pollen_counts$ID_SAMPLE)) %>%
  purrr::map_df(function(i) {
    embsecbio_extra_pollen_counts_2 %>%
      dplyr::filter(ID_SAMPLE == i) %>%
      dplyr::select(ID_SAMPLE, taxon_name = taxon_clean, count = taxon_count) %>%
      dplyr::filter(!is.na(count)) %>%
      dplyr::left_join(embsecbio_taxa_amalgamation,
                       by = "taxon_name") %>%
      dplyr::left_join(taxon_name_tb,
                       by = c("clean_name" = "taxon_name")) %>%
      dplyr::group_by(ID_SAMPLE, ID_TAXON) %>%
      dplyr::mutate(count = sum(count, na.rm = TRUE),
                    n = length(count)) %>%
      dplyr::distinct(ID_SAMPLE, ID_TAXON, .keep_all = TRUE) %>%
      dplyr::select(ID_SAMPLE, ID_TAXON, count) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(amalgamation_level = 0, .before = count) # Clean names only
  }) %>%
  dplyr::arrange(ID_SAMPLE, ID_TAXON)

embsecbio_extra_pollen_counts_3 %>%
  dplyr::filter(is.na(ID_TAXON))
dim(embsecbio_extra_pollen_counts_3)
embsecbio_extra_pollen_counts_3 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()

idx <- idx_pairs(nrow(embsecbio_extra_pollen_counts_3), 2000)
pb <- progress::progress_bar$new(total = nrow(idx))
meta_neo_res <-
  purrr::map2(idx$x,
              idx$y,
              ~ {
                pb$tick()
                embsecbio_extra_pollen_counts_3[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% embsecbio_extra_pollen_counts_3$ID_SAMPLE,
                amalgamation_level == 0)
waldo::compare(embsecbio_extra_pollen_counts_3 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)
# 9822
##### Intermediate ----
embsecbio_taxa_amalgamation_stage2 <- embsecbio_taxa_amalgamation %>%
  dplyr::select(-taxon_name) %>%
  dplyr::distinct(clean_name, intermediate, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(intermediate))

embsecbio_extra_pollen_counts_4 <-
  sort(unique(embsecbio_extra_pollen_counts_3$ID_SAMPLE)) %>%
  purrr::map_df(function(i) {
    embsecbio_extra_pollen_counts_3 %>%
      dplyr::filter(ID_SAMPLE == i) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "ID_TAXON") %>%
      dplyr::left_join(embsecbio_taxa_amalgamation_stage2,
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

embsecbio_extra_pollen_counts_4 %>%
  dplyr::filter(is.na(ID_TAXON))

embsecbio_extra_pollen_counts_4 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()
idx_stage2 <- idx_pairs(nrow(embsecbio_extra_pollen_counts_4), 2000)
pb <- progress::progress_bar$new(total = nrow(idx_stage2))
meta_neo_res <-
  purrr::map2(idx_stage2$x,
              idx_stage2$y,
              ~ {
                pb$tick()
                embsecbio_extra_pollen_counts_4[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% embsecbio_extra_pollen_counts_4$ID_SAMPLE,
                amalgamation_level == 1)
waldo::compare(embsecbio_extra_pollen_counts_4 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)
# 9688

##### Amalgamated ----
embsecbio_taxa_amalgamation_stage3 <- embsecbio_taxa_amalgamation_stage2 %>%
  dplyr::select(-clean_name, -dplyr::starts_with("action")) %>%
  dplyr::distinct(intermediate, amalgamated, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(amalgamated))

embsecbio_extra_pollen_counts_5 <-
  unique(embsecbio_extra_pollen_counts_4$ID_SAMPLE) %>%
  purrr::map_df(function(ID_SAMPLE) {
    embsecbio_extra_pollen_counts_4 %>%
      dplyr::filter(ID_SAMPLE == !!ID_SAMPLE) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "ID_TAXON") %>%
      dplyr::left_join(embsecbio_taxa_amalgamation_stage3,
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

embsecbio_extra_pollen_counts_5 %>%
  dplyr::filter(is.na(ID_TAXON)|is.na(count))

embsecbio_extra_pollen_counts_5 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()
idx_stage3 <- idx_pairs(nrow(embsecbio_extra_pollen_counts_5), 2000)
pb <- progress::progress_bar$new(total = nrow(idx_stage3))
meta_neo_res <-
  purrr::map2(idx_stage3$x,
              idx_stage3$y,
              ~ {
                pb$tick()
                embsecbio_extra_pollen_counts_5[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% embsecbio_extra_pollen_counts_5$ID_SAMPLE,
                amalgamation_level == 2)
waldo::compare(embsecbio_extra_pollen_counts_5 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)
# 9312

embsecbio_extra_pollen_counts %>%
  dplyr::distinct(entity_name, .keep_all = TRUE)


## IBERIA ----
# data("IBERIA_pollen")
# data("IBERIA_pollen_dates")
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
                IBERIA_entity_name = entity_name_in_iberia) %>%
  dplyr::mutate(IBERIA_site_name = IBERIA_site_name %>%
                  stringr::str_replace_all("Eix", "Elx"),
                IBERIA_entity_name = IBERIA_entity_name %>%
                  stringr::str_replace_all("EIX", "ELX"))

# iberia_repatriation %>%
#   dplyr::arrange(IBERIA_site_name, IBERIA_entity_name) %>%
#   dplyr::distinct(IBERIA_site_name) %>%
#   dplyr::mutate(IBERIA_ID_SITE = seq_along(IBERIA_site_name))
  # dplyr::mutate(IBERIA_ID_ENTITY = seq_along(IBERIA_entity_name)) %>%

# IBERIA_pollen %>%
#   dplyr::select(1:5) %>%
#   dplyr::distinct() %>%
#   dplyr::filter(entity_name %in% iberia_repatriation_dates$IBERIA_entity_name)

### IBERIA dates (84) ----
iberia_repatriation_dates <- iberia_repatriation %>%
  dplyr::filter(dates_to_be_extracted_from_iberia)

iberia_repatriated_dates_info <-
  iberia_repatriation_dates$IBERIA_entity_name %>%
  stringr::str_replace_all("EIX", "ELX") %>%
  extract_iberia()

iberia_repatriated_dates_info_2 <- iberia_repatriation_dates %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                entity_name = IBERIA_entity_name) %>%
  dplyr::inner_join(iberia_repatriated_dates_info$metadata,
                    by = "entity_name") %>%
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
dabr::select(conn,
             "SELECT * FROM external_link WHERE ID_ENTITY = 387 AND",
             "external_source = 'IBERIA'")
dabr::update(conn,
             "UPDATE external_link SET external_site_name = 'Elx',",
             "external_entity_name = 'ELX' WHERE",
             "ID_SITE = 351 AND ID_ENTITY = 387 AND external_source = 'IBERIA'")
# Results: 1 record was updated.

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
      rpd:::add_records(conn = conn, table = "external_link", dry_run = TRUE)
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
               EPD_NEO_DB,
               tolerance = 1E-9)

#### Dates ----
# Pull existing ID_DATE_INFO
EPD_DATES_NEO_DB <- dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in% iberia_repatriated_dates_info_4$ID_ENTITY)
unique(EPD_DATES_NEO_DB$ID_DATE_INFO)
unique(EPD_DATES_NEO_DB$ID_ENTITY)
# readr::read_csv("inst/extdata/epd-special-iberia-v2-dates_2022-03-16.csv")
# EPD_DATES_NEO_DB %>%
#   readr::write_excel_csv("inst/extdata/epd-special-iberia-v2-dates_2022-03-16.csv")

# dabr::select(conn,
#              "SELECT * FROM date_info WHERE ID_ENTITY IN (",
#              paste0(EPD_DATES_NEO_DB$ID_ENTITY, collapse = ", "),
#              ")")
# dabr::delete(conn,
#              "DELETE FROM date_info WHERE ID_ENTITY IN (",
#              paste0(EPD_DATES_NEO_DB$ID_ENTITY, collapse = ", "),
#              ")")
# Results: 618 records were deleted.

iberia_repatriated_dates_info_4 <-
  iberia_repatriated_dates_info$date_info %>%
  dplyr::select(-ID_SITE) %>%
  dplyr::mutate(
    lab_num = dplyr::case_when(
      stringr::str_detect(lab_num, "NULL") ~ NA_character_,
      TRUE ~ lab_num
    ),
    material_dated = dplyr::case_when(
      stringr::str_detect(material_dated, "NULL") ~ NA_character_,
      TRUE ~ material_dated
    )) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                reason_age_not_used = notes,
                age_calib = age_cal) %>%
  dplyr::mutate(age_used = ifelse(is.na(reason_age_not_used), "yes", "no")) %>%
  dplyr::left_join(iberia_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  dplyr::select(-external_ID_ENTITY, -entity_name) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::mutate(ID_DATE_INFO = 9938 + seq_along(ID_ENTITY)) # get_id_date_info(conn)
  # dplyr::mutate(ID_DATE_INFO = c(657:1010, 1471, 1472, 1011:1272))
  # dplyr::mutate(ID_DATE_INFO = c(657:1272, 1471, 1472))
# IDs 1471 and 1472 are linked to Lake Saloio (SALOIO, 828)

iberia_repatriated_dates_info_4 %>%
  dplyr::filter(is.na(ID_ENTITY) | is.na(ID_DATE_INFO))
# Find gaps in the table (i.e. non-continuous ID_DATE_INFO)
aa <- dabr::select_all(conn, "date_info")
tibble::tibble(ID_DATE_INFO = seq_len(max(aa$ID_ENTITY))) %>%
  dplyr::left_join(aa) %>%
  dplyr::filter(is.na(ID_ENTITY)) %>%
  .$ID_DATE_INFO

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
                 .[order(colnames(.))] %>%
                 dplyr::arrange(ID_ENTITY, depth),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-notes) %>%
                 dplyr::arrange(ID_ENTITY, depth),
               tolerance = 1E-9,
               max_diffs = Inf)

### IBERIA age models (69) ----
iberia_repatriation_am <- iberia_repatriation %>%
  dplyr::filter(age_model_to_be_extracted_from_iberia)

iberia_repatriated_am_info <-
  iberia_repatriation_am$IBERIA_entity_name %>%
  stringr::str_replace_all("EIX", "ELX") %>%
  extract_iberia()

iberia_repatriated_am_info_2 <- iberia_repatriation_am %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                entity_name = IBERIA_entity_name) %>%
  # ID_ENTITY = IBERIA_ID_ENTITY) %>%
  dplyr::left_join(iberia_repatriated_am_info$metadata,
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
  dplyr::mutate(ID_SAMPLE = c(6233L:7041L, 7144L:8130L, 8230L:9824L, 9857L:11040L, 12409L:12432L),
                .before = 1)
  # dplyr::mutate(ID_SAMPLE = c(6233:7041, 7144:7334, 7335:8130, 8230:8433, 8434:9433, 9434:9824, 9857:10465, 10466:11040, 12409:12432),
  #                 # 6232 + seq_along(ID_ENTITY), # get_id_sample(conn)
  #               .before = 1)

# Dump old samples:
# dabr::select(conn,
#              "SELECT * FROM sample WHERE ID_SAMPLE IN (",
#              paste0(c(6233L:7041L, 7144L:8130L, 8230L:9824L, 9857L:11040L, 12409L:12432L),
#                     collapse = ", "),
#              ")") %>%
#   readr::write_excel_csv("inst/extdata/epd-special-iberia-v2-samples_2022-03-16.csv")

# dabr::delete(conn,
#              "DELETE FROM sample WHERE ID_SAMPLE IN (",
#              paste0(c(6233L:7041L, 7144L:8130L, 8230L:9824L, 9857L:11040L, 12409L:12432L),
#                     collapse = ", "),
#              ")")
# # Results: 4599 records were deleted.
dabr::select(conn,
             "SELECT * FROM sample WHERE ID_SAMPLE IN (",
             paste0(c(6233L:7041L, 7144L:8130L, 8230L:9824L, 9857L:11040L, 12409L:12432L),
                    collapse = ", "),
             ")")

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
dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% iberia_repatriated_am_info_5$ID_ENTITY)

meta_neo_res <- seq_len(nrow(iberia_repatriated_am_info_5)) %>%
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
                               age = round(age)) %>%
                 dplyr::arrange(ID_ENTITY, depth),
               EPD_DATES_NEO_DB %>%
                 dplyr::select(-count_type, -sample_type) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)) %>%
                 dplyr::arrange(ID_ENTITY, depth),
               tolerance = 1E-9,
               max_diffs = Inf)

# sort(unique(EPD_DATES_NEO_DB$ID_ENTITY))[36:38]
# EPD_DATES_NEO_DB %>%
#   dplyr::filter(ID_ENTITY == 827) %>% .$ID_SAMPLE %>% range()
# iberia_repatriated_am_info_5 %>%
#   dplyr::filter(ID_ENTITY == 827) %>% .$ID_SAMPLE %>% range()
# aux <- sort(unique(EPD_DATES_NEO_DB$ID_ENTITY)) %>%
#   purrr::map(function(i) {
#     db <- EPD_DATES_NEO_DB %>%
#       dplyr::filter(ID_ENTITY == i) %>%
#       dplyr::select(-count_type, -sample_type) %>%
#       .[order(colnames(.))]
#     ss <- iberia_repatriated_am_info_5 %>%
#       dplyr::filter(ID_ENTITY == i) %>%
#       dplyr::select(1:9) %>%
#       .[order(colnames(.))]
#     waldo::compare(ss, db, x_arg = "SS", y_arg = "DB", tolerance = 1E-9)
#   })

#### Age models ----
meta_neo_res <- seq_len(nrow(iberia_repatriated_am_info_5)) %>%
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

### non-IBERIA age models (15) ----
# NOTE: even though these entities were marked as "non-IBERIAN", the latest
# version of the Iberian subset contains all the necessary updates.
non_iberia_repatriation_am <- iberia_repatriation %>%
  dplyr::filter(!age_model_to_be_extracted_from_iberia)

non_iberia_repatriated_am_info <-
  non_iberia_repatriation_am$IBERIA_entity_name %>%
  extract_iberia()

non_iberia_repatriated_am_info_2 <- non_iberia_repatriation_am %>%
  dplyr::select(neotoma_ID_SITE = site_id,
                neotoma_site_name = site_name,
                neotoma_entity_name = entity_name,
                entity_name = IBERIA_entity_name) %>%
  dplyr::left_join(non_iberia_repatriated_am_info$metadata,
                   by = "entity_name") %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                external_ID_SITE =  ID_SITE,
                external_site_name = site_name,
                external_entity_name = entity_name)

non_iberia_repatriated_am_info_3 <- EPD_METADATA %>%
  dplyr::select(1:4, 6, 10) %>%
  dplyr::right_join(non_iberia_repatriated_am_info_2 %>%
                      dplyr::select(1:7),
                    by = c("entity_name" = "neotoma_entity_name"))

non_iberia_repatriated_am_info_EPD_COUNTS <- EPD_COUNTS %>%
  dplyr::filter(entity_name %in%
                  non_iberia_repatriated_am_info_3$entity_name) %>%
  smpds::rm_na_taxa(1:16) %>%
  dplyr::select(-chronology_id) %>%
  dplyr::left_join(EPD_METADATA %>%
                     dplyr::select(1:3, 6, 10),
                   by = c("site_id", "dataset_id", "entity_name")) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1)

# non_iberia_repatriated_am_info_2 <- non_iberia_repatriated_am_info_EPD_COUNTS %>%
#   dplyr::mutate(ID_SAMPLE = 28481 + seq_along(ID_ENTITY), # get_id_sample(conn)
#                 .after = ID_ENTITY) %>%
#   dplyr::select(-ID_SITE, -site_id, -site_name, -site_name_clean, -dataset_id, -dataset_name, -entity_name, -sample_id, -unit_name)
#
# # Check if the "new" records are already in th DB:
# special.epd::snapshot(conn, ID_ENTITY = non_iberia_repatriated_am_info_2$ID_ENTITY)
# dabr::select_all(conn, "sample") %>%
#   dplyr::filter(ID_SAMPLE %in% non_iberia_repatriated_am_info_2$ID_SAMPLE |
#                   ID_ENTITY %in% non_iberia_repatriated_am_info_2$ID_ENTITY)


#### Samples ----
# non_iberia_repatriated_am_info_EPD_COUNTS <- EPD_COUNTS %>%
#   dplyr::filter(dataset_id %in%
#                   non_iberia_repatriated_am_info$external_ID_ENTITY) %>%
#   smpds::rm_na_taxa(1:16) %>%
#   dplyr::select(-chronology_id) %>%
#   dplyr::left_join(EPD_METADATA %>%
#                      dplyr::select(1:3, 6, 10),
#                    by = c("site_id", "dataset_id", "entity_name")) %>%
#   dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1) #%>%
# # dplyr::mutate(depth = ifelse(ID_ENTITY == 365 & depth == 0, 0.5, depth)) # Update depth GDU: 0 -> 0.5
# non_iberia_repatriated_am_info_EPD_COUNTS


non_iberia_repatriated_am_info_4 <- non_iberia_repatriated_am_info$age_model %>%
  dplyr::select(-ID_SITE) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY) %>%
  dplyr::select(-entity_name) %>%
  dplyr::left_join(non_iberia_repatriated_am_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY, entity_name),
                   by = "external_ID_ENTITY") %>%
  dplyr::select(-external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::select(-site_name, #-entity_name,
                -latitude, -longitude, -elevation, -source, -publication)

dim(non_iberia_repatriated_am_info_EPD_COUNTS)
dim(non_iberia_repatriated_am_info_4)

# a <- non_iberia_repatriated_am_info_EPD_COUNTS %>%
#   dplyr::group_by(entity_name) %>%
#   dplyr::select(1:16) %>%
#   dplyr::mutate(n_epd = length(ID_ENTITY),
#                 depths_epd = tibble::tibble(source = "EPD",
#                                             depth) %>% list()) %>%
#   dplyr::distinct(entity_name, n_epd, depths_epd)
#
# b <- non_iberia_repatriated_am_info_4 %>%
#   dplyr::group_by(entity_name) %>%
#   dplyr::select(1:10) %>%
#   dplyr::mutate(n_iberia = length(ID_ENTITY),
#                 depths_iberia = tibble::tibble(source = "IBERIA",
#                                                depth) %>% list()) %>%
#   dplyr::distinct(entity_name, n_iberia, depths_iberia)
#
# dplyr::full_join(a, b) %>%
#   dplyr::arrange(entity_name)
# dplyr::full_join(a, b) %>%
#   dplyr::arrange(entity_name) %>%
#   purrr::pmap_df(function(entity_name, depths_epd, depths_iberia, ...) {
#     dplyr::full_join(depths_epd, depths_iberia, by = "depth") %>%
#       dplyr::filter(is.na(source.x) | is.na(source.y)) %>%
#       dplyr::mutate(entity_name, .before = 1)
#   })

# Extract thickness from the EPD data
non_iberia_repatriated_am_info_5 <- non_iberia_repatriated_am_info_4 %>%
  dplyr::mutate(depth2 = round(depth, 3)) %>%
  dplyr::left_join(non_iberia_repatriated_am_info_EPD_COUNTS %>%
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
  dplyr::mutate(age = ifelse(is.na(age), round(age_EPD), age)) %>%
  dplyr::mutate(lower = round(age_EPD) * 0.99,
                upper = round(age_EPD) * 1.01,
                same_age = (round(age) >= lower & round(age) <= upper) | (is.na(age) & is.na(age_EPD)),
                chronology_name = ifelse(same_age, chronology_name, NA),
                age_younger = ifelse(same_age, age_younger, NA),
                age_older = ifelse(same_age, age_older, NA),
                age_type = ifelse(same_age, age_type, NA)) %>%
  dplyr::select(-age_EPD, -lower, -upper, -same_age)


# Find existing samples
existing_samples <- dabr::select(conn,
                                 "SELECT * FROM sample WHERE ID_ENTITY IN (",
                                 paste0(non_iberia_repatriated_am_info_5$ID_ENTITY,
                                        collapse = ", "),
                                 ")")

samples_already_in_the_db <- non_iberia_repatriated_am_info_5 %>%
  dplyr::left_join(existing_samples %>%
                     dplyr::select(ID_ENTITY, ID_SAMPLE, depth)) %>%
  dplyr::filter(!is.na(ID_SAMPLE)) %>% #.$ID_ENTITY %>% unique()
  dplyr::relocate(ID_SAMPLE, .after = 1)

waldo::compare(existing_samples %>%
                 .[order(colnames(.))] %>%
                 dplyr::select(-count_type, -sample_type),
               samples_already_in_the_db[1:9] %>%
                 .[order(colnames(.))],
               tolerance = 1E-9)

non_iberia_repatriated_am_info_6 <- non_iberia_repatriated_am_info_5 %>%
  dplyr::left_join(existing_samples %>%
                     dplyr::select(ID_ENTITY, ID_SAMPLE, depth)) %>%
  dplyr::filter(is.na(ID_SAMPLE)) %>% #.$ID_ENTITY %>% unique()
  dplyr::relocate(ID_SAMPLE, .after = 1) %>%
  dplyr::mutate(ID_SAMPLE = 101360 + seq_along(ID_ENTITY)) # get_id_sample(conn)

# Check if any of the samples exists
dabr::select(conn,
             "SELECT * FROM sample WHERE ID_SAMPLE IN (",
             paste0(non_iberia_repatriated_am_info_6$ID_SAMPLE,
                    collapse = ", "),
             ")")

meta_neo_res <- seq_len(nrow(non_iberia_repatriated_am_info_6)) %>%
  purrr::map(function(i) {
    non_iberia_repatriated_am_info_6[i, ] %>%
      dplyr::select(1:9) %>%
      rpd:::add_records(conn = conn, table = "sample", dry_run = TRUE)
  })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% non_iberia_repatriated_am_info_6$ID_ENTITY)
waldo::compare(non_iberia_repatriated_am_info_6 %>%
                 dplyr::select(1:9) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)) %>%
                 dplyr::select(-count_type, -sample_type),
               tolerance = 1E-9)

#### Age models (15) ----
# non_iberia_repatriated_am_new <-
#   find_age_models(path, non_iberia_repatriated_am_info$metadata$entity_name)
#
# # Check if the AM are ready to be uploaded
# non_iberia_repatriated_am_new %>%
#   dplyr::filter(entity_name %in% epd_age_models_ready_to_upload$entity_name)
#
# # Check if the age models already exist in the DB
# special.epd::snapshot(conn, entity_name = non_iberia_repatriated_am_new$entity_name)
#
# non_iberia_repatriated_am_new2 <- non_iberia_repatriated_am_new %>%
#   purrr::pmap_df(upload_age_model, conn = conn)
# # Check if all the age models were uploaded
# all(non_iberia_repatriated_am_new2$status)
# non_iberia_repatriated_am_new2
# non_iberia_repatriated_am_new3 <-
#   non_iberia_repatriated_am_new2$am %>%
#   purrr::map_df(~.x) %>%
#   magrittr::set_names(colnames(.) %>% stringr::str_to_upper())
samples_already_in_the_db
non_iberia_repatriated_am_info_6
# Delete old age models
dabr::select(conn,
             "SELECT * FROM age_model WHERE ID_SAMPLE IN (",
             paste0(samples_already_in_the_db$ID_SAMPLE, collapse = ", "),
             ") AND ID_MODEL = 8")
# dabr::delete(conn,
#              "DELETE FROM age_model WHERE ID_SAMPLE IN (",
#              paste0(samples_already_in_the_db$ID_SAMPLE, collapse = ", "),
#              ") AND ID_MODEL = 8")
# special.epd::snapshot(conn,
#                       entity_name = non_iberia_repatriated_am_new$entity_name)

samples_already_in_the_db %>%
  dplyr::select(ID_SAMPLE, mean, median, dplyr::starts_with("UNCERT")) %>%
  dplyr::mutate(ID_MODEL = 8, .before = 2) %>%
  rpd:::add_records(conn = conn, table = "age_model", dry_run = TRUE)
non_iberia_repatriated_am_info_6 %>%
  dplyr::select(ID_SAMPLE, mean, median, dplyr::starts_with("UNCERT")) %>%
  dplyr::mutate(ID_MODEL = 8, .before = 2) %>%
  rpd:::add_records(conn = conn, table = "age_model", dry_run = TRUE)
##### Validate ----
EPD_NEO_DB <- dabr::select_all(conn, "age_model") %>%
  dplyr::filter(ID_SAMPLE %in% samples_already_in_the_db$ID_SAMPLE)
waldo::compare(samples_already_in_the_db %>%
                 dplyr::select(ID_SAMPLE, mean, median,
                               dplyr::starts_with("UNCERT")) %>%
                 dplyr::mutate(ID_MODEL = 8, .before = 2) %>%
                 .[order(colnames(.))],
               EPD_NEO_DB %>%
                 .[order(colnames(.))],
               tolerance = 1E-9)

EPD_NEO_DB <- dabr::select_all(conn, "age_model") %>%
  dplyr::filter(ID_SAMPLE %in% non_iberia_repatriated_am_info_6$ID_SAMPLE)
waldo::compare(non_iberia_repatriated_am_info_6 %>%
                 dplyr::select(ID_SAMPLE, mean, median,
                               dplyr::starts_with("UNCERT")) %>%
                 dplyr::mutate(ID_MODEL = 8, .before = 2) %>%
                 .[order(colnames(.))],
               EPD_NEO_DB %>%
                 .[order(colnames(.))],
               tolerance = 1E-9)


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
  dplyr::rename(entity_name = entity_name_7) %>%
  dplyr::select(-site_id, -iberia_id_entity, -entity_name_12)

### IBERIA dates (38) ----
iberia_extra_repatriation_dates <- iberia_extra_repatriation %>%
  dplyr::filter(dates_to_be_extracted_from_iberia)

iberia_extra_repatriated_dates_info <-
  iberia_extra_repatriation_dates$entity_name %>%
  extract_iberia()

#### Entities ----
iberia_extra_repatriated_dates_info_entities <-
  iberia_extra_repatriated_dates_info$metadata %>%
  dplyr::arrange(site_name, entity_name) %>%
  # dplyr::mutate(ID_SITE = seq_along(ID_SITE) + get_id_site(conn),
  #               ID_ENTITY = seq_along(ID_ENTITY) + get_id_entity(conn))
  dplyr::mutate(ID_SITE = c(1454:1476, 1478:1492),
                ID_ENTITY = c(1628:1650, 1652:1666))

##### Verify if the "new" entities exist in the DB
dabr::select_all(conn, "entity") %>%
  dplyr::filter(ID_SITE %in%
                  iberia_extra_repatriated_dates_info_entities$ID_SITE)
dabr::select_all(conn, "entity") %>%
  dplyr::filter(ID_ENTITY %in%
                  iberia_extra_repatriated_dates_info_entities$ID_ENTITY)

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
  dplyr::filter(external_entity_name %in%
                  iberia_extra_repatriated_dates_info_3$external_entity_name) %>%
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

# Find existing dates
old_dates <- dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in%
                  iberia_extra_repatriated_dates_info_3$ID_ENTITY) #%>%
  # readr::write_excel_csv("inst/extdata/epd-special-iberia-extra-v2-dates_2022-03-17.csv")
dabr::select(conn,
             "SELECT * FROM date_info WHERE ID_DATE_INFO IN (",
             paste0(old_dates$ID_DATE_INFO, collapse = ", "),
             ")")
# dabr::delete(conn,
#              "DELETE FROM date_info WHERE ID_DATE_INFO IN (",
#              paste0(old_dates$ID_DATE_INFO, collapse = ", "),
#              ")")
# # Results: 332 records were deleted.

iberia_extra_repatriated_dates_info_4 <-
  iberia_extra_repatriated_dates_info$date_info %>%
  dplyr::select(-ID_SITE) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY,
                reason_age_not_used = notes,
                age_calib = age_cal) %>%
  dplyr::mutate(age_used = ifelse(is.na(reason_age_not_used), "yes", "no")) %>%
  dplyr::left_join(iberia_extra_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  dplyr::select(-external_ID_ENTITY, #-site_name,
                -entity_name) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::mutate(ID_DATE_INFO = 10556 + seq_along(ID_ENTITY)) #1273:1606)

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
               tolerance = 1E-9)

#### Samples ----
dabr::select(conn,
             "SELECT * FROM sample WHERE ID_ENTITY IN (",
             paste0(iberia_extra_repatriated_dates_info_4$ID_ENTITY,
                    collapse = ", "),
             ")")
# dabr::delete(conn,
#              "DELETE FROM sample WHERE ID_ENTITY IN (",
#              paste0(iberia_extra_repatriated_dates_info_4$ID_ENTITY,
#                     collapse = ", "),
#              ")")
# # Results: 2699 records were deleted.

iberia_extra_repatriated_am_info_4 <-
  iberia_extra_repatriated_dates_info$age_model %>%
  dplyr::select(-ID_SITE) %>%
  dplyr::rename(external_ID_ENTITY = ID_ENTITY) %>%
  dplyr::left_join(iberia_extra_repatriated_dates_info_3 %>%
                     dplyr::select(ID_ENTITY, external_ID_ENTITY)) %>%
  dplyr::select(-external_ID_ENTITY) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::select(-site_name, -entity_name, -latitude, -longitude, -elevation, -source, -publication) %>%
  dplyr::mutate(ID_SAMPLE = c(11041:12408, 12433:13763),
                  # seq_along(ID_ENTITY) + get_id_sample(conn),
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
               tolerance = 1E-9)

#### Age models ----
dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% iberia_extra_repatriated_am_info_4$ID_ENTITY)
dabr::select_all(conn, "age_model") %>%
  dplyr::filter(ID_SAMPLE %in% iberia_extra_repatriated_am_info_4$ID_SAMPLE)

iberia_extra_repatriated_am_info_5 <- iberia_extra_repatriated_am_info_4 %>%
  dplyr::select(-c(1, 3:4))
meta_neo_res <- seq_len(nrow(iberia_extra_repatriated_am_info_5)) %>%
  purrr::map(function(i) {
    iberia_extra_repatriated_am_info_5[i, ] %>%
      dplyr::mutate(ID_MODEL = 8, .before = 2) %>% # Bacon IntCal20
      rpd:::add_records(conn = conn, table = "age_model", dry_run = TRUE)
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
               tolerance = 1E-9)

#### (HERE) COUNTS ----
special.epd::snapshot(conn,
                      entity_name = iberia_extra_repatriation_dates$entity_name)
data("IBERIA_extra_counts_v3")
IBERIA_extra_counts_v3

iberia_extra_repatriated_counts <-
  iberia_extra_repatriated_am_info_4 %>%
  dplyr::select(ID_ENTITY, ID_SAMPLE, depth) %>%
  dplyr::mutate(depth_rnd = round(depth, 3)) %>%
  dplyr::right_join(IBERIA_extra_counts_v3 %>%
                     dplyr::mutate(depth_rnd = round(depth, 3)),
                   by = c("ID_ENTITY", "depth_rnd"))
  # dplyr::right_join(IBERIA_extra_counts_v3,
  #                   by = c("ID_ENTITY", "depth"))
dim(iberia_extra_repatriated_counts)
dim(IBERIA_extra_counts_v3)

id_sample_with_counts <- unique(iberia_extra_repatriated_counts$ID_SAMPLE)
id_sample_with_am <- unique(iberia_extra_repatriated_am_info_4$ID_SAMPLE)
id_samples_without_counts <-
  id_sample_with_am[!(id_sample_with_am %in% id_sample_with_counts)]
dabr::select(conn,
             "SELECT * FROM sample WHERE ID_SAMPLE IN (",
             paste0(id_samples_without_counts, collapse = ","),
             ")") %>%
  dplyr::select(1:7) %>%
  dplyr::left_join(dabr::select_all(conn, "entity")) %>%
  dplyr::select(1:10)

# NOTE: there are some entities with hiatus, so the samples were not included
## in the database
iberia_extra_repatriated_counts_not_used <- iberia_extra_repatriated_counts %>%
  dplyr::filter(is.na(ID_SAMPLE),
                entity_name %in% c("PERAFITA",
                                   "PozoN_2015 core"))
iberia_extra_repatriated_counts_2 <- iberia_extra_repatriated_counts %>%
  dplyr::filter(!is.na(ID_SAMPLE))
iberia_extra_repatriated_counts_2 %>%
  dplyr::filter(is.na(ID_ENTITY) | is.na(ID_SAMPLE)) %>%
  dplyr::distinct(ID_ENTITY, depth_rnd, .keep_all = TRUE)
  # .$ID_ENTITY %>% unique
waldo::compare(IBERIA_extra_counts_v3 %>%
                 dplyr::arrange(ID_ENTITY, depth),
               iberia_extra_repatriated_counts %>%
                 dplyr::arrange(ID_ENTITY, depth.x))

aux <- conn %>%
  special.epd::snapshot(ID_ENTITY = iberia_extra_repatriated_counts$ID_ENTITY)
aux$date_info$ID_ENTITY %>% unique() %>% length()
aux$sample$ID_ENTITY %>% unique() %>% length()
aux$age_model$ID_SAMPLE %>% unique() %>% length()
iberia_extra_repatriated_counts$ID_SAMPLE %>% unique() %>% length()
tibble::tibble(
  ID_SAMPLE = iberia_extra_repatriated_counts$ID_SAMPLE %>% unique(),
) %>%
  dplyr::anti_join(
    tibble::tibble(
      ID_SAMPLE = aux$sample$ID_SAMPLE %>% unique()
    )
  )

# special.epd::snapshot(ID_ENTITY = epd_repatriated_samples_2$ID_ENTITY)
existing_pollen_counts <-
  aux$pollen_count %>%
  purrr::map_df(~.x) %>% # Combine the three pollen count tables
  dplyr::select(ID_SAMPLE) %>%
  dplyr::group_by(ID_SAMPLE) %>%
  dplyr::summarise(n = dplyr::n())

existing_pollen_counts %>%
  dplyr::filter(n != 3) # Verify if sample does not have the 3 tables

taxon_name_tb <- dabr::select_all(conn, "taxon_name")
# Check if all the EMBSeCBIO taxons are in the `taxon_name` table
new_taxon_names <-
  c(embsecbio_taxa_amalgamation$clean_name,
    embsecbio_taxa_amalgamation$intermediate,
    embsecbio_taxa_amalgamation$amalgamated) %>%
  unique() %>%
  sort() %>%
  tibble::tibble() %>%
  magrittr::set_names("taxon_name") %>%
  dplyr::left_join(taxon_name_tb) %>%
  dplyr::filter(is.na(ID_TAXON))
new_taxon_names %>%
  rpd:::add_records(conn = conn, table = "taxon_name", dry_run = TRUE)

# Remove taxons not in the `embsecbio_taxa_amalgamation` table
iberia_extra_repatriated_counts_excluded <- iberia_extra_repatriated_counts %>%
  dplyr::filter(!(taxon_clean %in% embsecbio_taxa_amalgamation$taxon_name))
iberia_extra_repatriated_counts_2 <- iberia_extra_repatriated_counts %>%
  dplyr::filter(taxon_clean %in% embsecbio_taxa_amalgamation$taxon_name)
##### Clean ----
iberia_extra_repatriated_counts_3 <-
  sort(unique(iberia_extra_repatriated_counts$ID_SAMPLE)) %>%
  purrr::map_df(function(i) {
    iberia_extra_repatriated_counts_2 %>%
      dplyr::filter(ID_SAMPLE == i) %>%
      dplyr::select(ID_SAMPLE, taxon_name = taxon_clean, count = taxon_count) %>%
      dplyr::filter(!is.na(count)) %>%
      dplyr::left_join(embsecbio_taxa_amalgamation,
                       by = "taxon_name") %>%
      dplyr::left_join(taxon_name_tb,
                       by = c("clean_name" = "taxon_name")) %>%
      dplyr::group_by(ID_SAMPLE, ID_TAXON) %>%
      dplyr::mutate(count = sum(count, na.rm = TRUE),
                    n = length(count)) %>%
      dplyr::distinct(ID_SAMPLE, ID_TAXON, .keep_all = TRUE) %>%
      dplyr::select(ID_SAMPLE, ID_TAXON, count) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(amalgamation_level = 0, .before = count) # Clean names only
  }) %>%
  dplyr::arrange(ID_SAMPLE, ID_TAXON)

iberia_extra_repatriated_counts_3 %>%
  dplyr::filter(is.na(ID_TAXON))
dim(iberia_extra_repatriated_counts_3)
iberia_extra_repatriated_counts_3 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()

idx <- idx_pairs(nrow(iberia_extra_repatriated_counts_3), 2000)
pb <- progress::progress_bar$new(total = nrow(idx))
meta_neo_res <-
  purrr::map2(idx$x,
              idx$y,
              ~ {
                pb$tick()
                iberia_extra_repatriated_counts_3[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% iberia_extra_repatriated_counts_3$ID_SAMPLE,
                amalgamation_level == 0)
waldo::compare(iberia_extra_repatriated_counts_3 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)
# 9822
##### Intermediate ----
embsecbio_taxa_amalgamation_stage2 <- embsecbio_taxa_amalgamation %>%
  dplyr::select(-taxon_name) %>%
  dplyr::distinct(clean_name, intermediate, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(intermediate))

iberia_extra_repatriated_counts_4 <-
  sort(unique(iberia_extra_repatriated_counts_3$ID_SAMPLE)) %>%
  purrr::map_df(function(i) {
    iberia_extra_repatriated_counts_3 %>%
      dplyr::filter(ID_SAMPLE == i) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "ID_TAXON") %>%
      dplyr::left_join(embsecbio_taxa_amalgamation_stage2,
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

iberia_extra_repatriated_counts_4 %>%
  dplyr::filter(is.na(ID_TAXON))

iberia_extra_repatriated_counts_4 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()
idx_stage2 <- idx_pairs(nrow(iberia_extra_repatriated_counts_4), 2000)
pb <- progress::progress_bar$new(total = nrow(idx_stage2))
meta_neo_res <-
  purrr::map2(idx_stage2$x,
              idx_stage2$y,
              ~ {
                pb$tick()
                iberia_extra_repatriated_counts_4[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% iberia_extra_repatriated_counts_4$ID_SAMPLE,
                amalgamation_level == 1)
waldo::compare(iberia_extra_repatriated_counts_4 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)
# 9688

##### Amalgamated ----
embsecbio_taxa_amalgamation_stage3 <- embsecbio_taxa_amalgamation_stage2 %>%
  dplyr::select(-clean_name, -dplyr::starts_with("action")) %>%
  dplyr::distinct(intermediate, amalgamated, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(amalgamated))

iberia_extra_repatriated_counts_5 <-
  unique(iberia_extra_repatriated_counts_4$ID_SAMPLE) %>%
  purrr::map_df(function(ID_SAMPLE) {
    iberia_extra_repatriated_counts_4 %>%
      dplyr::filter(ID_SAMPLE == !!ID_SAMPLE) %>%
      dplyr::left_join(taxon_name_tb,
                       by = "ID_TAXON") %>%
      dplyr::left_join(embsecbio_taxa_amalgamation_stage3,
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

iberia_extra_repatriated_counts_5 %>%
  dplyr::filter(is.na(ID_TAXON)|is.na(count))

iberia_extra_repatriated_counts_5 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()
idx_stage3 <- idx_pairs(nrow(iberia_extra_repatriated_counts_5), 2000)
pb <- progress::progress_bar$new(total = nrow(idx_stage3))
meta_neo_res <-
  purrr::map2(idx_stage3$x,
              idx_stage3$y,
              ~ {
                pb$tick()
                iberia_extra_repatriated_counts_5[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% iberia_extra_repatriated_counts_5$ID_SAMPLE,
                amalgamation_level == 2)
waldo::compare(iberia_extra_repatriated_counts_5 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)
# 9312

iberia_extra_repatriated_counts %>%
  dplyr::distinct(entity_name, .keep_all = TRUE)







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


# EPD exclusive ----
external_link_tb <- dabr::select_all(conn, "external_link") %>%
  dplyr::group_by(ID_ENTITY) %>%
  dplyr::mutate(n = dplyr::n())

records_from_rpd <- tibble::tibble(entity_name = rpd_repatriation$entity_name, db = "RPD")
records_from_emb <- tibble::tibble(entity_name = embsecbio_repatriation$entity_name, db = "EMBSeCBIO")
records_from_ibe <- tibble::tibble(entity_name = iberia_repatriation$entity_name, db = "IBERIA")
records_from_ib2 <- tibble::tibble(entity_name = iberia_extra_repatriation$entity_name, db = "IBERIA")

records_from_other_sources <- dplyr::bind_rows(
  records_from_rpd,
  records_from_emb,
  records_from_ibe,
  records_from_ib2
) %>%
  dplyr::left_join(dabr::select(conn,
                                "SELECT ID_ENTITY, entity_name",
                                "FROM entity"),
                   by = "entity_name") %>%
  dplyr::relocate(ID_ENTITY, .before = 1)

EPD_exclusive <- external_link_tb %>%
  dplyr::filter(n == 1)
# Check if any records have been already repatriated
EPD_exclusive_snap <- conn %>%
  special.epd::snapshot(ID_ENTITY = EPD_exclusive$ID_ENTITY)
with_dates <- EPD_exclusive_snap$date_info %>%
  dplyr::filter(ID_ENTITY %in% records_from_other_sources$ID_ENTITY) %>%
  dplyr::distinct(ID_ENTITY) %>%
  dplyr::arrange(ID_ENTITY) %>%
  purrr::flatten_dbl()
  # unique(EPD_exclusive_snap$date_info$ID_ENTITY) %>% sort()
with_samples <- EPD_exclusive_snap$sample %>%
  dplyr::filter(ID_ENTITY %in% records_from_other_sources$ID_ENTITY) %>%
  dplyr::distinct(ID_ENTITY) %>%
  dplyr::arrange(ID_ENTITY) %>%
  purrr::flatten_dbl()
  # unique(EPD_exclusive_snap$sample$ID_ENTITY) %>% sort()

entities_from_other_dbs <- external_link_tb %>%
  dplyr::filter(n > 1)
# EPD_exclusive %>%
#   dplyr::filter(!(ID_ENTITY %in% records_from_other_sources$ID_ENTITY)) %>%
#   readr::write_excel_csv("~/Downloads/epd.csv", na = "")

#### Dates ----
epd_repatriated_dates_info <- EPD_DATES %>%
  dplyr::filter(entity_name %in% EPD_exclusive$external_entity_name) %>%
  dplyr::left_join(dabr::select(conn,
                                "SELECT ID_ENTITY, entity_name",
                                "FROM entity"),
                   by = "entity_name") %>%
  # dplyr::left_join(non_rpd_repatriated_dates_info_3 %>%
  #                    dplyr::select(ID_ENTITY, site_id, entity_name)) %>%
  dplyr::select(-ages_already, -site_id, -site_name, -site_name_clean, -entity_name) %>%
  dplyr::relocate(ID_ENTITY, .before = 1) %>%
  dplyr::rename(age_calib = age_cal) %>%
  dplyr::filter(!(ID_ENTITY %in% records_from_other_sources$ID_ENTITY)) %>%
  dplyr::filter(!(ID_ENTITY %in% with_dates)) %>%
  dplyr::mutate(ID_DATE_INFO = 2830 + seq_along(ID_ENTITY), .after = 1) #get_id_date_info(conn)

if (nrow(epd_repatriated_dates_info) == 0)
  epd_repatriated_dates_info <- dabr::select(conn,
                                             "SELECT * FROM date_info WHERE ID_DATE_INFO BETWEEN 2831 AND 9938")
# epd_repatriated_dates_info <- conn %>%
#   dabr::select("SELECT * FROM date_info WHERE ID_DATE_INFO BETWEEN 2831 AND 9938")

# Check for existing dates
special.epd::snapshot(conn, ID_ENTITY = epd_repatriated_dates_info$ID_ENTITY)
meta_neo_res <- seq_len(nrow(epd_repatriated_dates_info)) %>%
  purrr::map(function(i) {
    epd_repatriated_dates_info[i, ] %>%
      rpd:::add_records(conn = conn, table = "date_info", dry_run = TRUE)
  })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_DATES_NEO_DB <- dabr::select_all(conn, "date_info") %>%
  dplyr::filter(ID_ENTITY %in% epd_repatriated_dates_info$ID_ENTITY)
waldo::compare(epd_repatriated_dates_info %>%
                 .[order(colnames(.))] %>%
                 dplyr::arrange(ID_ENTITY, depth),
               EPD_DATES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 # dplyr::select(-ID_DATE_INFO) %>%
                 dplyr::arrange(ID_ENTITY, depth),
               tolerance = 1E-9,
               max_diffs = Inf)
EPD_DATES_NEO_DB %>%
  dplyr::filter(ID_ENTITY %in% records_from_other_sources$ID_ENTITY)


#### Samples ----
epd_repatriated_samples <- EPD_COUNTS %>%
  dplyr::filter(entity_name %in% EPD_exclusive$external_entity_name) %>%
  # smpds::rm_na_taxa(1:16) %>%
  dplyr::select(-chronology_id) %>%
  dplyr::left_join(EPD_METADATA %>%
                     dplyr::select(1:3, 6, 10),
                   by = c("site_id", "dataset_id", "entity_name")) %>%
  dplyr::relocate(ID_SITE, ID_ENTITY, .before = 1) %>%
  dplyr::filter(!(ID_ENTITY %in% records_from_other_sources$ID_ENTITY)) %>%
  dplyr::filter(!(ID_ENTITY %in% with_samples))
if (nrow(epd_repatriated_samples) == 0)
  epd_repatriated_dates_info <- dabr::select(conn,
                                             "SELECT * FROM sample WHERE ID_SAMPLE BETWEEN 29059 AND 101315")

epd_repatriated_samples

epd_repatriated_samples_2 <- epd_repatriated_samples %>%
  dplyr::mutate(ID_SAMPLE = 29059 + seq_along(ID_ENTITY), # get_id_sample(conn)
                .after = ID_ENTITY) %>%
  dplyr::select(-ID_SITE, -site_id, -site_name, -site_name_clean, -dataset_id, -dataset_name, -entity_name, -sample_id, -unit_name)

# Check if the "new" records are already in th DB:
special.epd::snapshot(conn, ID_ENTITY = epd_repatriated_samples_2$ID_ENTITY)
dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_SAMPLE %in% epd_repatriated_samples_2$ID_SAMPLE |
                  ID_ENTITY %in% epd_repatriated_samples_2$ID_ENTITY)
meta_neo_res <- seq_len(nrow(epd_repatriated_samples_2)) %>%
  purrr::map(function(i) {
    epd_repatriated_samples_2[i, ] %>%
      dplyr::select(1:9) %>%
      rpd:::add_records(conn = conn, table = "sample", dry_run = TRUE)
  })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()
##### Validate -----
EPD_SAMPLES_NEO_DB <- dabr::select_all(conn, "sample") %>%
  dplyr::filter(ID_ENTITY %in% epd_repatriated_samples_2$ID_ENTITY)
waldo::compare(epd_repatriated_samples_2 %>%
                 dplyr::select(1:9) %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)),
               EPD_SAMPLES_NEO_DB %>%
                 .[order(colnames(.))] %>%
                 dplyr::mutate(depth = round(depth, 3),
                               age = round(age)) %>%
                 dplyr::select(-count_type, -sample_type),
               tolerance = 2)

#### (HERE) Age models ----
epd_repatriated_new_am <-
  find_age_models(path, EPD_exclusive$external_entity_name)
# Find duplicated entries
epd_repatriated_new_am %>%
  dplyr::group_by(entity_name) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::mutate(path = path %>%
                  stringr::str_remove_all("/Users/roberto.villegas-diaz/Downloads/special_epd_am/")) %>%
  dplyr::filter(n > 1) %>%
  dplyr::arrange(entity_name, subfolder) #%>%
  # dplyr::select(entity_name, subfolder, path) %>%
  # readr::write_excel_csv("~/Downloads/epd_new_age_models_duplicated.csv")
epd_repatriated_new_am %>%
  dplyr::filter(entity_name %in% epd_age_models_ready_to_upload$entity_name)
epd_repatriated_new_am2 <- epd_repatriated_new_am %>%
  dplyr::filter(entity_name %in% epd_age_models_ready_to_upload$entity_name) %>%
  purrr::pmap_df(upload_age_model, conn = conn)

all(epd_repatriated_new_am2$status)
# if(!all(epd_repatriated_new_am2$status)) {
#   epd_repatriated_new_am2_v2 <- epd_repatriated_new_am2 %>%
#     dplyr::filter(!status) %>%
#     dplyr::select(-status, -reason) %>%
#     purrr::pmap_df(upload_age_model2, conn = conn)
# }
#
# all(epd_repatriated_new_am2_v2$status)
# if(!all(epd_repatriated_new_am2_v2$status)) {
#   epd_repatriated_new_am2_v3 <- epd_repatriated_new_am %>%
#     dplyr::filter(entity_name %in%
#                     (epd_repatriated_new_am2_v2 %>%
#                        dplyr::filter(!status) %>%
#                        .$entity_name)) %>%
#     purrr::pmap_df(upload_age_model2, conn = conn)
# }
# all(epd_repatriated_new_am2_v3$status)
#
# epd_repatriated_new_am2 %>%
#   dplyr::filter(!status) %>%
#   .$am %>%
#   purrr::map_df(~.x)

epd_repatriated_new_am3 <-
  epd_repatriated_new_am2 %>%
  dplyr::filter(status) %>%
  # dplyr::bind_rows(epd_repatriated_new_am2_v2 %>% dplyr::filter(status)) %>%#,
                   # epd_repatriated_new_am2_v3 %>% dplyr::filter(status)) %>%
  .$am %>%
  purrr::map_df(~.x) %>%
  magrittr::set_names(colnames(.) %>%
                        stringr::str_to_upper() %>%
                        stringr::str_replace_all("MEAN", "mean") %>%
                        stringr::str_replace_all("MEDIAN", "median")) %>%
  dplyr::distinct(ID_SAMPLE, .keep_all = TRUE) %>%
  dplyr::arrange(ID_SAMPLE)

EPD_AM <- dabr::select(conn,
                       "SELECT * FROM age_model WHERE ID_SAMPLE IN (",
                       paste0(epd_repatriated_new_am3$ID_SAMPLE, collapse = ", "),
                       ") AND ID_MODEL = 8")
waldo::compare(epd_repatriated_new_am3,
               EPD_AM)

# a <- epd_repatriated_new_am3[-seq_len(40000),]
# b <- EPD_AM[-seq_len(40000),]
# a[378,]
# b[378,]
# waldo::compare(a[seq_len(1000),],
#                b[seq_len(1000),])

special.epd::snapshot(conn, entity_name = epd_repatriated_new_am$entity_name)

# NOTE: Pending age models
epd_repatriated_new_am %>%
  dplyr::filter(!(entity_name %in% epd_age_models_ready_to_upload$entity_name)) %>%
  dplyr::select(-am)

tibble::tribble(
  ~entity_name,                   ~subfolder,                                                                                                            ~path,
  "ZS-9",              "Coastal sites",                  "/Users/roberto.villegas-diaz/Downloads/special_epd_am/Coastal sites/ZS-9/bacon_chronology.csv",
  "TELAKKO",        "PL Coastal entities",         "/Users/roberto.villegas-diaz/Downloads/special_epd_am/PL Coastal entities/TELAKKO/bacon_chronology.csv",
  "VK12",        "PL Coastal entities",            "/Users/roberto.villegas-diaz/Downloads/special_epd_am/PL Coastal entities/VK12/bacon_chronology.csv",
  "BXBX", "YS Batch2 (IBERIA-missing)",     "/Users/roberto.villegas-diaz/Downloads/special_epd_am/YS Batch2 (IBERIA-missing)/BXBX/bacon_chronology.csv",
  "MD992348", "YS Batch2 (IBERIA-missing)", "/Users/roberto.villegas-diaz/Downloads/special_epd_am/YS Batch2 (IBERIA-missing)/MD992348/bacon_chronology.csv",
  "TG8", "YS Batch2 (IBERIA-missing)",      "/Users/roberto.villegas-diaz/Downloads/special_epd_am/YS Batch2 (IBERIA-missing)/TG8/bacon_chronology.csv"
)


#### (HERE) Counts ----
# Find samples with existing pollen records
# existing_pollen_counts <- conn %>%
#   dabr::select("SELECT * FROM pollen_count WHERE ID_SAMPLE IN (",
#                paste0(epd_repatriated_samples_2$ID_SAMPLE, collapse = ", "),
#                ")",
#                quiet = TRUE)

aux <- conn %>%
  special.epd::snapshot(ID_ENTITY = EPD_exclusive$ID_ENTITY)
  # special.epd::snapshot(ID_ENTITY = epd_repatriated_samples_2$ID_ENTITY)
existing_pollen_counts <-
  aux$pollen_count %>%
  purrr::map_df(~.x) %>% # Combine the three pollen count tables
  dplyr::select(ID_SAMPLE) %>%
  dplyr::group_by(ID_SAMPLE) %>%
  dplyr::summarise(n = dplyr::n())

existing_pollen_counts %>%
  dplyr::filter(n != 3) # Verify if sample does not have the 3 tables

epd_repatriated_samples_3 <- epd_repatriated_samples_2 %>%
  dplyr::select(-ID_ENTITY, -depth, -thickness, -chronology_name, -age_type, -age, -age_younger, -age_older)

epd_repatriated_samples_3 %>%
  dplyr::filter(ID_SAMPLE %in% existing_pollen_counts$ID_SAMPLE)
taxon_name_tb <- dabr::select_all(conn, "taxon_name")
data("EPD_taxa_amalgamation")
##### Clean ----
oplan <- future::plan(future::multisession, workers = 8)
options(future.globals.maxSize = 2000*1024^2)
epd_repatriated_samples_4 <-
  seq_len(nrow(epd_repatriated_samples_3)) %>%
  furrr::future_map_dfr(function(i) {
    epd_repatriated_samples_3[i, ] %>%
      tidyr::pivot_longer(-1, names_to = "epd_taxa", values_to = "count") %>%
      dplyr::filter(!is.na(count)) %>%
      dplyr::left_join(EPD_taxa_amalgamation,
                       by = "epd_taxa") %>%
      dplyr::left_join(taxon_name_tb,
                       by = c("clean_name" = "taxon_name")) %>%
      dplyr::group_by(ID_SAMPLE, ID_TAXON) %>%
      dplyr::mutate(count = sum(count, na.rm = TRUE),
                    n = length(count)) %>%
      dplyr::distinct(ID_SAMPLE, ID_TAXON, .keep_all = TRUE) %>%
      dplyr::select(ID_SAMPLE, ID_TAXON, count) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(amalgamation_level = 0, .before = count) # Clean names only
  }) %>%
  dplyr::arrange(ID_SAMPLE, ID_TAXON)
future::plan(oplan)

epd_repatriated_samples_4 %>%
  dplyr::filter(is.na(ID_TAXON))
dim(epd_repatriated_samples_4)

# IDS <- unique(epd_repatriated_samples_4$ID_SAMPLE) %>% sort()
# idx <- idx_pairs(length(IDS), 1000)
# purrr::map2(idx$x, idx$y, ~dabr::delete(conn,
#                                         "DELETE FROM pollen_count WHERE ID_SAMPLE IN (",
#                                         paste0(IDS[.x:.y], collapse = ","),
#                                         ")"))
aux <- special.epd::snapshot(conn,
                             ID_ENTITY = epd_repatriated_samples_2$ID_ENTITY)
epd_repatriated_samples_4 %>%
  dplyr::filter(ID_SAMPLE %in% aux$pollen_count$clean$ID_SAMPLE)
# dabr::delete(conn,
#              "DELETE FROM pollen_count WHERE ID_SAMPLE IN (",
#              paste0(unique(aux$pollen_count$clean$ID_SAMPLE), collapse = ","),
#              ")")


epd_repatriated_samples_4 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()
idx <- idx_pairs(nrow(epd_repatriated_samples_4), 1000)
pb <- progress::progress_bar$new(total = nrow(idx))
# meta_neo_res <- seq_len(nrow(epd_repatriated_samples_4)) %>%
meta_neo_res <-
  purrr::map2(idx$x,
              idx$y,
              ~ {
                pb$tick()
                epd_repatriated_samples_4[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% epd_repatriated_samples_4$ID_SAMPLE,
                amalgamation_level == 0)
waldo::compare(epd_repatriated_samples_4 %>%
                 # dplyr::arrange(ID_SAMPLE, ID_TAXON) %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)
# 5655448
##### Intermediate ----
EPD_taxa_amalgamation_stage2 <- EPD_taxa_amalgamation %>%
  dplyr::select(-epd_taxa) %>%
  dplyr::distinct(clean_name, intermediate, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(intermediate))

# EPD_taxa_amalgamation_stage2 %>%
# dplyr::filter(intermediate != intermediate2)
oplan <- future::plan(future::multisession, workers = 8)
epd_repatriated_samples_5 <-
  unique(epd_repatriated_samples_4$ID_SAMPLE) %>%
  furrr::future_map_dfr(function(ID_SAMPLE) {
    epd_repatriated_samples_4 %>%
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
future::plan(oplan)

epd_repatriated_samples_5 %>%
  dplyr::filter(is.na(ID_TAXON))

epd_repatriated_samples_5 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()
idx_stage2 <- idx_pairs(nrow(epd_repatriated_samples_5), 2000)
pb <- progress::progress_bar$new(total = nrow(idx_stage2))
meta_neo_res <-
  purrr::map2(idx_stage2$x,
              idx_stage2$y,
              ~ {
                pb$tick()
                epd_repatriated_samples_5[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })

# meta_neo_res <- seq_len(nrow(epd_repatriated_samples_5)) %>%
#   purrr::map(function(i) {
#     if (i %% 10000 == 0)
#       print(i)
#     epd_repatriated_samples_5[i, ] %>%
#       rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
#   })

meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% epd_repatriated_samples_5$ID_SAMPLE,
                amalgamation_level == 1)
waldo::compare(epd_repatriated_samples_5 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)

##### Amalgamated ----
EPD_taxa_amalgamation_stage3 <- EPD_taxa_amalgamation_stage2 %>%
  dplyr::select(-clean_name, -dplyr::starts_with("action")) %>%
  dplyr::distinct(intermediate, amalgamated, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(amalgamated))

# EPD_taxa_amalgamation_stage3 %>%
#   dplyr::filter(amalgamated != amalgamated2)
oplan <- future::plan(future::multisession, workers = 8)
epd_repatriated_samples_6 <-
  unique(epd_repatriated_samples_5$ID_SAMPLE) %>%
  furrr::future_map_dfr(function(ID_SAMPLE) {
    epd_repatriated_samples_5 %>%
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
future::plan(oplan)

epd_repatriated_samples_6 %>%
  dplyr::filter(is.na(ID_TAXON)|is.na(count))

epd_repatriated_samples_6 %>%
  dplyr::filter(is.na(ID_TAXON)) %>% .$count %>% sum()
idx_stage3 <- idx_pairs(nrow(epd_repatriated_samples_6), 2000)
pb <- progress::progress_bar$new(total = nrow(idx_stage3))
meta_neo_res <-
  purrr::map2(idx_stage3$x,
              idx_stage3$y,
              ~ {
                pb$tick()
                epd_repatriated_samples_6[.x:.y, ] %>%
                  rpd:::add_records(conn = conn, table = "pollen_count", dry_run = TRUE, quiet = TRUE)
              })
meta_neo_res %>% purrr::flatten_lgl() %>% sum()

###### Validate -----
EPD_TAXA <- dabr::select_all(conn, "pollen_count") %>%
  dplyr::filter(ID_SAMPLE %in% epd_repatriated_samples_6$ID_SAMPLE,
                amalgamation_level == 2)
waldo::compare(epd_repatriated_samples_6 %>%
                 .[order(colnames(.))],
               EPD_TAXA %>%
                 .[order(colnames(.))],
               tolerance = 1e-9)

aux <- special.epd::snapshot(conn,
                             ID_ENTITY = epd_repatriated_samples_2$ID_ENTITY)
aux$entity %>%
  dplyr::filter(ID_ENTITY %in% aux$sample$ID_ENTITY) %>%
  .$ID_ENTITY %>% dput
non_embsecbio_repatriated_am_info %>%
  dplyr::filter(ID_ENTITY %in% epd_repatriated_samples_2$ID_ENTITY)
aux2

entities_with_pollen_counts <-
  c(1L, 3L, 4L, 6L, 7L, 8L, 9L, 10L, 12L, 13L, 14L, 15L, 16L, 17L,
    19L, 20L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 32L, 33L,
    35L, 36L, 37L, 38L, 40L, 41L, 43L, 44L, 45L, 46L, 47L, 48L, 50L,
    52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 68L,
    69L, 70L, 71L, 72L, 73L, 74L, 75L, 77L, 81L, 85L, 86L, 87L, 88L,
    91L, 92L, 95L, 96L, 97L, 98L, 99L, 101L, 103L, 104L, 105L, 106L,
    107L, 108L, 109L, 110L, 111L, 112L, 113L, 114L, 115L, 116L, 117L,
    118L, 119L, 120L, 121L, 122L, 123L, 124L, 125L, 126L, 129L, 130L,
    131L, 133L, 134L, 135L, 136L, 137L, 138L, 139L, 142L, 144L, 145L,
    146L, 147L, 148L, 149L, 150L, 152L, 153L, 154L, 155L, 156L, 158L,
    160L, 163L, 164L, 165L, 166L, 167L, 168L, 169L, 170L, 171L, 172L,
    175L, 179L, 180L, 181L, 182L, 183L, 184L, 185L, 186L, 187L, 188L,
    189L, 190L, 191L, 193L, 194L, 195L, 196L, 197L, 198L, 199L, 200L,
    201L, 204L, 205L, 207L, 208L, 209L, 210L, 211L, 212L, 213L, 214L,
    215L, 216L, 220L, 222L, 225L, 227L, 228L, 229L, 230L, 231L, 232L,
    233L, 236L, 238L, 239L, 243L, 244L, 245L, 246L, 247L, 248L, 249L,
    250L, 251L, 252L, 253L, 254L, 255L, 256L, 257L, 259L, 260L, 261L,
    262L, 263L, 264L, 267L, 268L, 269L, 270L, 271L, 272L, 273L, 275L,
    276L, 277L, 278L, 279L, 280L, 281L, 282L, 283L, 284L, 285L, 286L,
    287L, 288L, 289L, 290L, 291L, 292L, 293L, 294L, 295L, 296L, 297L,
    298L, 299L, 300L, 301L, 302L, 303L, 304L, 305L, 306L, 307L, 308L,
    309L, 310L, 311L, 312L, 313L, 315L, 316L, 317L, 318L, 319L, 320L,
    321L, 322L, 325L, 326L, 327L, 328L, 329L, 333L, 334L, 335L, 336L,
    337L, 338L, 339L, 341L, 342L, 345L, 346L, 347L, 348L, 350L, 351L,
    352L, 353L, 354L, 355L, 356L, 357L, 358L, 359L, 360L, 361L, 362L,
    363L, 364L, 366L, 367L, 368L, 369L, 370L, 371L, 372L, 374L, 375L,
    376L, 377L, 378L, 382L, 386L, 388L, 389L, 390L, 391L, 392L, 393L,
    394L, 395L, 396L, 397L, 399L, 402L, 403L, 404L, 407L, 408L, 409L,
    410L, 411L, 412L, 413L, 414L, 415L, 416L, 417L, 418L, 419L, 420L,
    421L, 422L, 423L, 424L, 425L, 426L, 428L, 429L, 430L, 431L, 433L,
    434L, 435L, 436L, 437L, 438L, 439L, 441L, 442L, 443L, 444L, 445L,
    446L, 447L, 448L, 449L, 451L, 452L, 453L, 455L, 456L, 457L, 458L,
    459L, 460L, 461L, 462L, 463L, 465L, 466L, 467L, 468L, 470L, 471L,
    472L, 473L, 474L, 475L, 477L, 479L, 480L, 482L, 483L, 484L, 485L,
    486L, 487L, 488L, 489L, 491L, 492L, 493L, 497L, 498L, 500L, 504L,
    505L, 506L, 507L, 508L, 511L, 512L, 513L, 514L, 515L, 516L, 517L,
    519L, 520L, 521L, 522L, 523L, 524L, 525L, 526L, 527L, 528L, 529L,
    530L, 533L, 534L, 535L, 536L, 537L, 538L, 539L, 540L, 542L, 543L,
    544L, 545L, 546L, 547L, 548L, 550L, 551L, 552L, 553L, 554L, 556L,
    557L, 558L, 559L, 560L, 561L, 564L, 565L, 566L, 568L, 573L, 574L,
    575L, 576L, 577L, 578L, 579L, 580L, 581L, 584L, 586L, 587L, 588L,
    590L, 591L, 592L, 593L, 594L, 595L, 596L, 597L, 598L, 600L, 601L,
    602L, 603L, 604L, 605L, 606L, 607L, 608L, 609L, 610L, 611L, 612L,
    613L, 614L, 615L, 616L, 617L, 619L, 620L, 622L, 624L, 625L, 626L,
    630L, 631L, 632L, 633L, 634L, 635L, 637L, 638L, 639L, 641L, 642L,
    643L, 645L, 646L, 647L, 648L, 649L, 650L, 651L, 654L, 655L, 656L,
    657L, 658L, 659L, 660L, 661L, 662L, 663L, 664L, 666L, 670L, 671L,
    672L, 673L, 674L, 675L, 676L, 677L, 679L, 680L, 681L, 682L, 683L,
    684L, 685L, 686L, 687L, 688L, 689L, 691L, 692L, 693L, 694L, 695L,
    696L, 697L, 698L, 699L, 701L, 703L, 704L, 705L, 706L, 707L, 708L,
    709L, 710L, 711L, 712L, 713L, 714L, 715L, 716L, 717L, 718L, 719L,
    720L, 721L, 722L, 723L, 724L, 725L, 726L, 728L, 729L, 730L, 731L,
    732L, 733L, 734L, 735L, 736L, 737L, 738L, 739L, 742L, 744L, 745L,
    746L, 747L, 748L, 749L, 750L, 752L, 753L, 757L, 763L, 764L, 767L,
    768L, 769L, 770L, 771L, 777L, 781L, 782L, 783L, 785L, 786L, 787L,
    788L, 789L, 792L, 793L, 794L, 795L, 796L, 797L, 798L, 799L, 800L,
    801L, 803L, 804L, 806L, 807L, 808L, 809L, 811L, 812L, 813L, 814L,
    815L, 816L, 817L, 818L, 822L, 824L, 825L, 829L, 830L, 834L, 836L,
    837L, 839L, 841L, 843L, 846L, 852L, 854L, 855L, 856L, 857L, 858L,
    859L, 860L, 862L, 865L, 866L, 867L, 868L, 869L, 870L, 871L, 872L,
    873L, 874L, 875L, 876L, 877L, 878L, 879L, 880L, 881L, 882L, 883L,
    884L, 885L, 886L, 887L, 888L, 891L, 892L, 893L, 894L, 895L, 896L,
    897L, 898L, 899L, 900L, 902L, 903L, 905L, 906L, 907L, 908L, 909L,
    911L, 913L, 914L, 915L, 916L, 917L, 918L, 919L, 920L, 921L, 922L,
    924L, 927L, 928L, 930L, 931L, 932L, 933L, 934L, 936L, 938L, 939L,
    940L, 942L, 943L, 944L, 945L, 946L, 948L, 949L, 950L, 951L, 952L,
    953L, 954L, 955L, 956L, 957L, 958L, 959L, 960L, 961L, 963L, 964L,
    965L, 966L, 967L, 968L, 969L, 970L, 971L, 972L, 973L, 974L, 977L,
    979L, 980L, 981L, 982L, 983L, 984L, 985L, 986L, 987L, 989L, 990L,
    991L, 992L, 995L, 996L, 997L, 998L, 999L, 1000L, 1001L, 1002L,
    1003L, 1004L, 1005L, 1006L, 1007L, 1008L, 1009L, 1010L, 1011L,
    1012L, 1013L, 1014L, 1015L, 1017L, 1018L, 1020L, 1021L, 1022L,
    1023L, 1024L, 1025L, 1026L, 1027L, 1028L, 1029L, 1030L, 1031L,
    1032L, 1033L, 1034L, 1035L, 1039L, 1040L, 1041L, 1042L, 1043L,
    1044L, 1045L, 1046L, 1047L, 1048L, 1049L, 1050L, 1051L, 1053L,
    1054L, 1055L, 1056L, 1057L, 1058L, 1059L, 1060L, 1061L, 1062L,
    1065L, 1066L, 1067L, 1068L, 1069L, 1070L, 1072L, 1073L, 1074L,
    1075L, 1076L, 1077L, 1080L, 1081L, 1082L, 1083L, 1084L, 1085L,
    1087L, 1090L, 1092L, 1094L, 1097L, 1099L, 1100L, 1101L, 1102L,
    1103L, 1104L, 1105L, 1106L, 1109L, 1110L, 1111L, 1112L, 1113L,
    1114L, 1116L, 1118L, 1119L, 1120L, 1121L, 1122L, 1123L, 1124L,
    1125L, 1126L, 1127L, 1128L, 1129L, 1130L, 1131L, 1133L, 1134L,
    1135L, 1137L, 1138L, 1139L, 1140L, 1141L, 1142L, 1144L, 1146L,
    1147L, 1148L, 1151L, 1156L, 1160L, 1161L, 1162L, 1163L, 1164L,
    1165L, 1167L, 1168L, 1169L, 1170L, 1171L, 1173L, 1174L, 1175L,
    1178L, 1179L, 1180L, 1181L, 1182L, 1183L, 1184L, 1185L, 1186L,
    1187L, 1189L, 1190L, 1191L, 1192L, 1194L, 1195L, 1196L, 1200L,
    1202L, 1204L, 1205L, 1207L, 1212L, 1213L, 1214L, 1215L, 1216L,
    1220L, 1221L, 1222L, 1223L, 1224L, 1225L, 1226L, 1227L, 1229L,
    1230L, 1231L, 1233L, 1234L, 1235L, 1236L, 1238L, 1239L, 1241L,
    1242L, 1243L, 1244L, 1245L, 1246L, 1247L, 1249L, 1250L, 1251L,
    1252L, 1253L, 1254L, 1255L, 1256L, 1257L, 1258L, 1259L, 1260L,
    1261L, 1262L, 1263L, 1264L, 1265L, 1266L, 1267L, 1268L, 1269L,
    1270L, 1271L, 1273L, 1274L, 1275L, 1277L, 1278L, 1279L, 1280L,
    1281L, 1282L, 1283L, 1284L, 1285L, 1286L, 1287L, 1288L, 1289L,
    1290L, 1291L, 1292L, 1293L, 1298L, 1299L, 1300L, 1301L, 1302L,
    1303L, 1304L, 1305L, 1306L, 1307L, 1308L, 1309L, 1310L, 1311L,
    1312L, 1313L, 1314L, 1315L, 1316L, 1317L, 1318L, 1319L, 1320L,
    1321L, 1323L, 1325L, 1326L, 1329L, 1330L, 1331L, 1332L, 1334L,
    1335L, 1338L, 1339L, 1340L, 1342L, 1343L, 1344L, 1345L, 1346L,
    1347L, 1348L, 1349L, 1350L, 1351L, 1352L, 1353L, 1354L, 1355L,
    1357L, 1358L, 1359L, 1360L, 1361L, 1362L, 1363L, 1364L, 1365L,
    1366L, 1367L, 1368L, 1370L, 1371L, 1372L, 1373L, 1375L, 1377L,
    1381L, 1382L, 1383L, 1384L, 1385L, 1386L, 1387L, 1388L, 1389L,
    1391L, 1392L, 1394L, 1397L, 1398L, 1399L, 1401L, 1402L, 1403L,
    1404L, 1405L, 1406L, 1407L, 1408L, 1409L, 1410L, 1411L, 1413L,
    1414L, 1415L, 1416L, 1417L, 1418L, 1419L, 1420L, 1421L, 1422L,
    1423L, 1424L, 1425L, 1426L, 1427L, 1428L, 1429L, 1430L, 1431L,
    1433L, 1434L, 1435L, 1437L, 1438L, 1439L, 1440L, 1441L, 1444L,
    1445L, 1446L, 1447L, 1449L, 1450L, 1452L, 1453L, 1454L, 1455L,
    1456L, 1457L, 1458L, 1459L, 1460L, 1461L, 1462L, 1463L, 1464L,
    1465L, 1466L, 1467L, 1468L, 1469L, 1471L, 1472L, 1474L, 1475L,
    1476L, 1478L, 1479L, 1480L, 1481L, 1482L, 1483L, 1486L, 1488L,
    1490L, 1491L, 1492L, 1493L, 1494L, 1495L, 1496L, 1497L, 1498L,
    1499L, 1500L, 1501L, 1502L, 1503L, 1504L, 1505L, 1508L, 1509L,
    1510L, 1511L, 1512L, 1513L, 1514L, 1515L, 1516L, 1517L, 1519L,
    1520L, 1521L, 1522L, 1523L, 1524L, 1525L, 1526L, 1528L, 1529L,
    1531L, 1533L, 1535L, 1536L, 1537L, 1538L, 1540L, 1541L, 1542L,
    1543L, 1544L, 1545L, 1546L, 1547L, 1548L, 1551L, 1552L, 1553L,
    1554L, 1555L, 1556L, 1557L, 1558L, 1559L, 1560L, 1561L, 1562L,
    1563L, 1564L, 1565L, 1566L, 1567L, 1568L, 1569L, 1570L, 1571L,
    1572L, 1573L, 1574L, 1575L, 1577L, 1580L, 1581L, 1582L, 1583L,
    1584L, 1585L, 1586L, 1587L, 1588L, 1589L, 1590L, 1591L, 1592L,
    1594L, 1597L, 1599L, 1600L, 1602L, 1604L, 1605L, 1606L, 1607L,
    1608L, 1609L, 1611L, 1612L, 1613L, 1614L, 1615L, 1616L, 1617L,
    1618L)
