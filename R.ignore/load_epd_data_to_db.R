# Open connection ----
conn <- dabr::open_conn_mysql("SPECIAL-EPD",
                              password = rstudioapi::askForPassword())

# Metadata ----
meta_res <- seq_len(nrow(EPD_METADATA)) %>%
  purrr::map(function(i) {
    EPD_METADATA[i, ] %>%
      dplyr::select(ID_SITE,
                    ID_ENTITY,
                    site_name = site_name_clean,
                    entity_name,
                    latitude,
                    longitude,
                    elevation,
                    source,
                    publication,
                    doi,
                    site_type) %>%
      # rpd:::update_records(conn = conn, table = "entity", PK = 1:2)
      rpd:::add_records(conn = conn, table = "entity")
  })
meta_res %>% purrr::flatten_lgl() %>% all()

## Validate metadata ----
EPD_METADATA_DB <- dabr::select_all(conn, "entity") %>%
  dplyr::select(ID_SITE,
                ID_ENTITY,
                site_name,
                entity_name,
                latitude,
                longitude,
                elevation,
                source,
                publication,
                doi,
                site_type)
waldo::compare(EPD_METADATA %>%
                 dplyr::select(ID_SITE,
                               ID_ENTITY,
                               site_name = site_name_clean,
                               entity_name,
                               latitude,
                               longitude,
                               elevation,
                               source,
                               publication,
                               doi,
                               site_type),
               EPD_METADATA_DB)

## Neotoma ----
meta_neo_res <- seq_len(nrow(EPD_METADATA)) %>%
  purrr::map(function(i) {
    EPD_METADATA[i, ] %>%
      dplyr::select(ID_SITE,
                    ID_ENTITY,
                    external_ID_SITE = site_id,
                    external_ID_ENTITY = dataset_id,
                    external_site_name = site_name,
                    external_entity_name = entity_name) %>%
      dplyr::mutate(external_source = "NEOTOMA") %>%
      rpd:::add_records(conn = conn, table = "external_link")
  })
meta_neo_res %>% purrr::flatten_lgl() %>% all()

### Validate Neotoma metadata ----
EPD_METADATA_NEO_DB <- dabr::select_all(conn, "neotoma") %>%
  dplyr::select(ID_SITE,
                ID_ENTITY,
                neotoma_ID_SITE,
                neotoma_ID_ENTITY,
                neotoma_site_name,
                neotoma_entity_name)
waldo::compare(EPD_METADATA %>%
                 dplyr::select(ID_SITE,
                               ID_ENTITY,
                               neotoma_ID_SITE = site_id,
                               neotoma_ID_ENTITY = dataset_id,
                               neotoma_site_name = site_name,
                               neotoma_entity_name = entity_name),
               EPD_METADATA_NEO_DB)

# Dates ----
EPD_METADATA
EPD_DATES %>%
  dplyr::select()
