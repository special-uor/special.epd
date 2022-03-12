# # Open connection ----
# conn <- dabr::open_conn_mysql("EMBSEcBIO",
#                               password = rstudioapi::askForPassword())
#
# ## Pull all the tables ----
# ### Age model ----
# embsecbio_age_model <- conn %>%
#   dabr::select_all("age_model") %>%
#   magrittr::set_class(c("age_model", class(.)))
#
# ### Date info ----
# embsecbio_date_type <- conn %>%
#   dabr::select_all("date_type") %>%
#   magrittr::set_class(c("date_type", class(.)))
#
# embsecbio_mat_dated <- conn %>%
#   dabr::select_all("mat_dated") %>%
#   magrittr::set_class(c("mat_dated", class(.)))
#
# embsecbio_date_info <- conn %>%
#   dabr::select_all("date_info") %>%
#   magrittr::set_class(c("date_info", class(.))) %>%
#   dplyr::left_join(embsecbio_date_type, by = "ID_DATE_TYPE") %>%
#   dplyr::left_join(embsecbio_mat_dated, by = "ID_MAT_DATED") %>%
#   dplyr::select(-ID_DATE_TYPE, -ID_MAT_DATED, -ID_MAT_DATED_HIGH)
#
# ### Entity ----
# embsecbio_entity_type <- conn %>%
#   dabr::select_all("entity_type") %>%
#   magrittr::set_class(c("entity_type", class(.)))
#
# embsecbio_entity <- conn %>%
#   dabr::select_all("entity") %>%
#   magrittr::set_class(c("entity", class(.))) %>%
#   dplyr::left_join(embsecbio_entity_type, by = "ID_ENTITY_TYPE") %>%
#   dplyr::select(-ID_ENTITY_TYPE)
#
# ### Entity link publication ----
# embsecbio_entity_pub <- conn %>%
#   dabr::select_all("entity_pub") %>%
#   magrittr::set_class(c("entity_pub", class(.)))
#
# ### Publication ----
# embsecbio_pub <- conn %>%
#   dabr::select_all("pub") %>%
#   magrittr::set_class(c("pub", class(.)))
#
# ### Pollen data ---
# embsecbio_pollen_data <- conn %>%
#   dabr::select_all("pollen_data") %>%
#   magrittr::set_class(c("pollen_data", class(.)))
#
# ### Sample ----
# embsecbio_sample_type <- conn %>%
#   dabr::select_all("sample_type") %>%
#   magrittr::set_class(c("sample_type", class(.)))
#
# embsecbio_sample <- conn %>%
#   dabr::select_all("sample") %>%
#   magrittr::set_class(c("sample", class(.))) %>%
#   dplyr::left_join(embsecbio_sample_type, by = "ID_SAMPLE_TYPE") %>%
#   dplyr::select(-ID_SAMPLE_TYPE)
#
# ### Site ----
# embsecbio_basin_size <- conn %>%
#   dabr::select_all("basin_size") %>%
#   magrittr::set_class(c("basin_size", class(.)))
#
# embsecbio_catch_size <- conn %>%
#   dabr::select_all("catch_size") %>%
#   magrittr::set_class(c("catch_size", class(.)))
#
# embsecbio_site_type <- conn %>%
#   dabr::select_all("site_type") %>%
#   magrittr::set_class(c("site_type", class(.)))
#
# embsecbio_site <- conn %>%
#   dabr::select_all("site") %>%
#   magrittr::set_class(c("site", class(.))) %>%
#   dplyr::left_join(embsecbio_site_type, by = "ID_SITE_TYPE") %>%
#   dplyr::left_join(embsecbio_basin_size, by = "ID_BASIN_SIZE") %>%
#   dplyr::left_join(embsecbio_catch_size, by = "ID_CATCH_SIZE") %>%
#   dplyr::select(-ID_SITE_TYPE, -ID_HIGHER, -ID_BASIN_SIZE, -ID_CATCH_SIZE) %>%
#   dplyr::rename(site_type = desc_site_type,
#                 basin_size = basin_desc)
#
#
# # Comparisons ----
# waldo::compare(embsecbio::site, embsecbio_site)
