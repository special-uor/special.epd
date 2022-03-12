## code to prepare a snapshot of the database
`%>%` <- magrittr::`%>%`
conn <- dabr::open_conn_mysql("special-epd", 
                              password = rstudioapi::askForPassword())

age_model <- dabr::select_all(conn, "age_model") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("age_model", class(.)))
usethis::use_data(age_model, overwrite = TRUE, compress = "xz")

date_info <- dabr::select_all(conn, "date_info") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("date_info", class(.)))
usethis::use_data(date_info, overwrite = TRUE, compress = "xz")

entity <- dabr::select_all(conn, "entity") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("entity", class(.)))
usethis::use_data(entity, overwrite = TRUE, compress = "xz")

external_link <- conn %>%
  dabr::select_all("external_link") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("external_link", class(.)))
usethis::use_data(external_link, overwrite = TRUE, compress = "xz")

model_name <- conn %>%
  dabr::select_all("model_name") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("model_name", class(.)))
usethis::use_data(model_name, overwrite = TRUE, compress = "xz")

pollen_count <- conn %>%
  dabr::select_all("pollen_count") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("pollen_count", class(.)))
usethis::use_data(pollen_count, overwrite = TRUE, compress = "xz")

sample <- conn %>%
  dabr::select_all("sample") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("sample", class(.))) %>%
  dplyr::select(-dplyr::starts_with("depth_"))
usethis::use_data(sample, overwrite = TRUE, compress = "xz")

taxon_name <- conn %>%
  dabr::select_all("taxon_name") %>%
  tibble::as_tibble() %>%
  magrittr::set_class(c("taxon_name", class(.)))
usethis::use_data(taxon_name, overwrite = TRUE, compress = "xz")

# Close database connection
dabr::close_conn(conn)