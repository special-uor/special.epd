## code to prepare `IBERIA_charcoal` dataset goes here
IBERIA_charcoal <- "data-raw/Iberia_charcoal_records_v2.csv" %>%
  readr::read_csv() %>%
  dplyr::rename(publication = reference)
usethis::use_data(IBERIA_charcoal, overwrite = TRUE, compress = "xz")
