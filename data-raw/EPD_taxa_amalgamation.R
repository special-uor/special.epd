## code to prepare `EPD_taxa_amalgamation` dataset goes here
`%>%` <- magrittr::`%>%`
EPD_taxa_amalgamation <-
  readxl::read_excel("inst/extdata/epd_taxa_list_2022-02-26_SPH.xlsx",
                     skip = 1) %>%
  janitor::clean_names() %>%
  dplyr::mutate(epd_taxa = epd_taxa %>%
                  stringr::str_replace_all(" type", "-type"),
                clean_name = clean_name %>%
                  stringr::str_squish())
usethis::use_data(EPD_taxa_amalgamation, overwrite = TRUE)
