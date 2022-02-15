`%>%` <- magrittr::`%>%`
# Load comparison files
comparisons_path <- "~/Downloads/SPECIAL-EPD/comparisons"

## EMBSeCBIO ----
### All the sites
embsecbio_all <-
  file.path(comparisons_path,
            "epd_embsecbio_2022-02-04.xlsx") %>%
  readxl::read_excel(sheet = 1, skip = 1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(EPD = dplyr::case_when(!is.na(ID_ENTITY) ~ FALSE,
                                       TRUE ~ TRUE),
                .before = 1) %>%
  dplyr::ungroup()
### Sites without matching record on the EMBSeCBIO (manually inspected)
embsecbio_missing <-
  file.path(comparisons_path,
            "epd_embsecbio_2022-02-04.xlsx") %>%
  readxl::read_excel(sheet = 2, skip = 1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(EPD = dplyr::case_when(!is.na(ID_ENTITY) ~ FALSE,
                                       TRUE ~ TRUE),
                .before = 1) %>%
  dplyr::ungroup()
### Sites with (potential) matching record on the EMBSeCBIO
embsecbio_matched <-
  file.path(comparisons_path,
            "epd_embsecbio_2022-02-04.xlsx") %>%
  readxl::read_excel(sheet = 3, skip = 1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(EPD = dplyr::case_when(!is.na(ID_ENTITY) ~ FALSE,
                                       TRUE ~ TRUE),
                .before = 1) %>%
  dplyr::ungroup()

## Iberia ----
### All the sites
iberia_all <-
  file.path(comparisons_path,
            "epd_iberia_pollen_2022-02-04.xlsx") %>%
  readxl::read_excel(sheet = 1, skip = 1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(EPD = dplyr::case_when(!is.na(site_name) ~ FALSE,
                                       TRUE ~ TRUE),
                .before = 1) %>%
  dplyr::ungroup()
### Sites without matching record on the Iberian subset (manually inspected)
iberia_missing <-
  file.path(comparisons_path,
            "epd_iberia_pollen_2022-02-04.xlsx") %>%
  readxl::read_excel(sheet = 2, skip = 1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(EPD = dplyr::case_when(!is.na(site_name) ~ FALSE,
                                       TRUE ~ TRUE),
                .before = 1) %>%
  dplyr::ungroup()
### Sites with (potential) matching record on the Iberian subset
iberia_matched <-
  file.path(comparisons_path,
            "epd_iberia_pollen_2022-02-04.xlsx") %>%
  readxl::read_excel(sheet = 3, skip = 1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(EPD = dplyr::case_when(!is.na(site_name) ~ FALSE,
                                       TRUE ~ TRUE),
                .before = 1) %>%
  dplyr::ungroup()


## RPD ----
### All the sites
rpd_all <-
  file.path(comparisons_path,
            "epd_rpd_2022-02-07.xlsx") %>%
  readxl::read_excel(sheet = 1, skip = 1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(EPD = dplyr::case_when(!is.na(ID_ENTITY) ~ FALSE,
                                       TRUE ~ TRUE),
                .before = 1) %>%
  dplyr::ungroup()
### Sites without matching record on the RPD (manually inspected)
rpd_missing <-
  file.path(comparisons_path,
            "epd_rpd_2022-02-07.xlsx") %>%
  readxl::read_excel(sheet = 2, skip = 1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(EPD = dplyr::case_when(!is.na(ID_ENTITY) ~ FALSE,
                                       TRUE ~ TRUE),
                .before = 1) %>%
  dplyr::ungroup()
### Sites with (potential) matching record on the RPD
rpd_matched <-
  file.path(comparisons_path,
            "epd_rpd_2022-02-07.xlsx") %>%
  readxl::read_excel(sheet = 3, skip = 1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(EPD = dplyr::case_when(!is.na(ID_ENTITY) ~ FALSE,
                                       TRUE ~ TRUE),
                .before = 1) %>%
  dplyr::ungroup()
