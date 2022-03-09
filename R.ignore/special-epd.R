`%>%` <- magrittr::`%>%`
WDIR <- "~/Downloads/ageR-training/tests"
data_raw_path <- "~/Downloads/SPECIAL-EPD/raw data"

# Download the latest version of ageR ----
remotes::install_github("special-uor/ageR", "dev")

# Load raw data ----
EPD_COUNTS <-
  file.path(data_raw_path,
            "epd-records-extracted-from-neotoma_counts_2022-02-08.csv") %>%
  readr::read_csv() %>%
  dplyr::rename(entity.name = handle) %>%
  janitor::clean_names()

EPD_DATES <-
  file.path(data_raw_path,
            "epd-records-extracted-from-neotoma_dates_2022-02-08_SPH.xlsx") %>%
  readxl::read_excel() %>%
  janitor::clean_names()

EPD_DATES_coretops <-
  file.path(data_raw_path,
            "epd-records-extracted-from-neotoma_dates_2022-02-08_SPH.xlsx") %>%
  readxl::read_excel(sheet = 2) %>%
  janitor::clean_names() %>%
  dplyr::filter(!is.na(depth_cm))

EPD_METADATA <-
  file.path(data_raw_path,
            "epd-records-extracted-from-neotoma_metadata_2022-02-08.csv") %>%
  readr::read_csv() %>%
  janitor::clean_names()

# Subset ----
## Extract only a subset of the EPD sites, 60-70 deg N and 0-10 deg E
epd_subset <- EPD_METADATA %>%
  # dplyr::filter(entity_name %in% c("DUNEJOLD"))
  dplyr::filter(dplyr::between(latitude, 60, 70),
                dplyr::between(longitude, 0, 10))

## Extract the counts and dates linked to the subset area
epd_subset_counts <- EPD_COUNTS %>%
  dplyr::filter(entity_name %in% epd_subset$entity_name) %>%
  dplyr::select(1:15)
epd_subset_dates <- EPD_DATES %>%
  dplyr::filter(entity_name %in% epd_subset$entity_name)
epd_subset_dates_coretops <- EPD_DATES_coretops %>%
  dplyr::filter(entity_name %in% epd_subset$entity_name)

# Create vectors of entities----
entities <- unique(epd_subset$entity_name)

# Extract hiatuses----
epd_subset_dates_hiatus <- epd_subset_dates %>%
  dplyr::filter(date_type %>% stringr::str_detect("hiatus|Hiatus"))

# Clean dates ----
charc_cats <- "carbon|AMS|Counting|counting"
marine_cats <- "Marine|Sea|Caspian|marine|sea|caspian"
na_codes <- c(-777777, -888888, -999999)
if (nrow(epd_subset_dates_coretops) > 0) {
  epd_subset_dates <- epd_subset_dates %>%
    dplyr::bind_rows(epd_subset_dates_coretops)
}
epd_subset_dates_clean <- epd_subset_dates %>%
  dplyr::arrange(entity_name, depth) %>%
  dplyr::filter(date_type %>% stringr::str_detect("hiatus|Hiatus", TRUE)) %>%
  dplyr::left_join(epd_subset %>% # Append lat and description from the metadata
                     dplyr::select(entity_name, latitude, description),
                   by = "entity_name") %>%
  dplyr::mutate(age_c14 = dplyr::case_when(is.na(age_c14) ~ NA_real_,
                                           age_c14 %in% na_codes ~ NA_real_,
                                           TRUE ~ age_c14),
                age_cal = dplyr::case_when(is.na(age_cal) ~ NA_real_,
                                           age_cal %in% na_codes ~ NA_real_,
                                           TRUE ~ age_cal),
                age = dplyr::coalesce(age_c14, age_cal),
                error = ifelse(is.na(error) | error <= 0, 1, error),
                cc = dplyr::case_when(stringr::str_detect(date_type, charc_cats) ~ 1,
                                      TRUE ~ 0),
                cc = dplyr::case_when(cc == 1 & latitude <= -15 ~ 3,
                                      cc == 1 &
                                        latitude < 15 &
                                        latitude > -15 ~ 4,
                                      TRUE ~ cc)
                ) %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::select(site_id, site_name, entity_name, lab_num, age, error, depth, cc)

# Create mixed calibration curve (cc = 4) ----
ccdir <- file.path(WDIR, "ccurves")
ageR::mix_curves(proportion = 0.5, cc1 =1, cc = 3, name = "neotropics.14C", dir = ccdir)

# Create input directories ----
clean_entities <- entities %>%
  purrr::map_chr(function(ent) {
    dates <- epd_subset_dates_clean %>%
      dplyr::filter(entity_name == ent) %>%
      dplyr::select(lab_num, age, error, depth, cc)
    sample_depths <- epd_subset_counts %>%
      dplyr::filter(entity_name == ent) %>%
      dplyr::select(depth) %>%
      dplyr::arrange(depth) %>%
      dplyr::mutate(id = seq_along(depth), .before = 1)
    hiatus <- epd_subset_dates_hiatus %>%
      dplyr::filter(entity_name == ent) %>%
      dplyr::select(depth)
    clean_ent_name <- ageR:::cln_str(ent)
    ageR::create_input(data =
                         list(sample_depths = sample_depths,
                              core = dates,
                              hiatus = hiatus),
                       wdir = file.path(WDIR, "EPD"),
                       entity = clean_ent_name,
                       am = "bacon")
    clean_ent_name
  })

## Create a tibble with the original and clean (without special characters)
## entity names
entities2 <- tibble::tibble(
  original = entities,
  clean = clean_entities
)

# Global parameters ----
MAX_SCENARIOS <- 100
CPUS <- 8 # Change based on your computer specs
## Run the age models (and hope for the best) ----
output <- entities2 %>%
  purrr::pmap(function(original, clean) {
    tryCatch({
      ageR:::msg(original)
      ageR::Bacon2(wdir = file.path(WDIR, "EPD"),
                   entity = clean,
                   cc4 = "neotropics.14C",
                   ccdir = ccdir,
                   max_scenarios = MAX_SCENARIOS,
                   cpus = CPUS,
                   dry_run = F,
                   thick = 1,
                   acc = c(25, 30, 35, 40, 45)) %>%
        ageR::pb()
    }, error = function(e) {
      message("ERROR while running `", original, "`: ", e)
      return(NULL)
    })
  })


