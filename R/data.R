#' @keywords internal
"_PACKAGE"

#' Age models
#'
#' Contains new age models created with the IntCal20 (Reimer et al., 2020),
#' SHCal20 (Hogg et al., 2020) and Marine20 (Heaton et al., 2020) calibration
#' curves, in combination with the package `ageR` (Villegas-Diaz et al., 2022).
#' Represents the `age_model` table in the database.
#'
#' @format A data frame (\code{tibble} object) with
#'  `r as.character(nrow(special.epd::age_model))` rows and
#'  `r as.character(ncol(special.epd::age_model))` variables:
#' \describe{
#'  \item{ID_MODEL}{\code{Unsigned integer}: Unique identifier for the type of
#'   model (e.g. IntCal20 = 8) (as in the \code{model_name} table)}
#'  \item{ID_SAMPLE}{\code{Unsigned integer}: Unique identifier for each
#'   sample (as in the \code{sample} table)}
#'  \item{mean}{\code{Integer}: Mean age of the sample}
#'  \item{median}{\code{Integer}: Median age of the sample}
#'  \item{UNCERT_5}{\code{Integer}: Lower bound of the 95% confidence interval
#'   for the median age}
#'  \item{UNCERT_25}{\code{Integer}: Lower bound of the 75% confidence interval
#'   for the median age}
#'  \item{UNCERT_75}{\code{Integer}: Upper bound of the 75% confidence interval
#'   for the median age}
#'  \item{UNCERT_95}{\code{Integer}: Upper bound of the 95% confidence interval
#'   for the median age}
#' }
#'
#' @references
#' Heaton, T., Köhler, P., Butzin, M., Bard, E., Reimer, R., Austin, W.,
#' Bronk Ramsey, C., Grootes, P., Hughen, K., Kromer, B., Reimer, P.,
#' Adkins, J., Burke, A., Cook, M., Olsen, J., & Skinner, L.: Marine20 - the
#' marine radiocarbon age calibration curve (0–55,000 cal BP). Radiocarbon, 62,
#' 779-820, doi: 10.1017/RDC.2020.68, 2020.
#'
#' Hogg, A., Heaton, T., Hua, Q., Palmer, J., Turney, C., Southon, J.,
#' Bayliss, A., Blackwell, P., Boswijk, G., Bronk Ramsey, C., Petchey, F.,
#' Reimer, P., Reimer, R., & Wacker, L.: SHCal20 Southern Hemisphere
#' calibration, 0–55,000 years cal BP. Radiocarbon, 62, 759-778
#' doi: 10.1017/RDC.2020.59, 2020.
#'
#' Reimer, P., Austin, W., Bard, E., Bayliss, A., Blackwell, P.,
#' Bronk Ramsey, C., Butzin, M., Cheng,H. Edwards, R.L., Friedrich, M.,
#' Grootes, P.M., Guilderson, T.P., Hajdas, I., Heaton, T.J., Hogg, A.G.,
#' Hughen, K.A., Kromer, B., Manning, S.W., Muscheler, R., Palmer, J.G.,
#' Pearson, C., van der Plicht, J., Reimer, R.W., Richards, D.A., Scott, E.M.,
#' Southon, J.R., Turney, C.S.M., Wacker, L., Adolphi, F., Buntgen, U.,
#' Capano, M., Fahrni, S.M., Fogtmann-Schulz, A., Friedrich, R., Kohler, P.,
#' Kudsk, S., Miyake, F., Olsen, J., Reinig, F., Sakamoto, M., Sookdeo, M.,
#' Talamo, S.: The INTCAL20 Northern Hemisphere radiocarbon age calibration
#' curve (0-55 calkBP), Radiocarbon, 62, 725-757,
#' doi: 10.1017/RDC.2020.41, 2020.
#'
#' Villegas-Diaz, R., Cruz-Silva, E., Harrison, S.P.: ageR: Supervised Age
#' Models. doi: 10.5281/zenodo.4636715, 2022.
#'
#' @examples
#' age_model_tb <- special.epd::age_model
#' head(age_model_tb)
"age_model"

#' Dating information
#'
#' Contains dating information linked to each entity. These dates were used
#' (or ignored, see the `notes` column) to create the age models. Represents
#' the `date_info` table in the database.
#'
#' @format A data frame (\code{tibble} object) with
#'  `r as.character(nrow(special.epd::date_info))` rows and
#'  `r as.character(ncol(special.epd::date_info))` variables:
#' \describe{
#'  \item{ID_ENTITY}{\code{Unsigned integer}: Unique identifier for the entity
#'   (as in the \code{entity} table)}
#'  \item{ID_DATE_INFO}{\code{Unsigned integer}: Unique identifier for each
#'   dating record}
#'  \item{date_type}{\code{Text}: }
#'  \item{depth}{\code{Double}: }
#'  \item{thickness}{\code{Double}: }
#'  \item{lab_num}{\code{Text}: }
#'  \item{age_c14}{\code{Integer}: }
#'  \item{age_calib}{\code{Integer}: }
#'  \item{error}{\code{Integer}: }
#'  \item{material_dated}{\code{Text}: }
#'  \item{age_used}{\code{Text}: }
#'  \item{reason_age_not_used}{\code{Text}: }
#'  \item{notes}{\code{Text}: }
#' }
#'
#' @examples
#' date_info_tb <- special.epd::date_info
#' head(date_info)
"date_info"

#' Entity metadata
#'
#' Contains metadata linked to each entity. Represents the `entity` table in
#' the database.
#'
#' @format A data frame (\code{tibble} object) with
#'  `r as.character(nrow(special.epd::entity))` rows and
#'  `r as.character(ncol(special.epd::entity))` variables:
#' \describe{
#'  \item{ID_SITE}{\code{Unsigned integer}: Unique identifier for the site,
#'   multiple entities can be located within the same site}
#'  \item{ID_ENTITY}{\code{Unsigned integer}: Unique identifier for the entity}
#'  \item{site_name}{\code{Text}: }
#'  \item{entity_name}{\code{Text}: }
#'  \item{latitude}{\code{Double}: }
#'  \item{longitude}{\code{Double}: }
#'  \item{elevation}{\code{Double}: }
#'  \item{site_type}{\code{Text}: }
#'  \item{source}{\code{Text}: }
#'  \item{publication}{\code{Text}: }
#'  \item{doi}{\code{Text}: }
#' }
#'
#' @examples
#' entity_tb <- special.epd::entity
#' head(entity_tb)
"entity"

#' External links
#'
#' Contains information linking the entities to their original sources
#' (including the IBERIAN subset (Harrison et al., 2022a),
#' EMBSeCBIO (Harrison et al., 2021),
#' NEOTOMA (Williams et al., 2018) and
#' RPD (Harrison et al., 2022a)). Represents the `external_link` table in the
#' database.
#'
#' @format A data frame (\code{tibble} object) with
#'  `r as.character(nrow(special.epd::external_link))` rows and
#'  `r as.character(ncol(special.epd::external_link))` variables:
#' \describe{
#'  \item{ID_SITE}{\code{Unsigned integer}: Unique identifier for the site,
#'   multiple entities can be located within the same site}
#'  \item{ID_ENTITY}{\code{Unsigned integer}: Unique identifier for the entity}
#'  \item{external_ID_SITE}{\code{Unsigned integer}: }
#'  \item{external_ID_ENTITY}{\code{Unsigned integer}: Unique identifier for the entity}
#'  \item{external_site_name}{\code{Text}: }
#'  \item{external_entity_name}{\code{Text}: }
#'  \item{external_source}{\code{Text}: }
#' }
#'
#' @references
#' Harrison, S.P., Marinova, E. and Cruz-Silva, E.: EMBSeCBIO pollen database.
#' University of Reading. Dataset. doi: https://doi.org/10.17864/1947.309, 2021.
#'
#' Harrison, S.P., Shen, Y. and Sweeney, L.: Pollen data and charcoal data of
#' the Iberian Peninsula (version 3). University of Reading. Dataset.
#' doi: https://doi.org/10.17864/1947.000369, 2022a.
#'
#' Harrison, S.P., Villegas-Diaz, R., Lincoln, P., Kesner, D.,
#' Cruz-Silva, E., Sweeney, L., Shen, Y. and Gallagher, D.:
#' The Reading Palaeofire Database v1b: an expanded global resource to document
#' changes in fire regimes from sedimentary charcoal records. University of
#' Reading. Dataset. doi: https://doi.org/10.17864/1947.000345, 2022b.
#'
#' Williams, J.W., Grimm, E.G., Blois, J., Charles, D.F., Davis, E., Goring,
#' S.J., Graham, R., Smith, A.J., Anderson, M., Arroyo-Cabrales, J., Ashworth,
#' A.C., Betancourt, J.L., Bills, B.W., Booth, R.K., Buckland, P., Curry, B.,
#' Giesecke, T., Hausmann, S., Jackson, S.T., Latorre, C., Nichols, J., Purdum,
#' T., Roth, R.E., Stryker, M., Takahara, H.: The Neotoma Paleoecology
#' Database: A multi-proxy, international community-curated data resource.
#' Quaternary Research 89, 156-177. http://www.neotomadb.org/, 2018.
#'
#' @examples
#' external_link_tb <- special.epd::external_link
#' head(external_link_tb)
"external_link"

#' Age model names
#'
#' Contains a list of valid age models. Represents the `model_name` table
#' in the database.
#'
#' This table was populated from the RPD (Harrison et al., 2022); however, all
#' the age models in the `special.epd` have `ID_MODEL = 8`, (IntCal20) models.
#'
#' @format A data frame (\code{tibble} object) with
#'  `r as.character(nrow(special.epd::model_name))` rows and
#'  `r as.character(ncol(special.epd::model_name))` variables:
#' \describe{
#'  \item{ID_MODEL}{\code{Unsigned integer}: Unique identifier for the age
#'   model}
#'  \item{model_name}{\code{Text}: }
#' }
#'
#' @references
#' Harrison, S.P., Villegas-Diaz, R., Lincoln, P., Kesner, D.,
#' Cruz-Silva, E., Sweeney, L., Shen, Y. and Gallagher, D.:
#' The Reading Palaeofire Database v1b: an expanded global resource to document
#' changes in fire regimes from sedimentary charcoal records. University of
#' Reading. Dataset. doi: https://doi.org/10.17864/1947.000345, 2022.
#'
#' @examples
#' model_name_tb <- special.epd::model_name
#' model_name_tb
"model_name"

#' Pollen counts
#'
#' Contains pollen counts for the samples, there are 3 levels of amalgamation,
#' represented by an integer in the column `amalgamation_level`metadata for each pollen sample record in the database, `sample`
#' table in the database.
#'
#' @format A data frame (\code{tibble} object) with
#'  `r trunc(nrow(special.epd::pollen_count))` rows and
#'  `r trunc(ncol(special.epd::pollen_count))` variables:
#' \describe{
#'  \item{ID_ENTITY}{\code{Unsigned integer}: Unique identifier for the entity
#'   (as in the \code{entity} table)}
#'  \item{ID_SAMPLE}{\code{Unsigned integer}: Unique identifier for each
#'   sample}
#'  \item{depth}{\code{Text}: Average sampling depth, in centimetres}
#'  \item{thickness}{\code{Text}: Sample thickness, in centimetres}
#'  \item{chronology_name}{\code{Text}: Name assigned to the chronology}
#'  \item{age_type}{\code{Text}: Type of dating for the sample}
#'  \item{age}{\code{Integer}: Value dated for the sample}
#'  \item{age_younger}{\code{Integer}: Lower bound for the sample's age}
#'  \item{age_older}{\code{Integer}: Upper bound for the sample's age}
#'  \item{count_type}{\code{Text}: Type of count for the sample}
#'  \item{sample_type}{\code{Text}: Type of sample}
#' }
#'
#' @examples
#' sample_tb <- special.epd::sample
#' head(sample_tb)
"pollen_count"

#' Pollen samples
#'
#' Contains metadata for each pollen sample record in the database, `sample`
#' table in the database.
#'
#' @format A data frame (\code{tibble} object) with
#'  `r trunc(nrow(special.epd::sample))` rows and
#'  `r trunc(ncol(special.epd::sample))` variables:
#' \describe{
#'  \item{ID_ENTITY}{\code{Unsigned integer}: Unique identifier for the entity
#'   (as in the \code{entity} table)}
#'  \item{ID_SAMPLE}{\code{Unsigned integer}: Unique identifier for each
#'   sample}
#'  \item{depth}{\code{Text}: Average sampling depth, in centimetres}
#'  \item{thickness}{\code{Text}: Sample thickness, in centimetres}
#'  \item{chronology_name}{\code{Text}: Name assigned to the chronology}
#'  \item{age_type}{\code{Text}: Type of dating for the sample}
#'  \item{age}{\code{Integer}: Value dated for the sample}
#'  \item{age_younger}{\code{Integer}: Lower bound for the sample's age}
#'  \item{age_older}{\code{Integer}: Upper bound for the sample's age}
#'  \item{count_type}{\code{Text}: Type of count for the sample}
#'  \item{sample_type}{\code{Text}: Type of sample}
#' }
#'
#' @examples
#' sample_tb <- special.epd::sample
#' head(sample_tb)
"sample"

#' Taxon names
#'
#' Contains a list of valid taxa. Represents the `taxon_name` table
#' in the database.
#'
#' @format A data frame (\code{tibble} object) with
#'  `r as.character(nrow(special.epd::taxon_name))` rows and
#'  `r as.character(ncol(special.epd::taxon_name))` variables:
#' \describe{
#'  \item{ID_TAXON}{\code{Unsigned integer}: Unique identifier for the taxon
#'   name}
#'  \item{taxon_name}{\code{Text}: }
#' }
#'
#' @examples
#' taxon_name_tb <- special.epd::taxon_name
#' head(taxon_name_tb)
"taxon_name"
