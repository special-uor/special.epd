#' @keywords internal
"_PACKAGE"

#' \code{age_model}
"age_model"

#' \code{date_info}
"date_info"

#' \code{entity}
"entity"

#' \code{external_link}
"external_link"

#' \code{model_name}
"model_name"

#' \code{pollen_count}
"pollen_count"

#' \code{sample} table
#'
#' Contains metadata for each sample record.
#' @format A data frame (\code{tibble} object) with
#'    `r nrow(special.epd::sample)` rows and
#'    `r ncol(special.epd::sample)` variables:
#' \describe{
#'   \item{ID_ENTITY}{\code{Unsigned integer}: Unique identifier for the entity (as in \code{entity} table)}
#'   \item{ID_SAMPLE}{\code{Unsigned integer}: Unique identifier for each sample}
#'   \item{depth}{\code{Text}: Average sampling depth, in centimetres}
#'   \item{thickness}{\code{Text}: Sample thickness, in centimetres}
#'   \item{chronology_name}{\code{Text}: Name assigned to the chronology}
#'   \item{age_type}{\code{Text}: Type of dating for the sample}
#'   \item{age}{\code{Integer}: Value dated for the sample}
#'   \item{age_younger}{\code{Integer}: Lower bound for the sample's age}
#'   \item{age_older}{\code{Integer}: Upper bound for the sample's age}
#'   \item{count_type}{\code{Text}: Type of count for the sample}
#'   \item{sample_type}{\code{Text}: Sample type}
#' }
#' \code{sample}
"sample"

#' \code{taxon_name}
"taxon_name"
