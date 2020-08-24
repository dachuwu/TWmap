#' Demo dataset: DALY estimates of cancers (level-3)
#'
#' A dataset containing the DALY, YLD and YLL of level-3 cancers (Taiwan 2015).
#'
#' @format A data frame with 352 rows and 7 variables:
#' \describe{
#'   \item{sex}{0 for female and 1 for male.}
#'   \item{town}{town code, MOI format.}
#'   \item{year}{2015 only}
#'   \item{cause_l3}{level-3 cause code}
#'   \item{daly_count}{DALY estimates (person-year)}
#'   \item{yld_count}{YLD estimates (person-year)}
#'   \item{yll_count}{YLL estimates (person-year)}
#'   \item{population}{population size}
#'
#' }
#' @source TBDC
#'
"daly_B01"
