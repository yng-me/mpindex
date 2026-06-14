#' Sample dataset of household members
#'
#' @description This dataset contains a many-to-one relationship with the \link[mpindex]{df_household} dataset. Hence, you can apply joins using the \code{uuid}.
#'
#' @format A tibble with 905 rows and 8 variables:
#' \describe{
#' \item{uuid}{Unique ID}
#' \item{line_number}{Number identifier for each member within the household}
#' \item{class}{Urbanity: \code{Rural} or \code{Urban}}
#' \item{sex}{Sex of the household member}
#' \item{age}{Age of the household member}
#' \item{attending_school}{Whether the household member (aged 5-24 years old) is currently attending school: \code{1} - currently attending; \code{2} - currently not attending}
#' \item{completed_6yrs_schooling}{Whether completed at least six (6) years of schooling: \code{1} - completed; \code{2} -not completed}
#' \item{undernourished}{Whether the household member (aged below 70 years old) is undernourished: \code{1} - undernourished; \code{2} - not undernourished}
#' }
#' @seealso \link[mpindex]{df_household}
#' @examples
#' df_household_roster
"df_household_roster"
