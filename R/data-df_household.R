#' Sample dataset of households
#'
#' @description This is a synthetic dataset containing household information primarily used for demonstration purposes on how to use the \code{mpindex} package.
#'
#' @format A tibble with 198 rows and 21 variables:
#' \describe{
#' \item{uuid}{Unique ID}
#' \item{class}{Urbanity: \code{Rural} or \code{Urban}}
#' \item{drinking_water}{Acess to drinking water: \code{1} - improved; \code{2} - unimproved}
#' \item{toilet}{Service level of toilet or sanitation facility: \code{1} - basic; \code{2} - limited; \code{3} - unimproved; \code{4} - open defecation}
#' \item{with_child_died}{With at least one (1) child died in the last five (5) years: \code{1} - with child died; \code{2} - without child died}
#' \item{roof}{Main construction material of the roof: \code{1} - galvanized iron/aluminum; \code{2} - concrete/clay tile; \code{3} - half galvanized iron and half concrete; \code{4} - wood/bamboo; \code{5} - cogon/nipa/anahaw; \code{6} - asbestos; \code{7} - makeshift/salvaged/improvised materials; \code{9} - other construction material}
#' \item{walls}{Main construction material of the outer walls: \code{1} - concrete/brick/stone; \code{2} - wood; \code{3} - half concrete/brick/stone and half wood; \code{4} - Galvanized iron/aluminum; \code{5} - bamboo/sawali/cogon/nipa; \code{6} - asbestos; \code{7} - glass; \code{8} - makeshift/salvaged/improvised materials; \code{9} - none; \code{10} - concrete hollow blocks; \code{11} - concrete hollow blocks/wood;  \code{12} - shear walls; \code{99} - other construction material}
#' \item{floor}{Main construction material of the floor: \code{1} - concrete; \code{2} - wood; \code{3} - coconut lumber; \code{4} - bamboo; \code{5} - earth/sand/mud; \code{6} - makeshift/salvaged/improvised materials; \code{9} - other construction material}
#' \item{electricity}{Access to electricity: \code{1} - with access to electricity; \code{2} - without access to electricity}
#' \item{cooking_fuel}{Fuel use for cooking: \code{1} - electricity; \code{2} - kerosene (gaas); \code{3} - liquified petroleum gas (LPG); \code{4} - charcoal; \code{5} - wood; \code{6} - none; \code{9} - other cooking fuel such as dung, agricultural crop, or shrubs}
#' \item{asset_radio}{Number of working radio owned by the household}
#' \item{asset_tv}{Number of working television owned by the household}
#' \item{asset_telephone}{Number of working telephone owned by the household}
#' \item{asset_mobile_phone}{Number of working mobile phone owned by the household}
#' \item{asset_computer}{Number of working computer owned by the household}
#' \item{asset_animal_cart}{Number of animal carts owned by the household}
#' \item{asset_bicycle}{Number of bicycle owned by the household}
#' \item{asset_motorcycle}{Number of motorcylce owned by the household}
#' \item{asset_refrigerator}{Number of working refrigerator owned by the household}
#' \item{asset_car}{Number of car owned by the household}
#' \item{asset_truck}{Number of trucks owned by the household}
#' }
#' @seealso \link[mpindex]{df_household_roster}
#' @examples
#' df_household
"df_household"
