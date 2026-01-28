#' @name us_counties
#' @export

us_counties <- function() {

  ADG_KEY <- get_adg_key()

  # pull county data
  sf::st_read(paste0(ADG_KEY,"Data/TIGER files/US_county_2023.shp"))
}
