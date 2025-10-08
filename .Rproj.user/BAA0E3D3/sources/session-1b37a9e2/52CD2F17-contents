#' @name us_places
#' @export

us_places <- function() {

  ADG_KEY <- get_adg_key()

  # pull county data
  sf::st_read(paste0(ADG_KEY,"Data/TIGER files/US_place_2023.shp"))
}
