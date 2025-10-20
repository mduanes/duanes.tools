#' @name us_states
#' @export

us_states <- function() {

  ADG_KEY <- get_adg_key()

  # pull county data
  sf::st_read(paste0(ADG_KEY,"Data/TIGER files/US_state_2023.shp"))
}
