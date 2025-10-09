#' @name us_zips
#' @export

us_zips <- function() {

  ADG_KEY <- get_adg_key()

  # pull county data
  zips <- sf::st_read(paste0(ADG_KEY,"Data/TIGER files/US_zcta_2023.shp")) %>%
    mutate(PREFIX=str_trunc(GEOID20,3,"right",""))
  crosswalk <- readxl::read_xlsx(paste0(ADG_KEY,"Data/Geometry Crosswalks/zip_state_crosswalk.xlsx")) %>%
    mutate(Prefix=str_pad(Prefix,3,"left","0"))
  zips %>%
    left_join(crosswalk,by=c("PREFIX"="Prefix"))
}
