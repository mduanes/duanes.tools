#' @name us_zips
#' @export

# pulls us zcta geoemtries from adg archives rather than tigris

us_zips <- function() {

  ADG_KEY <- get_adg_key()

  # pull county data
  zips <- sf::st_read(paste0(ADG_KEY,"Data/TIGER files/US_zcta_2023.shp")) %>%
    dplyr::mutate(PREFIX=str_trunc(GEOID20,3,"right",""))

  # add state prefix
  crosswalk <- readxl::read_xlsx(paste0(ADG_KEY,"Data/Geometry Crosswalks/zip_state_crosswalk.xlsx")) %>%
    dplyr::mutate(Prefix=stringr::str_pad(Prefix,3,"left","0"))
  # get state prefix from state data
  st <- us_states() %>%
    data.frame() %>%
    dplyr::select(STATEFP,STUSPS)
  # join and output
  zips %>%
    dplyr::left_join(crosswalk,by=c("PREFIX"="Prefix")) %>%
    dplyr::left_join(st,by=c("State"="STUSPS"))

}
