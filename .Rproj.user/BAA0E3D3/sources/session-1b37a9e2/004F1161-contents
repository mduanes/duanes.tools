#' @name get_fips
#' @export

get_fips <- function(ctys=c("Clarke","Russell,AL")) {

  # pull state data to match fips
  ADG_KEY <- get_adg_key()
  state_fips <- foreign::read.dbf(paste0(ADG_KEY,"Data/TIGER files/tl_2024_us_state.dbf"))

  # get states from provided counties
  states <- data.frame(ctys) %>%
    dplyr::mutate(state=ifelse(!stringr::str_detect(ctys,","),"GA",stringr::str_trunc(ctys,2,"left","")),
                  ctys=stringr::str_remove(ctys,",.*")) %>%
    dplyr::left_join(state_fips,by=c("state"="STUSPS")) %>%
    dplyr::select(ctys,STATEFP)

  # pull county data
  fips <- foreign::read.dbf(paste0(ADG_KEY,"Data/TIGER files/US_county_2023.dbf")) %>%
    dplyr:: select(STATEFP,COUNTYFP,NAME) %>%
    dplyr::right_join(states,by=c("STATEFP","NAME"="ctys")) %>%
    dplyr::mutate("GEOID"=paste0(STATEFP,COUNTYFP)) %>%
    pull(GEOID)

  # return list of complete fips
  fips
}
