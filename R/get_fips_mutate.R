#' @name get_fips_mutate
#' @export

# returns FIPS for set of counties

get_fips_mutate <- function(data,col="COUNTY" # Georgia counties do not need state specification, but all other states need a ,ABBREVIATION suffix
) {

  # pull state data to match fips
  ADG_KEY <- get_adg_key()
  # pull state data from ADG files
  state_fips <- foreign::read.dbf(paste0(ADG_KEY,"Data/TIGER files/tl_2024_us_state.dbf"))

  ctys <- data %>%
    dplyr::pull(col)

  # get states from provided counties
  states <- data.frame(ctys) %>%
    mutate(ctys=as.character(ctys))

  #print(states)

  states <- states %>%
    dplyr::mutate(state=ifelse(!stringr::str_detect(ctys,","),"GA",stringr::str_trunc(ctys,2,"left","")),
                  ctys=stringr::str_remove(ctys,",.*")) %>%
    dplyr::left_join(state_fips,by=c("state"="STUSPS")) %>%
    dplyr::select(ctys,STATEFP) %>%
    dplyr::mutate(ctys=tolower(ctys))

  # pull county data
  fips <- foreign::read.dbf(paste0(ADG_KEY,"Data/TIGER files/US_county_2023.dbf")) %>%
    dplyr::select(STATEFP,COUNTYFP,NAME) %>%
    dplyr::mutate(NAMEjoin=tolower(NAME)) %>%
    dplyr::right_join(states,by=c("STATEFP","NAMEjoin"="ctys")) %>%
    dplyr::mutate("GEOID"=paste0(STATEFP,COUNTYFP)) %>%
    dplyr::select(NAME,GEOID) %>%
    dplyr::distinct()

  data %>%
    dplyr::left_join(fips,relationship = "many-to-many")
}
