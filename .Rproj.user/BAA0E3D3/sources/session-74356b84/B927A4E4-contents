#' @name pull_projections
#' @export
#'
#'

# pull OPB projections for given years

pull_projections <- function(vintage=2026, # vintage to pull
                      adg_path=Sys.getenv("ADG_KEY"), # adg path
                      years=seq(year(Sys.Date()),2060,1), # years to pull data for
                      age=FALSE, # break down by age?
                      race=FALSE, # break down by race?
                      sex=FALSE # break down by sex?
                      ) {

  # pull adg path for user
  if(Sys.getenv("ADG_KEY") == "") {
    print("No ADG Path! Set using install_adg_key()")
  } else if (as.character(vintage) %in% c("2023","2024","2025","2026")) {

    # register vintage specific paths
    refs <-c("2026"="2026",
             "2025"="2025",
             "2025"="2025",
             "2024"="2024",
             "2023"="2023")
    # fetch based on vintage
    ref <- unname(refs[as.character(vintage)])
    # compile full path
    full_path <- paste0(adg_path,"Projects/Active Projects/Georgia Population Projections/Historical Projections/historical projections/",ref)

    # get breakdown characteristics based on selections
    chars <- c("COUNTY","YEAR")
    if (sex == TRUE) {
      chars <- c(chars,"SEX")
    }
    if(race == TRUE) {
      chars <- c(chars,"RACE_ETH")
    }
    if(age == TRUE) {
      chars <- c(chars,"AGE")
    }

    # # handle vintages based on different naming schemes,
    # # group by and summarize for desired breakdown characteristics
    # if (as.character(vintage) == "2025") {
    #   readr::read_delim(full_path) %>%
    #     dplyr::filter(BOUND=="POP_M") %>%
    #     dplyr::group_by(pick(chars)) %>%
    #     dplyr::summarize(POP=sum(TOTAL_POP)) %>%
    #     dplyr::ungroup() %>%
    #     # filter for only wanted years
    #     dplyr::filter(YEAR %in% years) %>%
    #     dplyr::mutate("VINTAGE"=vintage)
    # } else if (as.character(vintage) == "2023") {
    #   readr::read_delim(full_path) %>%
    #     dplyr::filter(BOUNDS=="Mean") %>%
    #     dplyr::group_by(pick(chars)) %>%
    #     dplyr::summarize(POP=sum(POP)) %>%
    #     dplyr:: ungroup() %>%
    #     # filter for only wanted years
    #     dplyr::filter(YEAR %in% years) %>%
    #     dplyr::mutate("VINTAGE"=vintage)
    # } else {
    # read in base data and summarize by preferences
      readr::read_delim(full_path) %>%
        dplyr::group_by(dplyr::pick(chars)) %>%
        dplyr:: summarize(POP=sum(POP)) %>%
        dplyr::ungroup() %>%
      # filter for only wanted years
        dplyr::filter(YEAR %in% years) %>%
        dplyr::mutate("VINTAGE"=vintage)
    #}
  } else {
    message("Invalid vintage! Supported vintages: 2023, 2024, 2025, 2026")
  }
}

