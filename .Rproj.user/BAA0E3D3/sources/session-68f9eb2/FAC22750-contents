#' @name pull_projections
#' @export
#'
#'
#` pull OPB projections for given years

pull_projections <- function(vintage=2025,
                      adg_path=Sys.getenv("ADG_KEY"),
                      years=seq(2025,2060,1),
                      age=TRUE,
                      race=TRUE,
                      sex=TRUE) {

  if(Sys.getenv("ADG_KEY") == "") {
    print("No ADG Path! Set using install_adg_key()")
  } else if (as.character(vintage) %in% c("2023","2024","2025")) {

    # fetch based on vintage
    refs <-c("2025"="2025/series2025_projections.txt",
             "2024"="2024/series2024_projections.txt",
             "2023"="2023/2023projections_all.txt")
    ref <- unname(refs[as.character(vintage)])
    full_path <- paste0(adg_path,"Projects/Active Projects/Georgia Population Projections/Historical Projections/data/",ref)

    # get grouping columns
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

    # rename 2024 POP
    if (as.character(vintage) == "2025") {
      read_delim(full_path) %>%
        filter(BOUND=="POP_M") %>%
        group_by(pick(chars)) %>%
        summarize(POP=sum(TOTAL_POP)) %>%
        ungroup() %>%
        # filter for only wanted years
        filter(YEAR %in% years) %>%
        mutate("VINTAGE"=vintage)
    } else if (as.character(vintage) == "2023") {
      read_delim(full_path) %>%
        filter(BOUNDS=="Mean") %>%
        group_by(pick(chars)) %>%
        summarize(POP=sum(POP)) %>%
        ungroup() %>%
        # filter for only wanted years
        filter(YEAR %in% years) %>%
        mutate("VINTAGE"=vintage)
    } else {
    # read in base data and summarize by preferences
    read_delim(full_path) %>%
      group_by(pick(chars)) %>%
      summarize(POP=sum(POP)) %>%
      ungroup() %>%
      # filter for only wanted years
      filter(YEAR %in% years) %>%
      mutate("VINTAGE"=vintage)
    }
  } else {
    print("Invalid vintage! Supported vintages: 2023, 2024, 2025")
  }
}

