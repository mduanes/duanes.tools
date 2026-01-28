#' @name pull_base
#' @export
#'
#'
#` pull population base for given years

pull_base <- function(vintage=2024,
                      adg_path=Sys.getenv("ADG_KEY"),
                      years=seq(2010,2024,1),
                      age=TRUE,
                      race=TRUE,
                      sex=TRUE) {

  if(Sys.getenv("ADG_KEY") == "") {
    print("No ADG Path! Set using install_adg_key()")
  } else {

  # fetch based on vintage
  refs <-c("2023"="results/2023/population_base_tidy_2023_rounded.csv",
           "2024"="results/2024/population_base_tidy_2024_updated.csv")
  ref <- unname(refs[as.character(vintage)])
  full_path <- paste0(adg_path,"Projects/Active Projects/Georgia Population Projections/Population Base/",ref)

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

  # read in base data and summarize by preferences
  read_csv(full_path) %>%
    group_by(pick(chars)) %>%
    summarize(POP=sum(POP)) %>%
    ungroup() %>%
    # filter for only wanted years
    filter(YEAR %in% years)
  }
}

