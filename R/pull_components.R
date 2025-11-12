#' @name pull_components
#' @export
#'
#'
#` pull population change components for given years

pull_components <- function(adg_path=Sys.getenv("ADG_KEY"),
                            ctys=NA) {

  if(is.na(ctys[1])) {
    ctys <- us_counties() %>%
      filter(STATEFP=="13") %>%
      pull(NAME)
  }
  if(Sys.getenv("ADG_KEY") == "") {
    print("No ADG Path! Set using install_adg_key()")
  } else {

    # fetch based on vintage
    full_path <- paste0(adg_path,"Data/Census Population Estimates (All Components)/ga_components_of_change_10_24.csv")

    # read in base data and summarize by preferences
    read_csv(full_path) %>%
      filter(GEOID %in% get_fips(ctys)) %>%
      mutate(CTYNAME =str_remove(CTYNAME," County"))
  }
}

