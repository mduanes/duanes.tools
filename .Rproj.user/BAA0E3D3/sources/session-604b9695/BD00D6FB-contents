#' @name census_ages
#' @export
#'
#'


# pull census age crosswalk

census_ages <- function(adg_path=Sys.getenv("ADG_KEY") # ADG Key/Path
) {

  # pull adg key
  if(Sys.getenv("ADG_KEY") == "") {
    print("No ADG Path! Set using install_adg_key()")
  } else {

    # compile file path based on adg key
    full_path <- paste0(adg_path,"Projects/Active Projects/Georgia Population Projections/Historical Projections/data/census age group crosswalk.xlsx")

    # read and return dimensions
    readxl::read_xlsx(full_path)
  }
}

