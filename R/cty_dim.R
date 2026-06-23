#' @name cty_dim
#' @export
#'
#'


# pull county dimensions

cty_dim <- function(adg_path=Sys.getenv("ADG_KEY") # ADG Key/Path
                    ) {

  # pull adg key
  if(Sys.getenv("ADG_KEY") == "") {
    print("No ADG Path! Set using install_adg_key()")
  } else {

    # compile file path based on adg key
    full_path <- paste0(adg_path,"Data/Dimensions (GitHub)/county_geography_info/county_dimensions_adg.csv")

    # read and return dimensions
    readr::read_csv(full_path) %>%
      dplyr::select(-CountyFips) %>%
      dplyr::rename("CountyFips"=CountyFipsLong)
    }
}

