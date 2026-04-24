#' @name get_parcels
#' @export
#'
#'

# pull parcel data

get_parcels <- function(
                             ctys="", # cty or ctys to retrieve
                             geom=FALSE, # include geometries
                             adg_path=Sys.getenv("ADG_KEY") # adg path
                             ) {
  ctys <- tolower(ctys) # set to lowercase for filtering
  # pull adg path for user
  if(Sys.getenv("ADG_KEY") == "") {
    print("No ADG Path! Set using install_adg_key()")
  }

  # flag if no data for county
  ctys_possible <- stringr::str_remove(list.files(paste0(adg_path,"/Data/Parcel Data/output/standard_file/county tabular")),".csv")

  bad_county <- FALSE

  for(c in ctys) {
  if(!(c %in% ctys_possible) & bad_county == FALSE) {
    bad_county <- TRUE
    print(c("Unrecognized county. Please select from: ",ctys_possible))
  }
  }
  if(bad_county == TRUE) {
     # otherwise read data
  } else {
    return <- purrr::map(ctys, ~{
      return_map <- readr::read_csv(paste0(adg_path,"/Data/Parcel Data/output/standard_file/county tabular/",.x,".csv")) # read in data

    # include geometry if requested
    if(geom == TRUE) {
      print("Adding geometries...")
      return_map <-  dplyr::left_join(return_map,sf::st_read(paste0(adg_path,"/Data/Parcel Data/output/standard_file/county spatial/",.x,".geojson")),
                                  relationship = "many-to-many")
      return_map <-  sf::st_as_sf(return_map)
    }

      return_map
    })

    return <- data.table::rbindlist(return)

    # return data
    return
  }

}
