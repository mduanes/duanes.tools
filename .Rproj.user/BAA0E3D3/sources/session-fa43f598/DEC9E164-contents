#' @name join_geometries
#' @export
#'
#'
# function to automatically join the relevant geometries from tigris
join_geometries <- function(data,st=13) {
  if(!("GEOID" %in% colnames(data))) {
    print("No GEOID Field! Cannot Join!")
  } else {
  GEOID <- data$GEOID

  # detect geometry type and load appropriate tigris shapefile
  if (nchar(GEOID[1]) == 5) {
    geometry <- mds_counties(state=st) %>%
      select(GEOID)
    print("Joining County Geometry...")
  } else {
    geometry <- tracts(state=st,county = ctys) %>%
      select(GEOID)
    print("Joining Tract Geometry...")
  }

  # convert to numeric if the dataset uses a numeric GEOID
  if(is.numeric(data$GEOID)) {
    geometry <- geometry %>%
      mutate(GEOID=as.numeric(GEOID))
  }

  # join data and geometry and convert to simple features
  output <- data %>%
    left_join(geometry) %>%
    st_as_sf()

  output
  }
}
