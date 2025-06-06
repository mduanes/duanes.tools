#' @name join_geometries
#' @export
#'
#'
# function to automatically join the relevant geometries from tigris
library(tigris)
library(sf)
library(dplyr)

join_geometries <- function(data,method="GEOID",joinfield="GEOID",st=13) {
  if(method == "GEOID") {
  if(!("GEOID" %in% colnames(data))) {
    print("No GEOID Field! Cannot Join!")
  } else {
    # pull field for testing
  GEOID <- data %>%
    pull(joinfield)

  # detect geometry type and load appropriate tigris shapefile
  if (nchar(GEOID[1]) == 5) {
    geometry <- counties(state=st) %>%
      select(GEOID)
    print("Joining County Geometry...")
  } else {
    geometry <- tracts(state=st,county = ctys) %>%
      select(GEOID)
    print("Joining Tract Geometry...")
  }

  # convert to numeric if the dataset uses a numeric GEOID
  if(is.numeric(GEOID)) {
    geometry <- geometry %>%
      mutate(GEOID=as.numeric(GEOID))
  }

  # join data and geometry and convert to simple features
  output <- data %>%
    left_join(geometry,by=c(joinfield="GEOID")) %>%
    st_as_sf()

  output
  }
    # county name method
  } else if (method == "Name") {

    print("Joining County Geometry...")

    # pull list of names
    state_name <- states() %>%
      filter(STATEFP==st) %>%
      pull(NAME)
    names <- data %>%
      pull(joinfield)
    # load geometry
    geometry <- counties(state=st) %>%
      select(NAME)
    # change to match source data
    if(str_detect(names[1]," County")) {
      geometry <- geometry %>%
        mutate(NAME=paste0(Name, " County"))
    } else if (str_detect(names[1]," County, ", state_name)) {
      geometry <- geometry %>%
        mutate(NAME=paste0(Name, " County, Georgia"))
    } else {
      print("Unrecognized Name Format. Recognized formats: NAME; NAME County; NAME County, STATE")
    }
    # join data and geometry and convert to simple features
    output <- data %>%
      left_join(geometry,by=c(joinfield="NAME")) %>%
      st_as_sf()

    output

  } else {
    print("Unrecognized method. Avalible methods: GEOID, Name")
  }
}
