#' @name acs_comprehensive
#' @export
#'
#'


# Loads ACS data for a set of geographies


acs_comprehensive <- function(geography="county", # geo levels: tracts, counties, state
                                  adg_path=Sys.getenv("ADG_KEY"), # pulls ADG path
                                  counties=c("Clarke","Oconee"), # note: req. even if geography is state, but does not affect output if that is the case
                                  state=13, # irrelevant if multi state = TRUE
                                  specify_vars = FALSE, # specify variables instead of pulling a list
                                  specified_vars = NULL, # specific vars to pull
                              y=y_acs, # year of data
                              survey="acs5", # survey to pull from
                              time_series=FALSE, # if true, pull past 3 non overlapping acs
                              multi_state=FALSE # set true if pulling data for geographies across state lines
                              ) {

  # multi state method ----
  if(multi_state==TRUE) {

    # if searching for multiple varaibles not on the standard metadata list
    if (specify_vars == TRUE) {
      # load metadata for specific var codes
      metadata <- load_variables(y,survey) %>%
        filter(name %in% specified_vars)

      vars <- metadata$name

      colnames(metadata) <- c("variable","var_name","description","geography")

      # if not specified, use general list
    } else if(Sys.getenv("ADG_KEY") == "") {
      print("No ADG Path! Set using install_adg_key()")
      # notify if adg path not found
    } else {
      # read in standard metadata
      metadata <- read_tsv(paste0(adg_path,"Data/ACS Metadata/acs_vars_metadata_long_2024.txt")) %>%
        # specify some specific variable groups
        mutate(var_group=case_when(var_name=="Rent50pct"~"Rent50",
                                   var_name=="Own50pct"~"Own50",
                                   TRUE~var_group))

      # pull out variables and specify desired vars
      vars <- metadata$variable
      # add extra vars
      vars <- c(vars, "B17001_001",
                "B17001_004","B17001_005","B17001_006","B17001_007","B17001_008","B17001_009","B17001_015","B17001_016",
                "B17001_018","B17001_019","B17001_020","B17001_021","B17001_022","B17001_023","B17001_029","B17001_030")
    }

    # go county by county to get fips for multiple states
    if(is.na(as.numeric(counties[1]))) {
      counties <- get_fips(counties)
    }

    # get data for each county one by one
    for(county in counties) {
    state_fip <- str_trunc(county,2,"right","")
    cty_fip <- str_trunc(county,3,"left","")
    # time series
    if(time_series == TRUE) {
      years <- c(y-10,y-5,y)

      for(y in years) {
          output_loop <- get_acs(geography = geography,
                                 county = cty_fip,
                                 state = state_fip,
                                 variable = vars,
                                 year = y,
                                 survey = 'acs5',
                                 geometry = FALSE) %>%
            left_join(metadata,by="variable",relationship = "many-to-many") %>%
            mutate("SERIES"=paste0(y-4,"-",y))
        if (y == years[1]) {
          output_cty <- output_loop
        } else {
          output_cty <- output_cty %>%
            rbind(output_loop)
        }
      }
      # non time series (so no loop)
    } else {
        output_cty <- get_acs(geography = geography,
                          county = cty_fip,
                          state = state_fip,
                          variable = vars,
                          year = y,
                          survey = survey,
                          geometry = FALSE) %>%
          left_join(metadata,by="variable",relationship = "many-to-many")

    }

    # append output into final file
    if (county == counties[1]) {
      output <- output_cty
    } else {
      output <- output %>%
        rbind(output_cty)
    }
    # end of multi state method
  }

  # non multi state method ----
  } else {
    # if wanted specific varaibles off of main list
  if (specify_vars == TRUE) {
    # load metadata for specific var codes
    metadata <- load_variables(y,survey) %>%
      filter(name %in% specified_vars)

    vars <- metadata$name

    colnames(metadata) <- c("variable","var_name","description","geography")

    # if not specified, use general list
  } else if(Sys.getenv("ADG_KEY") == "") {
    print("No ADG Path! Set using install_adg_key()")
  } else {

    metadata <- read_tsv(paste0(adg_path,"Data/ACS Metadata/acs_vars_metadata_long_2024.txt")) %>%
      # specify some specific variable groups
      mutate(var_group=case_when(var_name=="Rent50pct"~"Rent50",
                                 var_name=="Own50pct"~"Own50",
                                 TRUE~var_group))

    # pull out variables and specify desired vars
    vars <- metadata$variable
    # add some extra vars not on main list
    vars <- c(vars, "B17001_001",
              "B17001_004","B17001_005","B17001_006","B17001_007","B17001_008","B17001_009","B17001_015","B17001_016",
              "B17001_018","B17001_019","B17001_020","B17001_021","B17001_022","B17001_023","B17001_029","B17001_030")
  }

  # time series
  if(time_series == TRUE) {
    years <- c(y_acs-9,y_acs-4,y_acs)

    # go year by year
 for(y in years) {
      output_loop <- get_acs(geography = geography,
                        county = counties,
                        state = state,
                        variable = vars,
                        year = y,
                        survey = 'acs5',
                        geometry = FALSE) %>%
        left_join(metadata,by="variable",relationship = "many-to-many") %>%
        mutate("SERIES"=paste0(y-4,"-",y))

  # append output to final file
   if (y == years[1]) {
     output <- output_loop
   } else {
     output <- output %>%
       rbind(output_loop)
   }
 }
    # non time series (so no loop)
  } else {
    output <- get_acs(geography = geography,
                      county = counties,
                      state = state,
                      variable = vars,
                      year = y,
                      survey = survey,
                      geometry = FALSE) %>%
      left_join(metadata,by="variable",relationship = "many-to-many")

  }
  }

  # return final output regardless of method
  output
}
