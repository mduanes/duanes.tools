#' @name acs_comprehensive
#' @export
#'
acs_comprehensive <- function(geography="county",
                                  adg_path=Sys.getenv("ADG_KEY"),
                                  counties=c("Clarke","Oconee"),state=13,
                                  specify_vars = FALSE,
                                  y=y_acs,
                                  specified_vars = NULL,
                              survey="acs5",
                              time_series=FALSE,
                              multi_state=FALSE) {
  if(multi_state==TRUE) {

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
      vars <- c(vars, "B17001_001",
                "B17001_004","B17001_005","B17001_006","B17001_007","B17001_008","B17001_009","B17001_015","B17001_016",
                "B17001_018","B17001_019","B17001_020","B17001_021","B17001_022","B17001_023","B17001_029","B17001_030")
    }

    # go county by county
    if(is.na(as.numeric(counties[1]))) {
      counties <- get_fips(counties)
    }

    for(county in counties) {
    state_fip <- str_trunc(county,2,"right","")
    cty_fip <- str_trunc(county,3,"left","")
    # time series
    if(time_series == TRUE) {
      years <- c(y_acs-9,y_acs-4,y_acs)

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
          output <- output_loop
        } else {
          output <- output %>%
            rbind(output_loop)
        }
      }
      # non time series
    } else {
        output <- get_acs(geography = geography,
                          county = cty_fip,
                          state = state_fip,
                          variable = vars,
                          year = y,
                          survey = survey,
                          geometry = FALSE) %>%
          left_join(metadata,by="variable",relationship = "many-to-many")

    }
  }

  # non multi state method ----
  } else {
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
    vars <- c(vars, "B17001_001",
              "B17001_004","B17001_005","B17001_006","B17001_007","B17001_008","B17001_009","B17001_015","B17001_016",
              "B17001_018","B17001_019","B17001_020","B17001_021","B17001_022","B17001_023","B17001_029","B17001_030")
  }

  # time series
  if(time_series == TRUE) {
    years <- c(y_acs-9,y_acs-4,y_acs)

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

   if (y == years[1]) {
     output <- output_loop
   } else {
     output <- output %>%
       rbind(output_loop)
   }
 }
    # non time series
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
  output
  }
  output
}
