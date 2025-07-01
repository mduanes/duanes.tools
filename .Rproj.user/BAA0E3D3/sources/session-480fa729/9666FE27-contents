#' @name get_acs_multiple_vars
#'
#' @export
#'
#'
#'
library(ggplot2)
library(dplyr)
library(tidycensus)

source("R/settings.R")
get_acs_multiple_vars <- function(geography="county",
                                  counties=ctys,state=st,
                                  specify_vars = FALSE,
                                  y=y_acs,
                                  specified_vars = NULL) {

  if (specify_vars == TRUE) {
    # load metadata for specific var codes
    metadata <- load_variables(y,"acs5") %>%
      filter(name %in% specified_vars)

    vars <- metadata$name

    colnames(metadata) <- c("variable","var_name","description","geography")

    # if not specified, use general list
  } else {
    metadata <- read_tsv("D. data/acs_vars_metadata_long_2024.txt") %>%
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

  if (length(state) == 1) {
    output <- get_acs(geography = geography,
                      county = counties,
                      state = state,
                      variable = vars,
                      year = y,
                      survey = 'acs5',
                      geometry = FALSE) %>%
      left_join(metadata)
  } else {
    for (s in state) {
      counties_l <- str_remove(counties[str_detect(counties,s)],",.*")
      output_l <- get_acs(geography = geography,
                          county = counties_l,
                          state = s,
                          variable = vars,
                          year = y,
                          survey = 'acs5',
                          geometry = FALSE) %>%
        left_join(metadata)

      if(s == state[1]) {
        output <- output_l
      } else {
        output <- output %>%
          rbind(output_l)
      }
    }
  }
  output
}

# same as above function but for three 5-year acs periods
get_acs_time_series <- function(geography="county",
                                counties=ctys,state=st,
                                specify_vars = FALSE,
                                specified_vars = NULL) {

  if (specify_vars == TRUE) {
    # load metadata
    metadata <- load_variables(y_acs,"acs5") %>%
      filter(name %in% specified_vars)

    vars <- metadata$name

    colnames(metadata) <- c("variable","var_name","description","geography")
  } else {
    metadata <- read_tsv("D. data/acs_vars_metadata_long_2024.txt") %>%
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

  # set years for time series
  years <- c(y_acs-9,y_acs-4,y_acs)

  # run for each year

  for (i in years) {
    output_loop <- get_acs_multiple_vars(y_acs = i) %>%
      mutate("year"=i)

    if (i == years[1]) {
      output <- output_loop
    } else {
      output <- output %>%
        rbind(output_loop)
    }
  }
  output %>%
    left_join(metadata)
}
