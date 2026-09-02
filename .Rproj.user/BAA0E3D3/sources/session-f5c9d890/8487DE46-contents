#' @name pull_digest
#' @export

# fetches tax digest for county
pull_digest <- function(ctys="Clarke", # can be multiple
                        series="2000_24") {

  # series registry
  series_reg <- c("2000_24")

  # make path but check if ADG KEY exists
  if(Sys.getenv("ADG_KEY") == "") {
    print("No ADG Path! Set using install_adg_key()")
  } else {

  path <- paste0(get_adg_key(),"Data/Parcel Data/output/tax digest/county tax digests/")

  # check that series is proper
  if(!(series %in% series_reg)) {
    message(paste0("Invalid series. Valid series are: ",series_reg))
  } else {

    # fetch data for each county and append
    for(c in ctys) {
      p_loop <- paste0(path,series,"/",toupper(c),".csv")
      d <- readr::read_csv(p_loop)
      # append
      if(c == ctys[1] | length(ctys) == 1) {
        output <- d
      } else {
        output <- output %>%
          rbind(d)
      }

    }
    # return data
    output %>%
      select(COUNTY,GEOID,JURISDICTION,JUR_CODE,TAXYEAR,LANDUSE,DIGCLASS,DIGSTRAT,VARIABLE,VALUE,DIGCLASS_LONG,DIGSTRAT_LONG)

  }
  }
   }
