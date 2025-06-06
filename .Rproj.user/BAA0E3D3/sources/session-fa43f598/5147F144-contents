#' @name load_dependencies
#' @export
#' loads depdencies if missing

## Load or install packages
packages <- function(x) {
  x <- deparse(substitute(x))
  installed_packages <- as.character(installed.packages()[, 1])

  if (length(intersect(x, installed_packages)) == 0) {
    install.packages(pkgs = x, dependencies = TRUE, repos = "http://cran.r-project.org")
  }

  library(x, character.only = TRUE)
  rm(installed_packages) # Remove From Workspace
}

# packages(openxlsx)  # for reading/writing files as Excel workbooks'
library(ggnewscale)
packages(plyr)
packages(tidyverse)
packages(tmap)
packages(tidycensus)
packages(tigris)
packages(sf)
packages(ggrepel)
packages(RColorBrewer)
packages(lubridate)
packages(knitr)
packages(fredr)
packages(BAMMtools)
packages(readxl)
packages(flextable)
#packages(officedown)
#packages(officer)
packages(data.table)


