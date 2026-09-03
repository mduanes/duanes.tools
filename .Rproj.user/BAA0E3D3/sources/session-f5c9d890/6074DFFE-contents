
###################
# GLOBAL SETTINGS #
###################

library(tidyverse)
library(lubridate)

# FIND LATEST YEAR FOR ACS ----
# set acs to max year if too high
y_acs <- year(Sys.Date()) - 2


library(showtext)

font_add_google("Oswald", "oswald")

showtext_auto(enable = TRUE)

# setup environemnt
.dt_settings <- new.env(parent = emptyenv())


.dt_settings$default_pal_continuous <- RColorBrewer::brewer.pal(8,"Greens") # color palette for continuous data
.dt_settings$default_pal_1way <- c("#C8D8EB","#84B3C7", "#35889D","#196A7D","#004E60")
.dt_settings$pal_binary <- c("#D6D2C4","#C8D8EB") # palette for binary data

.dt_settings$default_pal_discrete <- c("#D6D2C4","#9EA2A2","#C8D8EB","#B4BD00","#00A3AD","#554F47","#004E60",
                                       "#594A25","#BA0C2F","#000")  # uga color palette

# line/point width defaults for line graph
.dt_settings$default_line_width <- 2 # sets the default width of the line(s)
.dt_settings$default_point_size <- 4 # sets the default size of the points

# default text label color for plots
.dt_settings$default_label_color <- "black" # sets the color of any text labels in graphs
.dt_settings$default_axis_text_size <- 25
.dt_settings$default_label_text_size <- 7.5
.dt_settings$default_legend_text_size <- 25
.dt_settings$default_graph_linewidth <- 1.5
.dt_settings$default_font <- "sans"
