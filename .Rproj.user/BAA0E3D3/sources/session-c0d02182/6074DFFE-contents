
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
font_add("formata",
         "fonts/formata/FormataCondensed.ttf",
         "fonts/formata/FormataCondensed_Bold.ttf",
         "fonts/formata/FormataCondensed_Italic.ttf",
         "fonts/formata/FormataBold_Italic.ttf")

showtext_auto(enable = TRUE)

# setup environemnt
.dt_settings <- new.env(parent = emptyenv())


.dt_settings$default_pal_continuous <- RColorBrewer::brewer.pal(8,"Greens") # color palette for continuous data
.dt_settings$default_pal_1way <- c("#C8D8EB","#84B3C7", "#35889D","#196A7D","#004E60")
.dt_settings$pal_binary <- c("#D6D2C4","#C8D8EB") # palette for binary data

.dt_settings$default_pal_discrete <- rev(c("#522b4a","#00acbd","#858919","#696059","#cd163f","#E2DDCB","#000000","#b4bd00","#00667D"))
# line/point width defaults for line graph
.dt_settings$default_line_width <- 1.5 # sets the default width of the line(s)
.dt_settings$default_point_size <- 3 # sets the default size of the points

# default text label color for plots
.dt_settings$default_label_color <- "black" # sets the color of any text labels in graphs
.dt_settings$default_axis_text_size <- 25
.dt_settings$default_label_text_size <- 7.5
.dt_settings$default_legend_text_size <- 25
.dt_settings$default_graph_linewidth <- 1.5
.dt_settings$default_font <- "oswald"
