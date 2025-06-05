###################
# GLOBAL SETTINGS #
###################

library(tidyverse)
library(lubridate)

# FIND LATEST YEAR FOR ACS ----
# set acs to max year if too high
y_acs <- year(Sys.Date()) - 2

# AESTHETICS ----

# default palettes based on data type
default_pal_continuous <- RColorBrewer::brewer.pal(8,"Greens") # color palette for continuous data

default_pal_2way <- c("#BA0C2F","#E4002B", "#D6D2C4", "#35889D","#004E60")
default_pal_1way <- c("#594A25","#D6D2C4","#C8D8EB", "#35889D","#004E60")

pal_binary <- c("#D6D2C4","#C8D8EB") # palette for binary data

default_pal_discrete <- c("#D6D2C4","#9EA2A2","#C8D8EB","#B4BD00","#00A3AD","#554F47","#004E60",
                          "#594A25","#BA0C2F","#000") # uga color palette

# line/point width defaults for line graph
default_line_width <- 3 # sets the default width of the line(s)
default_point_size <- 4 # sets the default size of the points

# default text label color for plots
default_label_color <- "black" # sets the color of any text labels in graphs
default_axis_text_size <- 25
default_graph_linewidth <- 1.5

# legend position for maps
default_legend_position <- c("right","top") # sets the default legend position for choropleth maps
# must be given as c(horizontal, vertical)
# can be given as numbers e.g. c(1,0)
# or text e.g. c("left","bottom")

# OTHER SETTINGS ----

# Decennial Race Variables
race_vars <- c(total = "P1_001N",
               white = "P1_003N",
               black = "P1_004N",
               asian = "P1_006N",
               hispanic = "P2_002N") # list of variable names for the race totals in the decennial
# census pull; might need to be changed if going to previous/future
# census


# themes
theme_adg_map <- theme_minimal() +
  theme(axis.text = element_blank(),
        plot.background = element_rect(fill="white",linewidth = 0),
        panel.grid = element_blank(),
        legend.text = element_text(size=default_axis_text_size*0.75),
        legend.title = element_text(size=default_axis_text_size,face="bold"),
        plot.caption = element_text(size=0.6*default_axis_text_size,face="italic",
                                    hjust=0.5))

theme_adg <- theme_minimal() +
  theme(axis.text = element_text(size=default_axis_text_size),
        axis.title = element_text(size=default_axis_text_size,face = "bold"),
        legend.text = element_text(size=default_axis_text_size*0.75),
        legend.title = element_text(size=default_axis_text_size,face="bold"),
        legend.position = default_legend_position,
        panel.grid = element_line(linewidth=default_graph_linewidth),
        panel.grid.minor = element_line(linetype="dashed"),
        panel.border = element_rect(linewidth=default_graph_linewidth*1.5),
        plot.caption = element_text(size=0.6*default_axis_text_size,face="italic",
                                    hjust=0.5))

theme_set(theme_adg)

