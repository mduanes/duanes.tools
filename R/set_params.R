#' @name set_params
#'
#' @export
#'
#'
#'

set_params <-  function(
default_pal_continuous = RColorBrewer::brewer.pal(8,"Greens"), # color palette for continuous data,
default_pal_1way = c("#C8D8EB","#84B3C7", "#35889D","#196A7D","#004E60"),
pal_binary = c("#D6D2C4","#C8D8EB"), # palette for binary data,
default_pal_discrete = c("#D6D2C4","#9EA2A2","#C8D8EB","#B4BD00","#00A3AD","#554F47","#004E60",
                          "#594A25","#BA0C2F","#000"), # uga color palette,
# line/point width defaults for line graph
default_line_width = 2, # sets the default width of the line(s)
default_point_size = 4, # sets the default size of the points
# default text label color for plots
default_label_color = "black", # sets the color of any text labels in graphs
default_axis_text_size = 25,
default_graph_linewidth = 1.5,
# legend position for maps
default_legend_position = c("right","top"), # sets the default legend position for choropleth maps
default_font = "sans",
man_override = TRUE
) {

  default_pal_continuous <<- default_pal_continuous # color palette for continuous data

  default_pal_1way <<- default_pal_1way
  pal_binary <<- pal_binary # palette for binary data

  default_pal_discrete <<- default_pal_discrete # uga color palette

  # line/point width defaults for line graph
  default_line_width <<- default_line_width # sets the default width of the line(s)
  default_point_size <<- default_point_size # sets the default size of the points

  # default text label color for plots
  default_label_color <<- default_label_color# sets the color of any text labels in graphs
  default_axis_text_size <<- default_axis_text_size
  default_graph_linewidth <<- default_graph_linewidth
  default_font <<- default_font

  dt_params_manual_override <<- man_override
}
