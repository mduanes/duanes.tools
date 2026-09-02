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
default_label_text_size = 25/3,
default_legend_text_size = 25,
default_graph_linewidth = 1.5,
# legend position for maps
default_legend_position = c("right","top"), # sets the default legend position for choropleth maps
default_font = "sans",
man_override = TRUE
) {

  .dt_settings$default_pal_continuous <-default_pal_continuous # color palette for continuous data

  .dt_settings$default_pal_1way <-default_pal_1way
  .dt_settings$pal_binary <-pal_binary # palette for binary data

  .dt_settings$default_pal_discrete <-default_pal_discrete # uga color palette

  # line/point width defaults for line graph
  .dt_settings$default_line_width <-default_line_width # sets the default width of the line(s)
  .dt_settings$default_point_size <-default_point_size # sets the default size of the points

  # default text label color for plots
  .dt_settings$default_label_color <-default_label_color# sets the color of any text labels in graphs
  .dt_settings$default_axis_text_size <-default_axis_text_size
  .dt_settings$default_label_text_size <-default_label_text_size
  .dt_settings$default_legend_text_size <-default_legend_text_size
  .dt_settings$default_graph_linewidth <-default_graph_linewidth
  .dt_settings$default_font <-default_font


  # update relevant functions
  # formals(graph)$label_color <-default_label_color
  # formals(graph)$base_font <-default_font
  # formals(graph)$pal <-default_pal_discrete
  # formals(graph)$graph_linewidth <-as.numeric(default_graph_linewidth)
  # formals(graph)$line_width <- as.numeric(default_line_width)
  # formals(graph)$point_size <- as.numeric(default_point_size)
  # formals(graph)$label_color <-default_label_color
  # formals(graph)$legend_text_size <- as.numeric(default_legend_text_size)
  # formals(graph)$label_size <- as.numeric(default_label_text_size)
  # formals(graph)$axis_text_size <-as.numeric(default_axis_text_size)
  #
  # formals(choropleth)$pal <-default_pal_discrete
  # formals(choropleth)$label_size <-as.numeric(default_label_text_size) -3.3
  # formals(choropleth)$legend_text_size <-as.numeric(default_legend_text_size)
  # formals(choropleth)$line_width <-as.numeric(default_graph_linewidth)
  # formals(choropleth)$axis_text_size <-as.numeric(default_axis_text_size)
}
