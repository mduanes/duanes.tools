
#' @name theme_adg
#' @export

# ggplot2 theme for maps

theme_adg <- function() {

  # load defaults
  base_font <- dt_params("default_font")
  pal <- dt_params("default_pal_discrete")
  graph_linewidth <- dt_params("default_graph_linewidth")
  line_width <- dt_params("default_line_width")
  point_size <- dt_params("default_point_size")
  label_color <- dt_params("default_label_color")
  legend_text_size <- dt_params("default_legend_text_size")
  label_size <- dt_params("default_label_text_size")
  axis_text_size <- dt_params("default_axis_text_size")


  ggplot2::theme_bw() +
  ggplot2::theme(axis.text = ggplot2::element_text(size=axis_text_size,
                                                     family=base_font),
                   axis.title.y=element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
                   axis.title.x=element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
                   axis.title = ggplot2::element_text(size=axis_text_size,face = "bold",
                                                      family=base_font),
                   legend.text = ggplot2::element_text(size=legend_text_size*0.5,
                                                       family=base_font),
                   legend.title = ggplot2::element_text(size=legend_text_size,face="bold",
                                                        family=base_font),
                   legend.position = legend_pos,
                   panel.grid = ggplot2::element_line(linewidth=graph_linewidth/2),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
                   axis.ticks.x = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(size=0.6*axis_text_size,face="italic",
                                                        hjust=0.5,
                                                        family=base_font),
                   strip.text = ggplot2::element_text(size=axis_text_size,face="bold",
                                                      family=base_font),
                   strip.background = ggplot2::element_blank())
}
