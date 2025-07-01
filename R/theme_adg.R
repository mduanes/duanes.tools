#' @name theme_adg
#'
#' @export
#'
#'
#'
theme_minimal() +
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
