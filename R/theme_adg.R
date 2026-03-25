
#' @name theme_adg
#' @export

# ggplot2 theme for maps

theme_adg <- function() {

  ggplot2::theme_bw() +
  ggplot2::labs(title=title,x=x_lab,y=y_lab,caption=caption) +
  ggplot2::theme(axis.text = ggplot2::element_text(size=axis_text_size),
                 axis.title = ggplot2::element_text(size=axis_text_size,face = "bold"),
                 legend.text = ggplot2::element_text(size=axis_text_size*0.5),
                 legend.title = ggplot2::element_text(size=axis_text_size,face="bold"),
                 legend.position = legend_pos,
                 panel.grid = ggplot2::element_line(linewidth=graph_linewidth/2),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.border = ggplot2::element_blank(),
                 legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
                 axis.ticks.x = ggplot2::element_blank(),
                 plot.caption = ggplot2::element_text(size=0.6*axis_text_size,face="italic",
                                                      hjust=0.5),
                 strip.text = ggplot2::element_text(size=axis_text_size,face="bold"),
                 strip.background = ggplot2::element_blank())
}
