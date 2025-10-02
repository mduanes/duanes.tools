#' @name graph
#'
#' @export
#'
#'
#'
library(ggplot2)
library(dplyr)

source("R/settings.R")

# creates a basic ggplot line chart given specifications
# options for graph are line, chart, and bar
graph <- function(data, x, y, graph_type = "line", groups=NULL,
                  pal=default_pal_discrete,
                  show_legend=FALSE,
                  title = NULL, x_lab = NULL, y_lab = NULL, legend_lab = "Legend",
                  commas_x = FALSE, commas_y = FALSE,
                  pct_x = FALSE, pct_y = FALSE,
                  label = NULL, legend_pos = "right",
                  disable_y = FALSE,
                  graph_linewidth = default_graph_linewidth,
                  legend_rows = 2,
                  line_width = default_line_width, point_size=default_point_size,
                  label_color=default_label_color,
                  label_size=default_axis_text_size/3,
                  caption = NULL,axis_text_size=default_axis_text_size) {

  # set group aesthetic and x/y labels to defaults if not specified in function call
  if(is.null(groups)) {
    data <- data %>%
      mutate("no groups"="no groups")
    groups <- "no groups"
  }

  if(is.null(x_lab)) {
    x_lab <- x
  }
  if(is.null(y_lab)) {
    y_lab <- y
  }

  # establish ggplot object based on call
  graph <- ggplot(data,mapping=aes(x=.data[[x]],
                                   y=.data[[y]],
                                   color=.data[[groups]],
                                   fill=.data[[groups]],
                                   group=.data[[groups]])) +
    # add basic aesthetic settings
    scale_color_manual(values=pal,name=legend_lab) +
    scale_fill_manual(values=pal,name=legend_lab) +
    theme_bw() +
    labs(title=title,x=x_lab,y=y_lab,caption=caption) +
    theme(axis.text = element_text(size=axis_text_size),
          axis.title = element_text(size=axis_text_size,face = "bold"),
          legend.text = element_text(size=axis_text_size*0.5),
          legend.title = element_text(size=axis_text_size,face="bold"),
          legend.position = legend_pos,
          panel.grid = element_line(linewidth=graph_linewidth/2),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          axis.ticks.x = element_blank(),
          plot.caption = element_text(size=0.6*axis_text_size,face="italic",
                                      hjust=0.5),
          strip.text = element_text(size=axis_text_size,face="bold"),
          strip.background = element_blank())

  if(tolower(legend_pos) == "bottom") {
    graph <- graph +
      guides(color=guide_legend(nrow=legend_rows))
  }

  # add geoms based on function specification
  if(tolower(graph_type)=="line") {
    graph <- graph +
      geom_line(lwd=line_width,show.legend = show_legend) +
      #geom_point(size=point_size) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text = element_text(face="bold"))
  }

  if(tolower(graph_type)=="bar") {
    graph <- graph +
      geom_col(linewidth=0,show.legend = show_legend) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text = element_text(face="bold"))
  }

  if(tolower(graph_type)=="col") {
    graph <- graph +
      geom_col(linewidth=0,show.legend = show_legend) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text = element_text(face="bold"))
  }

  # remove legend if specified
  if(show_legend == FALSE) {
    graph <- graph +
      theme(legend.position = "none")
  }

  # add labels to graph if specified
  if(!is.null(label)) {
    graph <- graph +
      geom_text(mapping=aes(label=format(.data[[label]],digits=2)),
                fontface="bold",color=label_color,
                position=position_stack(vjust=0.5),
                size=label_size)
  }

  # add axis comma/pct formatting if specified
  if (commas_x == TRUE & commas_y == FALSE) {
    graph <- graph +
      scale_x_continuous(labels = scales::comma)
  }

  if (commas_x == FALSE & commas_y == TRUE) {
    graph <-  graph +
      scale_y_continuous(labels = scales::comma)
  }

  if (commas_x == TRUE & commas_y == TRUE) {
    graph <- graph +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma)
  }

  if (pct_x == TRUE & pct_y == FALSE) {
    graph <- graph +
      scale_x_continuous(labels = percent_marker)
  }

  if (pct_x == FALSE & pct_y == TRUE) {
    graph <-  graph +
      scale_y_continuous(labels = percent_marker)
  }

  if (pct_x == TRUE & pct_y == TRUE) {
    graph <- graph +
      scale_y_continuous(labels = percent_marker) +
      scale_x_continuous(labels = percent_marker)
  }

  if (disable_y == TRUE) {
    graph <- graph +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }

  # return final object
  graph
}
