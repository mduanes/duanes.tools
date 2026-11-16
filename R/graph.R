#' @name graph
#'
#' @export
#'
#'
#'

# creates a basic ggplot line chart given specifications
# options for graph are line, chart, and bar'


graph <- function(data,
                  x, # x field
                  y, # y field
                  x_lab = NULL, # x axis label
                  y_lab = NULL, # y axis label
                  base_font="default", # base font
                  disable_y = FALSE, # turn off y axis ticks entirely
                  commas_x = FALSE, # format x axis with commas
                  commas_y = FALSE, # format y axis with commas
                  pct_x = FALSE, # format x axis with percent marker
                  pct_y = FALSE, # format y axis with percent marker
                  graph_type = "line", # graph types: line, col, bar
                  groups=NULL, # group var, also controls fill for col and bar as well as color for line
                  label = NULL, # label field for observations
                  pal="default", # color palette
                  show_legend=FALSE, # show legend or not
                  legend_lab = "", # legend label
                  legend_pos = "top", # legend position
                  title = "", # graph title
                  pos=FALSE, # position field for col/bar (options: "dodge")
                  graph_linewidth = "default", # width of lines in graph elements (e.g. axis ticks/lines)
                  legend_rows = 2, # rows in legend
                  line_width = "default",  # line width for lines in graph (e.g. line graph)
                  point_size="default",  # size of points
                  label_color="default",  # color of labels
                  legend_text_size="default",
                  label_size="default",  # size of label text
                  caption = "", # caption if desired
                  axis_text_size="default",  # base size of non-label text
                  points = TRUE,
                  dyn_col = TRUE,
                  dollars_y=FALSE,
                  extremes=TRUE
                  ) {



  # load defaults
  base_font <- ifelse(base_font=="default",dt_params("default_font"),base_font)
  if(("default" %in% pal)) {pal <- dt_params("default_pal_discrete")}
  graph_linewidth <- ifelse(graph_linewidth=="default",dt_params("default_graph_linewidth"),graph_linewidth)
  line_width <- ifelse(line_width=="default",dt_params("default_line_width"),line_width)
  point_size <- ifelse(point_size=="default",dt_params("default_point_size"),point_size)
  label_color <- ifelse(label_color=="default",dt_params("default_label_color"),label_color)
  legend_text_size <- ifelse(legend_text_size=="default",dt_params("default_legend_text_size"),legend_text_size)
  label_size <- ifelse(label_size=="default",dt_params("default_label_text_size"),label_size)
  axis_text_size <- ifelse(axis_text_size=="default",dt_params("default_axis_text_size"),axis_text_size)

  #update_geom_defaults("text", list(family = base_font, size = 5))
  # set group aesthetic and x/y labels to defaults if not specified in function call
  if(is.null(groups)) {
    data <- data %>%
      mutate("no groups"="no groups")
    groups <- "no groups"
  }

  # set defaults for axis labels if not specified
  if(is.null(x_lab)) {
    x_lab <- x
  }
  if(is.null(y_lab)) {
    y_lab <- y
  }

  # establish ggplot object based on call
  g <- ggplot2::ggplot(data,mapping=ggplot2::aes(x=.data[[x]],
                                   y=.data[[y]],
                                   color=.data[[groups]],
                                   fill=.data[[groups]],
                                   group=.data[[groups]])) +
    # add basic aesthetic settings
    ggplot2::scale_color_manual(values=pal,name=legend_lab,drop=FALSE) +
    ggplot2::scale_fill_manual(values=pal,name=legend_lab,drop=FALSE) +
    ggplot2::theme_bw() +
    ggplot2::labs(title=title,x=x_lab,y=y_lab,caption=caption) +
    ggplot2::theme(axis.text = ggplot2::element_text(size=axis_text_size,
                                                     family=base_font),
                   axis.ticks = element_blank(),
                   axis.title.y=element_text(margin = margin(t = 0, r = 15, b = 0, l = 0),
                                             face="plain"),
                   axis.title.x=element_text(margin = margin(t = 15, r = 0, b = 0, l = 0),
                                             face="plain"),
          axis.title = ggplot2::element_text(size=axis_text_size,face = "plain",
                                             family=base_font),
          legend.text = ggplot2::element_text(size=legend_text_size*0.5,
                                              family=base_font),
          legend.title = ggplot2::element_text(size=legend_text_size,face="plain",
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

  # put legend on bottom if specified and add n rows
  if(tolower(legend_pos) == "bottom") {
    g <- g +
      ggplot2::guides(color=ggplot2::guide_legend(nrow=legend_rows))
  }

  # add geoms based on function specification
  if(tolower(graph_type)=="line") {
    g <- g +
      ggplot2::geom_line(lwd=line_width,show.legend = show_legend) +
      #geom_point(size=point_size) +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(face="plain",
                                              family=base_font))

    if(points == TRUE) {
      g <- g +
        ggplot2::geom_point(size=point_size)
    }
  }
  # bar chart
  if(tolower(graph_type)=="bar") {
    g <- g +
      ggplot2::geom_col(linewidth=0,
                        show.legend = show_legend) +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(face="plain",
                                              family=base_font))
  }
  # column chart, dodge position
  if(tolower(graph_type)=="col" & pos == "dodge") {
    g <- g +
      ggplot2::geom_col(linewidth=0,show.legend = show_legend,
               position = ggplot2::position_dodge(),
               color="white") +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(face="plain",
                                              family=base_font))
  }
  # column chart, not dodge position
  if(tolower(graph_type)=="col" & pos != "dodge") {
    g <- g +
      ggplot2::geom_col(linewidth=0,
                        show.legend = show_legend,
                        color="white") +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(face="plain",
                                              family=base_font))

  }
  # remove legend if specified
  if(show_legend == FALSE) {
    g <- g +
      ggplot2::theme(legend.position = "none")
  }

  # assign label colors
  if(dyn_col == TRUE) {
    label_colors <- assign_text_col(pal)
  } else {
    label_colors <- rep(label_color,length(pal))
  }

  if (!is.null(label) & graph_type != "line") {
    g <- g +
      #ggnewscale::new_scale_color() +
      # ggnewscale::new_scale_fill() +
      ggplot2::geom_text(mapping = ggplot2::aes(label = format(.data[[label]],
                                                               digits = 2),
                                                color=.data[[groups]],
                                                group=.data[[groups]]), fontface = "plain",
                         position = ggplot2::position_stack(vjust = 0.5),
                         size = label_size, family = base_font,
                         show.legend = FALSE) +
      scale_color_manual(values=label_colors,name = legend_lab, drop = FALSE)

  } else if(!is.null(label))  {
    g <- g +
      #ggnewscale::new_scale_color() +
      # ggnewscale::new_scale_fill() +
      ggplot2::geom_text(mapping = ggplot2::aes(label = format(.data[[label]],
                                                               digits = 2)),# fontface = "bold",
                         position = ggplot2::position_stack(vjust = 0.5),
                         size = label_size, family = base_font,
                         color="black",
                         show.legend = FALSE)

  }


  if(extremes == TRUE) {
    if(graph_type == "col") {
      extremes <- data %>%
        ungroup() %>%
        select(all_of(c(x,y))) %>%
        rename(x=1,
               y=2) %>%
        group_by(x) %>%
        summarize(y=sum(y,na.rm=TRUE),
                  .groups = "drop_last")

      min <- ifelse(min(extremes$y)>=0,0,min(extremes$y)*0.95)
      max <- max(extremes$y)*1.1

      breaks <- pretty(c(min,max))
      #print(breaks)
    } else if (graph_type == "line") {
      extremes <- data %>%
        ungroup() %>%
        select(all_of(c(x,y,groups))) %>%
        rename(x=1,
               y=2,
               group=3) %>%
        group_by(x,group) %>%
        summarize(y=sum(y,na.rm=TRUE),
                  .groups = "drop_last")

      min <- min(extremes$y)*0.95
      max <- max(extremes$y)*1.05

      breaks <- pretty(c(min,max))

    }
  } else {
    extremes <- data %>%
      ungroup() %>%
      select(all_of(c(x,y))) %>%
      rename(x=1,
             y=2) %>%
      group_by(x) %>%
      summarize(y=sum(y,na.rm=TRUE),
                .groups = "drop_last")

    min <- ifelse(min(extremes$y)>=0,0,min(extremes$y))
    max <- max(extremes$y)

    breaks <- pretty(c(min,max))
  }
  #g <- g + scale_y_continuous(expand = expansion(mult = c(expan, expan)))

  if (commas_x == TRUE & commas_y == FALSE) {
    g <- g + ggplot2::scale_x_continuous(labels = scales::comma)
  }
  if (commas_x == FALSE & commas_y == FALSE) {
    g <- g + ggplot2::scale_y_continuous(limits=c(min(breaks),max(breaks)),
                                         breaks=breaks)
  }
  if (commas_x == FALSE & commas_y == TRUE) {
    g <- g + ggplot2::scale_y_continuous(labels = scales::comma,
                                         limits=c(min(breaks),max(breaks)),
                                         breaks=breaks)
  }
  if (commas_x == TRUE & commas_y == TRUE) {
    g <- g + ggplot2::scale_y_continuous(labels = scales::comma,
                                         limits=c(min(breaks),max(breaks)),
                                         breaks=breaks) +
      ggplot2::scale_x_continuous(labels = scales::comma)
  }
  if (pct_x == TRUE & pct_y == FALSE) {
    g <- g + ggplot2::scale_x_continuous(labels = percent_marker)
  }
  if (pct_x == FALSE & pct_y == TRUE) {
    g <- g + ggplot2::scale_y_continuous(labels = percent_marker,
                                         limits=c(min(breaks),max(breaks)),
                                         breaks=breaks)
  }
  if (pct_x == TRUE & pct_y == TRUE) {
    g <- g + ggplot2::scale_y_continuous(labels = percent_marker,
                                         limits=c(min(breaks),max(breaks)),
                                         breaks=breaks) +
      ggplot2::scale_x_continuous(labels = percent_marker)
  }
  if (disable_y == TRUE) {
    g <- g + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                            axis.ticks.y = ggplot2::element_blank())
  }
  if(dollars_y == TRUE) {

    g <- g + ggplot2::scale_y_continuous(labels = scales::dollar,
                                         limits=c(min(breaks),max(breaks)),
                                         breaks=breaks)
  }

  g <- g +
    theme(legend.key.spacing.x = unit(1.0, "cm"))
  # return final object
  g
}
