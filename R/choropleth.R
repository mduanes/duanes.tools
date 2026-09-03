#' @name choropleth
#' @export

# creates choropleth maps quickly
# returns a choropleth map of a single variable
choropleth <- function(data,
                       col, # column to use as color
                       pal="default", # palette
                       show_legend=FALSE, # show legend or not
                       title = NULL, # title of graph
                       legend_lab = "Legend", # label of legend
                       legend_pct=FALSE, # include % marker on legend or not (gradient only)
                       type="discrete", # types: discrete, gradient
                       caption=NULL, # caption if specified
                       label=NA, # field to use to label geometries
                       label_size="default", # size of label
                       axis_text_size="default", # base size of non-label text
                       legend_text_size ="default",
                       line_width="default", # width of lines
                       midpoint=NA,
                       maxpoint=NA,
                       minpoint=NA,
                       base_font="default",
                       grad_transform="identity"
                       ) {

  base_font <- ifelse(base_font=="default",dt_params("default_font"),base_font)
  if(("default" %in% pal)) {pal <- dt_params("default_pal_discrete")}
  graph_linewidth <- ifelse(graph_linewidth=="default",dt_params("default_graph_linewidth"),graph_linewidth)
  line_width <- ifelse(line_width=="default",dt_params("default_line_width"),line_width)
  #point_size <- ifelse(point_size=="default",dt_params("default_point_size"),point_size)
  #label_color <- ifelse(label_color=="default",dt_params("default_label_color"),label_color)
  legend_text_size <- ifelse(legend_text_size=="default",dt_params("default_legend_text_size"),legend_text_size)
  label_size <- ifelse(label_size=="default",dt_params("default_label_text_size"),label_size)
  axis_text_size <- ifelse(axis_text_size=="default",dt_params("default_axis_text_size"),axis_text_size)


  # DRAW LARGEST TO SMALLEST
  data <- data %>%
    dplyr::mutate(size=sf::st_area(geometry)) %>%
    dplyr::arrange(-size)

  # calc min max and midpoint of col field for gradient
  if(type == "gradient") {
  minpoint <- ifelse(!is.na(minpoint),midpoint,min(data[[col]]))
  midpoint <- ifelse(!is.na(midpoint),midpoint,mean(data[[col]]))
  maxpoint <- ifelse(!is.na(maxpoint),midpoint,max(data[[col]]))
  }
  # add label if specified
  if(!is.na(label)) {
  data <- data %>%
    dplyr::rename("graph_lab"=sym(label))
  }

  if(type=="discrete") {
    # base case discrete categories

    graph <- ggplot2::ggplot(data,mapping=aes(fill=.data[[col]])) +
      # add basic aesthetic settings
      ggplot2::scale_fill_manual(values=pal,name=legend_lab,drop=FALSE) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title=title,caption=caption)  +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
            legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
            panel.grid = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size=default_legend_text_size*0.5,
                                                family=base_font),
            legend.title = ggplot2::element_text(size=default_legend_text_size,face="bold",
                                                 family=base_font),
            plot.caption = ggplot2::element_text(size=0.6*axis_text_size,face="italic",
                                        hjust=0.5,
                                        family=base_font),
            strip.text = ggplot2::element_text(size=axis_text_size,face="bold",
                                               family=base_font),
            strip.background = ggplot2::element_blank()) +
      ggplot2::geom_sf(linewidth=line_width/1.5,color="white",show.legend = show_legend)

    # alt case where we want continuous variables
    } else if(type=="gradient") {
    graph <- ggplot2::ggplot(data,mapping=aes(fill=.data[[col]])) +
      ggplot2::scale_fill_gradient2(high=pal[5],
                           mid=pal[3],
                           low=pal[1],
                           name=legend_lab,
                           midpoint=midpoint,
                           transform=grad_transform,
                           labels=scales::comma,
                           breaks=c(minpoint,
                                    0,
                                    midpoint,
                                    maxpoint)) +
      #guides(fill=guide_legend(ncol=1)) +
      # add basic aesthetic settings
      ggplot2::theme_minimal() +
      ggplot2::labs(title=title,caption=caption)  +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
            legend.text = ggplot2::element_text(size=default_legend_text_size*0.5,
                                                family=base_font),
            legend.title = ggplot2::element_text(size=default_legend_text_size,face="bold",
                                                 family=base_font),
            plot.caption = ggplot2::element_text(size=0.6*axis_text_size,face="italic",
                                        hjust=0.5,
                                        family=base_font),
            strip.text = ggplot2::element_text(size=axis_text_size,face="bold",
                                               family=base_font),
            strip.background = ggplot2::element_blank()) +
      ggplot2::geom_sf(linewidth=line_width/1.5,color="white",show.legend = show_legend)

    # add percent marker to legend if requested
    if (legend_pct == TRUE) {
      graph <- graph +
        ggplot2::scale_fill_gradient2(high=pal[5],
                             mid=pal[3],
                             low=pal[1],name=legend_lab,
                             midpoint=midpoint,
                             labels=percent_marker,
                             breaks=c(min(data[[col]]),
                                      0,
                                      midpoint,
                                      max(data[[col]])))
      # limits=c(floor(min(data[[col]])/10)*10,
      #          ceiling(max(data[[col]])/10)*10))
    }
  } else {
    print("Invalid graph type")
  }
  # remove legend if specified
  if(show_legend == FALSE) {
    graph <- graph +
      ggplot2::theme(legend.position = "none")
  }
  # add labels if specified
  if(!is.na(label)) {
    graph <- graph +
      ggplot2::geom_sf_text(aes(label=graph_lab),size=label_size,fontface="bold",
                            family=base_font) +
      ggplot2::labs(x="",y="")
  }
  # return output
  graph
}
