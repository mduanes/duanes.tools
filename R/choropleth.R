#' @name choropleth
#' @export


# get base settings
source("R/settings.R")

# creates choropleth maps quickly
# returns a choropleth map of a single variable
choropleth <- function(data,
                       col, # column to use as color
                       pal=default_pal_discrete, # palette
                       show_legend=FALSE, # show legend or not
                       title = NULL, # title of graph
                       legend_lab = "Legend", # label of legend
                       legend_pct=FALSE, # include % marker on legend or not (gradient only)
                       type="discrete", # types: discrete, gradient
                       caption=NULL, # caption if specified
                       label=NA, # field to use to label geometries
                       label_size=5, # size of label
                       axis_text_size=default_axis_text_size, # base size of non-label text
                       line_width=default_graph_linewidth # width of lines
                       ) {
  # calc midpoint of col field for gradient
  midpoint <- mean(data[[col]])

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
            legend.text = ggplot2::element_text(size=axis_text_size*0.5),
            legend.title = ggplot2::element_text(size=axis_text_size,face="bold"),
            plot.caption = ggplot2::element_text(size=0.6*axis_text_size,face="italic",
                                        hjust=0.5),
            strip.text = ggplot2::element_text(size=axis_text_size,face="bold"),
            strip.background = ggplot2::element_blank()) +
      ggplot2::geom_sf(linewidth=line_width/1.5,color="white",show.legend = show_legend)

    # alt case where we want continuous variables
    } else if(type=="gradient") {
    graph <- ggplot2::ggplot(data,mapping=aes(fill=.data[[col]])) +
      ggplot2::scale_fill_gradient2(high=pal[5],
                           mid=pal[3],
                           low=pal[1],name=legend_lab,
                           midpoint=midpoint,
                           labels=scales::comma,
                           breaks=c(min(data[[col]]),
                                    0,
                                    midpoint,
                                    max(data[[col]]))) +
      #guides(fill=guide_legend(ncol=1)) +
      # add basic aesthetic settings
      ggplot2::theme_minimal() +
      ggplot2::labs(title=title,caption=caption)  +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
            legend.text = ggplot2::element_text(size=axis_text_size*0.5),
            legend.title = ggplot2::element_text(size=axis_text_size,face="bold"),
            plot.caption = ggplot2::element_text(size=0.6*axis_text_size,face="italic",
                                        hjust=0.5),
            strip.text = ggplot2::element_text(size=axis_text_size,face="bold"),
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
      ggplot2::geom_sf_text(aes(label=graph_lab),size=label_size,fontface="bold") +
      ggplot2::labs(x="",y="")
  }
  # return output
  graph
}
