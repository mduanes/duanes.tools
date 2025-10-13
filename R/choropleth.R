#' @name choropleth
#' @export





library(ggplot2)
library(dplyr)
library(tigris)

source("R/settings.R")

# returns a choropleth map of a single variable
choropleth <- function(data, col, pal=default_pal_discrete,
                       show_legend=FALSE, title = NULL,
                       legend_lab = "Legend",
                       legend_pct=FALSE,
                       type="discrete",
                       caption=NULL,
                       axis_text_size=default_axis_text_size,
                       line_width=default_graph_linewidth) {

  midpoint <- mean(data[[col]])

  if(type=="discrete") {
    graph <- ggplot(data,mapping=aes(fill=.data[[col]])) +
      # add basic aesthetic settings
      scale_fill_manual(values=pal,name=legend_lab,drop=FALSE) +
      theme_minimal() +
      labs(title=title,caption=caption)  +
      theme(axis.text = element_blank(),
            legend.background = element_rect(fill = "transparent", color = NA),
            panel.grid = element_blank(),
            legend.text = element_text(size=axis_text_size*0.5),
            legend.title = element_text(size=axis_text_size,face="bold"),
            plot.caption = element_text(size=0.6*axis_text_size,face="italic",
                                        hjust=0.5),
            strip.text = element_text(size=axis_text_size,face="bold"),
            strip.background = element_blank()) +
      geom_sf(linewidth=line_width/1.5,color="white",show.legend = show_legend)
  } else if(type=="gradient") {
    graph <- ggplot(data,mapping=aes(fill=.data[[col]])) +
      scale_fill_gradient2(high=pal[5],
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
      theme_minimal() +
      labs(title=title,caption=caption)  +
      theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(size=axis_text_size*0.5),
            legend.title = element_text(size=axis_text_size,face="bold"),
            plot.caption = element_text(size=0.6*axis_text_size,face="italic",
                                        hjust=0.5),
            strip.text = element_text(size=axis_text_size,face="bold"),
            strip.background = element_blank()) +
      geom_sf(linewidth=line_width/1.5,color="white",show.legend = show_legend)

    if (legend_pct == TRUE) {
      graph <- graph +
        scale_fill_gradient2(high=pal[5],
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
      theme(legend.position = "none")
  }
  graph
}
