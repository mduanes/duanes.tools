#' @name pal_gen
#' @export

library(colorspace)
pal_gen <- function(base_color,n_col=5) {
  colorspace::sequential_hcl(n_col,
                             h = as(hex2RGB(base_color), "polarLUV")@coords[3],
                             #c = as(hex2RGB(base_color), "polarLUV")@coords[2],
                             #l = as(hex2RGB(base_color), "polarLUV")@coords[1],
                             rev=TRUE)
}
