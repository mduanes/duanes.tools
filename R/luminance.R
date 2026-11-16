#' @name luminance
#'
#' @export
#'
#'
#'

# detects the luminance of a color

# LUMINANCE
luminance <- function(cols) {
  output <- NA
  for(c in cols) {
    col_info <- as(colorspace::hex2RGB(c),"polarLUV")
    lum <- unname(col_info@coords[, "L"])
    output <- c(output,lum)
  }
  output <- output[-1]
  output
}
