#' @name assign_text_col
#'
#' @export
#'
#'
#'

#  assigns text color basedo on luminance
# ASSIGN TEXT COLOR GIVEN BAR COLOR
assign_text_col <- function(cols) {
  output <- NA

  lums <- luminance(cols)
  for(l in lums) {
    r <- ifelse(l>=50,"black","white")
    output <- c(output,r)
  }
  output <- output[-1]
  output
}
