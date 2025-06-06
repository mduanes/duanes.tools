#' @name percent_market
#' @export

percent_marker <- function (x, accuracy = NULL, scale = 1, prefix = "",
                            suffix = "%", big.mark = " ", decimal.mark = ".",
                            trim = TRUE, ...)
{
  scales::number(x = x, accuracy = accuracy, scale = scale, prefix = prefix,
                 suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark,
                 trim = trim, ...)
}
