#' @name manual_breaks
#' @export

library(tidyverse)
library(BAMMtools)

manual_breaks <- function(data,field,breaks=c(), round=TRUE) {
  field_fun <- data %>%
    pull(field)
  classes <- breaks
  n <- length(breaks)
  for(i in 1:n) {
    if (i == 1) {
      class_lab <- paste0(format(classes[i],big.mark=","), "-", format(classes[i+1]-1,big.mark=","))
      order <- class_lab
      data_out <- data %>%
        rename("field_class"=sym(field)) %>%
        mutate(classified=case_when(field_class >= classes[i] & field_class < classes[i+1]~class_lab,
                                    TRUE~NA))
    } else if (i != n) {
      class_lab <- paste0(format(classes[i],big.mark=","), "-", format(classes[i+1]-1,big.mark=","))
      order <- c(order,class_lab)
      data_out <- data_out %>%
        mutate(classified=case_when(field_class >= classes[i] & field_class < classes[i+1]~class_lab,
                                    TRUE~classified))
    } else {
      class_lab <- paste0(format(classes[i],big.mark=","), "+")
      order <- c(order,class_lab)
      data_out <- data_out %>%
        mutate(classified=case_when(field_class >= classes[i]~class_lab,
                                    TRUE~classified))
    }
  }
  data_out %>%
    rename(field=field_class) %>%
    mutate(classified=factor(classified,order))
}

