#' @name nat_breaks
#' @export

library(tidyverse)
library(BAMMtools)

nat_breaks <- function(data,field,n,forcezero=TRUE,round=TRUE,class_name="classified") {
    field_fun <- data %>%
      pull(field)
    classes <- BAMMtools::getJenksBreaks(field_fun,n)
    if(round==TRUE) {
      magnitude <- as.numeric(paste0(1,paste0(rep(0,(nchar(round(classes[2]))-1)),collapse="")))
      classes <- plyr::round_any(classes,magnitude,floor)

    }
    if(forcezero==TRUE) {
      for(i in 1:length(classes)) {
        if(classes[i] < 0 & classes[i+1] >= 0) {
          classes <- c(classes[1:i],0,classes[i+1:length(classes)])
        }
      }
    }
  for(i in 1:n) {
    if (i == 1) {
      if(classes[i] > 0.01) {
      class_lab <- paste0(format(classes[i],big.mark=","), " - ", format(classes[i+1]-1,big.mark=","))
      order <- class_lab
      data_out <- data %>%
        rename("field_class"=sym(field)) %>%
        mutate(classified=case_when(field_class >= classes[i] & field_class < classes[i+1]~class_lab,
                                    TRUE~NA))
      } else {
        class_lab <- paste0("Less than ",format(classes[i+1],big.mark=","))
        order <- class_lab
        data_out <- data %>%
          rename("field_class"=sym(field)) %>%
          mutate(classified=case_when(field_class >= classes[i] & field_class < classes[i+1]~class_lab,
                                      TRUE~NA))
      }
    } else if (i != n & classes[i] != classes[i+1]) {
      class_lab <- paste0(format(classes[i],big.mark=","), " - ", format(classes[i+1]-1,big.mark=","))
      order <- c(order,class_lab)
      data_out <- data_out %>%
        mutate(classified=case_when(field_class >= classes[i] & field_class < classes[i+1]~class_lab,
                                    TRUE~classified))
    } else if (i != n & classes[i]) {
      class_lab <- paste0(format(classes[i],big.mark=","))
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
    data_out <- data_out %>%
      mutate(classified=factor(classified,order))

    colnames(data_out) <- c(colnames(data),class_name)

    data_out
}

