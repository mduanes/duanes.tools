#' @name equal_interval
#' @export

library(tidyverse)

equal_interval <- function(data,field,n,forcezero=TRUE) {
  field_fun <- data %>%
    pull(field)

  step_size <- (max(field_fun) - min(field_fun))/n
  magnitude <- as.numeric(paste0(1,paste0(rep(0,nchar(round(step_size))-1),collapse="")))
  classes <- plyr::round_any(seq(min(field_fun),max(field_fun),step_size),magnitude)

    if(forcezero==TRUE) {
    for(i in 1:length(classes)) {
      if(classes[i] < 0 & classes[i+1] >= 0) {
        classes <- c(classes[1:i],0,classes[i+1:length(classes)])
      }
    }
  }
  for(i in 1:n) {
    if (i == 1) {
      class_lab <- paste0(format(classes[i],big.mark=","), "-", format(classes[i+1],big.mark=","))
      order <- class_lab
      data_out <- data %>%
        rename("field_class"=sym(field)) %>%
        mutate(classified=case_when(field_class >= classes[i] & field_class < classes[i+1]~class_lab,
                                    TRUE~NA))
    } else if (i != n) {
      class_lab <- paste0(format(classes[i],big.mark=","), "-", format(classes[i+1],big.mark=","))
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

