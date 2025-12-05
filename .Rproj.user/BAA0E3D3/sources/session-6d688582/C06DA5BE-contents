#' @name nat_breaks
#' @export

# classifies data based on natural breaks algorithmn from BAMMtools

nat_breaks <- function(data,
                       field, # field to classify
                       n, # number of breaks
                       forcezero=TRUE, # whether or not to force 0 as a break if data crosses 0
                       round=TRUE, # whether to round breaks from algo
                       class_name="classified", # name of output class field
                       pct=FALSE # add percents or not
                       ) {

  # generate and clean breaks ----
  # pull out field data
    field_fun <- data %>%
      dplyr::pull(field)

    # get class cutoffs from algo
    classes <- BAMMtools::getJenksBreaks(field_fun,n)

    # round class cutoffs based on number of zeros (e.g. 1,014 -> 1,000)
    if(round==TRUE) {
      magnitude <- as.numeric(paste0(1,paste0(rep(0,(nchar(round(classes[2]))-1)),collapse="")))
      classes <- plyr::round_any(classes,magnitude,floor)
    }

    # insert zero if specified
    if(forcezero==TRUE) {
      for(i in 1:length(classes)) {
        if(classes[i] < 0 & classes[i+1] >= 0) {
          classes <- c(classes[1:i],0,classes[i+1:length(classes)])
        }
      }
    }

  n <- length(classes) # override n if highly clustered
    # classify data using breaks ----
  # loop over classes
  for(i in 1:n) {
    # special operations for fist class
    if (i == 1) {
      # case when first break is positive
      if(classes[i] > 0.01) {
        # make class label
        if (pct == TRUE) {
          class_lab <- paste0(format(classes[i],big.mark=","), "% - ", format(classes[i+1]-1,big.mark=","),"%")
        } else {
          class_lab <- paste0(format(classes[i],big.mark=","), " - ", format(classes[i+1]-0.01,big.mark=","))
        }
      # start order vector
      order <- class_lab

      data_out <- data %>%
      # rename field to generic name
      dplyr::rename("field_class"=sym(field)) %>%
          # classify
        dplyr::mutate(classified=dplyr::case_when(field_class >= classes[i] & field_class < classes[i+1]~class_lab,
                                    TRUE~NA))

      # case when first break is negative
      } else {
        # make class label
        if (pct == TRUE) {
          class_lab <- paste0("Less than ",format(classes[i+1],big.mark=","),"%")
        } else {
        class_lab <- paste0("Less than ",format(classes[i+1],big.mark=","))
        }
        # start order vector
        order <- class_lab
        data_out <- data %>%
          # rename field to generic name
          dplyr::rename("field_class"=sym(field)) %>%
          # classify
          dplyr::mutate(classified=dplyr::case_when(field_class >= classes[i] & field_class < classes[i+1]~class_lab,
                                      TRUE~NA))
      }

      # final class break
    } else if (i == n) {
      # make class label
      if (pct == TRUE) {
        class_lab <- paste0(format(classes[i],big.mark=","), "%+")
      } else {
      class_lab <- paste0(format(classes[i],big.mark=","), "+")
      }
      # finish order vector
      order <- c(order,class_lab)
      # classify
      data_out <- data_out %>%
        dplyr::mutate(classified=dplyr::case_when(field_class >= classes[i]~class_lab,
                                    TRUE~classified))
      # intermediate levels
    # handle case where two adjacent breaks are not identical
    } else if (i != n & classes[i] != classes[i+1]) {
    # make class label
    if (pct == TRUE) {
      class_lab <- paste0(format(classes[i],big.mark=","), "% - ", format(classes[i+1]-0.01,big.mark=","),"%")
    } else {
      class_lab <- paste0(format(classes[i],big.mark=","), " - ", format(classes[i+1]-0.01,big.mark=","))
    }
    # add to order vector
    order <- c(order,class_lab)
    # classify
    data_out <- data_out %>%
      dplyr::mutate(classified=dplyr::case_when(field_class >= classes[i] & field_class < classes[i+1]~class_lab,
                                                TRUE~classified))
    # case where two adjacent breaks are identical
  } else if (i != n & classes[i] == classes[i+1]) {
    # skip

  }

  }
    # order class factor by order vector
    data_out <- data_out %>%
      dplyr::mutate(classified=factor(classified,order))

    # set names back to default and name class field
    colnames(data_out) <- c(colnames(data),class_name)

    # output data
    data_out
  }


