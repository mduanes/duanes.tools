#' @name pal_gen_diverging
#' @export

# uses pal_gen to make a 2way diverging color palette

pal_gen_diverging <- function(low_color=uga_colors(4), # low color
                              high_color = uga_colors(5), # high color
                              n_col_high=3, # number of colors on high end
                              n_col_low=3, # number of colors on low end
                              neutral_midcol = TRUE # whether to include a neutral mid color
) {
  # calculate number of colors on each side
  high_cols <- pal_gen(high_color,n_col_high+1)
  low_cols <- pal_gen(low_color,n_col_low+1) # +1 is neutral midcol, which is removed if uneeded

  # remove neutral midcol if not needed
  if (neutral_midcol == FALSE) {
    low_cols <- low_cols[-1]
  }
  # remove neutral col from high cols
  high_cols <- high_cols[-1]

  # combine pals
  output <- c(rev(low_cols),high_cols)

  # push output
  output
}
