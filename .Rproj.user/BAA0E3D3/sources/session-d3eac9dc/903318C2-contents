#' @name dt_fonts
#'
#' @export
#'
#'
#'

dt_fonts <- function(font) {
  if(tolower(font) == "formata") {

    sysfonts::font.add("formata",
                       paste0(get_adg_key(),"Resources/fonts/formata/FormataCondensed.ttf"),
                       paste0(get_adg_key(),"Resources/fonts/formata/FormataCondensed_Bold.ttf"),
                       paste0(get_adg_key(),"Resources/fonts/formata/FormataCondensed_Italic.ttf"),
                       paste0(get_adg_key(),"Resources/fonts/formata/FormataBold_Italic.ttf"))
  } else {
    print("Unrecognized font.")
  }
}
