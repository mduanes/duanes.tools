#' @name install_adg_key
#' @export
#'
#'
#` installs adg key for future reference

install_adg_key <- function(key) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if (file.exists(renv)) {
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if (!file.exists(renv)) {
      file.create(renv)
    }
    key_save <- paste0("ADG_KEY='", key, "'")
    write(key_save, renv, sep = "\n", append = TRUE)
    message("ADG path stored. Accessible via Sys.getenv(\"ADG_KEY\").")
    Sys.setenv(ADG_KEY = key)
}
