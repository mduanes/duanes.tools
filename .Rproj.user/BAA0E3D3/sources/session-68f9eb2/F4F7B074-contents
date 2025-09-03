#' @name inflation_adjust
#' @export

inflation_adjust_cpiu <- function(data,field,yearfield,
                                  year_adj=year(Sys.Date())) {
  cpi <- fredr::fredr_series_observations("CPIAUCSL",realtime_start=lubridate::as_date("1980-01-01"),realtime_end = as_date(Sys.Date())) %>%
    mutate(year=year(date)) %>%
    group_by(year) %>%
    summarize(avg_cpi=mean(value)) %>%
    ungroup() %>%
    rename("YEARMATCH"=year)

  curryear_cpi <- cpi %>%
    filter(YEARMATCH==year_adj) %>%
    pull(avg_cpi)

  data %>%
    mutate("YEARMATCH"= !!rlang::sym(yearfield),
           "field"= !!rlang::sym(field)) %>%
    left_join(cpi,by=("YEARMATCH")) %>%
    # adjust for inflation
    mutate(inflation_adjusted = field * (curryear_cpi/avg_cpi)) %>%
    select(-c("avg_cpi","field","YEARMATCH"))
}
