#' This helper function deflates bis data by US CPI
#'
#'
#' @import dplyr
#'
#' @import rlang
#'
#' @export


deflate.bis.data = function(bis_df, cpi = NULL){

  if(is.null(cpi)){cpi = import.bis.cpi.data()}

  var = names(bis_df)[!names(bis_df) %in% c("Date",
                                            "CountryPair",
                                            "Balance")]
  stopifnot(length(var) == 1)

  var_name = paste(var, "real", sep = "_")

  var = quo(!!sym(var))

  bis_df = left_join(bis_df, cpi, by = "Date")

  bis_df = bis_df %>%
    mutate(!!var_name := !!var / US_CPI) %>%
    select(Date,CountryPair,Balance,!!var_name)

  return(bis_df)


}
