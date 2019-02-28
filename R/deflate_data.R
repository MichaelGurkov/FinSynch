#' This helper function deflates bis data by US CPI
#'
#'
#' @import dplyr
#'
#' @import rlang
#'
#' @export


deflate.data = function(df, vars_to_deflate,cpi = NULL){

  if(is.null(cpi)){cpi = import.bis.cpi.data()}

  vars_to_deflate_name = paste(vars_to_deflate, "real", sep = "_")

  vars_to_deflate = quo(!!sym(vars_to_deflate))

  df = left_join(df, cpi, by = "Date")

  df = df %>%
    mutate(!!vars_to_deflate_name := !!vars_to_deflate / US_CPI)

  return(df)


}
