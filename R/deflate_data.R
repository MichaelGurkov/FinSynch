#' This helper function deflates bis data by US CPI
#'
#'
#' @import dplyr
#'
#' @import rlang
#'
#' @export


deflate.data = function(df, vars_to_deflate,
                        cpi = NULL,
                        remove_cpi_col = TRUE){

  if(is.null(cpi)){cpi = import.bis.cpi.data()}


  df = left_join(df, cpi, by = "Date")

  for(temp_var in vars_to_deflate){

    temp_var_name = paste(temp_var, "real", sep = "_")

    temp_var = quo(!!sym(temp_var))

    df = df %>%
      mutate(!!temp_var_name := !!temp_var / US_CPI)

  }

  if(remove_cpi_col){df = df %>% select(-US_CPI)}

  return(df)


}
