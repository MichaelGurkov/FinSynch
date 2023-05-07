#' Import World Bank GDP dataframe
#'
#'
#' This function imports GDP data from World Bank format.
#'
#'
#' @details
#' Current data are in current U.S. dollars. Dollar figures for GDP are
#' converted from domestic currencies using single year official exchange rates.
#' For a few countries where the official exchange rate does not reflect the
#' rate effectively applied to actual foreign exchange transactions, an
#' alternative conversion factor is used. World Bank ID is NY.GDP.MKTP.CD
#'
#' Constant data are in constant 2015 U.S. dollars. Dollar figures for GDP are
#' converted from domestic currencies using 2015 official exchange rates.
#' For a few countries where the official exchange rate does not reflect the
#' rate effectively applied to actual foreign exchange transactions, an
#' alternative conversion factor is used. World Bank ID is NY.GDP.MKTP.KD
#'
#' @import dplyr
#'
#' @import zoo
#'
#' @param message_gdp_type boolean
#'
#' @param countries_codes vector of codes of required countries


get_wdi_gdp = function(file_path){

  raw_df = read_csv(file = file_path, show_col_types = FALSE, skip = 4)


  df = raw_df %>%
    select(country = `Country Name`, matches("[0-9]{4}")) %>%
    mutate(country = str_replace_all(country, " ","_")) %>%
    mutate(country = str_replace(country, "Korea,_Rep\\.","Korea")) %>%
    mutate(country = str_replace(country, "Slovak_Republic","Slovakia")) %>%
    mutate(country = str_replace(country, "Turkey","Turkiye")) %>%
    pivot_longer(-country,names_to = "year",values_to = "gdp_usd")

  df = df %>%
    mutate(gdp_usd = gdp_usd * 10 ^ (-6))


  return(df)

}
