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
#' Constant data are in constant 2010 U.S. dollars. Dollar figures for GDP are
#' converted from domestic currencies using 2010 official exchange rates.
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


import.wdi.gdp = function(filepath,countries_codes = NULL,
                          message_gdp_type = TRUE){

  df = read.csv(file = filepath, stringsAsFactors = FALSE, skip = 4)

  if(message_gdp_type){

    if(length(df$Indicator.Name[1]) ==1){message(df$Indicator.Name[1])}
  }

  df = df %>%
    select(Country.Name, Country.Code, starts_with("X")) %>%
    rename(Country = Country.Name, Code = Country.Code) %>%
    select_if(.predicate = list(~!all(is.na(.)))) %>%
    rename_all(.funs = list(~gsub("X","",.))) %>%
    {if(!is.null(countries_codes)) filter(.,Code %in% countries_codes) else .} %>%
    gather(key = Date,value = GDP_constant,-Country,-Code)

  return(df)

}
