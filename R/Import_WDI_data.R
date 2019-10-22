#' Import World Bank GDP dataframe
#'
#'
#' This function imports (constant) GDP data from World Bank format.
#'
#'
#' @details
#' Data are in current U.S. dollars. Dollar figures for GDP are converted from
#' domestic currencies using single year official exchange rates. For a few
#' countries where the official exchange rate does not reflect the rate
#' effectively applied to actual foreign exchange transactions, an
#' alternative conversion factor is used.
#'
#' World Bank ID is NY.GDP.MKTP.CD
#'
#' @import dplyr
#'
#' @import zoo
#'
#' @param countries_codes vector of codes of required countries


import.wdi.gdp.constant = function(
  filepath = paste0("C:\\Users\\Misha\\Documents\\Data\\World Bank\\",
                    "API_NY.GDP.MKTP.KD_DS2_en_csv_v2_316073.csv"),
  countries_codes = NULL){

  df = read.csv(file = filepath, stringsAsFactors = FALSE, skip = 4)

  df = df %>%
    select(Country.Name, Country.Code, starts_with("X")) %>%
    rename(Country = Country.Name, Code = Country.Code) %>%
    select_if(.predicate = list(~!all(is.na(.)))) %>%
    rename_all(.funs = list(~gsub("X","",.))) %>%
    {if(!is.null(countries_codes)) filter(.,Code %in% countries_codes) else .} %>%
    gather(key = Date,value = GDP_constant,-Country,-Code)

  return(df)

}


#' Import World Bank GDP dataframe
#'
#'
#' This function imports (current) GDP data from World Bank format.
#'
#'
#' @details
#' Data are in constant 2010 U.S. dollars. Dollar figures for GDP are converted
#' from domestic currencies using 2010 official exchange rates. For a few
#' countries where the official exchange rate does not reflect the rate
#' effectively applied to actual foreign exchange transactions, an
#' alternative conversion factor is used.
#'
#' World Bank ID is NY.GDP.MKTP.KD
#'
#' @import dplyr
#'
#' @import zoo
#'
#' @param countries_codes vector of codes of required countries


import.wdi.gdp.current = function(
  filepath = paste0("C:\\Users\\Misha\\Documents\\Data\\World Bank\\",
                    "API_NY.GDP.MKTP.KD_DS2_en_csv_v2_316073.csv"),
  countries_codes = NULL){

  df = read.csv(file = filepath, stringsAsFactors = FALSE, skip = 4)

  df = df %>%
    select(Country.Name, Country.Code, starts_with("X")) %>%
    rename(Country = Country.Name, Code = Country.Code) %>%
    select_if(.predicate = list(~!all(is.na(.)))) %>%
    rename_all(.funs = list(~gsub("X","",.))) %>%
    {if(!is.null(countries_codes)) filter(.,Code %in% countries_codes) else .} %>%
    gather(key = Date,value = GDP_constant,-Country,-Code)

  return(df)

}
