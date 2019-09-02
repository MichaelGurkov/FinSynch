#' Import OECD house price dataframe
#'
#'
#' This function imports house price data from OECD format.
#'
#'
#' @details
#' The housing prices indicator shows indices of residential property prices
#' over time. Included are rent prices, real and nominal house prices,
#' and ratios of price to rent and price to income;
#' the main elements of housing costs.
#'
#' In most cases, the nominal house price covers the sale of newly-built and
#' existing dwellings, following the recommendations from RPPI
#' (Residential Property Prices Indices) manual.
#'
#' The real house price is given by the ratio of nominal price
#' to the consumers’ expenditure deflator in each country,
#' both seasonally adjusted, from the OECD national accounts database.
#'
#' The price to income ratio is the nominal house price divided by the nominal
#' disposable income per head and can be considered as a measure of
#' affordability.
#'
#' The price to rent ratio is the nominal house price divided by the rent price
#' and can be considered as a measure of the profitability of house ownership.
#'
#' This indicator is an index with base year 2015.
#'
#' @import dplyr
#'
#' @import zoo
#'
#' @param my_subject This parameter filters the required data type
#' (nominal price, real price, price to rent, price to income or rent).
#'  The default is real price
#'
#' @param my_frequency filters the required frequency
#' (annual - A or quarter - Q). The default is Q


import.oecd.houseprice = function(
  filepath = paste0("C:\\Users\\Misha\\Documents\\Data",
                    "\\OECD\\House_prices.csv"),
  my_subject = "REAL", my_frequency = "Q"){

  df = read.csv(file = filepath, stringsAsFactors = FALSE)

  df = df %>%
  filter(SUBJECT == my_subject) %>%
  filter(FREQUENCY == my_frequency) %>%
  select(ï..LOCATION,TIME, Value) %>%
  rename(Country = ï..LOCATION, Date = TIME, HousePrice = Value) %>%
  mutate(Date = as.yearqtr(Date, format = "%Y-Q%q"))

  return(df)

}



#' Import OECD GDP quarterly dataframe
#'
#'
#' This function imports quarterly GDP data from OECD format.
#'
#'
#' @details
#' This indicator is based on real GDP (also called GDP at constant prices
#' or GDP in volume), i.e. the developments over time are adjusted for price
#' changes. The numbers are also adjusted for seasonal influences.
#' The indicator is available in different measures: percentage change from
#' the previous quarter, percentage change from the same quarter of the
#' previous year and volume index. All OECD countries compile their data
#' according to the 2008 System of National Accounts (SNA).
#'
#'
#' @import dplyr
#'
#' @import zoo
#'
#' @param my_subject filters the required data type
#' (total - TOT or volume index - VOLIDX).
#'
#' @param my_measure filters the required measurement type
#' (percentage change from the previous quarter  - PC_CHGPP,
#'  percentage change from the same quarter of the previous year - PC_CHGPY
#'  and volume index - IDX.) The default is PC_CHGPY.
#'
#' @param my_frequency filters the required frequency
#' (annual - A or quarter - Q). The default is Q

import.oecd.gdp.quarter = function(
  filepath = paste0("C:\\Users\\Misha\\Documents\\Data",
                    "\\OECD\\GDP_quarterly.csv"),
  my_subject = "TOT", my_measure = "PC_CHGPY", my_frequency = "Q"){

  df = read.csv(file = filepath, stringsAsFactors = FALSE)

  df = df %>%
    filter(SUBJECT == my_subject) %>%
    filter(MEASURE == my_measure) %>%
    filter(FREQUENCY == my_frequency) %>%
    select(ï..LOCATION,TIME, Value) %>%
    rename(Country = ï..LOCATION, Date = TIME, GDP = Value) %>%
    mutate(Date = as.yearqtr(Date, format = "%Y-Q%q"))

  return(df)

}
