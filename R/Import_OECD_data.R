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



#' Import OECD share price dataframe
#'
#'
#' This function imports share price data from OECD format.
#'
#'
#' @details
#' Share price indices are calculated from the prices of common shares of
#' companies traded on national or foreign stock exchanges. They are usually
#' determined by the stock exchange, using the closing daily values for the
#' monthly data, and normally expressed as simple arithmetic averages of the
#' daily data. A share price index measures how the value of the stocks in the
#' index is changing, a share return index tells the investor what their
#' “return” is, meaning how much money they would make as a result of investing
#' in that basket of shares. A price index measures changes in the market
#' capitalisation of the basket of shares in the index whereas a return index
#' adds on to the price index the value of dividend payments, assuming they
#' are re-invested in the same stocks. Occasionally agencies such as central
#' banks will compile share indices.
#'
#' @import dplyr
#'
#' @import zoo
#'
#'
#' @param my_frequency filters the required frequency
#' (annual - A , quarter - Q or monthly - M). The default is Q


import.oecd.houseprice = function(
  filepath = paste0("C:\\Users\\Misha\\Documents\\Data",
                    "\\OECD\\House_prices.csv"),
  my_frequency = "Q"){

  df = read.csv(file = filepath, stringsAsFactors = FALSE)

  df = df %>%
    filter(FREQUENCY == my_frequency) %>%
    select(ï..LOCATION,TIME, Value) %>%
    rename(Country = ï..LOCATION, Date = TIME, HousePrice = Value) %>%
    mutate(Date = as.yearqtr(Date, format = "%Y-Q%q"))

  return(df)

}


#' Import OECD share price dataframe
#'
#'
#' This function imports share price data from OECD format.
#'
#'
#' @details
#' Share price indices are calculated from the prices of common shares of
#' companies traded on national or foreign stock exchanges. They are usually
#' determined by the stock exchange, using the closing daily values for the
#' monthly data, and normally expressed as simple arithmetic averages of the
#' daily data. A share price index measures how the value of the stocks in the
#' index is changing, a share return index tells the investor what their
#' “return” is, meaning how much money they would make as a result of investing
#' in that basket of shares. A price index measures changes in the market
#' capitalisation of the basket of shares in the index whereas a return index
#' adds on to the price index the value of dividend payments, assuming they
#' are re-invested in the same stocks. Occasionally agencies such as central
#' banks will compile share indices.
#'
#' @import dplyr
#'
#' @import zoo
#'
#'
#' @param my_frequency filters the required frequency
#' (annual - A , quarter - Q or monthly - M). The default is Q


import.oecd.shareprice = function(
  filepath = paste0("C:\\Users\\Misha\\Documents\\Data",
                    "\\OECD\\Share_prices.csv"),
  my_frequency = "Q"){

  df = read.csv(file = filepath, stringsAsFactors = FALSE)

  df = df %>%
  filter(FREQUENCY == my_frequency) %>%
  select(ï..LOCATION,TIME, Value) %>%
  rename(Country = ï..LOCATION, Date = TIME, SharePrice = Value) %>%
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
#'  and volume index - IDX.) The default is PC_CHGPP.
#'
#' @param my_frequency filters the required frequency
#' (annual - A or quarter - Q). The default is Q

import.oecd.gdp.quarter = function(
  filepath = paste0("C:\\Users\\Misha\\Documents\\Data",
                    "\\OECD\\GDP_quarterly.csv"),
  my_subject = "TOT", my_measure = "PC_CHGPP", my_frequency = "Q"){

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



#' Import OECD GDP dataframe
#'
#'
#' This function imports (nominal) GDP data from OECD format.
#'
#'
#' @details
#' This indicator is based on nominal GDP (also called GDP at current prices
#' or GDP in value) and is available in different measures: US dollars and US
#' dollars per capita (current PPPs). All OECD countries compile their data
#' according to the 2008 System of National Accounts (SNA). This indicator is
#' less suited for comparisons over time, as developments are not only caused
#' by real growth, but also by changes in prices and PPPs.
#'
#' @import dplyr
#'
#' @import zoo
#'
#' @param my_measure filters the required measure
#' (US dollars - MLN_USD , US dollars per capita (current PPPs) - USD_CAP).
#' The default is MLN_USD


import.oecd.gdp = function(
  filepath ="C:\\Users\\Misha\\Documents\\Data\\OECD\\GDP.csv",
  my_measure = "MLN_USD"){

  df = read.csv(file = filepath, stringsAsFactors = FALSE)

  df = df %>%
    filter(MEASURE == my_measure) %>%
    select(ï..LOCATION,TIME, Value) %>%
    rename(Country = ï..LOCATION, Date = TIME, GDP = Value) %>%
    mutate(Date = as.yearqtr(Date, format = "%Y-Q%q"))

  return(df)

}
