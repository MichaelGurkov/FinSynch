#' This function appends to (Date, CountryPair) format data frame
#' data from (Date, Country) format
#'
#'  @import dplyr
#'
#'

append.countrypair.dataframe = function(countrypair_df, country_df){

  stopifnot(sum(c("Date","CountryPair") %in% names(countrypair_df)) == 2)

  stopifnot(sum(c("Date","Country") %in% names(country_df)) == 2)


  temp_df = countrypair_df %>%
    separate(.,col = CountryPair, into = c("Country_A", "Country_B"),
             sep = "-", remove = FALSE)

  temp_df = left_join(temp_df, country_df,
                      by = c("Date" = "Date","Country_A" = "Country"))

  temp_df = left_join(temp_df, country_df,
                      by = c("Date" = "Date","Country_B" = "Country"),
                      suffix = c("_A","_B"))

  temp_df = temp_df %>%
    select(-Country_A,-Country_B)


  return(temp_df)

}

#' This function normalizes the BIS country pairs data
#' The function divides each country pair position by the
#' sum of the countries  population (or GDP) in each year


normalize.bis.data = function(bis_df,wdi_df, norm_val = "GDP"){

  wdi_df = wdi_df %>%
    select(Country,Date, !!enquo(norm_val))

  var = names(bis_df)[!names(bis_df) %in% c("Date",
                                            "CountryPair",
                                            "Balance")]


  stopifnot(length(var) == 1)

  var_name = var

  var = quo(!!sym(var))

  bis_df = bis_df %>%
    separate(.,CountryPair, into = c("Country_A","Country_B"),
             sep = "-",remove = FALSE)

  bis_df = left_join(bis_df, wdi_df, by = c("Date" = "Date",
                                            "Country_A" = "Country"))

  bis_df = left_join(bis_df,suffix = c("_A","_B"),
                     wdi_df, by = c("Date" = "Date",
                                    "Country_B" = "Country"))

  bis_df = bis_df %>%
    mutate(Denom = rowSums(select(.,starts_with(norm_val)))) %>%
    mutate(!!var_name := !!var / Denom) %>%
    select(Date, CountryPair,Balance,!!var_name)


  return(bis_df)

}


#' This function normalizes IMF country pairs data
#' The function divides each country pair position by the
#' sum of the countries  population (or GDP) in each year


normalize.imf.data = function(imf_df,wdi_df, norm_val = "GDP"){

  wdi_df = wdi_df %>%
    select(Country,Date, !!enquo(norm_val))

  var = names(imf_df)[!names(imf_df) %in% c("Date",
                                            "CountryPair", "Balance")]


  stopifnot(length(var) == 1)

  var_name = var

  var = quo(!!sym(var))

  imf_df = imf_df %>%
    separate(.,CountryPair, into = c("Country_A","Country_B"),
             sep = "-",remove = FALSE)

  imf_df = left_join(imf_df, wdi_df, by = c("Date" = "Date",
                                            "Country_A" = "Country"))

  imf_df = left_join(imf_df,suffix = c("_A","_B"),
                     wdi_df, by = c("Date" = "Date",
                                    "Country_B" = "Country"))

  imf_df = imf_df %>%
    mutate(Denom = rowSums(select(.,starts_with(norm_val)))) %>%
    mutate(!!var_name := !!var / Denom) %>%
    select(Date, CountryPair,!!var_name)


  return(imf_df)

}


#' This function calculates all the synch measures and merges them
#' into one dataset named accordingly
#'
#' @import dplyr
#'
#' @import purrr
#'

make.synch.data = function(df,win_len){

  synch_1_df = df %>%
    get.synch1.list() %>%
    purrr::reduce(full_join, by = c("CountryPair", "Date")) %>%
    rename_at(vars(ends_with("_ret")), .funs = ~"Synch1_ret") %>%
    rename_at(vars(ends_with("_cycle")), .funs = ~"Synch1_cycle")


  synch_2_df = df %>%
    get.synch2.list() %>%
    purrr::reduce(full_join, by = c("CountryPair", "Date")) %>%
    rename_at(vars(ends_with("_ret")), .funs = ~"Synch2_ret") %>%
    rename_at(vars(ends_with("_cycle")), .funs = ~"Synch2_cycle")



  synch_3_df = df  %>%
    get.synch3.list(.,win_len) %>%
    purrr::reduce(full_join, by = c("CountryPair", "Date")) %>%
    rename_at(vars(ends_with("_ret")), .funs = ~"Synch3_ret") %>%
    rename_at(vars(ends_with("_cycle")), .funs = ~"Synch3_cycle")

  temp_df = list(synch_1_df, synch_2_df, synch_3_df) %>%
    purrr::reduce(full_join, by = c("CountryPair", "Date"))


  return(temp_df)

}


#' This function constructs full df for sensitivity regression
#'
#' @import dplyr
#'
#' @import purrr
#'

make.sens.reg.df = function(df,bank_int,trade_int,win_len,sector = "GDP"){

  if(sector == "GDP"){

    temp_df = make.synch.data(df %>%
                                select(Date, Country,
                                       GDP_per_Capita_real_ret,
                                       GDP_per_Capita_real_cycle),
                              win_len = win_len)
  } else {

    temp_df = make.synch.data(df %>%
                                select(Date, Country,Fin_ret,
                                       Fin_cycle),win_len = win_len)
    }

  temp_df = list(temp_df, bank_int$bank_pop, bank_int$bank_gdp,
                 trade_int$trade_pop, trade_int$trade_gdp) %>%
    reduce(full_join, by = c("CountryPair", "Date"))

  temp_df = append.countrypair.dataframe(temp_df, df %>%
                                             select(Date, Country,
                                                    GDP_per_Capita_real,
                                                    Pop))

  temp_df = temp_df  %>%
    mutate(LGDP = log(GDP_per_Capita_real_A) * log(GDP_per_Capita_real_B)) %>%
    mutate(LPop = log(Pop_A) * log(Pop_B)) %>%
    mutate(DGDP = log(GDP_per_Capita_real_A) - log(GDP_per_Capita_real_B)) %>%
    select(-ends_with("_A")) %>%
    select(-ends_with("_B"))




  return(temp_df)


}
