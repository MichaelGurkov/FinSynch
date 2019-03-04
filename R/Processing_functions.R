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


#' This function merges data required for sensitivity regression
#'
#'  @import dplyr
#'

make.sens.reg.df = function(var_name){

  temp_df = lapply(synch_measures, function(temp_list){
    temp_list[grep(paste0("^",var_name),
                   names(temp_list),value = TRUE)]})

  temp_df = unlist(temp_df, recursive = FALSE)

  temp_df = c(temp_df,
              list(bank_int$bank_pop, bank_int$bank_gdp,
                          trade_int$trade_pop, trade_int$trade_gdp)) %>%
  purrr::reduce(inner_join, by = c("Date","CountryPair"),
                suffix = c("_2","_3"))

  temp_df = append.countrypair.dataframe(temp_df,
                                         df %>%
                                           select(Date, Country,
                                                  GDP_per_Capita_real,
                                                  Pop)) %>%
  mutate(LGDP = log(GDP_per_Capita_real_A) * log(GDP_per_Capita_real_B)) %>%
  mutate(LPop = log(Pop_A) * log(Pop_B)) %>%
  mutate(DGDP = log(GDP_per_Capita_real_A) - log(GDP_per_Capita_real_B)) %>%
  select(-ends_with("_A")) %>%
  select(-ends_with("_B"))

  return(temp_df)

}
