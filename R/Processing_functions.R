


normalize_by_gdp = function(raw_df, deflate_data_inner){

  if(deflate_data_inner){

    cpi_df = raw_data$cpi

    gdp_df = raw_data$gdp_constant

    raw_df = raw_df %>%
      left_join(cpi_df, by = "date") %>%
      mutate(value = value / us_cpi)

  } else {


    gdp_df = raw_data$gdp_current

  }


  raw_df_gdp = raw_df %>%
    left_join(gdp_df,
              by = c("country" = "country", "year")) %>%
    left_join(gdp_df,
              by = c("counterparty_country" = "country", "year"),
              suffix = c("_country","_counterparty"))

  raw_df_normalized = raw_df_gdp %>%
    mutate(gdp_sum = gdp_usd_country + gdp_usd_counterparty) %>%
    mutate(value_gdp = value / gdp_sum) %>%
    group_by(country_pair, year) %>%
    summarise(value_gdp = mean(log(value_gdp)), .groups = "drop")



  return(raw_df_normalized)



}


#' This function preprocesses lbs data
#' The preprocessing is performed in two stages:
#' 1. Deflate lbs with US CPI
#' 2. Normalize lbs with corresponding GDP
#'
#' @import lubridate

preprocess_lbs_data = function(raw_data, deflate_data = FALSE){

  lbs_df = raw_data$bis_lbs %>%
    select(-balance_sheet_position) %>%
    filter(quarter(date) == 4) %>%
    mutate(year = as.character(year(date))) %>%
    select(-date) %>%
    rename(value = balance, country = reporting_country)

  lbs_normalized = normalize_by_gdp(lbs_df,
                                    deflate_data_inner = deflate_data) %>%
    rename(bank_gdp = value_gdp)

  return(lbs_normalized)





}


#' This function preprocesses imf trade data
#' The preprocessing is performed in two stages:
#' 1. Deflate lbs with US CPI
#' 2. Normalize lbs with corresponding GDP
#'
#' @import lubridate

preprocess_imf_trade_data = function(raw_data, deflate_data = FALSE){

  imf_trade_df = raw_data$imf_trade_df %>%
    rename(value = total_trade) %>%
    rename(year = date)

  imf_trade_normalized = normalize_by_gdp(imf_trade_df,
                   deflate_data_inner = deflate_data) %>%
    rename(trade_gdp = value_gdp)

  return(imf_trade_normalized)





}

#' This function calculates the financial cycle for countries
#'
preprocess_fin_cycle = function(raw_data){

  codes = raw_data %>%
    pluck("country_codes") %>%
    select(country_code = code, country) %>%
    mutate(country_code = str_replace_all(country_code,"KOREA-NS","KOR")) %>%
    mutate(country_code = str_replace_all(country_code,"CSFR-CZE","CZE")) %>%
    mutate(country_code = str_replace_all(country_code,"CSFR-SVK","SVK")) %>%
    mutate(country_code = str_replace_all(country_code,"USSR-EST","EST")) %>%
    mutate(country_code = str_replace_all(country_code,"USSR-LVA","LVA")) %>%
    mutate(country_code = str_replace_all(country_code,"USSR-RUS","RUS")) %>%
    mutate(country_code = str_replace_all(country_code,"FYUG-SVN","SVN"))


  country_df = raw_data[c("house_price","share_price")] %>%
    reduce(inner_join, by = c("country_code","date")) %>%
    left_join(codes, by = "country_code") %>%
    select(-country_code) %>%
    relocate(country) %>%
    inner_join(raw_data$total_credit, by = c("country", "date"))


  cycles_df = country_df %>%
    left_join(raw_data$cpi, by = "date") %>%
    mutate(total_credit = total_credit / us_cpi) %>%
    select(-us_cpi) %>%
    filter(quarter(date) == 4) %>%
    mutate(year = as.character(year(date))) %>%
    select(-date) %>%
    pivot_longer(-c("country","year"),names_to = "component") %>%
    group_by(country, component) %>%
    arrange(year) %>%
    mutate(value = log(value / dplyr::lag(value))) %>%
    filter(!is.na(value)) %>%
    group_by(country, year) %>%
    summarise(fin_cycle = mean(value), .groups = "drop")


  return(cycles_df)







}


#' This function calculates the financial synchronization measure
#'
#'
preprocess_synch_df = function(cycles_df){

  fin_synch_df = cycles_df %>%
    select(country, year) %>%
    distinct() %>%
    mutate(counter_country = country) %>%
    expand(country, counter_country, year) %>%
    filter(!country == counter_country) %>%
    mutate(country_pair = paste_country_pair(country, counter_country)) %>%
    select(country_pair, year) %>%
    distinct() %>%
    separate(country_pair,into = c("country", "counter_country"),sep = "-",
             remove = FALSE)


  fin_synch_df = fin_synch_df %>%
    left_join(cycles_df, by = c("country","year")) %>%
    left_join(cycles_df, by = c("counter_country" = "country","year"),
              suffix = c("_country","_counter_country")) %>%
    select(country_pair, year, contains("cycle")) %>%
    filter(complete.cases(.)) %>%
    mutate(fin_synch = -1 * abs(fin_cycle_country - fin_cycle_counter_country)) %>%
    select(-starts_with("fin_cycle"))

  return(fin_synch_df)





}


#' This function classifies crisis period in countrypairs
#'
preprocess_crisis_dates = function(raw_data){

  crises_df = raw_data$crises_df

  crises_df = crises_df %>%
    mutate(year = map2(start_year, end_year,
                       .f = function(start_year, end_year)
                       {seq(start_year, end_year)})) %>%
    select(-c("start_year", "end_year")) %>%
    unnest(year) %>%
    mutate(crisis_ind = 1)

  crises_country_pair_df = crises_df %>%
    distinct() %>%
    mutate(counter_country = country) %>%
    expand(country, counter_country, year) %>%
    filter(!country == counter_country) %>%
    mutate(country_pair = paste_country_pair(country, counter_country)) %>%
    select(country_pair, year) %>%
    distinct() %>%
    separate(country_pair,into = c("country", "counter_country"),sep = "-",
             remove = FALSE)

  crises_country_pair_df = crises_country_pair_df %>%
    left_join(crises_df, by = c("country","year")) %>%
    left_join(crises_df, by = c("counter_country" = "country","year"),
              suffix = c("_country","_counter")) %>%
    mutate(across(starts_with("crisis_ind"), ~replace_na(.,0))) %>%
    mutate(crisis_ind = map2_dbl(crisis_ind_country,
                                 crisis_ind_counter, max)) %>%
    select("country_pair", "year", "crisis_ind") %>%
    mutate(year = as.character(year))

  return(crises_country_pair_df)


}

#' This function calculates the financial development level for
#' each country-pair
#'
preprocess_fin_dev = function(raw_data, countries_df = NULL){

  if(is.null(countries_df)){

    countries_df = raw_data %>%
      pluck("country_codes") %>%
      filter(oecd_member == 1) %>%
      select(country)

  }

  fin_dev_df = raw_data$fin_dev_ind

  fin_dev_df = fin_dev_df %>%
    select(country, date, FD)

  country_pair_fin_df = fin_dev_df %>%
    inner_join(countries_df, by = "country") %>%
    expand_to_country_pairs()%>%
    left_join(fin_dev_df, by = c("country","date")) %>%
    left_join(fin_dev_df, by = c("counter_country" = "country","date"),
              suffix = c("_country","_counter")) %>%
    mutate(fd_ind = FD_country + FD_counter) %>%
    mutate(country_pair = paste_country_pair(country,counter_country)) %>%
    select(country_pair,year = date,fd_ind) %>%


  return(country_pair_fin_df)





}

#' This function calculates the trilemma indices level for
#' each country-pair
#'
preprocess_controls = function(raw_data, countries_df = NULL){

  if(is.null(countries_df)){

    countries_df = raw_data %>%
      pluck("country_codes") %>%
      filter(oecd_member == 1) %>%
      select(country)

  }

  trilemma_df = raw_data$trilemma_ind

  trilemma_df = trilemma_df %>%
    select(country, date, fx_stab, fo_ind)

  gdp_df = raw_data$gdp_constant %>%
    rename(date = year)

  country_pair_trilemma_df = trilemma_df %>%
    inner_join(countries_df, by = "country") %>%
    expand_to_country_pairs()%>%
    left_join(trilemma_df, by = c("country","date")) %>%
    left_join(trilemma_df, by = c("counter_country" = "country","date"),
              suffix = c("_country","_counter")) %>%
    filter(complete.cases(.)) %>%
    mutate(fx_stab = fx_stab_country + fx_stab_counter) %>%
    mutate(fo_ind = fo_ind_country + fo_ind_counter) %>%
    select(country_pair,date,fx_stab,fo_ind)

  country_pair_gdp_df = gdp_df %>%
    inner_join(countries_df, by = "country") %>%
    expand_to_country_pairs()%>%
    left_join(gdp_df, by = c("country","date")) %>%
    left_join(gdp_df, by = c("counter_country" = "country","date"),
              suffix = c("_country","_counter")) %>%
    filter(complete.cases(.)) %>%
    mutate(gdp_usd = gdp_usd_country + gdp_usd_counter) %>%
    select(country_pair, date, gdp_usd)

  controls_df = list(country_pair_trilemma_df,
                     country_pair_gdp_df) %>%
    reduce(full_join, by = c("country_pair","date")) %>%
    rename(year = date)

  return(controls_df)





}
