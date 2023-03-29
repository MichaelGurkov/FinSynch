#' This function preprocesses lbs data
#' The preprocessing is performed in two stages:
#' 1. Deflate lbs with US CPI
#' 2. Normalize lbs with corresponding GDP
#'
#' @import lubridate

preprocess_lbs_data = function(raw_data){

  lbs_df = raw_data$bis_lbs

  cpi_df = raw_data$cpi

  gdp_df = raw_data$gdp_constant

  lbs_deflated = lbs_df %>%
    left_join(cpi_df, by = "date") %>%
    mutate(balance_real = balance / us_cpi)

  lbs_normalized = lbs_deflated %>%
    filter(quarter(date) == 4) %>%
    mutate(year = as.character(year(date))) %>%
    select(-date) %>%
    left_join(gdp_df,
              by = c("reporting_country" = "country", "year")) %>%
    left_join(gdp_df,
              by = c("counterparty_country" = "country", "year"),
              suffix = c("_reporting","_counterparty"))

  lbs_processed = lbs_normalized %>%
    mutate(gdp_sum = gdp_usd_reporting + gdp_usd_counterparty) %>%
    mutate(balance_gdp = balance_real / gdp_sum) %>%
    group_by(country_pair, year) %>%
    summarise(balance_gdp = mean(log(balance_gdp)), .groups = "drop")

  return(lbs_processed)





}


#' This function preprocesses imf trade data
#' The preprocessing is performed in two stages:
#' 1. Deflate lbs with US CPI
#' 2. Normalize lbs with corresponding GDP
#'
#' @import lubridate

preprocess_imf_trade_data = function(raw_data){

  imf_trade_df = raw_data$imf_trade_df

  cpi_df = raw_data$cpi

  gdp_df = raw_data$gdp_constant

  cpi_df = cpi_df %>%
    group_by(date = as.character(year(date))) %>%
    summarise(us_cpi = mean(us_cpi), .groups = "drop")

  trade_df_deflated = imf_trade_df %>%
    left_join(cpi_df, by = "date") %>%
    mutate(total_trade_real = total_trade / us_cpi)

  trade_df_normalized = trade_df_deflated %>%
    rename(year = date) %>%
    left_join(gdp_df,
              by = c("country", "year")) %>%
    left_join(gdp_df,
              by = c("counterparty_country" = "country", "year"),
              suffix = c("_reporting","_counterparty"))

  trade_df_processed = trade_df_normalized %>%
    mutate(gdp_sum = gdp_usd_reporting + gdp_usd_counterparty) %>%
    mutate(total_trade_gdp = total_trade / gdp_sum) %>%
    group_by(country_pair, year) %>%
    summarise(total_trade_gdp = mean(log(total_trade_gdp)), .groups = "drop")

  return(trade_df_processed)





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
prepocess_crisis_dates = function(raw_data){

  crises_df = raw_data$crises_df

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
    mutate(crisis_ind = map2_dbl(crisis_ind_country,crisis_ind_counter, max)) %>%
    select("country_pair", "year", "crisis_ind")

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
    mutate(fd = FD_country + FD_counter) %>%
    select(country,counter_country,date,fd)

  return(country_pair_fin_df)





}
