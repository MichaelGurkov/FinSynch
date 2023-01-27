
#' This function imports export import data from IMF Direction of
#' Trade data base
#'

get_imf_trade_data = function(file_path = NULL){

  if(is.null(file_path)){

    file_path = paste0(Sys.getenv("USERPROFILE"),
                       "\\OneDrive - Bank Of Israel\\Data\\IMF",
                       "\\Export-Import\\DOT_01-27-2023",
                       " 19-22-54-12_timeSeries.csv")
  }

  raw_df = read_csv(file_path, show_col_types = FALSE)


  long_df = raw_df %>%
    filter(Attribute == "Value") %>%
    select(country = `Country Name`,
           counterparty_country = `Counterpart Country Name`,
           indicator = `Indicator Name`,
           matches("^[0-9]{4}$"))

  long_df = long_df %>%
    mutate(across(c("country","counterparty_country"),
                  ~str_replace_all(., " ","_"))) %>%
    mutate(across(c("country","counterparty_country"),
                  ~str_remove_all(., ",_Rep._of"))) %>%
    mutate(across(c("country","counterparty_country"),
                  ~str_replace(., "Slovak_Rep.","Slovakia"))) %>%
    mutate(across(c("country","counterparty_country"),
                  ~str_replace(., "Netherlands,_The","Netherlands"))) %>%
    mutate(across(c("country","counterparty_country"),
                  ~str_replace(., "Czech_Rep.","Czechia"))) %>%
    mutate(across(c("country","counterparty_country"),
                  ~str_replace(., "Turkey","Turkiye")))



  long_df = long_df %>%
    mutate(across(matches("^[0-9]{4}$"), as.numeric)) %>%
    pivot_longer(cols = matches("^[0-9]{4}$"),
                 names_to = "date")


  aggregated_df = long_df %>%
    filter(indicator %in% c(paste0("Goods, Value of Imports,",
                                   " Cost, Insurance, Freight (CIF), US Dollars"),
                            paste0("Goods, Value of Exports,",
                                   " Free on board (FOB), US Dollars"))) %>%
    group_by(country, counterparty_country, date) %>%
    summarise(total_trade = sum(value, na.rm = TRUE), .groups = "drop")

  aggregated_df = aggregated_df %>%
    mutate(country_pair = ifelse(
      country < counterparty_country,
      paste(country, counterparty_country, sep = "-"),
      paste(counterparty_country, country, sep = "-")))


  return(aggregated_df)




}



