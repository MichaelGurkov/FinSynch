
#' Import eu_membership data
get_eu_membership = function(){



}


#' Import trilemma indices data
get_trilemma_ind = function(file_path = NULL){

  if(is.null(file_path)){

    file_path = paste0(Sys.getenv("USERPROFILE"),
                      "\\OneDrive - Bank Of Israel\\Data",
                      "\\AizenmanChinnIto",
                      "\\trilemma_indexes_update2020.xlsx")
  }

  temp_df = read_xlsx(file_path)

  temp_df = temp_df %>%
    select(-`IMF-World Bank Country Code`) %>%
    rename(date = year) %>%
    mutate(date = as.character(date)) %>%
    rename(fx_stab = `Exchange Rate Stability Index`) %>%
    rename(mi_ind = `Monetary Independence Index`) %>%
    rename(fo_ind = `Financial Openness Index`) %>%
    rename(country = `Country Name`) %>%
    mutate(country = str_replace_all(country, "\\s","_")) %>%
    mutate(country = str_replace(country,"Korea, Rep.","Korea"))

  return(temp_df)




}

#' Import financial development data
get_fin_dev_ind = function(){

  file_path = paste0(Sys.getenv("USERPROFILE"),
                     "\\OneDrive - Bank Of Israel",
                     "\\Data\\IMF\\financial_development",
                     "\\FD Index Database (Excel).xlsx")

  temp_df = read_xlsx(file_path)

  df = temp_df %>%
    select(year, country, FD, FI, FM) %>%
    mutate(country = str_replace_all(country,"\\s","_")) %>%
    rename(date = year) %>%
    mutate(date = as.character(date))


  return(df)


}
#'
#'
#' This function imports harmon data
#'
#'  @import dplyr
#'
#'  @import readxl
#'
#'  @import zoo

import.harmon.data = function(filepath = paste0(
  file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
  "\\OneDrive - Bank Of Israel\\Data\\",
  "Kalemli_Ozcan_Papaionnou_Peydro\\harmon.xlsx"),
  myrange = "B4:AF32",
  codes_filepath = paste0(
    file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
    "\\OneDrive - Bank Of Israel\\Data\\ISO\\",
    "iso_2digit_alpha_country_codes.csv"),
  convert_country_names = TRUE){

  harmon = read_xlsx(path = filepath,range = myrange)

  temp_names = grep("[A-Z]{2}",names(harmon), value = TRUE)

  temp = lapply(temp_names,
                function(temp_name){
                  temp_col = which(names(harmon) == temp_name)
                  unite(harmon[,temp_col:(temp_col + 1)], !!temp_name,
                        sep = "-")}) %>%
    reduce(., cbind)

  temp = cbind.data.frame(Directive = harmon$Directive[-1], temp[-1,])

  if(convert_country_names){

    names(temp)[names(temp) == "UK"] = "GB"

    iso_names = read.csv(codes_filepath, stringsAsFactors = FALSE)

    names(iso_names) = c("Code","Country")

    source_names = colnames(temp)[-1]

    target_names = inner_join(data.frame(Code = source_names,
                                         stringsAsFactors = FALSE),
                              iso_names,
                              by = "Code") %>%
      select(Country) %>%
      unlist() %>%
      setNames(.,NULL)

    colnames(temp) = c(colnames(temp)[1],target_names)

  }

  temp = temp %>%
    gather(key = Country, value = Date, - Directive) %>%
    mutate(Date = str_replace(Date,
                              pattern = "(^[0-9]{4})(Q[0-9])-NA$",
                              replacement = "\\1-\\2")) %>%
    mutate(Date = as.yearqtr(Date, format = "%Y-Q%q")) %>%
    mutate(Directive = levels(Directive)[Directive]) %>%
    mutate(Country = str_replace(Country,"\\s","_"))


  return(temp)


}
#'
#'
#'
#'
#' This function calculates absolute GDP values given growth rates
#' The growth rates are at quarterly frequency and the gdp balance is at

calculate.gdp.df = function(growth_rates, gdp_balance){

  before_ind = growth_rates$Date <= as.yearqtr(
    paste(gdp_balance$Year, "Q4"))

  after_ind = growth_rates$Date > as.yearqtr(
    paste(gdp_balance$Year, "Q4"))

  gdp_balance_before_vec = rev(
    cumprod(c(gdp_balance$Value,
              rev((1 + 0.01 * growth_rates$Value[
                                           before_ind]) ^ -1)))
    )

  dates_before_vec = growth_rates$Date[before_ind]

  dates_before_vec = c(as.yearqtr(dates_before_vec[1]-0.25),
                       dates_before_vec)

  gdp_balance_after_vec = cumprod(
    c(gdp_balance$Value,1 + 0.01 * growth_rates$Value[after_ind])
    )

  dates_after_vec = growth_rates$Date[after_ind]

  gdp_df = data.frame(Date = c(dates_before_vec, dates_after_vec),
                      GDP = c(gdp_balance_before_vec,
                              gdp_balance_after_vec[-1])) %>%
                        mutate(Country = gdp_balance$Country)

}
#'
#'


#' This function imports BIS LBS from scratch
#'
#' @import dplyr
#'
#' @import readr
#'
#'
get_lbs_data = function(countries_df = NULL){

  if(is.null(countries_df)){

    countries_df = raw_data %>%
    pluck("country_codes") %>%
    filter(oecd_member == 1) %>%
    select(country)

  }

  file_path = paste0(Sys.getenv("USERPROFILE"),
                     "\\OneDrive - Bank Of Israel",
                     "\\Data\\BIS\\international_banking",
                     "\\WS_LBS_D_PUB_csv_col.csv")

  lbs_df = import_bis_lbs(
    file_path,
    my_frequency = "Quarterly",
    my_measure = "Amounts outstanding / Stocks",
    my_type_of_instruments = "All instruments",
    my_currency_denomination = "All currencies",
    my_currency_type_of_reporting_country = "All currencies (=D+F+U)",
    my_parent_country = "All countries (total)",
    my_type_of_reporting_institutions = paste0("All reporting banks/institutions",
                                               " (domestic, foreign, consortium",
                                               " and unclassified)"),
    my_counterparty_sector = "All sectors",
    my_position_type = "Cross-border",
    pivot_to_long = TRUE
  )

  lbs_df = lbs_df %>%
    select(-any_of(c("time_format","collection_indicator",
                     "organisation_visibility","series"))) %>%
    filter(complete.cases(.))

  filtered_lbs_df = lbs_df %>%
    mutate(reporting_country = str_replace_all(reporting_country, " ", "_")) %>%
    mutate(counterparty_country = str_replace_all(counterparty_country, " ", "_")) %>%
    mutate(across(c("reporting_country","counterparty_country"),
                  ~str_replace_all(.,"Turkey","Turkiye"))) %>%
    inner_join(countries_df, by = c("reporting_country" = "country")) %>%
    inner_join(countries_df, by = c("counterparty_country" = "country"))

  filtered_lbs_df = filtered_lbs_df %>%
    mutate(country_pair = paste_country_pair(reporting_country,
                                             counterparty_country))

  return(filtered_lbs_df)

}


#' This function imports US cpi
#'
get_us_cpi = function(){


  bis_cpi = import_bis_cpi_index(paste0(Sys.getenv("USERPROFILE"),
                                       "\\OneDrive - Bank Of Israel\\Data",
                                       "\\BIS\\cpi\\WS_LONG_CPI_csv_col.csv"),
    my_frequency = "Monthly",
    my_unit_of_measure = "Index, 2010 = 100",
    pivot_to_long = TRUE)

  us_cpi = bis_cpi %>%
    filter(country == "United_States") %>%
    group_by(date = as.yearqtr(date)) %>%
    summarise(us_cpi = mean(cpi), .groups = "drop")

  return(us_cpi)

}



#'
#'
#' This function imports BIS total credit data from scratch
#'
#' @import dplyr
#'
#' @import readr


get_total_credit_data = function(){

  file_path = paste0(Sys.getenv("USERPROFILE"),
                     "\\OneDrive - Bank Of Israel\\Data",
                     "\\BIS\\credit\\WS_TC_csv_col.csv")

  tot_credit = import_bis_total_credit(file_path,
                          my_frequency = "Quarterly",
                          my_lending_sector = "All sectors",
                          my_borrowing_sector = "Private non-financial sector",
                          my_valuation_method = "Market value",
                          my_unit_type = "US dollar",
                          my_adjustment = "Adjusted for breaks",
                          pivot_to_long = TRUE) %>%
    select(-c(unit_multiplier, unit_of_measure)) %>%
    filter(complete.cases(.))

  tot_credit = tot_credit %>%
    mutate(country = str_replace_all(country,"Turkey","Turkiye")) %>%
    inner_join(raw_data$country_codes %>%
                 filter(oecd_member == 1) %>%
                 select(country), by = "country")

  tot_credit = tot_credit%>%
    mutate(date = as.yearqtr(date, "%Y-Q%q"))

  return(tot_credit)
}




#'
#' @import readxl
#'
#' @import dplyr
#'

get_fin_crises_data_Lavaen = function(file_path = NULL){

  countries_df = raw_data %>%
    pluck("country_codes") %>%
    filter(oecd_member == 1) %>%
    select(country)


  if(is.null(file_path)){

    file_path = paste0(Sys.getenv("USERPROFILE"),
                       "\\OneDrive - Bank Of Israel\\Data",
                       "\\LavaenValencia\\SYSTEMIC BANKING ",
                       "CRISES DATABASE_2018.xlsx")
    }


  fin_crises_df = read_xlsx(file_path,
                   sheet = "Crisis Resolution and Outcomes",
                   range = "A1:K152")


  fin_crises_df = fin_crises_df %>%
    select(country = Country, start_year = Start, end_year = End) %>%
    mutate(across(everything(),
                  ~str_remove_all(.,pattern = " [0-9]{1}/"))) %>%
    mutate(country = str_replace_all(country," ","_"))%>%
    mutate(country = str_replace(country, "Slovak_Rep", "Slovakia")) %>%
    mutate(country = str_replace(country, "Turkey", "Turkiye")) %>%
    mutate(country = str_replace(country, "Czech_Republic", "Czechia"))

  fin_crises_df = fin_crises_df %>%
    inner_join(countries_df, by = "country")


  return(fin_crises_df)


}



#'
get_fin_crises_data_Nguen = function(file_path = NULL,
                                     crisis_category = NULL){

  extract_years = function(temp_str){

    if(is.na(temp_str)){return(NA)}

    if(nchar(temp_str) < 9){return(temp_str)}

    years_vec = unlist(str_extract_all(temp_str,"[0-9]{4}-[0-9]{4}"))

    years_seq = map(years_vec, function(temp_seq){

      seq_range = unlist(str_split(temp_seq,pattern = "-"))

      return(as.character(seq(from = seq_range[1],to = seq_range[2])))



    }) %>%
      unlist()

    return(years_seq)




  }

  if(is.null(file_path)){

    file_path = paste0(Sys.getenv("USERPROFILE"),
                       "\\OneDrive - Bank Of Israel\\Data",
                       "\\Nguyen\\fin_crises_database.csv")

  }

  raw_df = read_csv(file_path, show_col_types = FALSE)

  df = raw_df %>%
    rename_with(.cols = everything(), ~tolower(str_replace_all(.," ","_"))) %>%
    pivot_longer(-country,names_to = "category") %>%
    mutate(year = map(value, extract_years)) %>%
    select(-value) %>%
    unnest(year)

  if(!is.null(crisis_category)){

    df = df %>%
      filter(category == crisis_category) %>%
      select(-category)

  }

  df = df %>%
    filter(complete.cases(.)) %>%
    mutate(crises_ind = 1)

  return(df)

}


#'
#'
#' This function imports ISO codes for country names
#'

get_country_codes = function(filepath = NULL, oecd_members = TRUE){


  country_codes = read_csv(paste0(Sys.getenv("USERPROFILE"),
                  "\\OneDrive - Bank Of Israel\\Data",
                  "\\OECD\\country_codes.csv"),
           show_col_types = FALSE)

  country_codes = country_codes %>%
    mutate(country = str_replace_all(country, " ","_"))

  country_codes = country_codes %>%
    mutate(country = str_replace(country,"South_Korea","Korea")) %>%
    mutate(country = str_replace(country,"Czech_Republic","Czechia")) %>%
    mutate(country = str_replace(country,"Slovak_Republic","Slovakia")) %>%
    mutate(country = str_replace(country,"Turkey","Turkiye"))


  return(country_codes)

  }

