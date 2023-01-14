
#' This helper function imports BIS cpi data and converts it to tidy format
#'
#' @import dplyr
#'
#' @export
#'


import.bis.cpi.data = function(filepath = NULL,
                               annual_freq = TRUE){

  if(is.null(filepath)){filepath = paste0(
    file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
    "\\OneDrive - Bank Of Israel\\Data\\BIS",
    "\\WEBSTATS_LONG_",
    "CPI_DATAFLOW_csv_col.csv")}



  if(annual_freq){

    my_freq = "Annual"

    my_regex = "^X\\d{4}$"

  } else {


    my_freq = "Monthly"

    my_regex = "^X\\d{4}\\.\\d{2}$"


  }


  cpi = read.csv(filepath)

  cpi = cpi %>%
    filter(Frequency == my_freq) %>%
    filter(Unit.of.measure == "Index, 2010 = 100") %>%
    select(c("Reference.area",
             na.omit(str_extract(names(.),
                                 pattern = my_regex)))) %>%
    rename(Country = Reference.area) %>%
    gather(.,key = Date,value = US_CPI,-Country) %>%
    mutate(Country = gsub("\\s","_",Country)) %>%
    filter(Country == "United_States") %>%
    mutate(Date = gsub("X","",Date)) %>%
    filter(!is.na(US_CPI)) %>%
    select(Date,US_CPI) %>%
    mutate(US_CPI = US_CPI / 100)


  return(cpi)


}


#' This helper function imports GDP and population data from
#' WDI  data base
#'
#' @import readxl
#'
#' @import dplyr
#'
#' @import stringr
#'
#' @export
#'


import_wdi_df = function(filepath_list = NULL,
                         countries_vec = NULL){

  if(is.null(filepath_list)){

    dir_path = file.path(Sys.getenv("USERPROFILE"),fsep = "\\")


    filepath_list = list(
      GDP_per_Capita = paste0(
        dir_path,"\\OneDrive - Bank Of Israel\\Data\\",
        "World Bank\\GDP_per_capita_panel.csv"),
      GDP = paste0(
        dir_path,"\\Documents\\",
        "Data\\World Bank\\GDP_panel.csv"),
      Pop = paste0(
        dir_path,"\\Documents\\",
        "Data\\World Bank\\Population.csv"))


  }

  df = lapply(names(filepath_list), function(temp_name){

    res = read.csv(filepath_list[[temp_name]],
                   stringsAsFactors = FALSE) %>%
             process.wdi.file(.,var_name = temp_name)

    # Replace country names

    res = res %>%
      mutate(Country = str_replace(Country,"Korea, Rep.","Korea"))

    if(!is.null(countries_vec)){

             res = res %>%
               filter(Country %in% countries_vec)
    }

    return(res)

  })


  wdi_data = df %>%
    purrr::reduce(full_join, by = c("Country","Year"))

  return(wdi_data)

}
#'
#'
#' This helper function imports credit data from BIS  data base
#'
#' @import dplyr
#'
#' @export
#'


import_bis_fin_cycle_df = function(filepath_list = NULL,
                         countries_vec = NULL){

  if(is.null(filepath_list)){

    dir_path = file.path(Sys.getenv("USERPROFILE"),fsep = "\\")

    filepath_list = list(
      Credit_GDP = paste0(
        dir_path,"\\OneDrive - Bank Of Israel\\Data\\",
        "BIS\\temp_credit_gdp_BIS.rds"),
      Total_credit = paste0(
        dir_path,"\\OneDrive - Bank Of Israel\\Data\\",
        "BIS\\temp_tot_credit_BIS.rds"),
      House = paste0(
        dir_path,"\\OneDrive - Bank Of Israel\\Data\\",
        "BIS\\temp_house_bis.rds"),
      FX = paste0(
        dir_path,"\\OneDrive - Bank Of Israel\\Data\\",
        "BIS\\temp_FX_USD.rds"),
      Rate = paste0(
        dir_path,"\\OneDrive - Bank Of Israel\\Data\\",
        "BIS\\temp_Policy_Rates.rds"))

  }

  df = lapply(names(filepath_list), function(temp_name){

    res = readRDS(filepath_list[[temp_name]])

    if(!is.null(countries_vec)){

      res = res %>%
        filter(Country %in% countries_vec)
    }

    return(res)

  })

  bis_data = df %>%
    purrr::reduce(full_join, by = c("Country","Date"))

  return(bis_data)

}
#'
#'
#' This helper function imports cross border banking from BIS  data base
#'
#' @import dplyr
#'
#' @export
#'


import_cross_border_balance = function(filepath = NULL,
                                countries_vec = NULL,
                                annual_freq = TRUE){

  if(is.null(filepath)){

    filepath = paste0(
      file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
      "\\OneDrive - Bank Of Israel\\Data\\BIS",
      "\\temp_credit_flows.rds")
    }


  credit_flows_df = readRDS(filepath)

  credit_balance_df = credit_flows_df %>%
    {if(!is.null(countries_vec)) filter(.,
                                        Country %in% countries_vec) %>%
        filter(.,Counter_Country %in% countries_vec) else .} %>%
    rename(Balance_Pos = Balance.sheet.position) %>%
    {if(annual_freq) mutate(.,Date = format(Date,"%Y")) %>%
        group_by(.,Date, Country,Counter_Country,Balance_Pos) %>%
        summarise(.,Balance = mean(Flow_Val, na.rm = TRUE)) %>%
        ungroup(.) else rename(.,Balance = Flow_Val) %>%
        mutate(.,Date = as.yearqtr(Date))} %>%
    mutate(.,CountryPair = ifelse(Country < Counter_Country,
                                paste(Country, Counter_Country,
                                      sep = "-"),
                                paste(Counter_Country,Country,
                                      sep = "-"))) %>%
    ungroup(.)


  return(credit_balance_df)



}
#'
#'
#' This function imports indexes of financial trilemma (capital openess)
#'
#'  @import readxl
#'
#'  @import dplyr
#'
#'  @import stringr
#'

import.trilemma.ind = function(filepath = paste0(
  file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
  "\\OneDrive - Bank Of Israel\\Data",
  "\\AizenmanChinnIto\\trilemma_indexes_update2018.xlsx")){

  temp_df = read_xlsx(filepath)

  temp_df = temp_df %>%
    select(-`IMF-World Bank Country Code`) %>%
    rename(Date = year) %>%
    mutate(Date = as.character(Date)) %>%
    rename(FX_stab = `Exchange Rate Stability Index`) %>%
    rename(MI_ind = `Monetary Independence Index`) %>%
    rename(FO_ind = `Financial Openness Index`) %>%
    rename(Country = `Country Name`) %>%
    mutate(Country = gsub("\\s","_", Country)) %>%
    mutate(Country = str_replace(Country,"Korea, Rep.","Korea"))




}

#'
#' This function imports macroprudential (Cerutti) data
#'
#'  @import readxl
#'
#'  @import dplyr
#'

import.macropru.ind = function(filepath = paste0(
  file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
  "\\Documents",
  "\\Data\\Cerutti\\prudential_ind_3.xlsx"),
  countries_vec = NULL){

  temp_df = read_xlsx(filepath, sheet = "Data")

   df = temp_df %>%
    select(year, country, PruC, PruC2) %>%
    rename(Country = country) %>%
    mutate(Country = gsub("\\s","_", Country)) %>%
    {if(!is.null(countries_vec)) filter(.,Country %in% countries_vec) else .} %>%
    rename(Date = year) %>%
    mutate(Date = as.character(Date)) %>%
    group_by(Date, Country) %>%
    summarise_all(.,.funs = list(~max))


  return(df)


}
#'
#'
#' This function imports capital account openess (Chinn Ito) data
#'
#'  @import readxl
#'
#'  @import dplyr
#'

import.kaopen.ind = function(filepath = paste0(
  file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
  "\\OneDrive - Bank Of Israel\\Data\\Chin-Ito\\kaopen_2016.xls"),
  countries_vec = NULL){

  temp_df = read_xls(filepath)

  df = temp_df %>%
    select(year, country_name, kaopen, ka_open) %>%
    rename(Country = country_name) %>%
    mutate(Country = gsub("\\s","_", Country)) %>%
    {if(!is.null(countries_vec)) filter(.,Country %in% countries_vec) else .} %>%
    rename(Date = year) %>%
    mutate(Date = as.character(Date))


  return(df)


}
#'
#'
#' This function imports financial development data
#'
#'  @import readxl
#'
#'  @import dplyr
#'

import.fin.dev.ind = function(filepath = paste0(
  file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
  "\\OneDrive - Bank Of Israel\\Data\\Svirydzenka\\FinDev.xlsx"),
  countries_vec = NULL){

  temp_df = read_xlsx(filepath)

  df = temp_df %>%
    select(year, country, FD, FI, FM) %>%
    rename(Country = country) %>%
    mutate(Country = gsub("\\s","_", Country)) %>%
    {if(!is.null(countries_vec)) filter(.,Country %in% countries_vec) else .} %>%
    rename(Date = year) %>%
    mutate(Date = as.character(Date))


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
#' #' This function imports BIS LBS from scratch
#' #'
#' #' @import dplyr
#' #'
#' #' @import readr
#'
#'
get_lbs_data = function(){

  bis_oecd_codes_df = raw_data %>%
    pluck("bis_codes") %>%
    filter(oecd_member == 1) %>%
    select(country)

  file_path = paste0("C:\\Users\\Home",
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
    my_counterparty_sector = "All sectors",
    my_position_type = "Cross-border",
    pivot_to_long = TRUE
  )

  temp = lbs_df %>%
    select(-c("time_format","collection_indicator",
              "organisation_visibility")) %>%
    filter(complete.cases(.))

  temp = temp %>%
    mutate(parent_country = str_replace_all(parent_country," ","_")) %>%
    mutate(counterparty_country = str_replace_all(counterparty_country," ","_"))

  temp %>%
    # inner_join(bis_oecd_codes_df, by = c("parent_country" = "country")) %>%
    inner_join(bis_oecd_codes_df, by = c("counterparty_country" = "country"))


  if(collapse_countrypair){

    res_df = selected_df %>%
      mutate(CountryPair = ifelse(Country < Counter_Country,
                                  paste(Country, Counter_Country, sep = "-"),
                                  paste(Counter_Country,Country, sep = "-"))) %>%
      select(-Country,-Counter_Country) %>%
      gather(key = Date,value = Balance,
             -c(Balance_Pos,CountryPair))

  } else {

    res_df = selected_df %>%
      gather(key = Date,value = Balance,
             -c(Balance_Pos,Country,Counter_Country))


  }


  return(res_df)

}
#'
#'
#' This function imports BIS total credit data from scratch
#'
#' @import dplyr
#'
#' @import readr


get_total_credit_data = function(){

  file_path = paste0("C:\\Users\\Home",
                     "\\OneDrive - Bank Of Israel\\Data",
                     "\\BIS\\credit\\WS_TC_csv_col.csv")

  tot_credit = import_bis_total_credit(file_path,
                          my_frequency = "Quarterly",
                          my_lending_sector = "All sectors",
                          my_valuation_method = "Market value",
                          my_unit_type = "US dollar",
                          my_adjustment = "Adjusted for breaks",
                          pivot_to_long = TRUE) %>%
    select(-c(unit_multiplier, unit_of_measure)) %>%
    filter(complete.cases(.))

  tot_credit = tot_credit %>%
    inner_join(raw_data$bis_codes %>%
                 filter(oecd_member == 1) %>%
                 select(country), by = "country")

  tot_credit = tot_credit%>%
    mutate(date = as.yearqtr(date, "%Y-Q%q"))

  return(tot_credit)
}
#'
#'
#' This function imports BIS house price data from scratch
#'
#' @import dplyr
#'
#' @import readr


import.bis.property.price.data = function(filepath = paste0(
  file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
  "\\OneDrive - Bank Of Israel\\Data\\BIS\\",
  "WEBSTATS_SELECTED_PP_DATAFLOW_csv_col.csv"),
  my_value = "Real",
  my_measure = "Index, 2010 = 100",
  countries_vec = NULL){

  raw_df = read_csv(filepath, col_types = cols(), progress = FALSE)


  temp_df = raw_df %>%
    select(-c(grep("^([A-Z]*_*)+$",names(.), value = TRUE),
              "Time Period", "Frequency"))


  # Filter for default values

  filtered_df = temp_df %>%
    filter(Value %in% my_value) %>%
    filter(`Unit of measure` %in% my_measure)

  # Subsitute spaces in country names and filter for countries

  filtered_df = filtered_df %>%
    rename(Country = `Reference area`) %>%
    mutate(Country = gsub("\\s","_",Country)) %>%
    {if(!is.null(countries_vec)) filter(.,Country %in% countries_vec)}


  selected_df = filtered_df %>%
    select(-c("Value","Unit of measure"))

  long_df = gather(selected_df,key = Date,value = HousePrice,-Country)

  return(long_df)

}
#'
#'
#' This helper function imports Worldwide Governance Indicators data
#'
#' @import readr
#'
#' @import dplyr
#'

import.wgi.ind = function(filepath = paste0(
  file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
  "\\OneDrive - Bank Of Israel\\Data\\World Bank\\WGIData.csv"),
  countries_vec = NULL){

  temp_df = read_csv(filepath, col_types = NULL)

  df = temp_df %>%
    select(-`Country Code`,-`Indicator Code`) %>%
    rename(Country = `Country Name`, Indicator = `Indicator Name`) %>%
    mutate(Country = gsub("\\s","_", Country)) %>%
    {if(!is.null(countries_vec)) filter(.,Country %in% countries_vec) else .} %>%
    select(-X24) %>%
    gather(.,key = Date,value = Val, -Country,-Indicator)


  return(df)


}
#'
#'
#' This helper function imports banking crises dates data
#'
#' @import readxl
#'
#' @import dplyr
#'

import.crises.dates.df = function(filepath = paste0(
  file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
  "\\OneDrive - Bank Of Israel\\Data\\LavaenValencia\\SYSTEMIC BANKING ",
  "CRISES DATABASE_2018.xlsx"),
  countries_vec = NULL){

  temp = read_xlsx(filepath,
                   sheet = "Crisis Resolution and Outcomes",
                   range = "A1:K152")


  df = temp %>%
    setNames(gsub(pattern = "[0-9/\r\n]","", names(.))) %>%
    mutate(End = gsub(pattern = "[0-9]{1}/$","", End)) %>%
    mutate(End = gsub("\\s","",End)) %>%
    mutate(Country = gsub("\\s","_",Country)) %>%
    {if(!is.null(countries_vec)) filter(., Country %in% countries_vec) else .} %>%
    mutate_at(.vars = vars(-Country, -Start, -End),
              .funs = list(~round(as.numeric(.),2))) %>%
    mutate_at(.vars = vars(Start, End),
              .funs = list(~as.character(.)))


  return(df)


}
#'
#'
#' This function imports geo dist data from cepii
#'
#' @import readxl
#'
#' @import dplyr
#'

import.geodist.data = function(filepath = paste0(
  file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
  "\\OneDrive - Bank Of Israel\\Data\\CEPII\\",
  "dist_cepii.xls")){

  df = read_xls(filepath, col_types = c(rep("text",2),
                                        rep("numeric",12)))

  codes = import.iso.codes()

  df = df %>%
    left_join(.,codes, by = c("iso_o" = "Code")) %>%
    left_join(.,codes, by = c("iso_d" = "Code")) %>%
    select(-iso_o,-iso_d) %>%
    rename(Country = Country.x) %>%
    rename(Counter_Country = Country.y)

  return(df)
}
#'
#'
#' This function imports ISO codes for country names
#'

get_bis_country_codes = function(filepath = NULL, oecd_members = TRUE){


  country_codes = read_csv(paste0(Sys.getenv("USERPROFILE"),
                  "\\OneDrive - Bank Of Israel\\Data",
                  "\\OECD\\country_codes.csv"),
           show_col_types = FALSE)

  country_codes = country_codes %>%
    mutate(country = str_replace_all(country, " ","_"))

  country_codes = country_codes %>%
    mutate(country = str_replace(country,"South_Korea","Korea")) %>%
    mutate(country = str_replace(country,"Czech_Republic","Czechia")) %>%
    mutate(country = str_replace(country,"Slovak_Republic","Slovakia"))


  return(country_codes)

  }

