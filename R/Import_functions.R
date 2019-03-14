#' This helper function imports export import data from
#' IMF Direction of Trade data base
#'
#' The function returns country pairs formatted in:
#' small (namewise) country first order
#'
#' @import readxl
#'
#' @import dplyr
#'
#' @import stringr
#'
#' @export
#'


import_imf_df = function(filepath, countries_vec = NULL){

  category = str_extract(filepath,"-\\s?(\\w*?)_") %>%
    str_replace_all(.,pattern = "[-_\\s]","")

  country = str_extract(filepath,"(\\w*?)\\s-") %>%
    str_replace_all(.,pattern = "[-\\s]","")

  my_range = ifelse(country == "Belgium","B7:W219","B7:AP215")

  temp = suppressMessages(read_xlsx(filepath,range = my_range))

  names(temp)[1] = "Counter_Country"

  temp = temp %>%
    mutate(Counter_Country = gsub("\\s","_",Counter_Country)) %>%
    {if(!is.null(countries_vec)) filter(Country %in% countries_vec) else .} %>%
    gather(.,key = Date, value = !!quo_name(category),
           -Counter_Country) %>%
    mutate(CountryPair = ifelse(Counter_Country < country,
                                paste(Counter_Country,
                                      country, sep = "-"),
                                paste(country,Counter_Country,
                                      sep = "-"))) %>%
    select(Date, CountryPair, !!quo_name(category))

  return(temp)


}



#' This helper function imports BIS cpi data and converts it to tidy format
#'
#' @import dplyr
#'
#' @export
#'


import.bis.cpi.data = function(filepath =
                                 paste0("C:\\Users\\Misha\\",
                                        "Documents\\Data\\BIS",
                                        "\\WEBSTATS_LONG_",
                                        "CPI_DATAFLOW_csv_col.csv")){

  cpi = read.csv(filepath)

  cpi = cpi %>%
    filter(Frequency == "Annual") %>%
    filter(Unit.of.measure == "Index, 2010 = 100") %>%
    select(c("Reference.area",
             na.omit(str_extract(names(.),
                                 pattern = "^X\\d{4}$")))) %>%
    rename(Country = Reference.area) %>%
    gather(.,key = Date,value = US_CPI,-Country) %>%
    mutate(Country = gsub("\\s","_",Country)) %>%
    filter(Country == "United_States") %>%
    mutate(Date = gsub("X","",Date)) %>%
    filter(!is.na(US_CPI)) %>%
    select(Date,US_CPI)


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

    filepath_list = list(GDP_per_Capita =
                           paste0("C:\\Users\\Misha\\Documents\\Data\\",
                                  "World Bank\\GDP_per_capita_panel.csv"),
                         GDP = paste0("C:\\Users\\Misha\\Documents\\",
                                      "Data\\World Bank\\GDP_panel.csv"),
                         Pop = paste0("C:\\Users\\Misha\\Documents\\",
                                      "Data\\World Bank\\Population.csv"))


  }

  df = lapply(names(filepath_list), function(temp_name){

    res = read.csv(filepath_list[[temp_name]]) %>%
             process.wdi.file(.,var_name = temp_name)

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


#' This helper function imports credit data from BIS  data base
#'
#' @import dplyr
#'
#' @export
#'


import_bis_fin_cycle_df = function(filepath_list = NULL,
                         countries_vec = NULL){

  if(is.null(filepath_list)){

    filepath_list = list(Credit_GDP =
                           paste0("C:\\Users\\Misha\\Documents\\Data\\",
                                  "BIS\\temp_credit_gdp_BIS.rds"),
                         Total_credit =
                           paste0("C:\\Users\\Misha\\Documents\\Data\\",
                                  "BIS\\temp_tot_credit_BIS.rds"),
                         House =
                           paste0("C:\\Users\\Misha\\Documents\\Data\\",
                                  "BIS\\temp_house_bis.rds"),
                         FX =
                           paste0("C:\\Users\\Misha\\Documents\\Data\\",
                                  "BIS\\temp_FX_USD.rds"),
                         Rate =
                           paste0("C:\\Users\\Misha\\Documents\\Data\\",
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


#' This helper function imports cross border banking from BIS  data base
#'
#' @import dplyr
#'
#' @export
#'



import_cross_border_balance = function(filepath = NULL,
                                countries_vec = NULL){

  if(is.null(filepath)){
    filepath = paste0("C:\\Users\\Misha\\Documents\\Data\\BIS",
                                          "\\temp_credit_flows.rds")
    }


  credit_flows_df = readRDS(filepath)

  credit_flows_df = credit_flows_df %>%
    {if(!is.null(countries_vec)) filter(Country %in% countries_vec) else .} %>%
    filter(Counter_Country %in% countries_vec) %>%
    rename(Balance = Balance.sheet.position) %>%
    mutate(Date = format(Date,"%Y")) %>%
    group_by(Date, Country,Counter_Country,Balance) %>%
    summarise(Avg_Balance = mean(Flow_Val, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(CountryPair = ifelse(Country < Counter_Country,
                                paste(Country, Counter_Country, sep = "-"),
                                paste(Counter_Country,Country,
                                      sep = "-"))) %>%
    select(Date,CountryPair,Balance,Avg_Balance) %>%
    ungroup()


  return(credit_flows_df)



}


#' This function imports indexes of financial trilemma (capital openess)
#'
#'  @import readxl
#'
#'  @import dplyr
#'

import.trilemma.ind = function(filepath = paste0(
  "C:\\Users\\Misha\\Documents\\Data",
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
    mutate(Country = gsub("\\s","_", Country))




}


#' This function imports macroprudential (Cerutti) data
#'
#'  @import readxl
#'
#'  @import dplyr
#'

import.macropru.ind = function(filepath = paste0(
  "C:\\Users\\Misha\\Documents",
  "\\Data\\Cerutti\\prudential_ind_3.xlsx"),
  countries_vec = NULL){

  temp_df = read_xlsx(filepath, sheet = "Data")

   df = temp_df %>%
    select(year, country, PruC, PruC2) %>%
    rename(Country = country) %>%
    mutate(Country = gsub("\\s","_", Country)) %>%
    {if(!is.null(countries_vec)) filter(Country %in% countries_vec) else .} %>%
    rename(Date = year) %>%
    mutate(Date = as.character(Date)) %>%
    group_by(Date, Country) %>%
    summarise_all(.,.funs = list(~max))


  return(df)


}


#' This function imports capital account openess (Chinn Ito) data
#'
#'  @import readxl
#'
#'  @import dplyr
#'

import.kaopen.ind = function(filepath = paste0(
  "C:\\Users\\Misha\\Documents\\Data\\Chin-Ito\\kaopen_2016.xls"),
  countries_vec = NULL){

  temp_df = read_xls(filepath)

  df = temp_df %>%
    select(year, country_name, kaopen, ka_open) %>%
    rename(Country = country_name) %>%
    mutate(Country = gsub("\\s","_", Country)) %>%
    {if(!is.null(countries_vec)) filter(Country %in% countries_vec) else .} %>%
    rename(Date = year) %>%
    mutate(Date = as.character(Date))


  return(df)


}


#' This function imports financial development data
#'
#'  @import readxl
#'
#'  @import dplyr
#'

import.fin.dev.ind = function(filepath = paste0(
  "C:\\Users\\Misha\\Documents\\Data\\Svirydzenka\\FinDev.xlsx"),
  countries_vec = NULL){

  temp_df = read_xlsx(filepath)

  df = temp_df %>%
    select(year, country, FD, FI) %>%
    rename(Country = country) %>%
    mutate(Country = gsub("\\s","_", Country)) %>%
    {if(!is.null(countries_vec)) filter(Country %in% countries_vec) else .} %>%
    rename(Date = year) %>%
    mutate(Date = as.character(Date))


  return(df)


}
