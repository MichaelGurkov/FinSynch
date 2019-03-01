#' This helper function imports export import data from
#' IMF Direction of Trade data base
#'
#' @import readxl
#'
#' @import dplyr
#'
#' @import stringr
#'
#' @export
#'


import_imf_df = function(filepath, filter_countries = NULL){

  category = str_extract(filepath,"-\\s?(\\w*?)_") %>%
    str_replace_all(.,pattern = "[-_\\s]","")

  country = str_extract(filepath,"(\\w*?)\\s-") %>%
    str_replace_all(.,pattern = "[-\\s]","")

  my_range = ifelse(country == "Belgium","B7:W219","B7:AP215")

  temp = suppressMessages(read_xlsx(filepath,range = my_range))

  names(temp)[1] = "Counter_Country"

  temp = temp %>%
    mutate(Counter_Country = gsub("\\s","_",Counter_Country))

  if(!is.null(filter_countries)){temp = temp %>%
    filter(Counter_Country %in% filter_countries)}

  temp = temp %>%
    gather(.,key = Date, value = !!quo_name(category),
           -Counter_Country) %>%
    mutate(CountryPair = ifelse(Counter_Country > country,
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


#' This helper function imports export import data from
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


#' This helper function imports export import data from
#' WDI  data base
#'
#' @import readxl
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
                                  "BIS\\temp_house_bis.rds"))


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

