#' This helper function imports data file formatted in IMF Direction of
#' Trade data base style
#'
#' The function returns country pairs formatted in:
#' small (namewise) country first order
#'
#' @import readxl
#'
#' @import dplyr
#'
#' @import stringr



import_imf_df = function(filepath, countries_vec = NULL){

  id_details = extract_id_from_imf_trade_filepath(filepath)

  category = id_details$category

  country = id_details$country

  if(str_detect(tolower(country),"export")){browser()}

  temp = suppressMessages(read_xlsx(filepath,
                                    range = cell_limits(c(7, 2),
                                                        c(NA, NA))))

  names(temp)[1] = "Counter_Country"

  # # Remove empty (all NA) columns and rows
  #
  # temp = temp %>%
  #   select_if(list(~!all(is.na(.)))) %>%
  #   filter_all(any_vars(!is.na(.)))

  temp = temp %>%
    mutate(Counter_Country = gsub("\\s","_",Counter_Country)) %>%
    {if(!is.null(countries_vec)) filter(
      .,Counter_Country %in% countries_vec) else .} %>%
    gather(.,key = Date, value = !!quo_name(category),
           -Counter_Country) %>%
    mutate(CountryPair = ifelse(Counter_Country < country,
                                paste(Counter_Country,
                                      country, sep = "-"),
                                paste(country,Counter_Country,
                                      sep = "-"))) %>%
    select(Date, CountryPair, !!quo_name(category)) %>%
    mutate(!!quo_name(category) := str_remove(
      !!quo(!!sym(category)),"e"))

  return(temp)


}


#' This function imports export import data from IMF Direction of
#' Trade data base
#'
#' @param export_dirpath (optional) path to directory holding export
#' data files
#'
#' @param import_dirpath (optional) path to directory holding import
#' data files
#'
#' @return data frame
#'
#' @usage import.imf.trade.data(export_dirpath, import_dirpath)
#'

import_imf_trade_data = function(
  export_dirpath = paste0(
    file.path(Sys.getenv("USERPROFILE"), fsep = "\\"),
    "\\OneDrive - Bank Of Israel\\Data",
    "\\IMF\\Export-Import\\Export"),
  import_dirpath = paste0(
    file.path(Sys.getenv("USERPROFILE"), fsep = "\\"),
    "\\OneDrive - Bank Of Israel\\Data",
    "\\IMF\\Export-Import\\Import"),
  countries_vec = NULL){



export_df = lapply(list.files(export_dirpath,full.names = TRUE),
                   import_imf_df,
                   countries_vec = countries_vec) %>%
  bind_rows() %>%
  mutate(Exports = as.numeric(Exports)) %>%
  group_by(Date, CountryPair) %>%
  summarise(Exports = sum(Exports, na.rm = TRUE))


import_df = lapply(list.files(import_dirpath,full.names = TRUE),
                   import_imf_df,
                   countries_vec = countries_vec) %>%
  bind_rows() %>%
  mutate(Imports = as.numeric(Imports)) %>%
  group_by(Date, CountryPair) %>%
  summarise(Imports = sum(Imports, na.rm = TRUE))

trade_df = full_join(export_df,import_df, by = c("Date", "CountryPair")) %>%
  gather(.,key = Balance_Pos, value = Trade, -Date, - CountryPair)

return(trade_df)

}
