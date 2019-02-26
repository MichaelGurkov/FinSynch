#' This function imports export import data from
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
