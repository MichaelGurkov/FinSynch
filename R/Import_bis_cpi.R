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
