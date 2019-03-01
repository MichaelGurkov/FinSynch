
#' This function process the data files from
#' World Development Indicators and converts
#' the data to tidy format
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @return tidy tibble with Country, Year, Varname columns
#'
#' @export


process.wdi.file = function(temp_file, var_name){

  stopifnot("Country.Name" %in%  names(temp_file))

  temp_file = temp_file %>%
    select(c(Country.Name, grep("^X", names(.),value = TRUE))) %>%
    rename(Country = Country.Name)

  temp_file = temp_file %>%
    gather(.,key = Year,value = UQ(var_name),-Country) %>%
    mutate(Year = str_extract(Year, "X[\\d]{4}")) %>%
    mutate(Year = sub("X","",Year)) %>%
    mutate(Country = levels(Country)[Country]) %>%
    mutate(Country = gsub("\\s","_",Country))


  temp_file = temp_file %>%
    mutate(!! var_name := as.numeric((!!quo(!!sym(var_name)))))

  return(temp_file)

}
