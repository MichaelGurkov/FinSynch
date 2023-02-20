#' This function returns the relative path of bibliography file
get.bib = function(){

  bib_path = paste0(file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
                    "\\OneDrive - Bank Of Israel\\References",
                    "\\Financial_Cycle-Global_Financial_Cycle.bib")

  return(bib_path)
}

#' @title Construct country-pair name id
#' This function takes two named vectors (columns with country names) and
#' pastes them together in alphabetical order in order to construct
#'  a country pair name
paste_country_pair = function(country_name, counter_country_name){

  country_pair_name = if_else(country_name < counter_country_name,
                          paste(country_name,counter_country_name,sep = "-"),
                          paste(counter_country_name,country_name,sep = "-"))

  return(country_pair_name)


}
