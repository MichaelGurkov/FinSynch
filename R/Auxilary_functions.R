#' This function returns the relative path of bibliography file
get.bib = function(){

  bib_path = paste0(file.path(Sys.getenv("USERPROFILE"),fsep = "\\"),
                    "\\OneDrive - Bank Of Israel\\References",
                    "\\Financial_Cycle-Global_Financial_Cycle.bib")

  return(bib_path)
}
