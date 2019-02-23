#' This function returns the cyclical component of time series
#' in case of insufficient observation the function returns NA
#'
#'@import mFilter
#'
#'@export

get.clean.cycle = function(timeseries, my_pl = 8,
                           my_pu = 20, filter = "bk"){

  if(sum(is.na(timeseries)) >=5){return(rep(NA, length(timeseries)))}

  if(filter == "bk"){return(bkfilter(timeseries,pl = my_pl,
                                     pu = my_pu)[["cycle"]])}

  else if(filter == "cf"){return(cffilter(timeseries,pl = my_pl,
                                          pu = my_pu,
                                          drift = TRUE)[["cycle"]])}


}
