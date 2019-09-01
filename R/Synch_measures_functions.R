#' This function calculates the negative absolute difference measure
#' The function takes a data frame with
#' Date, Country, VarName columns
#'
#' @import dplyr
#' @import tidyr
#'
#'


get.neg.abs.diff = function(df){

  stopifnot("Country" %in% names(df))

  stopifnot("Date" %in% names(df))

  var_name = names(df)[!names(df) %in% c("Country","Date")]

  stopifnot(length(var_name) == 1)

  if(class(df$Country) == "factor"){

    df = df %>%
      mutate(Country = levels(Country)[Country])
  }

  country_combs = combn(unique(df$Country),2)

  wide_df = df %>%
    spread(.,key = Country, value = !!quo_name(var_name))


  neg_abs = apply(country_combs,2, function(temp_row){

    neg_abs = -1 *  abs(wide_df[, temp_row[1]] -
                          wide_df[, temp_row[2]])

    return(data.frame(Date = wide_df$Date,
                      Neg_Abs_Diff = neg_abs))

  })

  neg_abs = neg_abs %>%
    purrr::reduce(inner_join, by = c("Date"))

  colnames(neg_abs) = c("Date", apply(country_combs,2,
                                      function(temp_row){

                                        ifelse(temp_row[1] < temp_row[2],
                                               paste(temp_row[1], temp_row[2],
                                                      sep = "-"),
                                               paste(temp_row[2], temp_row[1],
                                                     sep = "-"))

                                      }))

  neg_abs = neg_abs %>%
    gather(.,key = CountryPair,value = Neg_Abs_Diff, -Date) %>%
    rename(!!var_name := Neg_Abs_Diff)


  return(neg_abs)


}


#' This function calculates the quasi correlation measure
#' The function takes a data frame with
#' Date, Country, VarName columns
#'
#' @import dplyr
#' @import tidyr
#'
#'


get.quasi.cor = function(df){

  stopifnot("Country" %in% names(df))

  stopifnot("Date" %in% names(df))

  var_name = names(df)[!names(df) %in% c("Country","Date")]

  stopifnot(length(var_name) == 1)

  if(class(df$Country) == "factor"){

    df = df %>%
      mutate(Country = levels(Country)[Country])
  }

  country_combs = combn(unique(df$Country),2)

  wide_df = df %>%
    spread(.,key = Country, value = !!quo_name(var_name))


  quasi_corr = apply(country_combs,2, function(temp_row){

    x = unlist(wide_df[, temp_row[1]])

    y = unlist(wide_df[, temp_row[2]])

    quasi_corr = (x - mean(x, na.rm = TRUE)) * (y - mean(y, na.rm = TRUE))

    quasi_corr = quasi_corr / (sd(x, na.rm = TRUE) * sd(y, na.rm = TRUE))

    return(data.frame(Date = wide_df$Date,
                      Quasi_Corr = quasi_corr))

  })

  quasi_corr = quasi_corr %>%
    purrr::reduce(inner_join, by = c("Date"))

  colnames(quasi_corr) = c("Date", apply(country_combs,2,
                                      function(temp_row){

                                        ifelse(temp_row[1] < temp_row[2],
                                               paste(temp_row[1], temp_row[2],
                                                     sep = "-"),
                                               paste(temp_row[2], temp_row[1],
                                                     sep = "-"))

                                      }))

  quasi_corr = quasi_corr %>%
    gather(.,key = CountryPair,value = Quasi_Corr, -Date) %>%
    rename(!!var_name := Quasi_Corr)


  return(quasi_corr)


}



#' This function calculates the residuals of time and country fixed effect
#' The function takes a data frame with
#' Date, Country, VarName columns
#'
#' @import dplyr
#' @import tidyr
#'
#'


get.panel.resid = function(df){

  stopifnot("Country" %in% names(df))

  stopifnot("Date" %in% names(df))

  var_name = names(df)[!names(df) %in% c("Date","Country")]

  temp = lapply(var_name, function(temp_name){

    lm_mod = lm(formula = paste(temp_name, "~",
                               "factor(Date) + factor(Country)"),
               data = df[,c("Date","Country",temp_name)])

    res_df = cbind.data.frame(df[!is.na(df[,temp_name]),
                                 c("Date","Country")],
                     data.frame(residuals(lm_mod)))

    names(res_df)[3] = temp_name

    return(res_df)

  })

  names(temp) = var_name

  return(temp)


}


#' This function calculates the rolling pairwise correlation
#' The function takes a data frame with
#' Date, Country, VarName columns
#'
#' @import dplyr
#' @import tidyr
#' @import zoo
#'


get.roll.cor = function(df, win_len){

  stopifnot("Country" %in% names(df))

  stopifnot("Date" %in% names(df))

  var_name = names(df)[!names(df) %in% c("Country","Date")]

  stopifnot(length(var_name) == 1)

  if(class(df$Country) == "factor"){

    df = df %>%
      mutate(Country = levels(Country)[Country])
  }

  country_combs = combn(unique(df$Country),2)

  wide_df = df %>%
    spread(.,key = Country, value = !!quo_name(var_name))

  dates_vec = wide_df$Date[win_len:length(wide_df$Date)]


  roll_cor = apply(country_combs,2, function(temp_row){

    res = data.frame(Date = dates_vec,
                     CountryPair = paste(temp_row[1],
                                         temp_row[2],
                                         sep = "-"),
                     Roll_cor = zoo::rollapply(wide_df[,
                                                       c(temp_row[1],
                                                         temp_row[2])],
                                               width = win_len,
                                               function(temp){cor(temp)[2,1]},
                                               by.column = FALSE),
                     stringsAsFactors = FALSE)


    return(res)

  })

  roll_cor = do.call(bind_rows, roll_cor)

  names(roll_cor)[3] = var_name

  return(roll_cor)

}




#' This function returns the synch1 measure
#' The function takes a data frame with
#' Date, Country, VarName(s) columns
#'
#' @import  dplyr
#' @import tidyr
#'
#' @export

get.synch1.list = function(df){

  stopifnot("Country" %in% names(df))

  stopifnot("Date" %in% names(df))

  var_name = names(df)[!names(df) %in% c("Country","Date")]

  temp = lapply(var_name, function(temp_name){

    get.neg.abs.diff(df[,c("Date","Country",temp_name)])})

  names(temp) = var_name

  return(temp)

}


#' This function returns the synch2 measure
#' The function takes a data frame with
#' Date, Country, VarName(s) columns
#'
#' @import  dplyr
#' @import tidyr
#'
#' @export

get.synch2.list = function(df){

  stopifnot("Country" %in% names(df))

  stopifnot("Date" %in% names(df))

  var_name = names(df)[!names(df) %in% c("Country","Date")]

  resid_df = get.panel.resid(df) %>%
    purrr::reduce(full_join, by = c("Date","Country"))

  return(get.synch1.list(resid_df))

}



#' This function returns the synch3 measure
#' The function takes a data frame with
#' Date, Country, VarName(s) columns
#'
#' @import  dplyr
#' @import tidyr
#'
#' @export

get.synch3.list = function(df, win_len){

  stopifnot("Country" %in% names(df))

  stopifnot("Date" %in% names(df))

  var_name = names(df)[!names(df) %in% c("Country","Date")]

  temp = lapply(var_name, function(temp_name){

    get.roll.cor(df[,c("Date","Country",temp_name)],
                 win_len = win_len)})

  names(temp) = var_name

  return(temp)

}
