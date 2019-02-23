#' This function calculates rolling correlation between country pairs
#'
#' @import xts
#'
#'
#' @export
#'

get.roll_cor = function(df, win_len){

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
                     Roll_cor = rollapply(wide_df[,
                                                  c(temp_row[1],
                                                    temp_row[2])],
                                    width = win_len,
                                    function(temp){cor(temp)[2,1]},
                                    by.column = FALSE),
                     stringsAsFactors = FALSE)


    return(res)

  })

  roll_cor = do.call(bind_rows, roll_cor)

  return(roll_cor)

}
