#' This function normalizes the BIS country pairs data
#' The function divides each country pair position by the
#' sum of the countries  population (or GDP) in each year


normalize.bis.data = function(bis_df,wdi_df, norm_val = "GDP"){

  wdi_df = wdi_df %>%
    select(Country,Date, !!enquo(norm_val))

  var = names(bis_df)[!names(bis_df) %in% c("Date",
                                            "CountryPair",
                                            "Balance")]


  stopifnot(length(var) == 1)

  var_name = var

  var = quo(!!sym(var))

  bis_df = bis_df %>%
    separate(.,CountryPair, into = c("Country_A","Country_B"),
             sep = "-",remove = FALSE)

  bis_df = left_join(bis_df, wdi_df, by = c("Date" = "Date",
                                     "Country_A" = "Country"))

  bis_df = left_join(bis_df,suffix = c("_A","_B"),
                     wdi_df, by = c("Date" = "Date",
                                     "Country_B" = "Country"))

  bis_df = bis_df %>%
    mutate(Denom = rowSums(select(.,starts_with(norm_val)))) %>%
    mutate(!!var_name := !!var / Denom) %>%
    select(Date, CountryPair,Balance,!!var_name)


 return(bis_df)

}
