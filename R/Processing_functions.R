#' This function appends to (Date, CountryPair) format data frame
#' data from (Date, Country) format
#'
#'  @import dplyr
#'
#'

append.countrypair.dataframe = function(countrypair_df, country_df){

  stopifnot(sum(c("Date","CountryPair") %in% names(countrypair_df)) == 2)

  stopifnot(sum(c("Date","Country") %in% names(country_df)) == 2)


  temp_df = countrypair_df %>%
    separate(.,col = CountryPair, into = c("Country_A", "Country_B"),
             sep = "-", remove = FALSE)

  temp_df = left_join(temp_df, country_df,
                      by = c("Date" = "Date","Country_A" = "Country"))

  temp_df = left_join(temp_df, country_df,
                      by = c("Date" = "Date","Country_B" = "Country"),
                      suffix = c("_A","_B"))

  temp_df = temp_df %>%
    select(-Country_A,-Country_B)


  return(temp_df)

}

#' This function normalizes the BIS country pairs data
#' The function divides each country pair position by the
#' sum of the countries  population (or GDP) in each year
#' BIS df's structure should be (Date, CountryPair, Balance_Pos, )


normalize.bis.data = function(bis_df,norm_df, norm_val = "GDP"){

  norm_df = norm_df %>%
    select(Country,Date, !!enquo(norm_val))

  var = names(bis_df)[!names(bis_df) %in% c("Date",
                                            "CountryPair",
                                            "Balance_Pos")]


  stopifnot(length(var) == 1)

  var_name = var

  var = quo(!!sym(var))

  bis_df = bis_df %>%
    separate(.,CountryPair, into = c("Country_A","Country_B"),
             sep = "-",remove = FALSE)

  bis_df = left_join(bis_df, norm_df, by = c("Date" = "Date",
                                            "Country_A" = "Country"))

  bis_df = left_join(bis_df,suffix = c("_A","_B"),
                     norm_df, by = c("Date" = "Date",
                                    "Country_B" = "Country"))

  bis_df = bis_df %>%
    mutate(Denom = rowSums(select(.,starts_with(norm_val)))) %>%
    mutate(!!var_name := !!var / Denom) %>%
    select(Date, CountryPair,Balance_Pos,!!var_name)


  return(bis_df)

}


#' This function normalizes IMF country pairs data
#' The function divides each country pair position by the
#' sum of the countries  population (or GDP) in each year


normalize.imf.data = function(imf_df,wdi_df, norm_val = "GDP"){

  wdi_df = wdi_df %>%
    select(Country,Date, !!enquo(norm_val))

  var = names(imf_df)[!names(imf_df) %in% c("Date",
                                            "CountryPair", "Balance_Pos")]


  stopifnot(length(var) == 1)

  var_name = var

  var = quo(!!sym(var))

  imf_df = imf_df %>%
    separate(.,CountryPair, into = c("Country_A","Country_B"),
             sep = "-",remove = FALSE)

  imf_df = left_join(imf_df, wdi_df, by = c("Date" = "Date",
                                            "Country_A" = "Country"))

  imf_df = left_join(imf_df,suffix = c("_A","_B"),
                     wdi_df, by = c("Date" = "Date",
                                    "Country_B" = "Country"))

  imf_df = imf_df %>%
    mutate(Denom = rowSums(select(.,starts_with(norm_val)))) %>%
    mutate(!!var_name := !!var / Denom) %>%
    select(Date, CountryPair,!!var_name)


  return(imf_df)

}


#' This function calculates all the synch measures and merges them
#' into one dataset named accordingly
#'
#' @import dplyr
#'
#' @import purrr
#'

make.synch.data = function(df,win_len){

  synch_1_df = df %>%
    get.synch1.list() %>%
    purrr::reduce(full_join, by = c("CountryPair", "Date")) %>%
    rename_at(vars(ends_with("_ret")), .funs = ~"Synch1_ret") %>%
    rename_at(vars(ends_with("_cycle")), .funs = ~"Synch1_cycle")


  synch_2_df = df %>%
    get.synch2.list() %>%
    purrr::reduce(full_join, by = c("CountryPair", "Date")) %>%
    rename_at(vars(ends_with("_ret")), .funs = ~"Synch2_ret") %>%
    rename_at(vars(ends_with("_cycle")), .funs = ~"Synch2_cycle")



  synch_3_df = df  %>%
    get.synch3.list(.,win_len) %>%
    purrr::reduce(full_join, by = c("CountryPair", "Date")) %>%
    rename_at(vars(ends_with("_ret")), .funs = ~"Synch3_ret") %>%
    rename_at(vars(ends_with("_cycle")), .funs = ~"Synch3_cycle")

  temp_df = list(synch_1_df, synch_2_df, synch_3_df) %>%
    purrr::reduce(full_join, by = c("CountryPair", "Date"))


  return(temp_df)

}


#' This function constructs full df for sensitivity regression
#'
#' @import dplyr
#'
#' @import purrr
#'

make.sens.reg.df = function(df,bank_int,trade_int,win_len,
                            sector = "GDP"){

  if(sector == "GDP"){

    temp_df = make.synch.data(df %>%
                                select(Date, Country,
                                       GDP_per_Capita_real_ret,
                                       GDP_per_Capita_real_cycle),
                              win_len = win_len)

    temp_df = list(temp_df, bank_int$bank_pop, bank_int$bank_gdp,
                   trade_int$trade_pop, trade_int$trade_gdp) %>%
      reduce(full_join, by = c("CountryPair", "Date"))

    temp_df = append.countrypair.dataframe(temp_df, df %>%
                                             select(Date, Country,
                                                    GDP_per_Capita_real,
                                                    Pop))

    temp_df = temp_df %>%
      mutate(LPop = log(Pop_A) * log(Pop_B)) %>%
      mutate(LGDP = log(GDP_per_Capita_real_A) * log(GDP_per_Capita_real_B)) %>%
      select(-ends_with("_A")) %>%
      select(-ends_with("_B"))

    return(temp_df)

  } else {

    temp_df = make.synch.data(df %>%
                                select(Date, Country,Fin_ret,
                                       Fin_cycle),win_len = win_len)

    temp_df = list(temp_df, bank_int$bank_pop, bank_int$bank_gdp,
                   trade_int$trade_pop, trade_int$trade_gdp) %>%
      reduce(full_join, by = c("CountryPair", "Date"))

    temp_df = append.countrypair.dataframe(temp_df, df %>%
                                             select(Date, Country,
                                                    GDP_per_Capita_real,
                                                    Pop,FX, Avg_Policy_Rate))

    temp_df = temp_df %>%
      mutate_at(vars(starts_with("GDP")),.funs = list(~log)) %>%
      mutate_at(vars(starts_with("Pop")),.funs = list(~log)) %>%
      get.suffix.diff(.) %>%
      select(-ends_with("_A")) %>%
      select(-ends_with("_B"))

    return(temp_df)

    }

}


#' This function calculates the difference of columns
#' with given suffixes


get.suffix.diff = function(df, suffix_A = "_A", suffix_B = "_B"){

  a_cols = names(df)[grepl(suffix_A,names(df))]

  a_cols = a_cols[order(a_cols)]

  b_cols = names(df)[grepl(suffix_B,names(df))]

  b_cols = b_cols[order(b_cols)]

  new_cols = gsub(suffix_A,"_diff",a_cols)

  df[,new_cols] = df[,a_cols] - df[,b_cols]

  return(df)


}

#' This function creates parameter list
#'

make.params.list = function(){

  oecd_countries_vec = c("Australia","Austria","Belgium","Canada","Chile",
                         "Czech_Republic","Denmark","Estonia","Finland","France",
                         "Germany","Greece","Hungary", "Iceland","Ireland",
                         "Israel","Italy","Japan","Korea","Latvia",
                         "Lithuania","Luxembourg","Mexico","Netherlands",
                         "New_Zealand","Norway","Poland","Portugal",
                         "Slovak_Republic","Slovenia","Spain","Sweden",
                         "Switzerland","Turkey","United_Kingdom"
                         ,"United_States")

  strong_countries = c("Australia","Austria","Belgium","Canada",
                       "Switzerland","Germany","Denmark","Spain",
                       "Finland","France","United_Kingdom","Ireland",
                       "Italy","Japan","Netherlands","Portugal",
                       "Sweden","United_States")

  weak_countries = oecd_countries_vec[!oecd_countries_vec %in%
                                        strong_countries]


  strong_countries_pairs = apply(combn(strong_countries,2), 2,
                                 function(temp_col){
                                   ifelse(temp_col[1]<temp_col[2],
                                          paste(temp_col[1],temp_col[2],
                                                sep = "-"),
                                          paste(temp_col[2],temp_col[1],
                                                sep = "-"))})

  weak_countries_pairs = apply(combn(weak_countries,2), 2,
                               function(temp_col){
                                 ifelse(temp_col[1]<temp_col[2],
                                        paste(temp_col[1],temp_col[2],
                                              sep = "-"),
                                        paste(temp_col[2],temp_col[1],
                                              sep = "-"))})

  oecd_countries_pairs = apply(combn(oecd_countries_vec,2), 2,
                               function(temp_col){
                                 ifelse(temp_col[1]<temp_col[2],
                                        paste(temp_col[1],temp_col[2],
                                              sep = "-"),
                                        paste(temp_col[2],temp_col[1],
                                              sep = "-"))})

  cross_country_pairs = oecd_countries_pairs[!oecd_countries_pairs %in%
                                               strong_countries_pairs &
                                               !oecd_countries_pairs %in%
                                               weak_countries_pairs]


  return(list(oecd_countries_vec = oecd_countries_vec,
              strong_countries = strong_countries,
              weak_countries = weak_countries,
              strong_countries_pairs = strong_countries_pairs,
              weak_countries_pairs = weak_countries_pairs,
              oecd_countries_pairs = oecd_countries_pairs,
              cross_country_pairs = cross_country_pairs))

}


#' This function collapses (calculates sum, difference and min)


collapse_pair_controls = function(fin_reg_df, control_vars){


 for(temp_var in control_vars){

   fin_reg_df[paste0(temp_var,"_tot")] = fin_reg_df %>%
     select(!!paste0(temp_var[1],c("_A","_B"))) %>%
     apply(.,1,sum, na.rm = TRUE)

   fin_reg_df[paste0(temp_var,"_diff")] = fin_reg_df %>%
     select(!!paste0(temp_var[1],c("_A","_B"))) %>%
     apply(.,1,function(temp_row){temp_row[1] - temp_row[2]})

   fin_reg_df[paste0(temp_var,"_min")] = fin_reg_df %>%
     select(!!paste0(temp_var[1],c("_A","_B"))) %>%
     apply(.,1,min, na.rm = TRUE)



 }

  fin_reg_df = fin_reg_df %>%
    select(-ends_with("_A")) %>%
    select(-ends_with("_B")) %>%
    filter_all(all_vars(!is.infinite(.))) %>%
    ungroup()


  return(fin_reg_df)

}


#' This function constructs regression dataset
#'
#'  @import dplyr
#'
#'

construct_fin_reg = function(df, control_vars = c("FD",
                                              "FX_stab",
                                              "MI_ind",
                                              "FO_ind")){

  fin_reg_df = df %>%
    filter(Country %in% oecd_countries_vec) %>%
    select(Date, Country,Fin_ret) %>%
    get.neg.abs.diff()


  fin_reg_df = list(fin_reg_df,bank_gdp, trade_gdp) %>%
    reduce(full_join, by = c("Date","CountryPair"))


  fin_reg_df = append.countrypair.dataframe(fin_reg_df,
                                            df %>%
                                              select(Date,Country,
                                                     !!control_vars))

  fin_reg_df = collapse_pair_controls(fin_reg_df, control_vars)

  return(fin_reg_df)

}


#' This function constracts country pair harmon index
#'
#'  @import dplyr
#'
#'

construct_countrypair_harmon_index = function(df){


  country_pairs_list = combn(unique(df$Country),2) %>%
    apply(.,2,as.list)

  reg_list = df$Directive %>%
    unique() %>%
    as.list()

  dates_vec = unique(df$Date) %>%
    na.omit() %>%
    .[order(.)]

  res = lapply(country_pairs_list,
               function(country_pair, dates_vec){

    countryA = country_pair[[1]]

    countryB = country_pair[[2]]

    dates_vec_list = lapply(reg_list,
                            function(temp_reg, countryA, countryB,
                                     dates_vec){

                              date = max(df$Date[df$Directive == temp_reg &
                                                   df$Country == countryA],
                                         df$Date[df$Directive == temp_reg &
                                                   df$Country == countryB])

                              date = ifelse(date == -Inf, NA, date)

                              temp_vec = as.numeric(dates_vec >= date)

                              return(temp_vec)


                            },
                            countryA = countryA,
                            countryB = countryB,
                            dates_vec = dates_vec)

    dates_df = do.call(cbind.data.frame, dates_vec_list)

    colnames(dates_df) = unlist(reg_list)

    dates_df$Date = dates_vec

    dates_df = gather(dates_df,key = Directive, value = Transposed, -Date)

    return(dates_df)



  },
               dates_vec = dates_vec)

  names(res) = sapply(country_pairs_list,
                      function(temp_row){ifelse(temp_row[[1]]< temp_row[[2]],
                                                paste(temp_row[[1]],
                                                       temp_row[[2]],
                                                       sep = "-"),
                                                paste(temp_row[[2]],
                                                      temp_row[[1]],
                                                      sep = "-"))})

  res = lapply(names(res),
                function(name){res[[name]] = mutate(res[[name]],
                                                    CountryPair = name)})

  res = do.call(rbind.data.frame,res)


  return(res)



}
