#' This helper function deflates bis data by US CPI
#'
#'
#' @import dplyr
#'
#' @import rlang
#'
#' @export


deflate.data = function(df, vars_to_deflate,
                        cpi = NULL,
                        remove_cpi_col = TRUE){

  if(is.null(cpi)){cpi = import.bis.cpi.data()}


  df = left_join(df, cpi, by = "Date")

  for(temp_var in vars_to_deflate){

    temp_var_name = paste(temp_var, "real", sep = "_")

    temp_var = quo(!!sym(temp_var))

    df = df %>%
      mutate(!!temp_var_name := !!temp_var / US_CPI * 100)

  }

  if(remove_cpi_col){df = df %>% select(-US_CPI)}

  return(df)


}





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

  temp_df = full_join(temp_df, country_df,
                      by = c("Date" = "Date","Country_A" = "Country"))

  temp_df = full_join(temp_df, country_df,
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


collapse_pair_controls = function(fin_reg_df, control_vars,
                                  collapse_funcs = c("sum","diff","min")){


 for(temp_var in control_vars){

   if("sum" %in% collapse_funcs){

     fin_reg_df[paste0(temp_var,"_tot")] = fin_reg_df %>%
       select(!!paste0(temp_var[1],c("_A","_B"))) %>%
       apply(.,1,function(temp_row){
         ifelse(sum(is.na(temp_row)) == length(temp_row),
                NA,sum(temp_row, na.rm = TRUE))})
   }

   if("diff" %in% collapse_funcs){

     fin_reg_df[paste0(temp_var,"_diff")] = fin_reg_df %>%
       select(!!paste0(temp_var[1],c("_A","_B"))) %>%
       apply(.,1,function(temp_row){
         ifelse(sum(is.na(temp_row)) == length(temp_row),
                NA,temp_row[1] - temp_row[2])})

   }

   if("min" %in% collapse_funcs){

     fin_reg_df[paste0(temp_var,"_min")] = fin_reg_df %>%
       select(!!paste0(temp_var[1],c("_A","_B"))) %>%
       apply(.,1,function(temp_row){
         ifelse(sum(is.na(temp_row)) == length(temp_row),
                NA,min(temp_row, na.rm = TRUE))})

   }

 }

  fin_reg_df = fin_reg_df %>%
    select(-ends_with("_A")) %>%
    select(-ends_with("_B")) %>%
    ungroup()


  return(fin_reg_df)

}


#' This function constructs regression dataset
#'
#'  @import dplyr
#'
#'

construct_fin_reg = function(df,countries_vec = NULL,
                             control_vars = c("FD","FX_stab","MI_ind",
                                              "FO_ind","PruC","PruC2",
                                              "FD","FI"),
                             collapse_funcs = c("sum","diff","min"),
                             construct_func = "get.neg.abs.diff"){

  temp_function = match.fun(construct_func, descend = FALSE)

  fin_reg_df = df %>%
  {if(!is.null(countries_vec)) filter(.,Country %in% countries_vec) else .} %>%
    select(Date, Country,Fin_ret) %>%
    temp_function() %>%
    rename(Fin_synch = Fin_ret)

  if(!is.null(control_vars)){

    fin_reg_df = append.countrypair.dataframe(fin_reg_df,
                                              df %>%
                                                select(Date,Country,
                                                       !!control_vars))


  }

  if(!is.null(collapse_funcs)){

    fin_reg_df = collapse_pair_controls(fin_reg_df, control_vars,
                                        collapse_funcs = collapse_funcs)

  }


  return(fin_reg_df)

}


#' This function constructs country pair harmon index
#'
#'  @import dplyr
#'
#'

construct_countrypair_harmon_index = function(df, dates_vec = NULL,
                                              index_status = "both"){


  country_pairs_list = combn(unique(df$Country),2) %>%
    apply(.,2,as.list)

  reg_list = df$Directive %>%
    unique() %>%
    as.list()

  # Default dates vec

  if(is.null(dates_vec)){

    dates_vec = unique(df$Date) %>%
      na.omit() %>%
      .[order(.)]

  }

  res = lapply(country_pairs_list,
               function(country_pair, dates_vec){

    countryA = country_pair[[1]]

    countryB = country_pair[[2]]

    dates_vec_list = lapply(reg_list,
                            function(temp_reg, countryA, countryB,
                                     dates_vec){

                              date_max = max(df$Date[df$Directive == temp_reg &
                                                   df$Country == countryA],
                                         df$Date[df$Directive == temp_reg &
                                                   df$Country == countryB])

                              date_min = min(df$Date[df$Directive == temp_reg &
                                                       df$Country == countryA],
                                             df$Date[df$Directive == temp_reg &
                                                       df$Country == countryB])

                              if(is.na(date_min)){
                                return(rep(0, length(dates_vec)))
                                }

                              if(index_status == "both"){

                                temp_vec = as.numeric(dates_vec >= date_max)

                              } else {

                                temp_vec = as.numeric(dates_vec >= date_min &
                                                        dates_vec <= date_max)

                              }

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

#' This function constructs country pair harmon index
#'
#'  @import dplyr
#'
#'


construct_countrypair_EU_index = function(eu_df, dates_vec = NULL,
                                          countries = NULL,
                                          index_status = "both"){

  # Set parameters
  #----------------------------------------------------------------------------

  # Default dates vec

  if(is.null(dates_vec)){

    dates_vec = unique(df$Date) %>%
      na.omit() %>%
      .[order(.)]

  }

  # Default countrypairs

  if(is.null(countries)){

    countries = unique(eu_df$Country)
  }

  eu_countries = unique(eu_df$Country)

  # Construct countrypair df (indicate non EU/cross/both EU countries)
  #----------------------------------------------------------------------------

  countrypairs_df = combn(countries,2) %>%
    t() %>%
    as.data.frame() %>%
    setNames(c("Country","Counter_Country")) %>%
    mutate_all(list(~as.character(.))) %>%
    mutate(CountryPair = ifelse(Country < Counter_Country,
                                paste(Country,Counter_Country, sep = "-"),
                                paste(Counter_Country,Country, sep = "-")))


  countrypairs_df = countrypairs_df %>%
    rowwise() %>%
    mutate(EU_Status = sum(Country %in% eu_countries,
                           Counter_Country %in% eu_countries))

  # Construct output df
  #----------------------------------------------------------------------------

  if(index_status == "both"){

  # None EU countrypairs

  non_eu_countrypairs = countrypairs_df %>%
    filter(EU_Status == 0) %>%
    select(CountryPair) %>%
    ungroup() %>%
    expand(Date = dates_vec, CountryPair) %>%
    mutate(Status = 0)

  # Cross EU countrypairs

  cross_country_pairs = countrypairs_df %>%
    filter(EU_Status == 1) %>%
    select(-EU_Status) %>%
    left_join(eu_df, by = c("Country" = "Country")) %>%
    left_join(eu_df, by = c("Counter_Country" = "Country")) %>%
    select(-Country,-Counter_Country) %>%
    gather(key = Indicator, value = Val, -CountryPair) %>%
    group_by(CountryPair) %>%
    summarise(Date = sum(as.numeric(Val), na.rm = TRUE)) %>%
    apply(1,function(temp_row){

    return(data.frame(Date = dates_vec,
                      CountryPair = temp_row[[1]],
                      Status = as.numeric(dates_vec >= temp_row[[2]])) %>%
             mutate(CountryPair = as.character(CountryPair)))

  }) %>%
    bind_rows()


  # EU countrypairs

  eu_country_pairs = countrypairs_df %>%
    filter(EU_Status == 2) %>%
    select(-EU_Status) %>%
    left_join(eu_df, by = c("Country" = "Country")) %>%
    left_join(eu_df, by = c("Counter_Country" = "Country")) %>%
    select(-Country,-Counter_Country) %>%
    gather(key = Indicator, value = Val, -CountryPair) %>%
    group_by(CountryPair) %>%
    summarise(Date = max(as.numeric(Val), na.rm = TRUE)) %>%
    apply(1,function(temp_row){

      return(data.frame(Date = dates_vec,
                        CountryPair = temp_row[[1]],
                        Status = as.numeric(dates_vec >= temp_row[[2]])) %>%
               mutate(CountryPair = as.character(CountryPair)))

    }) %>%
    bind_rows()


  # Bind all

  df_names = c("non_eu_countrypairs","cross_country_pairs",
               "eu_country_pairs")

  return(bind_rows(mget(df_names[df_names %in% ls()])))

  } else if (index_status == "one"){

    # None and cross EU countrypairs

    non_cross_eu_countrypairs = countrypairs_df %>%
      filter(EU_Status %in% c(0,1)) %>%
      select(CountryPair) %>%
      ungroup() %>%
      expand(Date = dates_vec, CountryPair) %>%
      mutate(Status = 0)

    # EU countrypairs

    eu_country_pairs = countrypairs_df %>%
      filter(EU_Status == 2) %>%
      select(-EU_Status) %>%
      left_join(eu_df, by = c("Country" = "Country")) %>%
      left_join(eu_df, by = c("Counter_Country" = "Country")) %>%
      select(-Country,-Counter_Country) %>%
      gather(key = Indicator, value = Val, -CountryPair) %>%
      group_by(CountryPair) %>%
      summarise(Date = min(as.numeric(Val), na.rm = TRUE)) %>%
      apply(1,function(temp_row){

        return(data.frame(Date = dates_vec,
                          CountryPair = temp_row[[1]],
                          Status = as.numeric(dates_vec >= temp_row[[2]])) %>%
                 mutate(CountryPair = as.character(CountryPair)))

      }) %>%
      bind_rows()


    df_names = c("non_cross_eu_countrypairs",
                 "eu_country_pairs")

    return(bind_rows(mget(df_names[df_names %in% ls()])))

  }

}


#'This function runs panel regression for each strata separately
#'


make_strata_reg_list = function(reg_df,reg_formula,
                             my_effect = "twoways",
                             my_model = "within",
                             criteria_list = NULL){

  if(is.null(criteria_list)){

    countries_list = list(
      oecd_countries = c("Australia","Austria","Belgium","Canada","Chile",
                         "Czech_Republic","Denmark","Estonia","Finland","France",
                         "Germany","Greece","Hungary", "Iceland","Ireland",
                         "Israel","Italy","Japan","Korea","Latvia",
                         "Lithuania","Luxembourg","Mexico","Netherlands",
                         "New_Zealand","Norway","Poland","Portugal",
                         "Slovak_Republic","Slovenia","Spain","Sweden",
                         "Switzerland","Turkey","United_Kingdom"
                         ,"United_States"),
      strong_countries = c("Australia","Austria","Belgium","Canada",
                           "Switzerland","Germany","Denmark","Spain",
                           "Finland","France","United_Kingdom","Ireland",
                           "Italy","Japan","Netherlands","Portugal",
                           "Sweden","United_States"),
      fsap_countries = c("Austria","Belgium","Germany","Denmark","Spain",
                         "France","Finland","Greece","Ireland","Italy",
                         "Luxembourg","Netherlands","Portugal",
                         "Sweden","United_Kingdom"))

    countries_list$weak_countries = countries_list$oecd_countries[!countries_list$oecd_countries %in% countries_list$strong_countries]


    pairs_list = lapply(names(countries_list),
                        function(temp_name){
                          apply(combn(countries_list[[temp_name]],2), 2,
                                function(temp_col){
                                  ifelse(temp_col[1]<temp_col[2],
                                         paste(temp_col[1],temp_col[2],sep = "-"),
                                         paste(temp_col[2],temp_col[1],sep = "-"))})

                        })

    names(pairs_list) = paste(names(countries_list), "pairs", sep = "_")

    countries_list = c(countries_list, pairs_list)

    rm(pairs_list)

    countries_list$cross_country_pairs = countries_list$oecd_countries_pairs[!countries_list$oecd_countries_pairs %in% countries_list$strong_countries_pairs & !countries_list$oecd_countries_pairs %in% countries_list$weak_countries_pairs]


    criteria_list = list(NULL,
                         countries_list$strong_countries_pairs,
                         countries_list$weak_countries_pairs,
                         countries_list$cross_country_pairs)

    names(criteria_list) = c("OECD","KPP","Complement","Cross")


  }

  fin_reg__list = lapply(criteria_list, function(temp_vec){
    temp_reg_df = reg_df %>%
      {if(!is.null(temp_vec)) filter(.,CountryPair %in% temp_vec) else .}

    temp_reg = tryCatch(plm(formula = reg_formula,
                            model = my_model,effect = my_effect,
                            data = temp_reg_df, index = c("CountryPair","Date")),
                        error = function(e){return(NA)})

    return(temp_reg)})

  names(fin_reg__list) = names(criteria_list)

  return(fin_reg__list)

}


#' This function classifies for given country the crises dates
#'

classify_crises_dates = function(Target_Country, dates_vec, crises_df){

  temp_crises_df = crises_df %>%
    filter(Country == Target_Country)

  if(nrow(temp_crises_df) == 0){return(rep(0,length(dates_vec)))}

   crises_dates = apply(temp_crises_df,1,
                        function(temp_row){
                          as.numeric(dates_vec >= temp_row[2] &
                                       dates_vec <= temp_row[3])})

   if(ncol(crises_dates) > 1){

     return(apply(crises_dates,1, sum))

     } else {return(as.vector(crises_dates))}


}


#' This function extracts significant coefficient names from lm model
#'

get_significant_names = function(temp_lm){

  names = names(coefficients(temp_lm))

  names = names[summary(temp_lm)$coefficients[,4] <= 0.1]

  names = names[!grepl("Time_trend$", names)]

  return(names)
}


#' This function imports all data
#'
import.all.data= function(countries_list =NULL){

  if(is.null(countries_list)){

    countries_list = list(
      oecd_countries = c("Australia","Austria","Belgium","Canada","Chile",
                         "Czech_Republic","Denmark","Estonia","Finland","France",
                         "Germany","Greece","Hungary", "Iceland","Ireland",
                         "Israel","Italy","Japan","Korea","Latvia",
                         "Lithuania","Luxembourg","Mexico","Netherlands",
                         "New_Zealand","Norway","Poland","Portugal",
                         "Slovak_Republic","Slovenia","Spain","Sweden",
                         "Switzerland","Turkey","United_Kingdom"
                         ,"United_States"),
      strong_countries = c("Australia","Austria","Belgium","Canada",
                           "Switzerland","Germany","Denmark","Spain",
                           "Finland","France","United_Kingdom","Ireland",
                           "Italy","Japan","Netherlands","Portugal",
                           "Sweden","United_States"),
      fsap_countries = c("Austria","Belgium","Germany","Denmark","Spain",
                         "France","Finland","Greece","Ireland","Italy",
                         "Luxembourg","Netherlands","Portugal",
                         "Sweden","United_Kingdom"))

    countries_list$weak_countries = countries_list$oecd_countries[!countries_list$oecd_countries %in% countries_list$strong_countries]


    pairs_list = lapply(names(countries_list),
                        function(temp_name){
                          apply(combn(countries_list[[temp_name]],2), 2,
                                function(temp_col){
                                  ifelse(temp_col[1]<temp_col[2],
                                         paste(temp_col[1],temp_col[2],sep = "-"),
                                         paste(temp_col[2],temp_col[1],sep = "-"))})

                        })

    names(pairs_list) = paste(names(countries_list), "pairs", sep = "_")

    countries_list = c(countries_list, pairs_list)

    rm(pairs_list)

    countries_list$cross_country_pairs = countries_list$oecd_countries_pairs[!countries_list$oecd_countries_pairs %in% countries_list$strong_countries_pairs & !countries_list$oecd_countries_pairs %in% countries_list$weak_countries_pairs]



  }

  raw_data = list()

  raw_data$HousePrice = import.bis.property.price.data(
    countries_vec = countries_list$oecd_countries) %>%
    mutate(Date = as.yearqtr(Date, format = "%Y-Q%q"))

  raw_data$TotalCredit = import.bis.tot.credit.data(
    countries_vec = countries_list$oecd_countries) %>%
    mutate(Date = as.yearqtr(Date, format = "%Y-Q%q"))

  raw_data$WDI_annual = import_wdi_df(
    countries_vec = countries_list$oecd_countries)


  raw_data$bis_lbs = import.bis.lbs.data(
    countries_vec = countries_list$oecd_countries) %>%
    mutate(Date = as.yearqtr(Date, format = "%Y-Q%q"))

  raw_data$Harmon_both_quarter = import.harmon.data() %>%
    construct_countrypair_harmon_index(.,dates_vec = seq.Date(
      from = as.Date(min(raw_data$bis_lbs$Date)),
      to = as.Date(max(raw_data$bis_lbs$Date)),
      by = "quarter") %>% as.yearqtr())

  raw_data$Harmon_one_quarter = import.harmon.data() %>%
    construct_countrypair_harmon_index(.,dates_vec = seq.Date(
      from = as.Date(min(raw_data$bis_lbs$Date)),
      to = as.Date(max(raw_data$bis_lbs$Date)),
      by = "quarter") %>% as.yearqtr(),index_status = "one")

  raw_data$codes = read.csv(paste0("C:\\Users\\Misha\\Documents",
                                   "\\Data\\ISO\\",
                                   "iso_2digit_alpha_country",
                                   "_codes.csv")) %>%
    setNames(c("Code","Country"))

  # Import EU membership
  #---------------------------------------------------------

   eu_df = read.csv(paste0("C:\\Users\\Misha\\Documents\\",
                          "Data\\Misc\\EU_membership.csv"),
                   stringsAsFactors = FALSE) %>%
    setNames(c("Country","Euro_area","EU"))


  eu_dates_vec = seq.Date(
    from = as.Date(min(raw_data$bis_lbs$Date)),
    to = as.Date(max(raw_data$bis_lbs$Date)),
    by = "year") %>%
    format(.,"%Y")

  raw_data$EU_both = construct_countrypair_EU_index(
    df = eu_df %>%
      select(Country, EU) %>%
      rename(Date = EU),dates_vec = eu_dates_vec) %>%
    rename(EU_both = Status) %>%
    mutate(Date = as.character(Date))

  raw_data$EU_one = construct_countrypair_EU_index(
    df = eu_df %>%
      select(Country, EU) %>%
      rename(Date = EU),dates_vec = eu_dates_vec,
    index_status = "one") %>%
    rename(EU_one = Status) %>%
    mutate(Date = as.character(Date))


  raw_data$Euro_both = construct_countrypair_EU_index(
    df = eu_df %>%
      select(Country, Euro_area) %>%
      rename(Date = Euro_area),dates_vec = eu_dates_vec) %>%
    rename(Euro_both = Status) %>%
    mutate(Date = as.character(Date))

  raw_data$Euro_one = construct_countrypair_EU_index(
    df = eu_df %>%
      select(Country, Euro_area) %>%
      rename(Date = Euro_area),dates_vec = eu_dates_vec,
    index_status = "one") %>%
    rename(Euro_one = Status) %>%
    mutate(Date = as.character(Date))

  rm(eu_df, eu_dates_vec)


  # Import_raw_data_crises_dates

  raw_data$crises_df =  import.crises.dates.df(
    countries_vec = countries_list$oecd_countries)


 # make_annual_df
#------------------------------------------------

  df_list = list(raw_data$TotalCredit %>%
                   filter(quarters(Date) == "Q4") %>%
                   mutate(Date = format(Date, "%Y")) %>%
                   deflate.data(.,vars_to_deflate = "Total_Credit") %>%
                   select(-Total_Credit),
                 raw_data$HousePrice %>%
                   filter(quarters(Date) == "Q4") %>%
                   mutate(Date = format(Date, "%Y")),
                 raw_data$WDI_annual %>%
                   rename(Date = Year) %>%
                   deflate.data(.,vars_to_deflate = c("GDP","GDP_per_Capita"),
                                cpi = raw_data$CPI) %>%
                   select(-GDP, -GDP_per_Capita))


  df = df_list %>%
    reduce(right_join, by = c("Date", "Country")) %>%
    group_by(Country) %>%
    mutate_at(.vars = c("Total_Credit_real","HousePrice"),
              .funs = list(ret = ~c(NA,diff(log(.))))) %>%
    mutate(Fin_ret = rowMeans(data.frame(Total_Credit_real_ret,
                                         HousePrice_ret),na.rm = TRUE)) %>%
    ungroup() %>%
    filter(is.finite(Fin_ret)) %>%
    filter(Date >=1978)

  rm(df_list)




  # make_bank_list
  #---------------------------------------------------

  bank_list = list()

  bank_balance_real = raw_data$bis_lbs %>%
    filter(quarters(Date) == "Q4") %>%
    mutate(Date = format(Date, "%Y")) %>%
    mutate(Balance = Balance * 10 ^ 6) %>%
    deflate.data(.,vars_to_deflate = "Balance") %>%
    select(Date, CountryPair,Balance_Pos, Balance_real)


  bank_list$bank_gdp = bank_balance_real  %>%
    normalize.bis.data(.,norm_df = df[,c("Date","Country", "GDP_real")],
                       norm_val = "GDP_real") %>%
    group_by(Date, CountryPair) %>%
    summarise(bank_gdp = mean(log(Balance_real), na.rm = TRUE)) %>%
    filter(!is.na(bank_gdp))

  # bank_pop = bank_balance_real  %>%
  #   normalize.bis.data(.,norm_df = df[,c("Date","Country", "Pop")],
  #                      norm_val = "Pop") %>%
  #   group_by(Date, CountryPair) %>%
  #   summarise(bank_pop = mean(log(Balance_real), na.rm = TRUE)) %>%
  #   filter(!is.na(bank_pop))

  rm(bank_balance_real)


  # make_indicators_list

  ind_list = list()

  ind_list$Harmon_both =  raw_data$Harmon_both_quarter %>%
    mutate(Date = format(Date, "%Y")) %>%
    group_by(CountryPair, Date, Directive) %>%
    summarise(Transposed = max(Transposed)) %>%
    group_by(Date,CountryPair) %>%
    summarise(Harmon_both_Index = log(
      sum(Transposed + 1,na.rm = TRUE)))

  ind_list$Harmon_one = raw_data$Harmon_one_quarter %>%
    mutate(Date = format(Date, "%Y")) %>%
    group_by(CountryPair, Date, Directive) %>%
    summarise(Transposed = max(Transposed)) %>%
    group_by(Date,CountryPair) %>%
    summarise(Harmon_one_Index = log(
      sum(Transposed + 1,na.rm = TRUE)))

  ind_list$EU_both = raw_data$EU_both

  ind_list$EU_one =  raw_data$EU_one

  ind_list$Euro_both =  raw_data$Euro_both

  ind_list$Euro_one = raw_data$Euro_one


  # Import_IMF_Data

  trade_list = list()

  export_df = lapply(list.files(paste0("C:\\Users\\Misha\\Documents\\Data",
                                       "\\IMF\\Export-Import\\Export"),
                                full.names = TRUE),
                     import_imf_df,
                     countries_vec = countries_list$oecd_countries) %>%
    bind_rows() %>%
    mutate(Exports = as.numeric(Exports)) %>%
    group_by(Date, CountryPair) %>%
    summarise(Exports = sum(Exports, na.rm = TRUE))


  import_df = lapply(list.files(paste0("C:\\Users\\Misha\\Documents\\Data",
                                       "\\IMF\\Export-Import\\Import"),
                                full.names = TRUE),
                     import_imf_df,
                     countries_vec = countries_list$oecd_countries) %>%
    bind_rows() %>%
    mutate(Imports = as.numeric(Imports)) %>%
    group_by(Date, CountryPair) %>%
    summarise(Imports = sum(Imports, na.rm = TRUE))

  trade_df = full_join(export_df,import_df) %>%
    gather(.,key = Balance_Pos, value = Trade, -Date, - CountryPair) %>%
    deflate.data(.,vars_to_deflate = "Trade") %>%
    select(-Trade)


  trade_list$trade_gdp = trade_df %>%
    ungroup() %>%
    normalize.imf.data(.,wdi_df = df[,c("Date","Country", "GDP_real")],
                       norm_val = "GDP_real") %>%
    group_by(Date, CountryPair) %>%
    summarise(trade_gdp = mean(log(Trade_real), na.rm = TRUE)) %>%
    filter(!is.na(trade_gdp)) %>%
    filter(is.finite(trade_gdp))

  rm(export_df, import_df, trade_df)



  # make_countrypair_df


  country_pair_df = unlist(list(bank_list, trade_list, ind_list),
                           recursive = FALSE) %>%
    reduce(left_join, by = c("Date","CountryPair"))


  country_pair_df = country_pair_df %>%
    mutate_at(.vars = vars(c("EU_both","EU_one","Euro_both","Euro_one")),
              .funs = ~(ifelse(is.na(.),0,.)))



  # Import_trilemma_data

  df = left_join(df, import.trilemma.ind(),
                 by = c("Country","Date"))



  # Import_fin_development_data

  df = left_join(df, import.fin.dev.ind(),
                 by = c("Country","Date"))



  # Import_WGI_data

  df = left_join(df, import.wgi.ind(
    countries_vec = countries_list$oecd_countries) %>%
      filter(grepl("Estimate$", Indicator)) %>%
      group_by(Country, Date) %>%
      summarise(WGI = mean(Val, na.rm = TRUE)),
    by = c("Country","Date"))


  #  Dataset construction

  # make_fin_reg_df

  fin_reg_df_annual = construct_fin_reg(
    df = df %>%
      mutate(GDP_real = log(GDP_real),
             Pop = log(Pop)),
    countries_vec = countries_list$oecd_countries,
    control_vars = names(df)[!names(df) %in% c("Country","Date","Fin_ret")],
    collapse_funcs = c("sum"))

  fin_reg_df_annual = fin_reg_df_annual %>%
    full_join(.,country_pair_df, by = c("Date","CountryPair"))

  fin_reg_df_annual = fin_reg_df_annual %>%
    filter(!is.na(CountryPair)) %>%
    filter(!is.na(Fin_synch)) %>%
    filter(!is.na(bank_gdp)) %>%
    filter(!is.na(Date))


  temp_lm_resid = function(x,Time){

    if(sum(!is.na(x)) < 2){return(rep(NA, length(x)))}

    return(residuals(lm(x ~ Time)))

  }


  fin_reg_df_annual = fin_reg_df_annual %>%
    group_by(CountryPair) %>%
    mutate(Time_trend = seq.int(from = 1,to = length(Date))) %>%
    mutate(bank_gdp_delta = c(NA, diff(bank_gdp))) %>%
    mutate(Fin_synch_delta = c(NA, diff(Fin_synch))) %>%
    mutate(bank_gdp_detrended = temp_lm_resid(bank_gdp, Time_trend)) %>%
    mutate(Fin_synch_detrended = temp_lm_resid(Fin_synch, Time_trend)) %>%
    mutate(Harmon_both_detrended = temp_lm_resid(Harmon_both_Index,
                                                 Time_trend)) %>%
    ungroup()

  fin_reg_df_annual$CountryPair_Category[
    fin_reg_df_annual$CountryPair %in%
      countries_list$strong_countries_pairs] = "Strong"

  fin_reg_df_annual$CountryPair_Category[
    fin_reg_df_annual$CountryPair %in%
      countries_list$cross_country_pairs] = "Cross"

  fin_reg_df_annual$CountryPair_Category[
    fin_reg_df_annual$CountryPair %in%
      countries_list$weak_countries_pairs] = "Weak"


  # fin_reg_df_add_crises_indicator

  fin_reg_df_annual = fin_reg_df_annual %>%
    separate(col = CountryPair,into = c("Country_A","Country_B"),
             sep = "-", remove = FALSE) %>%
    group_by(Country_A) %>%
    mutate(Country_A_crises = classify_crises_dates(
      Target_Country = Country_A[1],
      dates_vec = Date,
      crises_df = raw_data$crises_df[,1:3])) %>%
    group_by(Country_B) %>%
    mutate(Country_B_crises = classify_crises_dates(
      Target_Country = Country_B[1],
      dates_vec = Date,
      crises_df = raw_data$crises_df[,1:3])) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(Crises_tot = sum(Country_A_crises, Country_B_crises)) %>%
    mutate(Crises_one = as.numeric(Crises_tot ==1)) %>%
    mutate(Crises_both = as.numeric(Crises_tot ==2)) %>%
    mutate(Crises = min(Crises_tot,1)) %>%
    ungroup() %>%
    select(-Country_A,-Country_B)


  return(fin_reg_df_annual)

}
