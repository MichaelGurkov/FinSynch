#' This function encapsulates the import and processing of the dataset
#'
#' @import dplyr
#'
#' @import purrr
#'

import_dataset = function(){

  params_list = make.params.list()

  # Import wdi_data and bis data and merge into df

  wdi_data = import_wdi_df(NULL,
                           countries_vec = params_list$oecd_countries_vec) %>%
    rename(Date = Year) %>%
    mutate_at(.,vars(starts_with("GDP")),.funs = list(~./1000)) %>%
    deflate.data(.,vars_to_deflate = "GDP") %>%
    deflate.data(.,vars_to_deflate = "GDP_per_Capita") %>%
    select(-GDP,-GDP_per_Capita )


  bis_data = import_bis_fin_cycle_df(filepath_list = list(
    Total_credit =
      paste0("C:\\Users\\Misha\\Documents\\Data\\",
             "BIS\\temp_tot_credit_BIS.rds"),
    House =
      paste0("C:\\Users\\Misha\\Documents\\Data\\",
             "BIS\\temp_house_bis.rds")),
    countries_vec = params_list$oecd_countries_vec) %>%
    group_by(Country, Date = format(Date, "%Y")) %>%
    summarise_all(.,.funs = mean, na.rm = TRUE) %>%
    deflate.data(.,vars_to_deflate = "TotalCredit") %>%
    select(-TotalCredit)

  df = full_join(wdi_data, bis_data)

  rm(wdi_data, bis_data)

  df = df %>%
    filter(Date >=1978 & Date <= 2017) %>%
    group_by(Country) %>%
    mutate_at(.vars = c("GDP_per_Capita_real",
                        "GDP_real", "TotalCredit_real",
                        "HousePrice"),
              .funs = list(ret = ~c(NA,diff(log(.))))) %>%
    mutate_at(.vars = c("TotalCredit_real","HousePrice"),
              .funs = list(fin_cycle = ~ get.clean.cycle(.,filter = "cf"))) %>%
    mutate(GDP_per_Capita_real_cycle = get.clean.cycle(
      GDP_per_Capita_real,my_pl = 2,my_pu = 8)) %>%
    mutate(Fin_ret = rowMeans(data.frame(TotalCredit_real_ret,
                                         HousePrice_ret),na.rm = TRUE),
           Fin_cycle = rowMeans(data.frame(TotalCredit_real_fin_cycle,
                                           HousePrice_fin_cycle),na.rm = TRUE)) %>%
    select(Country, Date, Pop, GDP_per_Capita_real,GDP_real,GDP_real_ret,
           GDP_per_Capita_real_ret,Fin_ret)

  # Merge df with trilemma data

  df = left_join(df, import.trilemma.ind(),
                 by = c("Country","Date"))

  # Merge df with financial development data

  df = left_join(df, import.fin.dev.ind(),
                 by = c("Country","Date"))



  # Import IMF trade data and merge into trade_gdp df

  export_df = lapply(list.files(paste0("C:\\Users\\Misha\\Documents\\Data",
                                       "\\IMF\\Export-Import\\Export"),
                                full.names = TRUE),
                     import_imf_df,
                     countries_vec = params_list$oecd_countries_vec) %>%
    bind_rows() %>%
    mutate(Exports = as.numeric(Exports)) %>%
    group_by(Date, CountryPair) %>%
    summarise(Exports = sum(Exports, na.rm = TRUE))


  import_df = lapply(list.files(paste0("C:\\Users\\Misha\\Documents\\Data",
                                       "\\IMF\\Export-Import\\Import"),
                                full.names = TRUE),
                     import_imf_df,
                     countries_vec = params_list$oecd_countries_vec) %>%
    bind_rows() %>%
    mutate(Imports = as.numeric(Imports)) %>%
    group_by(Date, CountryPair) %>%
    summarise(Imports = sum(Imports, na.rm = TRUE))

  trade_gdp = full_join(export_df,import_df) %>%
    gather(.,key = Balance, value = Trade, -Date, - CountryPair) %>%
    deflate.data(.,vars_to_deflate = "Trade") %>%
    select(-Trade) %>%
    ungroup() %>%
    normalize.imf.data(.,wdi_df = df[,c("Date","Country", "GDP_real")],
                       norm_val = "GDP_real") %>%
    group_by(Date, CountryPair) %>%
    summarise(trade_gdp = mean(log(Trade_real), na.rm = TRUE)) %>%
    filter(!is.na(trade_gdp)) %>%
    filter(is.finite(trade_gdp))

  rm(export_df,import_df)

  # Import bank gdp data

  bank_gdp = import_cross_border_balance(NULL,
                                         countries_vec =
                                           params_list$oecd_countries_vec) %>%
    deflate.data(.,vars_to_deflate = "Avg_Balance") %>%
    select(-Avg_Balance) %>%
    normalize.bis.data(.,wdi_df = df[,c("Date","Country", "GDP_real")],
                       norm_val = "GDP_real") %>%
    group_by(Date, CountryPair) %>%
    summarise(bank_gdp = mean(log(Avg_Balance_real), na.rm = TRUE)) %>%
    filter(!is.na(bank_gdp))


  fin_reg_df = df %>%
    filter(Country %in% params_list$oecd_countries_vec) %>%
    select(Date, Country,Fin_ret) %>%
    get.neg.abs.diff()


  fin_reg_df = list(fin_reg_df,
                    bank_gdp, trade_gdp) %>%
    reduce(full_join, by = c("Date","CountryPair"))


  fin_reg_df = append.countrypair.dataframe(fin_reg_df,
                                            df %>%
                                              select(Date,Country,
                                                     FD,
                                                     FX_stab,
                                                     MI_ind,
                                                     FO_ind)) %>%
    rowwise() %>%
    mutate(FX_stab_avg = mean(c(FX_stab_A,FX_stab_B), na.rm = TRUE)) %>%
    mutate(FO_ind_tot = sum(c(FO_ind_A,FO_ind_B), na.rm = TRUE)) %>%
    mutate(MI_ind_tot = sum(c(MI_ind_A,MI_ind_B), na.rm = TRUE)) %>%
    mutate(FD_avg = mean(c(FD_A, FD_B), na.rm = TRUE)) %>%
    select(-ends_with("_A")) %>%
    select(-ends_with("_B")) %>%
    filter_all(all_vars(!is.infinite(.)))

  rm(bank_gdp, trade_gdp)


  return(dataset_list = list(df = df, fin_reg_df = fin_reg_df))


}



