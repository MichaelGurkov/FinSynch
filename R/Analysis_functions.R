#' This function performs regression alternating between "pop" and "gdp"
#' specs of explanatory vars
#'
#' @import plm
#'
#' @import dplyr
#'
#' @export


get.reg.list = function(reg_df, dep_var_name, xvars = NULL){

  if(is.null(xvars)){xvars = c("LGDP","LPop","Pop_diff",
                               "GDP_per_Capita_real_diff",
                               "FX_diff","Avg_Policy_Rate_diff")}

  x_vars_pop = c("bank_pop","trade_pop",xvars)

  x_vars_pop = paste(names(reg_df)[names(reg_df) %in% x_vars_pop],
                     collapse = "+")

  x_vars_gdp = c("bank_gdp","trade_gdp",xvars)

  x_vars_gdp = paste(names(reg_df)[names(reg_df) %in% x_vars_gdp],
                     collapse = "+")

  pop_reg = plm(formula(paste(dep_var_name, x_vars_pop, sep = "~")),
                data = reg_df, model = "within", effect = "twoways")

  gdp_reg = plm(formula(paste(dep_var_name, x_vars_gdp, sep = "~")),
                data = reg_df, model = "within", effect = "twoways")

  return(list(pop_reg = pop_reg, gdp_reg = gdp_reg))
}


#' This function performs regressions on every dep var in the data frame
#'
#' @import plm
#'
#' @import dplyr
#'
#' @export

get.sens.reg = function(reg_df){

  dep_vars = names(reg_df)[!grepl(
    paste("bank","trade","LGDP","LPop","DGDP","CountryPair","Date",
          "Avg_Policy_Rate_diff","FX_diff","GDP_per_Capita_real_diff",
          "Pop_diff",sep = "|"),
                           names(reg_df))]

  sens_reg = lapply(dep_vars, get.reg.list, reg_df = reg_df)

  names(sens_reg) = dep_vars

  return(sens_reg)
}
