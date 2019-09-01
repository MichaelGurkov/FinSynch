# test_df = data.frame(Country = rep(LETTERS[1:3],5),
#                      Date = rep(seq.Date(from = as.Date("2000-01-01"),
#                                          by = "year",length.out = 5),
#                                 each = 3),
#                      Credit_ret = rep(1:3,5) * rep(1:5, each = 3),
#                      Credit_cycle = rep(1:3,5) * rep(1:5, each = 3))
#
#
# test_bank_int = list(bank_pop = data.frame(Date = rep(seq.Date(
#   from = as.Date("2000-01-01"),by = "year",length.out = 5),3),
#   CountryPair = rep(c("A-B","A-C","B-C"), each = 5),
#   bank_pop = rep(-1:-5,3) * rep(c(1,2,1), each = 5),
#   stringsAsFactors = FALSE),
#   bank_gdp = data.frame(Date = rep(seq.Date(
#     from = as.Date("2000-01-01"),by = "year",length.out = 5),3),
#     CountryPair = rep(c("A-B","A-C","B-C"), each = 5),
#     bank_gdp = rep(-1:-5,3) * rep(c(1,2,1), each = 5),
#     stringsAsFactors = FALSE))
#
#
# test_trade_int = list(trade_pop = data.frame(Date = rep(seq.Date(
#   from = as.Date("2000-01-01"),by = "year",length.out = 5),3),
#   CountryPair = rep(c("A-B","A-C","B-C"), each = 5),
#   trade_pop = rep(-1:-5,3) * rep(c(1,2,1), each = 5),
#   stringsAsFactors = FALSE),
#   trade_gdp = data.frame(Date = rep(seq.Date(
#     from = as.Date("2000-01-01"),by = "year",length.out = 5),3),
#     CountryPair = rep(c("A-B","A-C","B-C"), each = 5),
#     trade_gdp = rep(-1:-5,3) * rep(c(1,2,1), each = 5),
#     stringsAsFactors = FALSE))
