dates_vec = seq.Date(from = as.Date("2000-01-01"),
                     to = as.Date("2003-01-01"),
                     by = "year")

countries_vec = LETTERS[1:3]

test_df = expand.grid(dates_vec, countries_vec)

test_df$Value = c(1:4,2 * 1:4, -1 * 1:4)

names(test_df)[1:2] = c("Date","Country")

test_df = get.roll.cor(test_df,win_len = 2)

expected_df = data.frame(Date = rep(dates_vec[-1]),
                         CountryPair = c(rep("A-B",3),
                                        rep("A-C",3),
                                        rep("B-C",3)),
                         Roll_cor = c(rep(1,3),
                                  rep(-1,3),
                                  rep(-1,3)),
                         stringsAsFactors = FALSE)

testthat::expect_equivalent(test_df, expected_df)
