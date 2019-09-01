countrypair_df = data.frame(Date = c(as.Date("2000-01-01"),
                                          as.Date("2001-01-01")),
                                 CountryPair = c("A-B", "B-C"),
                                 Val = c(1,2))

country_df = data.frame(Date = rep(c(as.Date("2000-01-01"),
                                     as.Date("2001-01-01")),3),
                        Country = rep(c("A","B","C"), each = 2),
                        Temp_val = c(1,2,4,8,10,15))

test_df = append.countrypair.dataframe(countrypair_df,country_df)


expected_df = data.frame(Date = c(as.Date("2000-01-01"),
                                  as.Date("2001-01-01")),
                         CountryPair = c("A-B", "B-C"),
                         Val = c(1,2),
                         Temp_val_A = c(1,8),
                         Temp_val_B = c(4,15))


testthat::expect_equivalent(test_df, expected_df)
