
test_df = data.frame(Country = rep(LETTERS[1:3],5),
                     Date = rep(seq.Date(from = as.Date("2000-01-01"),
                                         by = "year",length.out = 5),
                                each = 3),
                     Credit_ret = rep(1:3,5) * rep(1:5, each = 3),
                     Credit_cycle = rep(1:3,5) * rep(1:5, each = 3))


test_df = make.synch.data(test_df,2)


expected_df = data.frame(Date = rep(seq.Date(from = as.Date("2000-01-01"),
                                             by = "year",length.out = 5),
                                    3),
                         CountryPair = rep(c("A-B","A-C","B-C"), each = 5),
                         Synch1_ret = rep(-1:-5,3) * rep(c(1,2,1), each = 5),
                         Synch1_cycle = rep(-1:-5,3) * rep(c(1,2,1), each = 5),
                         Synch2_ret = c(-2,-1,-2.220446e-16,-1,-2,-4,-2,
                                    -1.998401e-15,-2,-4,-2,-1,-1.776357e-15,
                                    -1,-2),
                         Synch2_cycle = c(-2,-1,-2.220446e-16,-1,-2,-4,-2,
                                        -1.998401e-15,-2,-4,-2,-1,-1.776357e-15,
                                        -1,-2),
                         Synch3_ret = c(rep(c(NA, rep(1,4)),3)),
                         Synch3_cycle = c(rep(c(NA, rep(1,4)),3)),
                         stringsAsFactors = FALSE)

testthat::expect_equivalent(test_df, expected_df)
