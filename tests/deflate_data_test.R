test_df = data.frame(Date = c(2000,2001),
                         CountryPair = c("Austria-Australia",
                                         "Belgium-Australia"),
                     Var1 = c(100,200),
                     Var2 = c(100,200),
                     stringsAsFactors = FALSE)

test_cpi_df = data.frame(Date = c(2000,2001),
                         US_CPI = c(1,2), stringsAsFactors = FALSE)

test_df = deflate.data(df = test_df,vars_to_deflate = c("Var1","Var2"),
                       cpi = test_cpi_df)

expected_df = data.frame(Date = c(2000,2001),
                         CountryPair = c("Austria-Australia",
                                         "Belgium-Australia"),
                         Var1 = c(100,200),
                         Var2 = c(100,200),
                         Var1_real = c(100,100),
                         Var2_real = c(100,100),
                         stringsAsFactors = FALSE)

testthat::expect_equivalent(test_df, expected_df)
