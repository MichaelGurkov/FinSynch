test_bis_df = data.frame(Date = c(2000,2001),
                         CountryPair = c("Austria-Australia",
                                         "Belgium-Australia"),
                         Balance = c("Total claims",
                                     "Total claims"),
                         Stock = c(100,200), stringsAsFactors = FALSE)

test_cpi_df = data.frame(Date = c(2000,2001),
                         US_CPI = c(1,2), stringsAsFactors = FALSE)

test_df = deflate.bis.data(bis_df = test_bis_df, cpi = test_cpi_df)

expected_df = data.frame(Date = c(2000,2001),
                         CountryPair = c("Austria-Australia",
                                         "Belgium-Australia"),
                         Balance = c("Total claims",
                                     "Total claims"),
                         Stock = c(100,100), stringsAsFactors = FALSE)

testthat::expect_equivalent(test_df, expected_df)
