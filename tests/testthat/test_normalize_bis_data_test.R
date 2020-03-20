
test_bis_df = data.frame(Date = c(2000,2001),
                         CountryPair = c("Austria-Australia",
                                         "Belgium-Australia"),
                         Balance_Pos = c("Total claims",
                                     "Total claims"),
                         Stock = c(100,200), stringsAsFactors = FALSE)

test_wdi_df = data.frame(Date = c(2000,2001,2000,2001,2000,2001),
                         Country = c("Australia","Australia",
                                     "Austria","Austria",
                                     "Belgium","Belgium"),
                         Pop = c(2,2,3,3,8,8), stringsAsFactors = FALSE)

test_df = normalize.bis.data(bis_df = test_bis_df,norm_df = test_wdi_df,
                             norm_val = "Pop")

expected_df = data.frame(Date = c(2000,2001),
                         CountryPair = c("Austria-Australia",
                                         "Belgium-Australia"),
                         Balance_Pos = c("Total claims",
                                     "Total claims"),
                         Stock = c(20,20), stringsAsFactors = FALSE)

testthat::expect_equivalent(test_df, expected_df)
