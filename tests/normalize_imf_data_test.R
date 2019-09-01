
test_imf_df = data.frame(CountryPair = c("A-B","B-C"),
                     Date = c(as.Date("2000-01-01"),as.Date("2001-01-01")),
                     Value = c(120,100))

test_wdi_df = data.frame(Country = rep(c("A","B","C"),2),
                         Date = rep(c(as.Date("2000-01-01"),
                                      as.Date("2001-01-01")), each = 3),
                         Pop = c(1:3,2 * 1:3), stringsAsFactors = FALSE)

expected_df = data.frame(Date = c(as.Date("2000-01-01"),as.Date("2001-01-01")),
                         CountryPair = c("A-B","B-C"),
                         Value = c(40,10))

test_df = normalize.imf.data(imf_df = test_imf_df,
                             wdi_df = test_wdi_df,
                             norm_val = "Pop")

testthat::expect_equivalent(object = test_df,
                            expected = expected_df)
