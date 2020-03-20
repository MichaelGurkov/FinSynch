
dates_vec = seq.Date(from = as.Date("2000-01-01"),
                     to = as.Date("2003-01-01"),
                     by = "year")

countries_vec = LETTERS[1:2]

test_df = expand.grid(dates_vec, countries_vec)

test_df$Value = c(1:4,4:1)

names(test_df)[1:2] = c("Date","Country")

test_df = get.neg.abs.diff(test_df)

expected_df = data.frame(Date = dates_vec,
                         CountryPair = rep("A-B",4),
                         Neg_Abs_Diff = c(-3,-1,-1,-3),
                         stringsAsFactors = FALSE)

testthat::expect_equivalent(test_df, expected_df)
