test_df = data.frame(Country.Name = c("Algeria","Argentina"),
                     Country.Code  = c("DZA","ARG"),
                     X1960..YR1960. = c("2466.03823773921",
                                        "5605.19172213837"),
                     X1961..YR1961 = c("2078.22193264149",
                                       "5815.2335573695"))


test_df = process.wdi.file(test_df, var = GDP_per_Capita)

expected_df = data.frame(Country = c("Algeria","Argentina",
                                     "Algeria","Argentina"),
                         Year = c("1960","1960",
                                  "1961","1961"),
                         GDP_per_capita = c(2466.03823773921,
                                            5605.19172213837,
                                            2078.22193264149,
                                            5815.2335573695),
                         stringsAsFactors = FALSE)



testthat::expect_that(test_df,
                      testthat::is_equivalent_to(expected_df))
