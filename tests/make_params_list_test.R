
params_list = make.params.list()


testthat::expect_equal(names(params_list),
                       c("oecd_countries_vec","strong_countries" ,
                         "weak_countries","strong_countries_pairs",
                         "weak_countries_pairs","oecd_countries_pairs",
                         "cross_country_pairs"))
