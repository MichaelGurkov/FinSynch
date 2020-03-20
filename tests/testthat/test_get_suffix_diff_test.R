test_df = data.frame(Var_A = c(100,100), Var_B = c(20,20),
                     Feature_A = c(50,50), Feature_B = c(10,10))



test_df = get.suffix.diff(test_df)

expected_df = data.frame(Var_A = c(100,100), Var_B = c(20,20),
                     Feature_A = c(50,50), Feature_B = c(10,10),
                     Feature_diff = c(40,40), Var_diff = c(80,80))

testthat::expect_equivalent(test_df, expected_df)
