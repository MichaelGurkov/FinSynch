
eu_df = data.frame(Country = c("Austria","Belgium"),
                Date = c(2000,2001),
                stringsAsFactors = FALSE)

countries = c("Austria","Belgium","Canada", "Djibouti")


# In order to test the "both" dataframe we'll test 3 cases:
#-------------------------------------------------------------------------------
test_df_both = construct_countrypair_EU_index(eu_df = eu_df,
                                              dates_vec = 1999:2002,
                                              countries = countries)


# Case1 : both countries are absent

testthat::expect_equal(sum(test_df_both$Status[
  test_df_both$CountryPair == "Canada-Djibouti"]),0)

# Case2 : only one country is present, the other absent

testthat::expect_equal(test_df_both$Status[
  test_df_both$CountryPair == "Austria-Belgium" &
  test_df_both$Date == 2000],0)

testthat::expect_equal(sum(test_df_both$Status[
  test_df_both$CountryPair == "Austria-Canada"]),0)

# Case3 : both countries are present

testthat::expect_equal(sum(test_df_both$Status[
  test_df_both$CountryPair == "Austria-Belgium" &
    test_df_both$Date == 2001]),1)


# In order to test the "one" dataframe we'll test 3 cases:
#-------------------------------------------------------------------------------
test_df_one = construct_countrypair_EU_index(eu_df = eu_df,
                                             dates_vec = 1999:2002,
                                             countries = countries,
                                             index_status = "one")


# Case1 : both countries are absent

testthat::expect_equal(sum(test_df_one$Status[
  test_df_one$CountryPair == "Canada-Djibouti"]),0)

# Case2 : only one country is present, the other absent

testthat::expect_equal(test_df_one$Status[
  test_df_one$CountryPair == "Austria-Belgium" &
    test_df_one$Date == 2000],1)

testthat::expect_equal(sum(test_df_one$Status[
  test_df_one$CountryPair == "Austria-Canada"]),3)

# Case3 : both countries are present

testthat::expect_equal(sum(test_df_one$Status[
  test_df_one$CountryPair == "Austria-Belgium" &
    test_df_one$Date == 2001]),1)
