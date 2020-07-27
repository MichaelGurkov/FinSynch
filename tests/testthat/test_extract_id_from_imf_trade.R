filepath_list = list(
  first = paste0("C:\\Users\\Misha\\OneDrive - Bank Of Israel",
         "\\Data\\IMF\\Export-Import\\Export\\",
         "Australia -Exports_to_Counterpart_Countries",
         ".xlsx"),
  second = paste0("C:\\Users\\Misha\\Documents",
                  "\\Data\\IMF\\Export-Import\\Export\\",
                  "Australia -Exports_to_Counterpart_Countries",
                  ".xlsx"),
  third = paste0("C:\\Users\\internet\\Documents",
                 "\\Data\\IMF\\Export-Import\\Export\\",
                 "Australia -Exports_to_Counterpart_Countries",
                 ".xlsx"),
  fourth = paste0("C:\\Users\\Misha\\OneDrive - Bank Of Israel",
                  "\\Data\\IMF\\Export-Import\\Export",
                  "/Australia -Exports_to_Counterpart_Countries.xlsx")
  )

results_list = map(filepath_list,
                   extract_id_from_imf_trade_filepath)


for (i in length(results_list)) {

  test_that(desc = paste0("extract_id_from_imf_trade_filepath",
                          "returns country"),
            code = expect_equal(
    object = results_list[[i]][["country"]],
    expected = "Australia"))

  test_that(desc = paste0("extract_id_from_imf_trade_filepath",
                          "returns Exports category"),
            code = expect_equal(
              object = results_list[[i]][["category"]],
              expected = "Exports"))

}
