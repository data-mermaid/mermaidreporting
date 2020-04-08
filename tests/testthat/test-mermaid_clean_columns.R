test_that("mermaid_clean_columns unpacks df cols, adds prefix by default", {
  df <- tibble::tibble(x = 1,
                       y = tibble::tibble(a = 1, b = 2))

  output <- mermaid_clean_columns(df)
  expect_named(output, c("x", "y_a", "y_b"))
})

test_that("mermaid_clean_columns doesn't add prefix if set not to", {
  df <- tibble::tibble(x = 1,
                        y = tibble::tibble(a = 1, b = 2))
  expect_named(mermaid_clean_columns(df, .append_column_prefix = FALSE), c("x", "a", "b"))
})

test_that("mermaid_clean_columns cleans names by default, doesn't if set not to", {
  df <- tibble::tibble(x = 1,
                       y = tibble::tibble(`Type One` = 1, `type-2` = 2))
  expect_named(mermaid_clean_columns(df), c("x", "y_type_one", "y_type_2"))
  expect_named(mermaid_clean_columns(df, .clean_names = FALSE), c("x", "y_Type One", "y_type-2"))
})

test_that("mermaid_clean_columns allows different cases, errors if not in list", {
  df <- tibble::tibble(x = 1,
                       y = tibble::tibble(`Type One` = 1, `type-2` = 2))
  expect_named(mermaid_clean_columns(df, .append_column_prefix = FALSE, .clean_names_case = "sentence"), c("X", "Type one", "Type 2"))
  expect_named(mermaid_clean_columns(df, .append_column_prefix = FALSE, .clean_names_case = "title"), c("X", "Type One", "Type 2"))
  expect_named(mermaid_clean_columns(df, .append_column_prefix = FALSE, .clean_names_case = "lower_camel"), c("x", "typeOne", "type2"))
  expect_named(mermaid_clean_columns(df, .append_column_prefix = FALSE, .clean_names_case = "upper_camel"), c("X", "TypeOne", "Type2"))
  expect_error(mermaid_clean_columns(df, .clean_names_case = "mixed"), "one of")
})