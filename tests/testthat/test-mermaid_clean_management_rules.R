df <- dplyr::tibble(rules = c(
  "gear restriction",
  "Gear restriction",
  "no take",
  "No take",
  "Open access",
  "open access",
  "gear restriction, period closure",
  "gear restriction;size limit",
  "gear restriction;size limits",
  "species resriction, periodic closures",
  "",
  NA_character_
))

test_that("mermaid_clean_management_rules properly recodes", {
  output <- mermaid_clean_management_rules(df, rules)

  expect_equal(output, dplyr::tibble(rules = c(
    rep("Partial Restrictions", 2),
    rep("No Take", 2),
    rep("Open Access", 2),
    rep("Partial Restrictions", 4),
    rep(NA_character_, 2)
  )))
})

test_that("mermaid_clean_management_rules properly renames and removes", {
  expect_named(mermaid_clean_management_rules(df, rules, name = "Management Rules"), "Management Rules")

  expect_named(mermaid_clean_management_rules(df, rules, name = "Management Rules", remove = FALSE), c("rules", "Management Rules"))
})

test_that(
  "mermaid_clean_management_rules warns if it couldn't parse a value",
  {
    expect_silent(mermaid_clean_management_rules(df, rules))
    expect_warning(mermaid_clean_management_rules(dplyr::tibble(rules = "a"), rules), "Failed to clean")
  }
)

test_that(
  "mermaid_clean_management_rules provides a message if .name is management_rules",
  expect_message(mermaid_clean_management_rules(df, rules, name = "rules"), "Don't specify")
)

test_that("mermaid_clean_management_rules errors if management_rules is not in .data", {
  expect_error(mermaid_clean_management_rules(df, rule), "does not contain column")
})

test_that("mermaid_clean_management_rules errors if .data is not a df", {
  expect_error(mermaid_clean_management_rules(list()), "must be a data frame")
  expect_error(mermaid_clean_management_rules(c()), "must be a data frame")
  expect_silent(mermaid_clean_management_rules(df, rules))
})

test_that("mermaid_clean_management_rules errors if .name is not set and remove is TRUE", {
  expect_error(mermaid_clean_management_rules(df, rules, remove = TRUE), "You can't")
})
