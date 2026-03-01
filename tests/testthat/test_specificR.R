library(testthat)

source(file.path("scripts", "specificR_lib.R"))

test_that("parse_cli_args reads key-value pairs", {
  args <- c("--config", "config/example_config.yml", "--seed", "42")
  parsed <- parse_cli_args(args)
  expect_equal(parsed$config, "config/example_config.yml")
  expect_equal(parsed$seed, "42")
})

test_that("load_config falls back to defaults when missing", {
  cfg <- load_config("config/does-not-exist.yml")
  expect_equal(cfg$seed, 1337)
  expect_equal(cfg$n_decks, 1)
})

test_that("make_example_deck returns required schema fields", {
  deck <- make_example_deck(1)
  expect_true(!is.null(deck$name))
  expect_true(length(deck$cards) > 0)
  expect_true(all(c("id", "name", "qty", "tags", "metadata") %in% names(deck$cards[[1]])))
})

test_that("validate_deck_schema catches duplicate ids and negative qty", {
  bad_deck <- list(
    name = "Bad Deck",
    cards = list(
      list(id = "x", name = "A", qty = -1, tags = c("spell"), metadata = list(cost = 1, color = "red")),
      list(id = "x", name = "B", qty = 1, tags = c("unit"), metadata = list(cost = 2, color = "blue"))
    )
  )

  res <- validate_deck_schema(bad_deck)
  expect_false(res$valid)
  expect_true(any(grepl("qty", res$errors)))
  expect_true(any(grepl("unique", res$errors)))
})

test_that("summarize_deck computes totals", {
  deck <- make_example_deck(2)
  summary <- summarize_deck(deck)
  expect_equal(summary$deck_name, "Example Deck 2")
  expect_true(summary$n_cards > 0)
  expect_true(summary$avg_cost >= 0)
})

test_that("run_specificr writes required outputs", {
  td <- tempfile("specificr-test-")
  dir.create(td, recursive = TRUE)

  cfg_file <- file.path(td, "cfg.yml")
  writeLines(c(
    "seed: 99",
    "n_decks: 1",
    "strict: false",
    sprintf("output_dir: %s", gsub("\\\\", "/", file.path(td, "out"))),
    sprintf("input_dir: %s", gsub("\\\\", "/", file.path(td, "in")))
  ), cfg_file)

  result <- run_specificr(c("--config", cfg_file))

  expect_true(file.exists(result$files$manifest))
  expect_true(file.exists(result$files$decks))
  expect_true(file.exists(result$files$summary))
  expect_true(file.exists(result$files$qc))
})
