# Functions for the SpecificR harness.

`%||%` <- function(x, y) if (is.null(x)) y else x

parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- list()
  i <- 1

  while (i <= length(args)) {
    key <- args[[i]]
    if (!startsWith(key, "--")) {
      stop(sprintf("Unexpected argument: %s", key), call. = FALSE)
    }

    value <- args[[i + 1]]
    if (is.null(value) || startsWith(value, "--")) {
      stop(sprintf("Missing value for argument: %s", key), call. = FALSE)
    }

    key_clean <- sub("^--", "", key)
    parsed[[key_clean]] <- value
    i <- i + 2
  }

  parsed
}

default_config <- function() {
  list(
    seed = 1337,
    n_decks = 1,
    strict = FALSE,
    input_dir = "in",
    output_dir = "out",
    run_name = "specificr-example"
  )
}

load_config <- function(config_path = "config/example_config.yml") {
  cfg <- default_config()

  if (!is.null(config_path) && nzchar(config_path) && file.exists(config_path)) {
    loaded <- yaml::read_yaml(config_path)
    if (!is.null(loaded)) {
      for (nm in names(loaded)) {
        cfg[[nm]] <- loaded[[nm]]
      }
    }
  }

  cfg
}

as_flag <- function(value) {
  tolower(as.character(value)) %in% c("true", "1", "yes", "y")
}

apply_cli_overrides <- function(config, cli_args) {
  if (!is.null(cli_args$config)) config$config_path <- cli_args$config
  if (!is.null(cli_args$seed)) config$seed <- as.integer(cli_args$seed)
  if (!is.null(cli_args$n_decks)) config$n_decks <- as.integer(cli_args$n_decks)
  if (!is.null(cli_args$strict)) config$strict <- as_flag(cli_args$strict)
  if (!is.null(cli_args$input_dir)) config$input_dir <- cli_args$input_dir
  if (!is.null(cli_args$output_dir)) config$output_dir <- cli_args$output_dir
  config
}

init_logger <- function(log_path) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(character(), con = log_path)

  function(level, message) {
    line <- sprintf("[%s] [%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, message)
    cat(line, "\n")
    cat(line, "\n", file = log_path, append = TRUE)
  }
}

make_example_deck <- function(deck_index = 1) {
  deck_name <- sprintf("Example Deck %d", deck_index)

  cards <- list(
    list(id = sprintf("D%d-C1", deck_index), name = "Starter Scout", qty = 4L, tags = c("unit", "cheap"), metadata = list(cost = 1L, color = "blue")),
    list(id = sprintf("D%d-C2", deck_index), name = "Stone Golem", qty = 3L, tags = c("unit", "tank"), metadata = list(cost = 3L, color = "gray")),
    list(id = sprintf("D%d-C3", deck_index), name = "Arc Spark", qty = 2L, tags = c("spell"), metadata = list(cost = 2L, color = "red"))
  )

  list(name = deck_name, cards = cards)
}

validate_card <- function(card) {
  errors <- character()

  required <- c("id", "name", "qty", "tags", "metadata")
  for (field in required) {
    if (is.null(card[[field]])) {
      errors <- c(errors, sprintf("card missing required field '%s'", field))
    }
  }

  if (!is.null(card$qty) && (!is.numeric(card$qty) || card$qty < 0)) {
    errors <- c(errors, "card qty must be numeric and >= 0")
  }

  errors
}

validate_deck_schema <- function(deck) {
  errors <- character()
  warnings <- character()

  if (is.null(deck$name) || !nzchar(deck$name)) {
    errors <- c(errors, "deck name is required")
  }

  if (is.null(deck$cards) || length(deck$cards) == 0) {
    warnings <- c(warnings, "deck has no cards")
  }

  card_ids <- character()
  if (!is.null(deck$cards)) {
    for (card in deck$cards) {
      errors <- c(errors, validate_card(card))
      if (!is.null(card$id)) card_ids <- c(card_ids, card$id)
    }
  }

  if (length(card_ids) > 0 && any(duplicated(card_ids))) {
    errors <- c(errors, "card ids must be unique within a deck")
  }

  list(valid = length(errors) == 0, errors = unique(errors), warnings = unique(warnings))
}

summarize_deck <- function(deck) {
  cards <- deck$cards %||% list()
  qty <- vapply(cards, function(card) as.numeric(card$qty %||% 0), numeric(1))
  costs <- vapply(cards, function(card) as.numeric(card$metadata$cost %||% 0), numeric(1))
  colors <- vapply(cards, function(card) as.character(card$metadata$color %||% "unknown"), character(1))

  list(
    deck_name = as.character(deck$name %||% "unknown"),
    n_cards = as.integer(sum(qty)),
    n_unique_cards = as.integer(length(cards)),
    avg_cost = if (length(costs) > 0) mean(costs) else 0,
    max_cost = if (length(costs) > 0) max(costs) else 0,
    colors = paste(sort(unique(colors)), collapse = "|")
  )
}

run_qc <- function(decks, strict = FALSE) {
  deck_results <- lapply(decks, validate_deck_schema)

  errors <- unlist(lapply(deck_results, `[[`, "errors"), use.names = FALSE)
  warnings <- unlist(lapply(deck_results, `[[`, "warnings"), use.names = FALSE)

  report <- list(
    counts = list(
      n_decks = length(decks),
      n_errors = length(errors),
      n_warnings = length(warnings)
    ),
    validation_outcomes = unname(lapply(deck_results, function(x) list(valid = x$valid, errors = x$errors, warnings = x$warnings))),
    errors = errors,
    warnings = warnings,
    strict_mode = strict,
    passed = length(errors) == 0
  )

  if (strict && !report$passed) {
    stop("QC failed in strict mode. See out/qc_report.json for details.", call. = FALSE)
  }

  report
}

get_git_commit <- function() {
  result <- tryCatch(
    system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) NA_character_
  )

  if (length(result) == 0 || is.na(result[[1]]) || !nzchar(result[[1]])) {
    return(NA_character_)
  }

  result[[1]]
}

write_json <- function(x, path) {
  jsonlite::write_json(x, path = path, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

write_outputs <- function(decks, summaries, qc_report, config, run_id, output_dir, logger) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  files <- list(
    decks = file.path(output_dir, "decks.json"),
    summary = file.path(output_dir, "summary.csv"),
    qc = file.path(output_dir, "qc_report.json"),
    manifest = file.path(output_dir, "run_manifest.json")
  )

  write_json(list(decks = decks), files$decks)
  write.csv(do.call(rbind, lapply(summaries, as.data.frame)), files$summary, row.names = FALSE)
  write_json(qc_report, files$qc)

  manifest <- list(
    run_id = run_id,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    git_commit = get_git_commit(),
    config_used = config,
    files_written = unname(unlist(files[c("decks", "summary", "qc")]))
  )
  write_json(manifest, files$manifest)

  logger("INFO", sprintf("Wrote outputs: %s", paste(unlist(files), collapse = ", ")))

  files
}

run_specificr <- function(args = commandArgs(trailingOnly = TRUE)) {
  cli <- parse_cli_args(args)

  config_path <- cli$config %||% "config/example_config.yml"
  config <- load_config(config_path)
  config$config_path <- config_path
  config <- apply_cli_overrides(config, cli)

  set.seed(as.integer(config$seed))

  output_dir <- config$output_dir %||% "out"
  logger <- init_logger(file.path(output_dir, "run.log"))
  logger("INFO", "Starting SpecificR run")
  logger("INFO", sprintf("Using config path: %s", config_path))

  n_decks <- as.integer(config$n_decks %||% 1)
  if (is.na(n_decks) || n_decks < 1) stop("n_decks must be >= 1", call. = FALSE)

  decks <- lapply(seq_len(n_decks), make_example_deck)
  summaries <- lapply(decks, summarize_deck)
  qc_report <- run_qc(decks, strict = as_flag(config$strict))

  run_id <- paste0("specificr-", format(Sys.time(), "%Y%m%d%H%M%S"))
  files <- write_outputs(decks, summaries, qc_report, config, run_id, output_dir, logger)

  logger("INFO", "SpecificR run completed successfully")
  invisible(list(run_id = run_id, files = files, qc_report = qc_report))
}
