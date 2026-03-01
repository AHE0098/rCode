#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(here)
})

project_root <- here::here()
source(file.path(project_root, "scripts", "specificR_lib.R"))

args <- commandArgs(trailingOnly = TRUE)

normalize_arg_path <- function(all_args, key) {
  key_token <- paste0("--", key)
  idx <- which(all_args == key_token)

  if (length(idx) == 1 && length(all_args) >= idx + 1) {
    val <- all_args[[idx + 1]]
    if (!startsWith(val, "/") && !grepl("^[A-Za-z]:[/\\\\]", val)) {
      all_args[[idx + 1]] <- file.path(project_root, val)
    }
  }

  all_args
}

for (k in c("config", "input_dir", "output_dir")) {
  args <- normalize_arg_path(args, k)
}

if (!any(args == "--config")) {
  args <- c(args, "--config", file.path(project_root, "config", "example_config.yml"))
}

run_specificr(args)
