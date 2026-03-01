rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
gc()

library(tidyverse)
library(glue)
library(stringr)

# =========================================================
# HANDLES
# =========================================================
CFG <- list(
  p_min = 0, p_max = 10,
  t_min = 0, t_max = 10,
  id_start = 2000,
  
  cost_span = 2,
  cost_rule = "halfPT",   # "halfPT" | "offset"
  rounding = "ceiling",   # "round" | "floor" | "ceiling"
  default_cost_offset = -2,
  
  # Baseline: E(C) = a*C + b
  a = 2,
  b = 0,
  
  # Ensure cards also have name/color
  default_name_fmt = "",
  default_color = "",
  
  # If you later join your own name/color data, NA will become these
  na_name_to = "",
  na_color_to = ""
)

rounder <- function(x, mode = c("round","floor","ceiling")) {
  mode <- match.arg(mode)
  switch(mode,
         round   = as.integer(round(x)),
         floor   = as.integer(floor(x)),
         ceiling = as.integer(ceiling(x)))
}

# minimal JS string escape (quotes + backslashes)
js_escape <- function(x) {
  x %>%
    replace_na("") %>%
    str_replace_all("\\\\", "\\\\\\\\") %>%
    str_replace_all("\"", "\\\\\"")
}

make_cards_tbl <- function(cfg = CFG) {
  span <- as.integer(cfg$cost_span)
  
  base <- expand_grid(
    power = cfg$p_min:cfg$p_max,
    toughness = cfg$t_min:cfg$t_max
  ) %>%
    mutate(
      PT = power + toughness,
      C0 = case_when(
        cfg$cost_rule == "halfPT" ~ pmax(0L, rounder(PT / 2, cfg$rounding)),
        cfg$cost_rule == "offset" ~ pmax(0L, PT + as.integer(cfg$default_cost_offset)),
        TRUE ~ pmax(0L, rounder(PT / 2, "round"))
      ),
      base_id_num = cfg$id_start + row_number() - 1L,
      base_id = as.character(base_id_num),
      
      # defaults (you can overwrite later)
      name = glue(cfg$default_name_fmt),
      type = "creature",
      color = cfg$default_color
    )
  
  base %>%
    rowwise() %>%
    mutate(cost = list(seq.int(from = max(0L, C0 - span), to = C0 + span))) %>%
    ungroup() %>%
    unnest(cost) %>%
    mutate(
      id = glue("{base_id}_{cost}"),
      expected_PT = cfg$a * cost + cfg$b,
      value = PT - expected_PT,
      
      # enforce no NA for name/color (and allow you to choose "" or "colorless", etc.)
      name = if_else(is.na(name) | name == "NA", cfg$na_name_to, name),
      color = if_else(is.na(color) | color == "NA", cfg$na_color_to, color)
    ) %>%
    select(id, name, color, cost, type, power, toughness, value)
}

cards_tbl <- make_cards_tbl(CFG)
cards_tbl





# =========================================================
# draft() — one-call deck drafter with locked lands + value targeting
# Requires: cards_tbl exists (from your generator)
# =========================================================

.land_id_map <- c(W="101", U="102", B="103", R="104", G="105")  # edit if your repo differs

.normalize_pool <- function(pool) {
  pool %>%
    mutate(
      id = as.character(id),
      name = as.character(name),
      color = as.character(color),
      cost = as.integer(cost),
      type = as.character(type),
      power = as.integer(power),
      toughness = as.integer(toughness),
      value = as.numeric(value)
    )
}

.make_locked_lands <- function(W=0L,U=0L,B=0L,R=0L,G=0L, land_id_map=.land_id_map, debug=FALSE) {
  
  # force integers explicitly (no named-vector weirdness)
  W <- as.integer(W); U <- as.integer(U); B <- as.integer(B); R <- as.integer(R); G <- as.integer(G)
  if (is.na(W)) W <- 0L; if (is.na(U)) U <- 0L; if (is.na(B)) B <- 0L; if (is.na(R)) R <- 0L; if (is.na(G)) G <- 0L
  if (W < 0L) W <- 0L; if (U < 0L) U <- 0L; if (B < 0L) B <- 0L; if (R < 0L) R <- 0L; if (G < 0L) G <- 0L
  
  counts <- c(W=W, U=U, B=B, R=R, G=G)
  
  if (debug) {
    message("DEBUG counts: ", paste(names(counts), counts, sep="=", collapse=" "))
    message("DEBUG sum(counts)=", sum(counts))
  }
  
  if (sum(counts) <= 0L) return(tibble())
  
  sym <- rep(names(counts), times = as.integer(counts))
  if (debug) message("DEBUG length(sym)=", length(sym))
  if (length(sym) == 0L) return(tibble())
  
  copy <- ave(sym, sym, FUN = seq_along)
  
  id <- paste0(unname(land_id_map[sym]), "_", copy)
  nm <- unname(c(W="Plains", U="Island", B="Swamp", R="Mountain", G="Forest")[sym])
  
  tibble(
    id        = as.character(id),
    name      = as.character(nm),
    color     = as.character(sym),
    cost      = NA_integer_,
    type      = as.character("basic_land"),
    power     = NA_integer_,
    toughness = NA_integer_,
    value     = NA_integer_
  )
}






.value_weights <- function(values, target, spread) {
  if (is.null(spread) || !is.finite(spread) || spread <= 0) return(rep(1, length(values)))
  pmax(dnorm(values, mean = target, sd = spread), 1e-12)
}

.sample_nonlands <- function(pool, n, seed, allow_duplicates, averageValue, spread, tries, tol) {
  if (n <= 0) return(pool[0, , drop = FALSE])
  
  replace_ok <- allow_duplicates || nrow(pool) < n
  vals <- pool$value
  
  # no targeting: plain random
  if (is.null(averageValue) || !is.finite(averageValue)) {
    set.seed(seed)
    return(pool %>% slice_sample(n = n, replace = replace_ok))
  }
  
  # default spread: pool SD
  if (is.null(spread) || !is.finite(spread)) {
    spread <- stats::sd(vals, na.rm = TRUE)
    if (!is.finite(spread) || spread == 0) spread <- 1
  }
  
  w <- .value_weights(vals, averageValue, spread)
  
  best <- NULL
  best_gap <- Inf
  
  set.seed(seed)
  for (i in seq_len(tries)) {
    idx <- sample.int(nrow(pool), size = n, replace = replace_ok, prob = w)
    pick <- pool[idx, , drop = FALSE]
    gap <- abs(mean(pick$value, na.rm = TRUE) - averageValue)
    if (gap < best_gap) { best <- pick; best_gap <- gap }
    if (gap <= tol) break
  }
  best
}


# =========================================================
# Curve helpers (percentages -> counts) + bucket sampler
# =========================================================

.bucketize_cost <- function(cost, bucket_max = 5L) {
  # bucket_max = 5 means "5+"
  cost <- as.integer(cost)
  ifelse(is.na(cost), NA_character_,
         ifelse(cost >= bucket_max, paste0(bucket_max, "+"), as.character(cost)))
}

.curve_pct_to_counts <- function(curve_pct, n_total) {
  # curve_pct: named numeric proportions (0.x)
  # returns named integer vector summing to n_total (largest remainder)
  
  stopifnot(is.numeric(curve_pct), length(curve_pct) > 0)
  
  nm <- names(curve_pct)
  if (is.null(nm) || any(nm == "")) stop("curve_pct must be a *named* numeric vector (names are buckets).")
  
  w <- as.numeric(curve_pct)
  names(w) <- nm                      # <-- KEEP NAMES
  w[!is.finite(w) | w < 0] <- 0
  s <- sum(w)
  if (s <= 0) stop("curve_pct sums to 0 after cleanup.")
  w <- w / s
  
  raw <- w * as.integer(n_total)
  base <- floor(raw)
  rem  <- raw - base
  
  # <-- RESTORE / KEEP NAMES AFTER OPS
  names(base) <- nm
  names(rem)  <- nm
  
  left <- as.integer(n_total) - sum(base)
  
  if (left > 0) {
    add_idx <- order(rem, decreasing = TRUE)[seq_len(left)]
    base[add_idx] <- base[add_idx] + 1L
  }
  
  base <- as.integer(base)
  names(base) <- nm
  base
}

.sample_by_curve <- function(pool,
                             targets,
                             seed,
                             allow_duplicates,
                             averageValue,
                             spread,
                             tries,
                             tol,
                             bucket_max = 5L,
                             spill_order = NULL) {
  # pool must contain: id, cost, value (and any other columns you want to keep)
  # targets: named int vector, names are buckets (e.g. "1","2","3","4","5+")
  if (sum(targets) <= 0) return(pool[0, , drop = FALSE])
  
  pool <- pool %>%
    mutate(.bucket = .bucketize_cost(cost, bucket_max = bucket_max)) %>%
    filter(!is.na(.bucket))
  
  buckets <- names(targets)
  if (is.null(buckets) || any(buckets == "")) stop("targets must be a *named* integer vector (names are buckets).")
  
  # Default spill logic: try neighbors first, then broader
  if (is.null(spill_order)) {
    spill_order <- setNames(vector("list", length(buckets)), buckets)
    top_bucket <- paste0(bucket_max, "+")
    for (b in buckets) {
      if (b == top_bucket) {
        spill_order[[b]] <- setdiff(rev(buckets), b)
      } else {
        bi <- suppressWarnings(as.integer(b))
        if (is.na(bi)) {
          spill_order[[b]] <- setdiff(buckets, b)
        } else {
          neigh <- c(as.character(bi - 1L), as.character(bi + 1L), top_bucket)
          spill_order[[b]] <- c(neigh, buckets) %>% unique() %>% intersect(buckets)
          spill_order[[b]] <- setdiff(spill_order[[b]], b)
        }
      }
    }
  }
  
  picks <- pool[0, , drop = FALSE]
  
  for (k in buckets) {
    need <- as.integer(targets[[k]])
    if (!is.finite(need) || need <= 0) next
    
    sub <- pool %>% filter(.bucket == k) %>% select(-.bucket)
    got <- 0L
    
    if (nrow(sub) > 0) {
      pick_k <- .sample_nonlands(
        pool = sub,
        n = need,
        seed = seed + sum(utf8ToInt(k)),
        allow_duplicates = allow_duplicates,
        averageValue = averageValue,
        spread = spread,
        tries = tries,
        tol = tol
      )
      got <- nrow(pick_k)
      if (got > 0) picks <- bind_rows(picks, pick_k)
    }
    
    missing <- need - got
    if (missing > 0) {
      for (fb in spill_order[[k]]) {
        if (missing <= 0) break
        sub2 <- pool %>% filter(.bucket == fb) %>% select(-.bucket)
        if (nrow(sub2) == 0) next
        
        pick_fb <- .sample_nonlands(
          pool = sub2,
          n = missing,
          seed = seed + sum(utf8ToInt(paste0(k, "->", fb))),
          allow_duplicates = allow_duplicates,
          averageValue = averageValue,
          spread = spread,
          tries = tries,
          tol = tol
        )
        if (nrow(pick_fb) > 0) {
          picks <- bind_rows(picks, pick_fb)
          missing <- missing - nrow(pick_fb)
        }
      }
    }
  }
  
  picks
}



# =========================================================
# Value refinement swaps (preserve curve by swapping within bucket)
# =========================================================
.refine_value_swaps <- function(picks, pool,
                                averageValue,
                                seed = 1337,
                                steps = 400,
                                bucket_max = 5L,
                                allow_duplicates = TRUE) {
  if (is.null(averageValue) || !is.finite(averageValue)) return(picks)
  if (nrow(picks) <= 1) return(picks)
  
  # bucketize both
  picks <- picks %>% mutate(.bucket = .bucketize_cost(cost, bucket_max = bucket_max))
  pool  <- pool  %>% mutate(.bucket = .bucketize_cost(cost, bucket_max = bucket_max))
  
  # only buckets that exist in picks and pool
  buckets <- intersect(unique(picks$.bucket), unique(pool$.bucket))
  buckets <- buckets[!is.na(buckets)]
  if (length(buckets) == 0) return(picks)
  
  # helper: can we reuse card IDs?
  current_ids <- function(df) df$id
  
  set.seed(seed)
  best <- picks
  best_gap <- abs(mean(best$value, na.rm = TRUE) - averageValue)
  
  for (s in seq_len(as.integer(steps))) {
    # pick a bucket that has at least 1 pick and at least 1 pool candidate
    b <- sample(buckets, 1)
    idx_in_bucket <- which(best$.bucket == b)
    if (length(idx_in_bucket) == 0) next
    
    # pick a random slot to potentially replace
    i <- sample(idx_in_bucket, 1)
    
    # candidates from same bucket
    cand <- pool %>% filter(.bucket == b)
    if (nrow(cand) == 0) next
    
    # if no duplicates, exclude already-chosen IDs (except the one we're replacing)
    if (!allow_duplicates) {
      used <- setdiff(current_ids(best), best$id[i])
      cand <- cand %>% filter(!(id %in% used))
      if (nrow(cand) == 0) next
    }
    
    # propose candidate
    j <- sample.int(nrow(cand), 1)
    proposal <- cand[j, , drop = FALSE]
    
    # compute new mean efficiently
    old_vals <- best$value
    old_mean <- mean(old_vals, na.rm = TRUE)
    
    # handle NA values defensively (shouldn't happen for your nonlands)
    old_i <- best$value[i]
    new_i <- proposal$value[[1]]
    if (!is.finite(old_i) || !is.finite(new_i)) next
    
    n <- sum(is.finite(old_vals))
    if (n <= 0) next
    
    new_mean <- old_mean + (new_i - old_i) / n
    new_gap  <- abs(new_mean - averageValue)
    
    # accept if improvement
    if (new_gap < best_gap) {
      best[i, names(proposal)] <- proposal[1, names(proposal)]
      best$.bucket[i] <- b
      best_gap <- new_gap
      
      # optional early stop if very close
      if (best_gap <= 1e-6) break
    }
  }
  
  best %>% select(-.bucket)
}



# =========================================================
# KEYWORDS SYSTEM (value map + assignment + reshuffle)
# Paste this block AFTER your existing helpers.
# It will extend draft() + inspect_deck() without breaking old behavior
# when keyword_assignment_mode = "none".
# =========================================================

# ---- run an expression with a temporary seed (restores .Random.seed)
.with_seed <- function(seed, expr) {
  expr <- substitute(expr)
  
  # .Random.seed is stored in the user's workspace in R (typically .GlobalEnv)
  # We'll restore it to avoid leaking RNG consumption from keyword logic.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }
  }, add = TRUE)
  
  set.seed(as.integer(seed))
  eval(expr, envir = parent.frame())
}

# ---- keyword map cleanup
.kw_prepare_map <- function(keyword_value_map, keyword_pool = NULL) {
  if (is.null(keyword_value_map) || length(keyword_value_map) == 0) {
    return(setNames(numeric(0), character(0)))
  }
  if (is.null(names(keyword_value_map)) || any(names(keyword_value_map) == "")) {
    stop("keyword_value_map must be a *named* numeric vector, e.g. c('Flying'=0.4, 'Defender'=-0.3).")
  }
  kv <- as.numeric(keyword_value_map)
  names(kv) <- names(keyword_value_map)
  
  kv <- kv[is.finite(kv)]
  kv <- kv[!is.na(names(kv)) & names(kv) != ""]
  
  if (!is.null(keyword_pool)) {
    keyword_pool <- as.character(keyword_pool)
    kv <- kv[names(kv) %in% keyword_pool]
  }
  
  kv
}

# ---- keyword count spec: fixed integer OR named probability vector
.kw_parse_count_spec <- function(keyword_count_per_card) {
  if (is.null(keyword_count_per_card)) {
    return(list(type = "fixed", fixed = 0L, min = 0L, max = 0L, expected = 0))
  }
  
  # fixed integer
  if (length(keyword_count_per_card) == 1 && is.numeric(keyword_count_per_card) && is.finite(keyword_count_per_card)) {
    f <- max(0L, as.integer(keyword_count_per_card))
    return(list(type = "fixed", fixed = f, min = f, max = f, expected = as.numeric(f)))
  }
  
  # distribution (names are counts, values are probs)
  probs <- as.numeric(keyword_count_per_card)
  nm <- names(keyword_count_per_card)
  if (is.null(nm) || any(nm == "")) {
    stop("keyword_count_per_card as a distribution must be a *named* numeric vector, e.g. c('0'=0.6,'1'=0.3,'2'=0.1).")
  }
  
  counts <- suppressWarnings(as.integer(nm))
  if (any(is.na(counts))) {
    stop("keyword_count_per_card distribution names must be integers like '0','1','2'.")
  }
  
  probs[!is.finite(probs) | probs < 0] <- 0
  s <- sum(probs)
  if (s <= 0) stop("keyword_count_per_card distribution sums to 0 after cleanup.")
  probs <- probs / s
  
  ord <- order(counts)
  counts <- counts[ord]
  probs  <- probs[ord]
  
  list(
    type = "dist",
    counts = counts,
    probs = probs,
    min = min(counts),
    max = max(counts),
    expected = sum(counts * probs)
  )
}

# ---- sample per-card keyword counts, optionally matching a total keyword count
.kw_sample_counts <- function(n_cards, count_spec, seed, keyword_total_n = NULL) {
  if (n_cards <= 0) {
    return(list(counts = integer(0), feasible = TRUE, msg = "no nonland cards", total = 0L))
  }
  
  # initial draw
  counts <- if (count_spec$type == "fixed") {
    rep.int(count_spec$fixed, n_cards)
  } else {
    .with_seed(seed, {
      sample(count_spec$counts, size = n_cards, replace = TRUE, prob = count_spec$probs)
    })
  }
  
  counts <- as.integer(pmax(count_spec$min, pmin(count_spec$max, counts)))
  
  # optional: force the deck-level keyword total
  if (!is.null(keyword_total_n) && is.finite(keyword_total_n)) {
    target_total <- max(0L, as.integer(keyword_total_n))
    current_total <- sum(counts)
    delta <- target_total - current_total
    
    feasible <- TRUE
    msg <- NULL
    counts2 <- counts
    
    if (delta > 0) {
      for (k in seq_len(delta)) {
        idx <- which(counts2 < count_spec$max)
        if (length(idx) == 0) { feasible <- FALSE; break }
        i <- .with_seed(seed + 10L + k, sample(idx, 1))
        counts2[i] <- counts2[i] + 1L
      }
    } else if (delta < 0) {
      for (k in seq_len(abs(delta))) {
        idx <- which(counts2 > count_spec$min)
        if (length(idx) == 0) { feasible <- FALSE; break }
        i <- .with_seed(seed + 20L + k, sample(idx, 1))
        counts2[i] <- counts2[i] - 1L
      }
    }
    
    counts <- counts2
    if (!feasible) {
      msg <- paste0(
        "Could not reach keyword_total_n=", target_total,
        " given per-card bounds [", count_spec$min, ",", count_spec$max, "]. ",
        "Achieved total=", sum(counts), "."
      )
    }
    
    return(list(counts = counts, feasible = feasible, msg = msg, total = sum(counts)))
  }
  
  list(counts = counts, feasible = TRUE, msg = NULL, total = sum(counts))
}

# ---- pick lambda for exponential-tilted weights so E[v] ~= desired_mean
.kw_tilt_lambda <- function(values, desired_mean, L = 50) {
  if (length(values) == 0) return(0)
  if (!is.finite(desired_mean)) return(0)
  
  vmin <- min(values)
  vmax <- max(values)
  
  # If desired mean is beyond achievable range, clamp.
  if (desired_mean <= vmin) return(-L)
  if (desired_mean >= vmax) return(L)
  
  f <- function(lambda) {
    x <- lambda * values
    x <- x - max(x)            # stabilize exp
    w <- exp(x)
    sum(values * w) / sum(w) - desired_mean
  }
  
  out <- tryCatch(
    stats::uniroot(f, interval = c(-L, L), extendInt = "yes", tol = 1e-10)$root,
    error = function(e) 0
  )
  out
}

# ---- greedy refinement to reduce |desired_total - sum(chosen)| by single-slot replacements
.kw_refine_indices <- function(chosen_idx, values, desired_total, tol_total, max_steps = 200L) {
  if (length(chosen_idx) == 0) return(chosen_idx)
  
  current_vals <- values[chosen_idx]
  current_sum  <- sum(current_vals)
  
  if (!is.finite(desired_total)) return(chosen_idx)
  if (!is.finite(tol_total)) tol_total <- 0
  
  ord <- order(values)
  v_sorted <- values[ord]
  nK <- length(values)
  
  for (step in seq_len(as.integer(max_steps))) {
    gap <- desired_total - current_sum
    if (abs(gap) <= tol_total) break
    
    # For each slot i, we want value_new close to (gap + value_old)
    target <- gap + current_vals
    pos <- findInterval(target, v_sorted)
    
    pos1 <- pmax(1L, pmin(nK, pos))
    pos2 <- pmax(1L, pmin(nK, pos1 + 1L))
    
    cand1 <- v_sorted[pos1]
    cand2 <- v_sorted[pos2]
    
    pick_pos <- ifelse(abs(target - cand1) <= abs(target - cand2), pos1, pos2)
    cand_val <- v_sorted[pick_pos]
    cand_idx <- ord[pick_pos]
    
    new_gap <- gap + current_vals - cand_val
    best_i <- which.min(abs(new_gap))
    
    # Stop if no improvement
    if (abs(new_gap[best_i]) >= abs(gap) - 1e-12) break
    
    # Apply best single-slot replacement
    current_sum <- current_sum + (cand_val[best_i] - current_vals[best_i])
    chosen_idx[best_i] <- cand_idx[best_i]
    current_vals[best_i] <- cand_val[best_i]
  }
  
  chosen_idx
}

# ---- one attempt to pick S keyword slots to match desired_total as close as possible
.kw_pick_keywords_for_total <- function(kv, S, desired_total, seed,
                                        tol_total = 0,
                                        refine_steps = 200L,
                                        tilt_L = 50) {
  if (S <= 0 || length(kv) == 0) {
    return(list(idx = integer(0), total = 0, mode = "none"))
  }
  
  kw_names  <- names(kv)
  kw_values <- as.numeric(kv)
  
  # If no target, sample uniformly and skip refining.
  if (is.null(desired_total) || !is.finite(desired_total)) {
    idx0 <- .with_seed(seed, sample.int(length(kw_values), size = S, replace = TRUE))
    return(list(idx = idx0, total = sum(kw_values[idx0]), mode = "uniform"))
  }
  
  min_total <- S * min(kw_values)
  max_total <- S * max(kw_values)
  
  # If target is out of range, best possible is all-min or all-max.
  if (desired_total <= min_total) {
    imin <- which.min(kw_values)
    idx0 <- rep.int(imin, S)
    return(list(idx = idx0, total = sum(kw_values[idx0]), mode = "all_min"))
  }
  if (desired_total >= max_total) {
    imax <- which.max(kw_values)
    idx0 <- rep.int(imax, S)
    return(list(idx = idx0, total = sum(kw_values[idx0]), mode = "all_max"))
  }
  
  desired_mean <- desired_total / S
  lambda <- .kw_tilt_lambda(kw_values, desired_mean, L = tilt_L)
  
  x <- lambda * kw_values
  x <- x - max(x)
  w <- exp(x)
  prob <- w / sum(w)
  
  idx0 <- .with_seed(seed, sample.int(length(kw_values), size = S, replace = TRUE, prob = prob))
  idx1 <- .kw_refine_indices(idx0, kw_values, desired_total, tol_total = tol_total, max_steps = refine_steps)
  
  list(idx = idx1, total = sum(kw_values[idx1]), mode = "tilt+refine")
}

# ---- apply picked keywords to a nonlands tibble (NO DUPLICATE keywords per card)
.kw_apply_keywords_to_nonlands <- function(nonlands, kv, counts, kw_idx, seed_shuffle = 1L) {
  n_cards <- nrow(nonlands)
  counts <- as.integer(counts)
  
  out0 <- nonlands %>%
    mutate(
      value_base = as.numeric(value),
      keywords = "",
      keyword_value = 0,
      value_final = value_base
    )
  
  if (n_cards == 0 || length(kv) == 0 || sum(counts) == 0) return(out0)
  
  kw_names <- names(kv)
  n_kw <- length(kw_names)
  
  # Cap per-card counts so we can sample without replacement per card
  # (can't assign > number of unique keywords)
  counts_cap <- pmin(counts, n_kw)
  # If you want to keep the *deck total* keyword slots constant, you'd need
  # to redistribute the "lost" slots to other cards; for now we just drop them.
  
  kw_list <- vector("list", n_cards)
  
  # Deterministic per-card sampling: each card gets its own derived seed
  for (i in seq_len(n_cards)) {
    k <- counts_cap[i]
    if (k <= 0L) next
    
    kw_list[[i]] <- .with_seed(seed_shuffle + 9973L * i, {
      sample(kw_names, size = k, replace = FALSE)   # <-- key change
    })
  }
  
  kw_str <- vapply(kw_list, function(x) paste(x, collapse = "|"), character(1))
  kw_val <- vapply(kw_list, function(x) {
    if (length(x) == 0) return(0)
    sum(kv[x], na.rm = TRUE)
  }, numeric(1))
  
  out0 %>%
    mutate(
      keywords = kw_str,
      keyword_value = as.numeric(kw_val),
      value_final = as.numeric(value_base) + as.numeric(keyword_value)
    )
}

# ---- reshuffle keyword assignments until mean(value_final) hits target_avg within tol (or best effort)
.reshuffle_keywords_until_target <- function(nonlands,
                                             keyword_value_map,
                                             keyword_pool = NULL,
                                             keyword_count_per_card = 0L,
                                             keyword_total_n = NULL,
                                             keyword_budget_value = NULL,
                                             target_avg = NULL,
                                             tol = 0.05,
                                             max_tries = 2000L,
                                             refine_steps = 200L,
                                             seed = 1337L,
                                             verbose = FALSE) {
  n_cards <- nrow(nonlands)
  
  # Always add the new columns even if we do "none"
  base <- nonlands %>%
    mutate(
      value_base = as.numeric(value),
      keywords = "",
      keyword_value = 0,
      value_final = as.numeric(value),
    )
  
  kv <- .kw_prepare_map(keyword_value_map, keyword_pool)
  if (n_cards == 0 || length(kv) == 0) {
    diag <- list(
      success = TRUE,
      reason = if (n_cards == 0) "no nonlands" else "empty keyword map/pool",
      tries_used = 0L,
      best_gap = if (!is.null(target_avg) && is.finite(target_avg)) abs(mean(base$value_final, na.rm = TRUE) - target_avg) else NA_real_
    )
    return(list(deck = base, diag = diag))
  }
  
  # infer how many nonlands are actually value-bearing
  base_vals <- base$value_base
  K <- sum(is.finite(base_vals))
  if (K <= 0L) K <- n_cards
  
  base_mean <- mean(base_vals[is.finite(base_vals)], na.rm = TRUE)
  
  cs <- .kw_parse_count_spec(keyword_count_per_card)
  
  # total tolerance on deck total keyword value, derived from mean tolerance
  tol_total <- if (!is.null(target_avg) && is.finite(target_avg)) as.numeric(tol) * K else 0
  
  tries <- as.integer(max_tries)
  best_deck <- NULL
  best_gap <- Inf
  best_diag <- NULL
  
  for (i in seq_len(tries)) {
    s_counts <- .kw_sample_counts(
      n_cards = n_cards,
      count_spec = cs,
      seed = seed + 1000L + i,
      keyword_total_n = keyword_total_n
    )
    counts <- s_counts$counts
    S <- sum(counts)
    
    # determine the keyword total we want to hit
    desired_total <- NA_real_
    if (!is.null(target_avg) && is.finite(target_avg)) {
      desired_total <- (as.numeric(target_avg) - base_mean) * K
    } else if (!is.null(keyword_budget_value) && is.finite(keyword_budget_value)) {
      desired_total <- as.numeric(keyword_budget_value)
    } else {
      desired_total <- NA_real_  # means "random-ish"
    }
    
    pick <- .kw_pick_keywords_for_total(
      kv = kv,
      S = S,
      desired_total = desired_total,
      seed = seed + 20000L + i,
      tol_total = tol_total,
      refine_steps = refine_steps
    )
    
    deck_i <- .kw_apply_keywords_to_nonlands(
      nonlands = nonlands,
      kv = kv,
      counts = counts,
      kw_idx = pick$idx,
      seed_shuffle = seed + 30000L + i
    )
    
    # evaluate gap (mean value_final vs target_avg if present)
    mean_final <- mean(deck_i$value_final, na.rm = TRUE)
    gap <- if (!is.null(target_avg) && is.finite(target_avg)) abs(mean_final - target_avg) else NA_real_
    
    # if no target_avg, accept first deterministic assignment
    if (is.na(gap)) {
      best_deck <- deck_i
      best_gap <- NA_real_
      best_diag <- list(
        success = TRUE,
        reason = "no target averageValue; assigned keywords once",
        tries_used = i,
        base_mean = base_mean,
        mean_final = mean_final,
        total_keywords = S,
        total_keyword_value = sum(deck_i$keyword_value, na.rm = TRUE),
        counts_feasible = s_counts$feasible,
        counts_msg = s_counts$msg,
        pick_mode = pick$mode
      )
      break
    }
    
    if (gap < best_gap) {
      best_gap <- gap
      best_deck <- deck_i
      best_diag <- list(
        success = (gap <= tol),
        reason = if (gap <= tol) "hit target within tolerance" else "best-so-far",
        tries_used = i,
        base_mean = base_mean,
        mean_final = mean_final,
        target_avg = target_avg,
        total_keywords = S,
        total_keyword_value = sum(deck_i$keyword_value, na.rm = TRUE),
        counts_feasible = s_counts$feasible,
        counts_msg = s_counts$msg,
        pick_mode = pick$mode
      )
      if (gap <= tol) break
    }
  }
  
  if (is.null(best_deck)) best_deck <- base
  if (is.null(best_diag)) {
    best_diag <- list(success = FALSE, reason = "unexpected: no keyword attempt produced output", tries_used = tries)
  }
  
  if (verbose && !is.null(target_avg) && is.finite(target_avg)) {
    message(
      "Keyword reshuffle: mean(value_final)=",
      round(mean(best_deck$value_final, na.rm = TRUE), 3),
      " | target=", target_avg,
      " | gap=", round(best_gap, 4),
      " | tries=", best_diag$tries_used
    )
  }
  
  list(deck = best_deck, diag = best_diag)
}

# ---- helper: compute compact keyword stats
.keyword_summary <- function(deck) {
  if (!("keywords" %in% names(deck))) return(tibble(keyword = character(), n = integer()))
  
  x <- deck$keywords
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(tibble(keyword = character(), n = integer()))
  
  kw <- unlist(strsplit(x, "\\|", fixed = FALSE), use.names = FALSE)
  kw <- kw[!is.na(kw) & kw != ""]
  if (length(kw) == 0) return(tibble(keyword = character(), n = integer()))
  
  tibble(keyword = kw) %>% count(keyword, sort = TRUE)
}

# =========================================================
# UPDATED inspect_deck(): prefer value_final when available
# =========================================================
inspect_deck <- function(deck,
                         value_binwidth = 1,
                         show_top_cards = 6,
                         show = TRUE) {
  
  req <- c("type","cost","name","color")
  stopifnot(all(req %in% names(deck)))
  
  value_col <- if ("value_final" %in% names(deck)) "value_final" else "value"
  deck <- deck %>%
    mutate(
      is_land = type == "basic_land",
      value_num = suppressWarnings(as.numeric(.data[[value_col]]))
    )
  
  n_total <- nrow(deck)
  n_lands <- sum(deck$is_land, na.rm = TRUE)
  n_non   <- n_total - n_lands
  
  non <- deck %>% filter(!is_land, is.finite(value_num))
  vals <- non$value_num
  
  val_mean <- if (length(vals)) mean(vals) else NA_real_
  val_sd   <- if (length(vals)) stats::sd(vals) else NA_real_
  pct_neg  <- if (length(vals)) mean(vals < 0) else NA_real_
  pct_pos  <- if (length(vals)) mean(vals > 0) else NA_real_
  
  land_breakdown <- deck %>%
    filter(is_land) %>%
    mutate(color = if_else(is.na(color) | color == "", "?", color),
           name  = if_else(is.na(name)  | name  == "",  "Basic land", name)) %>%
    count(name, color, sort = TRUE)
  
  bins <- non %>%
    mutate(bin = value_binwidth * floor(value_num / value_binwidth)) %>%
    count(bin, name = "n") %>%
    arrange(bin)
  
  suggestions <- c()
  if (n_total == 40 && n_lands < 14) suggestions <- c(suggestions, "Lands ser lave ud for 40 kort. Overvej ~16–18 medmindre du vil have meget lav curve.")
  if (n_total == 40 && n_lands > 19) suggestions <- c(suggestions, "Lands ser høje ud for 40 kort. Overvej ~16–18 medmindre din curve er tung.")
  if (is.finite(val_mean) && val_mean < -0.5) suggestions <- c(suggestions, "Gennemsnitlig value < 0: mange bodies er under-rate. Hæv averageValue eller stram spread.")
  if (is.finite(val_mean) && val_mean > 0.5) suggestions <- c(suggestions, "Gennemsnitlig value > 0: meget effektivt deck. Hvis det føles for stærkt, sænk averageValue eller øg lands.")
  if (is.finite(val_sd) && val_sd < 1) suggestions <- c(suggestions, "Lav spread (sd < 1): konsistent men evt. kedeligt. Hæv spread for mere varians.")
  if (is.finite(val_sd) && val_sd > 3) suggestions <- c(suggestions, "Høj spread (sd > 3): høj varians. Sænk spread for at glatte draws.")
  if (is.finite(pct_neg) && pct_neg > 0.6) suggestions <- c(suggestions, "De fleste nonlands er negative. Overvej højere averageValue eller anden curve.")
  if (length(suggestions) == 0) suggestions <- "Value-distribution + land count ser rimelig ud."
  
  top_cards <- non %>% arrange(desc(value_num)) %>% slice_head(n = show_top_cards) %>%
    select(id, cost, power, toughness, value = value_num)
  bottom_cards <- non %>% arrange(value_num) %>% slice_head(n = show_top_cards) %>%
    select(id, cost, power, toughness, value = value_num)
  
  p <- ggplot(bins, aes(x = bin, y = n)) +
    geom_col() +
    labs(
      title = paste0("Deck value distribution (", value_col, ")"),
      subtitle = glue::glue("N={n_total} | Lands={n_lands} | Nonlands={n_non} | mean={round(val_mean,2)} | sd={round(val_sd,2)} | neg={round(100*pct_neg,0)}%"),
      x = "Card value (binned)",
      y = "Count"
    ) +
    theme_minimal(base_size = 12)
  
  if (show) {
    print(p)
    if (n_lands > 0) {
      cat("\nLands:\n")
      print(land_breakdown)
    }
    cat("\nSuggestions:\n- ", paste(suggestions, collapse = "\n- "), "\n", sep = "")
    cat("\nTop value cards:\n"); print(top_cards)
    cat("\nBottom value cards:\n"); print(bottom_cards)
  }
  
  list(
    summary = tibble(
      N = n_total,
      lands = n_lands,
      nonlands = n_non,
      value_col_used = value_col,
      mean_value_nonlands = val_mean,
      sd_value_nonlands = val_sd,
      pct_negative_nonlands = pct_neg,
      pct_positive_nonlands = pct_pos
    ),
    land_breakdown = land_breakdown,
    value_bins = bins,
    suggestions = suggestions,
    top_cards = top_cards,
    bottom_cards = bottom_cards,
    plot = p
  )
}

# =========================================================
# Manual-add cards (user supplies specs; unspecified cols -> NA)
# =========================================================

.make_manual_cards <- function(manual_add, pool_cols) {
  if (is.null(manual_add) || length(manual_add) == 0) return(tibble())
  
  # allow single spec (a list) or list-of-specs
  if (!is.list(manual_add[[1]])) manual_add <- list(manual_add)
  
  rows <- vector("list", length(manual_add))
  
  for (i in seq_along(manual_add)) {
    s <- manual_add[[i]]
    if (is.null(s$n)) stop("Each manual_add spec must include n= (how many copies).")
    n <- as.integer(s$n)
    if (!is.finite(n) || n <= 0) next
    
    # start as 1-row tibble with ALL pool columns = NA
    one <- as_tibble(setNames(rep(list(NA), length(pool_cols)), pool_cols))
    
    # write provided fields into the row (only those matching pool columns)
    for (nm in names(s)) {
      if (nm == "n") next
      if (nm %in% pool_cols) one[[nm]] <- s[[nm]]
    }
    
    # replicate n times; add unique ids if user didn't specify id
    one_rep <- one[rep(1, n), , drop = FALSE]
    
    if (!("id" %in% names(s)) || is.null(s$id) || is.na(s$id) || s$id == "") {
      base_id <- paste0("MANUAL_", i, "_")
      one_rep$id <- paste0(base_id, seq_len(n))
    } else {
      # if user supplied id, still ensure unique if n>1
      one_rep$id <- paste0(as.character(s$id), "_", seq_len(n))
    }
    
    rows[[i]] <- one_rep
  }
  
  out <- bind_rows(rows)
  
  # normalize types to match the rest of your pipeline as best as possible
  out %>%
    mutate(
      id        = as.character(id),
      name      = as.character(name),
      color     = as.character(color),
      cost      = suppressWarnings(as.integer(cost)),
      type      = as.character(type),
      power     = suppressWarnings(as.integer(power)),
      toughness = suppressWarnings(as.integer(toughness)),
      value     = suppressWarnings(as.numeric(value))
    )
}

# =========================================================
# UPDATED draft(): adds keyword system, keeps old behavior by default
# =========================================================
draft <- function(N = 40L,
                  W = 0L, U = 0L, B = 0L, R = 0L, G = 0L,
                  averageValue = NULL,
                  spread = NULL,
                  tol = 0.05,
                  tries = 200,
                  seed = 1337,
                  allow_duplicates = TRUE,
                  pool = cards_tbl,
                  verbose = TRUE,
                  curve_pct = NULL,
                  bucket_max = 5L,
                  manual_add = NULL,
                  
                  # ---- keyword handles
                  keyword_assignment_mode = c("none", "assign_once", "reshuffle_until_target"),
                  keyword_value_map = NULL,
                  keyword_pool = NULL,
                  keyword_count_per_card = 0L,
                  keyword_total_n = NULL,
                  keyword_budget_value = NULL,
                  keyword_tol = NULL,
                  keyword_max_tries = 2000L,
                  keyword_refine_steps = 200L) {
  
  keyword_assignment_mode <- match.arg(keyword_assignment_mode)
  
  pool <- .normalize_pool(pool)
  lands <- .make_locked_lands(W = W, U = U, B = B, R = R, G = G)
  
  if (verbose) {
    message("Requested lands = ", W+U+B+R+G, " | Built lands = ", nrow(lands))
    if (nrow(lands) > 0 && all(c("type","name","color") %in% names(lands))) {
      print(lands %>% count(.data[["type"]], .data[["name"]], .data[["color"]]))
    }
  }
  if ((W+U+B+R+G) > 0 && nrow(lands) == 0) {
    stop("You requested lands, but .make_locked_lands() returned 0 rows.")
  }
  
  if (nrow(lands) > N) lands <- lands %>% slice_head(n = as.integer(N))
  
  # ---- manual cards (count toward N and curve buckets) ----
  manual <- .make_manual_cards(manual_add, pool_cols = names(pool))
  
  # CRITICAL FIX: ensure manual always has pool columns even when empty
  if (nrow(manual) == 0) manual <- pool[0, , drop = FALSE]
  
  # truncate if user overfills the deck
  if (nrow(lands) + nrow(manual) > N) {
    manual <- manual %>% slice_head(n = max(0L, as.integer(N) - nrow(lands)))
  }
  
  remaining <- as.integer(N) - nrow(lands) - nrow(manual)
  if (remaining < 0) remaining <- 0L
  
  # ---- curve targets (percentages -> counts) ----
  if (is.null(curve_pct)) {
    curve_pct <- c(`1`=0.22, `2`=0.30, `3`=0.22, `4`=0.14, `5+`=0.12)
  }
  
  targets <- .curve_pct_to_counts(curve_pct, remaining)
  names(targets) <- names(curve_pct)
  
  # ---- subtract manual nonland bucket counts from curve targets ----
  if (nrow(manual) > 0 && "type" %in% names(manual)) {
    manual_non <- manual %>% filter(.data[["type"]] != "basic_land")
    if (nrow(manual_non) > 0) {
      manual_buckets <- manual_non %>%
        mutate(bucket = .bucketize_cost(.data[["cost"]], bucket_max = bucket_max)) %>%
        count(bucket, name = "n") %>%
        mutate(bucket = as.character(bucket))
      
      target_tbl <- tibble(bucket = names(targets), target = as.integer(targets))
      
      manual_buckets <- manual_buckets %>%
        right_join(target_tbl, by = "bucket") %>%
        mutate(n = tidyr::replace_na(n, 0L)) %>%
        select(bucket, n)
      
      targets2 <- targets
      targets2[manual_buckets$bucket] <- as.integer(targets2[manual_buckets$bucket]) - as.integer(manual_buckets$n)
      
      if (any(targets2 < 0, na.rm = TRUE)) {
        bad <- names(targets2)[targets2 < 0]
        stop("Manual cards exceed curve targets in bucket(s): ", paste(bad, collapse = ", "),
             ". Reduce manual cards in those costs or adjust curve_pct.")
      }
      targets <- targets2
    }
  }
  
  # ---- if keywords are enabled, interpret averageValue as a target on value_final.
  avg_base_target <- averageValue
  
  if (keyword_assignment_mode != "none" && !is.null(averageValue) && is.finite(averageValue)) {
    kv <- .kw_prepare_map(keyword_value_map, keyword_pool)
    cs <- .kw_parse_count_spec(keyword_count_per_card)
    
    exp_count <- if (!is.null(keyword_total_n) && is.finite(keyword_total_n) && remaining > 0) {
      as.numeric(keyword_total_n) / remaining
    } else {
      cs$expected
    }
    
    exp_kw_val <- if (length(kv) > 0) mean(as.numeric(kv)) else 0
    exp_kw_mean_per_card <- exp_count * exp_kw_val
    avg_base_target <- averageValue - exp_kw_mean_per_card
    
    if (verbose && is.finite(exp_kw_mean_per_card) && exp_kw_mean_per_card != 0) {
      message(
        "Keyword on: drafting base toward ",
        round(avg_base_target, 3),
        " (averageValue_final target=",
        averageValue,
        " minus expected keyword mean ",
        round(exp_kw_mean_per_card, 3),
        ")."
      )
    }
  }
  
  # ---- sample nonlands by curve (value-targeted on base)
  nonlands <- .sample_by_curve(
    pool = pool,
    targets = targets,
    seed = seed,
    allow_duplicates = allow_duplicates,
    averageValue = avg_base_target,
    spread = spread,
    tries = tries,
    tol = tol,
    bucket_max = bucket_max
  )
  
  # ---- refine base mean value without breaking curve
  nonlands <- .refine_value_swaps(
    picks = nonlands,
    pool = pool,
    averageValue = avg_base_target,
    seed = seed + 999,
    steps = 400,
    bucket_max = bucket_max,
    allow_duplicates = allow_duplicates
  )
  
  # ---- apply keywords
  if (is.null(keyword_tol) || !is.finite(keyword_tol)) keyword_tol <- tol
  
  kw_diag <- NULL
  if (keyword_assignment_mode == "none") {
    nonlands2 <- nonlands %>%
      mutate(
        value_base = as.numeric(.data[["value"]]),
        keywords = "",
        keyword_value = 0,
        value_final = as.numeric(.data[["value"]])
      )
  } else if (keyword_assignment_mode == "assign_once") {
    res <- .reshuffle_keywords_until_target(
      nonlands = nonlands,
      keyword_value_map = keyword_value_map,
      keyword_pool = keyword_pool,
      keyword_count_per_card = keyword_count_per_card,
      keyword_total_n = keyword_total_n,
      keyword_budget_value = keyword_budget_value,
      target_avg = averageValue,
      tol = keyword_tol,
      max_tries = 1L,
      refine_steps = keyword_refine_steps,
      seed = seed + 50000L,
      verbose = FALSE
    )
    nonlands2 <- res$deck
    kw_diag <- res$diag
  } else {
    res <- .reshuffle_keywords_until_target(
      nonlands = nonlands,
      keyword_value_map = keyword_value_map,
      keyword_pool = keyword_pool,
      keyword_count_per_card = keyword_count_per_card,
      keyword_total_n = keyword_total_n,
      keyword_budget_value = keyword_budget_value,
      target_avg = averageValue,
      tol = keyword_tol,
      max_tries = keyword_max_tries,
      refine_steps = keyword_refine_steps,
      seed = seed + 50000L,
      verbose = verbose
    )
    nonlands2 <- res$deck
    kw_diag <- res$diag
  }
  
  # keep exact pool columns plus new keyword columns
  lands0  <- lands  %>% select(names(pool))
  manual0 <- manual %>% select(names(pool))
  nonlands0 <- nonlands2 %>% select(names(pool), value_base, keywords, keyword_value, value_final)
  
  # ensure lands + manual also have the new columns
  lands0 <- lands0 %>%
    mutate(
      value_base = suppressWarnings(as.numeric(.data[["value"]])),
      keywords = "",
      keyword_value = 0,
      value_final = as.numeric(value_base) + as.numeric(keyword_value)
    ) %>%
    select(names(pool), value_base, keywords, keyword_value, value_final)
  
  manual0 <- manual0 %>%
    mutate(
      value_base = suppressWarnings(as.numeric(.data[["value"]])),
      keywords = "",
      keyword_value = 0,
      value_final = as.numeric(value_base) + as.numeric(keyword_value)
    ) %>%
    select(names(pool), value_base, keywords, keyword_value, value_final)
  
  out <- bind_rows(lands0, manual0, nonlands0) %>%
    mutate(slot = row_number()) %>%
    select(slot, everything())
  
  attr(out, "keyword_diag") <- kw_diag
  
  if (verbose) {
    if (!("type" %in% names(out))) {
      warning("Could not print curve summary: column 'type' not found in output deck.")
    } else {
      target_tbl <- tibble(bucket = names(targets), target = as.integer(targets))
      
      curve_tbl <- out %>%
        filter(.data[["type"]] != "basic_land") %>%
        mutate(bucket = .bucketize_cost(.data[["cost"]], bucket_max = bucket_max)) %>%
        count(bucket, name = "n") %>%
        mutate(bucket = as.character(bucket)) %>%
        right_join(target_tbl, by = "bucket") %>%
        mutate(n = tidyr::replace_na(n, 0L)) %>%
        arrange(factor(bucket, levels = target_tbl$bucket))
      
      message("Curve (nonlands):")
      print(curve_tbl)
    }
    
    if (!is.null(averageValue) && is.finite(averageValue) &&
        all(c("value_base","value_final","type") %in% names(out))) {
      message(
        "Mean value_base (nonlands) = ",
        round(mean(out$value_base[out$type != "basic_land"], na.rm = TRUE), 3),
        " | Mean value_final (nonlands) = ",
        round(mean(out$value_final[out$type != "basic_land"], na.rm = TRUE), 3),
        " | target averageValue_final = ",
        averageValue
      )
    }
    
    if (keyword_assignment_mode != "none" &&
        all(c("keywords","type") %in% names(out))) {
      kw_sum <- .keyword_summary(out %>% filter(.data[["type"]] != "basic_land")) %>% slice_head(n = 10)
      message("Top keywords (nonlands, top 10):")
      print(kw_sum)
    }
    
    if (!is.null(kw_diag) && is.list(kw_diag) && isFALSE(kw_diag$success)) {
      message("Keyword diagnostics: target could not be hit within tolerance.")
      print(kw_diag)
    }
  }
  
  out
}


# ---- examples ----
kw_map <- c(
  "Flying" = 0.4,
  "Trample" = 0.3,
  "Lifelink" = 0.2,
  "Defender" = -0.3,
  "ETB:Draw" = 0.5
)

e <- draft(
  N = 50,
  U = 9, R = 8,
  curve_pct = c(`1`=.20, `2`=.32, `3`=.24, `4`=.14, `5+`=.10),
  averageValue = 0.2,
  spread = 1,
  seed = 1334,
  
  manual_add = list(
    list(n = 3, name = "Storm", type = "instant", cost = 2),
    list(n = 3, name = "Storm", type = "instant", cost = 1)
  ),
  
  keyword_assignment_mode = "none",
  keyword_value_map = kw_map,
  keyword_count_per_card = c("0"=0.6, "1"=0.3, "2"=0.1),
  keyword_max_tries = 1500,
  keyword_tol = 0.05
)

#attr(d, "keyword_diag")
inspect_deck(e)     # bruger value_final automatisk hvis den findes











rep <- inspect_deck(d, value_binwidth = 1)

rep$summary
rep$land_breakdown
rep$suggestions












# =========================================================
# JS EXPORT: 1+ decks -> copy/paste into decks.paste.js
# - Produces: { id, name, author, createdAt, notes, format, cards:[{cardId,qty}], embeddedCards:{...} }
# - Uses qty format (NO duplicate keys).
# - embeddedCards only for IDs NOT already in repo (by default: MANUAL_*, R_*, or anything not in known_repo_ids)
# =========================================================

# ---- small helpers ----
js_q <- function(x) glue::glue("\"{js_escape(as.character(x))}\"")

js_val <- function(x) {
  if (length(x) == 0 || is.na(x)) return("null")
  if (is.numeric(x)) return(format(as.numeric(x), scientific = FALSE, trim = TRUE))
  if (is.integer(x)) return(as.character(as.integer(x)))
  if (is.logical(x)) return(ifelse(isTRUE(x), "true", "false"))
  # strings
  js_q(x)
}

# turn one card row (from your R deck tibble) into an embeddedCards JS object entry
.to_embedded_card_entry <- function(row) {
  stopifnot(nrow(row) == 1)
  
  id <- as.character(row$id)
  name <- js_escape(as.character(row$name))
  color <- js_escape(as.character(row$color))
  
  cost_js  <- if (is.na(row$cost)) "\"\"" else glue::glue("\"{as.integer(row$cost)}\"")
  power_js <- if (is.na(row$power)) "null" else as.character(as.integer(row$power))
  tough_js <- if (is.na(row$toughness)) "null" else as.character(as.integer(row$toughness))
  value_js <- if (is.na(row$value)) "null" else format(as.numeric(row$value), scientific = FALSE, trim = TRUE)
  
  # optional fields if present in your tibble
  kind_js <- if ("kind" %in% names(row) && !is.na(row$kind) && row$kind != "") js_q(row$kind) else NULL
  type_js <- if ("type" %in% names(row) && !is.na(row$type) && row$type != "") js_q(row$type) else NULL
  
  extras <- c()
  if (!is.null(kind_js)) extras <- c(extras, glue::glue("kind: {kind_js}"))
  if (!is.null(type_js)) extras <- c(extras, glue::glue("type: {type_js}"))
  
  base <- glue::glue(
    "{js_q(id)}: {{ name: \"{name}\", color: \"{color}\", cost: {cost_js}, power: {power_js}, toughness: {tough_js}, value: {value_js}{if (length(extras)) paste0(', ', paste(extras, collapse = ', ')) else ''} }}"
  )
  base
}

# decide whether a card should be embedded (self-contained) vs assumed in CARD_REPO
.should_embed_card <- function(cardId, known_repo_ids = character(0), embed_mode = c("custom_only","missing_only","all_nonlands")) {
  embed_mode <- match.arg(embed_mode)
  id <- as.character(cardId)
  
  if (embed_mode == "missing_only") return(!(id %in% known_repo_ids))
  if (embed_mode == "all_nonlands") return(TRUE)
  
  # custom_only (default): embed only "custom" ids or ids missing from repo
  is_custom <- grepl("^(MANUAL_|R_)", id)
  is_missing <- !(id %in% known_repo_ids)
  is_custom || is_missing
}

# =========================================================
# MAIN: export a list of deck tibbles into decks.paste.js format
# =========================================================
export_decks_paste_js <- function(decks,
                                  meta = NULL,
                                  known_repo_ids = NULL,
                                  embed_mode = c("custom_only","missing_only","all_nonlands"),
                                  createdAt = format(Sys.Date(), "%Y-%m-%d"),
                                  author = "R export",
                                  format = "cardboard") {
  
  embed_mode <- match.arg(embed_mode)
  
  # allow a single tibble
  if (inherits(decks, "data.frame")) decks <- list(decks)
  stopifnot(is.list(decks), length(decks) >= 1)
  
  # known repo ids (optional). If you have cards_tbl / cards.js ids, pass them here.
  if (is.null(known_repo_ids)) known_repo_ids <- character(0)
  known_repo_ids <- as.character(known_repo_ids)
  
  # meta: list of per-deck metadata (same length as decks), or NULL
  if (is.null(meta)) {
    meta <- lapply(seq_along(decks), function(i) list(
      id = glue::glue("r_{format(Sys.Date(), '%Y_%m_%d')}_deck_{i}"),
      name = glue::glue("R Deck {i}"),
      notes = ""
    ))
  }
  if (length(meta) != length(decks)) stop("meta must be NULL or same length as decks.")
  
  deck_objs <- vector("character", length(decks))
  
  for (i in seq_along(decks)) {
    d <- decks[[i]]
    m <- meta[[i]]
    
    req <- c("id","name","color","cost","type","power","toughness","value")
    if (!all(req %in% names(d))) {
      stop("Deck ", i, " is missing required columns: ", paste(setdiff(req, names(d)), collapse=", "))
    }
    
    # collapse to qty by id (exclude slot if present)
    qty_tbl <- d %>%
      dplyr::count(.data$id, name = "qty") %>%
      dplyr::mutate(cardId = as.character(.data$id)) %>%
      dplyr::select(cardId, qty)
    
    cards_lines <- qty_tbl %>%
      dplyr::mutate(js = glue::glue("{{ cardId: {js_q(cardId)}, qty: {as.integer(qty)} }}")) %>%
      dplyr::pull(js)
    
    cards_js <- paste(c("cards: [", paste0("  ", cards_lines, collapse = ",\n"), "]"), collapse = "\n")
    
    # build embeddedCards
    unique_cards <- d %>% dplyr::distinct(.data$id, .keep_all = TRUE)
    embed_ids <- unique_cards$id[vapply(unique_cards$id, .should_embed_card, logical(1),
                                        known_repo_ids = known_repo_ids, embed_mode = embed_mode)]
    
    embedded_js <- ""
    if (length(embed_ids) > 0) {
      emb_entries <- unique_cards %>%
        dplyr::filter(.data$id %in% embed_ids) %>%
        dplyr::group_split(.data$id) %>%
        lapply(.to_embedded_card_entry)
      
      embedded_js <- paste0(
        ",\nembeddedCards: {\n  ",
        paste0(unlist(emb_entries, use.names = FALSE), collapse = ",\n  "),
        "\n}"
      )
    }
    
    deck_objs[[i]] <- paste0(
      "{\n",
      "id: ", js_q(m$id %||% glue::glue("r_{format(Sys.Date(), '%Y_%m_%d')}_deck_{i}")), ",\n",
      "name: ", js_q(m$name %||% glue::glue("R Deck {i}")), ",\n",
      "author: ", js_q(m$author %||% author), ",\n",
      "createdAt: ", js_q(m$createdAt %||% createdAt), ",\n",
      "notes: ", js_q(m$notes %||% ""), ",\n",
      "format: ", js_q(m$format %||% format), ",\n",
      cards_js,
      embedded_js,
      "\n}"
    )
  }
  
  paste0(
    "window.CARDBOARD_PASTED_DECKS = window.CARDBOARD_PASTED_DECKS || [\n",
    paste0("  ", gsub("\n", "\n  ", deck_objs), collapse = ",\n\n"),
    "\n];\n"
  )
}

# helper like %||% in rlang
`%||%` <- function(x, y) if (is.null(x) || length(x)==0 || is.na(x)) y else x


















