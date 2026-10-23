# Shared scheduling infrastructure for parallelizing the three
# run_benchmark_*() orchestration functions over the replication ("rep")
# grain - each independent unit of work is one full replication's worth of
# every engine/variant row, for a given outcome/n/rep[/response_parallel]
# combination. See /Users/a1238677/.claude/plans/tingly-foraging-twilight.md
# for the full design rationale; in short: plain `future` rather than
# furrr/progressr, so progress reporting stays on progress_note()'s proven
# raw-stderr mechanism (R/cache.R) instead of routing through R's condition
# system - the same system that made message() get buffered under knitr's
# evaluate() during Quarto Render, which is why progress_note() bypasses
# message() in the first place.

# Enumerates one row per independent unit of parallel work. Column order
# matches the original nested-loop order (outcome outermost, then
# response_parallel if present, then n, then rep innermost), so that
# iterating the grid sequentially in row order reproduces the exact same
# computation in the exact same sequence as the pre-parallel code - this is
# what step 3 of the plan's verification checks (bit-identical output under
# future::plan(sequential)).
build_benchmark_grid <- function(outcomes, n_values, B, response_parallel_values = NULL) {
  if (is.null(response_parallel_values)) {
    grid <- expand.grid(
      rep = seq_len(B),
      n_idx = seq_along(n_values),
      outcome = outcomes,
      stringsAsFactors = FALSE
    )
  } else {
    grid <- expand.grid(
      rep = seq_len(B),
      n_idx = seq_along(n_values),
      response_parallel = response_parallel_values,
      outcome = outcomes,
      stringsAsFactors = FALSE
    )
  }
  # expand.grid() varies the first column fastest - reverse column order so
  # bind_rows()-over-grid-order below matches "outcome outermost...rep
  # innermost" rather than the alphabetical-ish default expand.grid() gives.
  grid <- grid[, rev(names(grid)), drop = FALSE]
  grid$n <- n_values[grid$n_idx]
  grid$cell_index <- seq_len(nrow(grid))
  rownames(grid) <- NULL
  grid
}

# Dispatches one future per grid row (a "cell"), polls for completion, and
# calls progress_note() from the *dispatching* process only - each cell's
# actual engine/row-level fitting happens entirely inside its own worker,
# invisible to the dispatcher until the whole cell resolves. This is the
# reason progress granularity necessarily coarsens from "per fit" to
# "per replication" under parallel execution (see the design doc).
#
# `cell_fn(cell_row)` must return a named list of tibbles (or NULL entries
# where a field doesn't apply, e.g. causal's conditional `agreement`) -
# mirroring exactly what one replication currently contributes to each
# accumulator in the sequential code.
# `label_fn(cell_row)` returns the human-readable string passed to
# progress_note() once that cell completes.
#
# Results are returned in ORIGINAL grid order (not completion order), via
# future::value() indexed by cell_index - so the final dplyr::bind_rows()
# over the returned list produces byte-identical row ordering to today's
# sequential accumulation, independent of which cell happens to finish
# first. This is the invariant the plan's parallel-invariance check (step 4)
# relies on.
run_cells_parallel <- function(grid, cell_fn, label_fn, poll_interval = 0.2) {
  n_cells <- nrow(grid)
  futures <- vector("list", n_cells)
  for (i in seq_len(n_cells)) {
    cell <- grid[i, , drop = FALSE]
    futures[[i]] <- future::future(cell_fn(cell), globals = TRUE, seed = NULL)
  }

  completed <- rep(FALSE, n_cells)
  n_done <- 0L
  while (n_done < n_cells) {
    for (i in which(!completed)) {
      if (future::resolved(futures[[i]])) {
        completed[i] <- TRUE
        n_done <- n_done + 1L
        progress_note(n_done, n_cells, label_fn(grid[i, , drop = FALSE]))
      }
    }
    if (n_done < n_cells) Sys.sleep(poll_interval)
  }

  lapply(seq_len(n_cells), function(i) future::value(futures[[i]]))
}

# Runs setup_fn() (library()+source() calls) at the start of every cell
# invocation - needed because future::multisession (PSOCK) workers start
# with a near-bare search path and do NOT inherit the dispatcher's attached
# packages (a real, previously-hit failure mode: tidybayes::epred_draws()
# S3 dispatch failing when tidytreatment/tidybayes weren't library()-
# attached).
#
# Originally attempted a "run once per worker" version gated by a sentinel
# in the worker's .GlobalEnv, on the assumption that multisession workers
# are long-lived R processes that would retain it across calls. Verified
# empirically that this does NOT hold: future intentionally does not let
# .GlobalEnv side effects leak between separate future() calls, even to the
# same worker PID across the same plan() (confirmed directly - a value
# assigned to .GlobalEnv in one future() call was reliably absent when
# checked from a later future() call reusing the identical PID). Package
# *attachment* did appear to persist in a separate check, but relying on
# that distinction wasn't worth the fragility given the alternative is
# simple and cheap: library()/source() cost ~0.7s cold and ~0.002s warm
# (measured directly), utterly negligible next to a single MCMC fit, let
# alone the several fits one cell (one replication) contains. So: just
# re-run setup_fn() every time, unconditionally, and don't depend on any
# cross-call persistence assumption at all.
with_worker_setup <- function(setup_fn, cell_fn) {
  function(cell) {
    setup_fn()
    cell_fn(cell)
  }
}

# Reads the `parallel_workers` document parameter (NULL -> availableCores()-1,
# floored at 1) - a single place all three .qmd files call into so the
# default policy only needs to change in one spot.
benchmark_worker_count <- function(parallel_workers = NULL) {
  if (!is.null(parallel_workers)) return(max(1L, as.integer(parallel_workers)))
  max(1L, future::availableCores() - 1L)
}
