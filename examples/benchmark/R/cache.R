# Bust the cache by changing cache_file or deleting it, not by editing this function.
cached_run <- function(cache_file, expr_fn, force = FALSE) {
  if (!force && file.exists(cache_file)) {
    return(readRDS(cache_file))
  }
  result <- expr_fn()
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, cache_file)
  result
}

# One line per row fit, printed before that fit starts - engines print their
# own (often lengthy) MCMC logs with no indication of where in the overall
# run they are, so this is the only progress signal identifying which
# specific combination is currently running versus just "still going".
# Writes straight to the stderr connection and flushes it, rather than going
# through message()'s condition system: message() raises a condition that
# knitr's evaluate() (what Quarto's Render button runs chunks through) can
# intercept to embed in the document, which was delaying the actual write
# until the whole chunk finished - invisible in RStudio's live Render pane
# even though it worked fine for a plain Rscript/Background Job.
progress_note <- function(i, total, ...) {
  # Leading newline only on the first call: Quarto's own render-progress line
  # has no trailing newline, so without this the first entry runs onto it.
  prefix <- if (i == 1) "\n" else ""
  cat(sprintf("%s---- [%d/%d] %s ----\n", prefix, i, total, paste(..., sep = " ")), file = stderr())
  flush(stderr())
}
