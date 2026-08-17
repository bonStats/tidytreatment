# Shared file-based cache: re-running a .qmd during prose edits shouldn't
# refit everything. Bust the cache by changing cache_file (e.g. include a
# hash of the params in its name) or deleting it, not by editing this
# function.
cached_run <- function(cache_file, expr_fn, force = FALSE) {
  if (!force && file.exists(cache_file)) {
    return(readRDS(cache_file))
  }
  result <- expr_fn()
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, cache_file)
  result
}
