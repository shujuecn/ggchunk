# ---------------------------------------------------------------------------
# User-facing S3 methods and accessors for ggchunk objects. A ggchunk IS a
# ggplot carrying the truncation spec; when no truncation is needed the
# methods fall through to the regular ggplot behaviour.
# ---------------------------------------------------------------------------

#' Print, plot and inspect a ggchunk object
#'
#' `print()` / `plot()` render the single figure holding all axis segments.
#' `chunk_plot()` returns the figure object itself (a ggplot for a single
#' segment, a patchwork composition when patchwork is installed, otherwise a
#' `ggchunk_page`); with an index it returns one raw segment plot.
#' `chunk_info()` summarises the segments and `n_chunks()` counts them.
#'
#' @name chunk-accessors
#' @rdname chunk-accessors
#' @param x A `ggchunk` object (a ggplot carrying a truncation spec).
#' @param i Optional segment index for `chunk_plot()`.
#' @param ... Passed on to the print methods of the figure objects.
#' @return `chunk_info()` returns (invisibly) a data frame with one row per
#'   segment; `chunk_plot()` returns the figure (or segment) object;
#'   `n_chunks()` returns an integer; `print()`/`plot()` return the object
#'   invisibly.
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   variable = factor(rep(sprintf("v%02d", seq_len(40)), each = 5)),
#'   value = rnorm(200)
#' )
#' p <- ggplot(df, aes(variable, value)) +
#'   geom_boxplot() +
#'   ggchunk(method = "x", n_per_chunk = 10)
#' n_chunks(p)
#' info <- chunk_info(p)
#' fig <- chunk_plot(p)    # the whole figure (patchwork if installed)
#' seg2 <- chunk_plot(p, 2) # second segment as a standalone ggplot
NULL

#' @rdname chunk-accessors
#' @export
print.ggchunk <- function(x, ...) {
  x <- ensure_finalized(x)
  st <- chunk_state(x)
  if (st$method == "none") {
    NextMethod("print")
    return(invisible(x))
  }
  print(build_figure(x), ...)
  invisible(x)
}

#' @rdname chunk-accessors
#' @export
plot.ggchunk <- function(x, ...) {
  x <- ensure_finalized(x)
  st <- chunk_state(x)
  if (st$method == "none") {
    print(x, ...)
    return(invisible(x))
  }
  print(build_figure(x), ...)
  invisible(x)
}

#' @rdname chunk-accessors
#' @export
chunk_plot <- function(x, i = NULL) {
  x <- ensure_finalized(x)
  st <- chunk_state(x)
  if (st$method == "none") {
    return(x)
  }
  if (is.null(i)) {
    return(build_figure(x))
  }
  build_chunk_plot(x, i)
}

#' @rdname chunk-accessors
#' @export
n_chunks <- function(x) {
  x <- ensure_finalized(x)
  st <- chunk_state(x)
  if (st$method == "none") {
    1L
  } else {
    length(st$chunks)
  }
}

#' @rdname chunk-accessors
#' @export
chunk_info <- function(x) {
  x <- ensure_finalized(x)
  st <- chunk_state(x)
  if (st$method == "none") {
    cat("ggchunk object (no truncation needed)\n")
    return(invisible(NULL))
  }
  n <- length(st$chunks)
  info <- data.frame(
    segment = seq_len(n),
    n_items = vapply(st$chunks, function(ch) {
      if (!is.null(ch$keep)) length(ch$keep) else length(ch$rows_plot)
    }, numeric(1)),
    detail = vapply(st$chunks, function(ch) ch$detail, character(1)),
    stringsAsFactors = FALSE
  )
  cat(sprintf(
    "ggchunk object\n  axis    : %s (%s)\n  segments: %d in 1 figure (%d x %d grid)\n",
    st$axis, st$reason, n, st$layout$nrow, st$layout$ncol
  ))
  invisible(info)
}
