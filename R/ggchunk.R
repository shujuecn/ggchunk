#' Fold a ggplot axis into one continuous multi-segment figure
#'
#' `ggchunk()` is added to a ggplot pipeline with `+`. It **folds the x or y
#' axis into several consecutive segments** and draws all segments in **one
#' figure**: x-axis segments become rows and y-axis segments become columns.
#' The segments keep the perpendicular scale, touch edge-to-edge, and share
#' figure-level labels and legends where appropriate.
#'
#' @details
#' Methods:
#'
#' * `"x"`: truncate the x axis. Discrete x: `n_per_chunk` levels per segment.
#'   Continuous x: the x range is cut into `n_chunks` equal-width segments
#'   (or equal-density segments with `even = TRUE`), so a dense strip along x
#'   is folded into readable strips.
#' * `"y"`: truncate the y axis (segments are arranged in columns and share
#'   the x axis). Continuous y only.
#' * `"auto"` (default): a discrete x variable with more than `threshold_col`
#'   levels triggers `"x"`; a continuous x with more than `threshold_points`
#'   rows triggers `"x"` on the x range. Small plots pass through unchanged.
#'
#' Layout: x-axis segments are stacked into **rows** and y-axis segments are
#' arranged into **columns** by default; `nrow`/`ncol` can be set explicitly to
#' arrange them differently. All segments always render in **one figure** --
#' there are no pages.
#'
#' The returned object is still a full ggplot (with the chunking spec
#' attached), so `theme()`, `labs()`, scales and geoms keep working before or
#' after `+ ggchunk()`. When no explicit theme is supplied, ggchunk uses a
#' restrained 14 pt text baseline with 12 pt axis text; user-supplied themes
#' are preserved. Use the usual ggplot2 theme, text-size and font-family
#' settings for the target journal. When patchwork is installed, the figure is
#' a patchwork object and
#' `+ patchwork::plot_layout()` / `+ patchwork::plot_annotation()` style it.
#'
#' Accessors: [n_chunks()], [chunk_info()], [chunk_plot()], [ggsave_chunk()].
#'
#' @param method Folding strategy: `"auto"` (default), `"x"` or `"y"`.
#'   (`"ncol"` is accepted as an alias of `"x"`; the grouping-based methods of
#'   early drafts were removed -- use `facet_wrap()` for those.)
#' @param n_chunks Number of axis segments for continuous axes (`"x"` on
#'   continuous data, `"y"`). Default 3.
#' @param n_per_chunk Number of discrete x levels per segment. Default 20.
#' @param even Continuous axes only: cut at quantiles so every segment holds
#'   roughly the same number of observations instead of covering equal axis
#'   ranges. Default `FALSE` (equal-width segments).
#' @param nrow,ncol Grid of segments in the single figure. `NULL` (default)
#'   puts x-axis segments into rows and y-axis segments into columns.
#' @param threshold_col `"auto"`: truncate the x axis when the discrete x
#'   variable has more than this many levels.
#' @param threshold_points `"auto"`: truncate the continuous x axis when the
#'   data has more than this many rows.
#' @param verbose Print the detection result and segment statistics.
#' @param show_indicator Add a small continuation arrow to every non-final
#'   segment. The arrow is drawn in the gutter between segments, never inside
#'   a data panel: it points downward for x-axis rows and rightward for
#'   y-axis columns. It is omitted when an explicit multi-panel layout is
#'   supplied, so that user-specified dimensions are not changed. Default
#'   `FALSE`; set to `TRUE` to show the continuation cue.
#' @param fixed_perpendicular Fix the non-chunked axis to the full plot range
#'   across all segments. For `method = "x"`, this fixes y; for `method = "y"`,
#'   this fixes x. Default `TRUE`; set to `FALSE` for per-segment ranges.
#' @param ... Unused; a warning is issued for unknown arguments (catches
#'   typos).
#'
#' @return When used with `+`: the ggplot object carrying a `ggchunk` class
#'   and the truncation specification (an ordinary ggplot if no truncation is
#'   needed). Calling `ggchunk()` on its own returns the internal
#'   specification object.
#'
#' @examples
#' library(ggplot2)
#'
#' # 60 x levels: fold the x axis into 3 segments of 20, ONE figure
#' df <- data.frame(
#'   variable = factor(rep(sprintf("v%02d", seq_len(60)), each = 10)),
#'   value = rnorm(600)
#' )
#' p <- ggplot(df, aes(variable, value)) +
#'   geom_boxplot() +
#'   ggchunk(method = "x", n_per_chunk = 20)
#' p
#' n_chunks(p)
#' chunk_info(p)
#'
#' # explicit 2 x 2 folding of a dense scatter
#' p2 <- ggplot(diamonds_sample, aes(carat, price)) +
#'   geom_point(alpha = 0.3) +
#'   ggchunk(method = "x", n_chunks = 4, nrow = 2, ncol = 2)
#' @export
ggchunk <- function(method = "auto",
                    n_chunks = NULL,
                    n_per_chunk = NULL,
                    even = FALSE,
                    nrow = NULL,
                    ncol = NULL,
                    threshold_col = 30,
                    threshold_points = 10000,
                    verbose = FALSE,
                    show_indicator = FALSE,
                    fixed_perpendicular = TRUE,
                    ...) {
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("`method` must be a single string.", call. = FALSE)
  }
  if (!method %in% c("auto", "x", "y", "ncol", "by_group", "n_points")) {
    stop('`method` must be one of "auto", "x" or "y".', call. = FALSE)
  }
  if (identical(method, "ncol")) {
    warning('method = "ncol" has been renamed to method = "x".', call. = FALSE)
    method <- "x"
  }
  if (identical(method, "by_group")) {
    stop(
      'method = "by_group" was removed: grouping-based splitting duplicates ',
      'facet_wrap(); ggchunk now only truncates axes (method = "x" / "y" / "auto").',
      call. = FALSE
    )
  }
  if (identical(method, "n_points")) {
    warning(
      'method = "n_points" has been folded into method = "x": continuous axes ',
      'are truncated by x range (use even = TRUE for equal-density segments).',
      call. = FALSE
    )
    method <- "x"
  }

  dots <- list(...)
  if (length(dots) > 0L) {
    warning(
      sprintf("Unknown argument(s) ignored: %s", paste(names(dots), collapse = ", ")),
      call. = FALSE
    )
  }

  int_arg <- function(v, nm) {
    if (is.null(v)) {
      return(NULL)
    }
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || !is.finite(v) || v < 1) {
      stop(sprintf("`%s` must be NULL or a single finite number >= 1.", nm), call. = FALSE)
    }
    max(1L, as.integer(v))
  }
  n_chunks <- int_arg(n_chunks, "n_chunks")
  n_per_chunk <- int_arg(n_per_chunk, "n_per_chunk")
  nrow <- int_arg(nrow, "nrow")
  ncol <- int_arg(ncol, "ncol")

  num_arg <- function(v, nm) {
    if (is.null(v)) {
      return(NULL)
    }
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || !is.finite(v) || v < 1) {
      stop(sprintf("`%s` must be a single finite number >= 1.", nm), call. = FALSE)
    }
    as.numeric(v)
  }
  threshold_col <- num_arg(threshold_col, "threshold_col")
  threshold_points <- num_arg(threshold_points, "threshold_points")

  if (!is.logical(even) || length(even) != 1L || is.na(even)) {
    stop("`even` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(show_indicator) || length(show_indicator) != 1L ||
    is.na(show_indicator)) {
    stop("`show_indicator` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(fixed_perpendicular) || length(fixed_perpendicular) != 1L ||
    is.na(fixed_perpendicular)) {
    stop("`fixed_perpendicular` must be TRUE or FALSE.", call. = FALSE)
  }

  structure(
    list(
      method = method,
      n_chunks = n_chunks,
      n_per_chunk = n_per_chunk,
      even = even,
      nrow = nrow,
      ncol = ncol,
      show_indicator = show_indicator,
      fixed_perpendicular = fixed_perpendicular,
      threshold_col = threshold_col,
      threshold_points = threshold_points,
      verbose = verbose,
      pw_layout = NULL,
      pw_annotation = NULL
    ),
    class = "ggchunk_spec"
  )
}

# ---------------------------------------------------------------------------
# `+` integration: the spec is attached to the ggplot object itself, and the
# object gets a leading "ggchunk" class. The plot remains a fully functional
# ggplot, so later ggplot additions keep working and simply invalidate the
# computed segments (recomputed lazily on render).
# ---------------------------------------------------------------------------

new_chunk_state <- function(spec) {
  list(
    spec = spec,
    finalized = FALSE,
    method = NULL,
    key_label = NULL,
    axis = NULL,
    reason = NULL,
    report = NULL,
    chunks = list(),
    layout = NULL,
    layer_datas = NULL,
    drop_syms = character(0),
    perpendicular_limits = NULL,
    cache = new.env(parent = emptyenv())
  )
}

chunk_state <- function(x) {
  attr(x, "ggchunk_state")
}

`chunk_state<-` <- function(x, value) {
  attr(x, "ggchunk_state") <- value
  x
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggchunk_spec <- function(object, plot, ...) {
  if (!is_ggplot(plot)) {
    stop("`ggchunk()` must be added to a ggplot with `+`.", call. = FALSE)
  }
  chunk_state(plot) <- new_chunk_state(object)
  class(plot) <- c("ggchunk", class(plot))
  finalize_ggchunk(plot, verbose = isTRUE(object$verbose))
}

#' @export
`+.ggchunk` <- function(e1, e2) {
  st <- chunk_state(e1)
  if (inherits(e2, "plot_layout")) {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("patchwork is required for `+ plot_layout()`.", call. = FALSE)
    }
    st$spec$pw_layout <- e2
    chunk_state(e1) <- st
    return(e1)
  }
  if (inherits(e2, "plot_annotation")) {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("patchwork is required for `+ plot_annotation()`.", call. = FALSE)
    }
    st$spec$pw_annotation <- e2
    chunk_state(e1) <- st
    return(e1)
  }
  # Any other addition: let ggplot handle it. The "ggchunk" class rides along
  # (S7 objects mutate in place; list-based plots copy attributes), but the
  # computed segments are stale afterwards.
  e1 <- ggplot_add(e2, e1, "ggchunk_addition")
  st <- chunk_state(e1)
  st$finalized <- FALSE
  st$cache <- new.env(parent = emptyenv())
  chunk_state(e1) <- st
  e1
}

# ---------------------------------------------------------------------------
# Finalisation: resolve method, compute segments and the single-figure layout
# (cheap index work; segment plots themselves are built lazily and cached).
# ---------------------------------------------------------------------------

ensure_finalized <- function(x) {
  st <- chunk_state(x)
  if (is.null(st)) {
    return(x)
  }
  if (!isTRUE(st$finalized)) {
    x <- finalize_ggchunk(x, verbose = FALSE)
  }
  x
}

finalize_ggchunk <- function(x, verbose = FALSE) {
  st <- chunk_state(x)
  if (is.null(st)) {
    return(x)
  }
  if (isTRUE(st$finalized)) {
    return(x)
  }
  spec <- st$spec
  p <- x
  announce <- isTRUE(spec$verbose) || isTRUE(verbose)

  noop <- function(reason) {
    st$method <- "none"
    st$reason <- reason
    st$report <- det$report
    st$chunks <- list()
    st$layout <- list(nrow = 1L, ncol = 1L)
    st$finalized <- TRUE
    chunk_state(x) <- st
    if (announce) {
      message(sprintf("ggchunk: no truncation needed (%s).", reason))
    }
    x
  }

  det <- if (identical(spec$method, "auto")) {
    resolve_method(p, spec)
  } else {
    resolve_explicit(p, spec)
  }

  if (is.null(det$method)) {
    return(noop(det$reason))
  }

  info <- compute_chunks(p, spec, det)
  if (length(info$chunks) <= 1L) {
    if (!identical(spec$method, "auto")) {
      message("ggchunk: the chosen settings produce a single segment; the plot is returned unchanged.")
    }
    return(noop("a single segment covers the whole axis"))
  }

  st$method <- det$method
  st$axis <- det$method # "x" or "y"
  st$key_label <- info$key_label
  st$chunks <- info$chunks
  st$layer_datas <- info$layer_datas
  st$drop_syms <- drop_syms_for_method(det$method, info$keyquo)
  st$perpendicular_limits <- if (isTRUE(spec$fixed_perpendicular)) {
    compute_perpendicular_limits(p, det$method)
  } else {
    NULL
  }
  st$layout <- compute_layout(length(info$chunks), spec, det$method)
  st$report <- det$report
  st$reason <- det$reason
  st$finalized <- TRUE
  chunk_state(x) <- st

  if (announce) {
    message(sprintf(
      "ggchunk: truncated the %s axis (%s) into %d segments in one figure.",
      st$axis, st$reason, length(st$chunks)
    ))
  }
  x
}

drop_syms_for_method <- function(method, keyquo) {
  if (identical(method, "x") && !is.null(keyquo)) {
    expr <- rlang::quo_get_expr(keyquo)
    if (rlang::is_symbol(expr)) {
      return(as.character(expr))
    }
  }
  character(0)
}
