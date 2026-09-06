# ---------------------------------------------------------------------------
# Method detection ("auto") and resolution of explicit methods. ggchunk only
# truncates axes: discrete x level count or continuous x/y ranges.
# ---------------------------------------------------------------------------

# Detect whether (and where) the axis should be truncated.
# Returns list(method, keyquo, label, reason, report).
resolve_method <- function(p, spec) {
  report <- character(0)
  note <- function(...) report <<- c(report, paste0(...))

  dat <- primary_data(p)
  if (is.null(dat) || nrow(dat) == 0L) {
    note("plot data is empty")
    return(list(method = NULL, keyquo = NULL, label = NULL, reason = "empty data", report = report))
  }

  m <- combined_mapping(p)
  gi <- extract_geom_info(p)
  note(sprintf(
    "data: %d rows; geoms: %s",
    nrow(dat),
    if (nrow(gi)) paste(unique(gi$geom), collapse = ", ") else "(none)"
  ))

  # Rule 1: discrete x with many levels -> truncate x
  xv <- eval_key(m$x, dat)
  xlab <- as_label_safe(m$x)
  if (is_discrete(xv)) {
    xn <- length(unique(xv[!is.na(xv)]))
    note(sprintf(
      "x `%s` is discrete with %d level(s) (threshold_col = %g)",
      xlab %||% "<x>", xn, spec$threshold_col
    ))
    if (xn > spec$threshold_col) {
      reason <- sprintf(
        "discrete x `%s` has %d levels (> %g)",
        xlab %||% "x", xn, spec$threshold_col
      )
      note(sprintf("-> truncate x (%s)", reason))
      return(list(method = "x", keyquo = m$x, label = xlab, reason = reason, report = report))
    }
  } else {
    # Rule 2: continuous x with a lot of rows -> truncate x
    if (!is.null(xv) && is.numeric(xv) && nrow(dat) > spec$threshold_points) {
      reason <- sprintf(
        "continuous x `%s` with %d rows (> %g)",
        xlab %||% "x", nrow(dat), spec$threshold_points
      )
      note(sprintf("-> truncate x (%s)", reason))
      return(list(method = "x", keyquo = m$x, label = xlab, reason = reason, report = report))
    }
    note(sprintf(
      "x %s needs no truncation%s",
      if (is.null(xlab)) "(unmapped)" else sprintf("(`%s`)", xlab),
      if (!is.null(xlab) && !is.null(xv) && is.numeric(xv)) {
        sprintf(": %d rows <= threshold_points (%g)", nrow(dat), spec$threshold_points)
      } else {
        ""
      }
    ))
  }

  note("all checks below thresholds; no truncation needed")
  list(method = NULL, keyquo = NULL, label = NULL, reason = "below thresholds", report = report)
}

# Resolve an explicitly requested method into the same shape.
resolve_explicit <- function(p, spec) {
  report <- sprintf("method '%s' requested explicitly", spec$method)
  m <- combined_mapping(p)
  if (identical(spec$method, "x")) {
    keyquo <- m$x
    if (is.null(keyquo)) {
      stop("method = \"x\" requires a mapped x aesthetic.", call. = FALSE)
    }
    xv <- eval_key(keyquo, primary_data(p))
    if (!is.null(xv) && is_discrete(xv) && !is.null(spec$n_chunks) && is.null(spec$n_per_chunk)) {
      warning("`n_chunks` on a discrete x is interpreted as levels per segment; ",
              "prefer `n_per_chunk`.", call. = FALSE)
    }
    return(list(
      method = "x", keyquo = keyquo, label = as_label_safe(keyquo),
      reason = report, report = report
    ))
  }
  # method "y"
  keyquo <- m$y
  if (is.null(keyquo)) {
    stop("method = \"y\" requires a mapped y aesthetic.", call. = FALSE)
  }
  yv <- eval_key(keyquo, primary_data(p))
  if (!is.null(yv) && is_discrete(yv)) {
    stop("method = \"y\" needs a continuous y axis; discrete y truncation is ",
         "not supported.", call. = FALSE)
  }
  list(
    method = "y", keyquo = keyquo, label = as_label_safe(keyquo),
    reason = report, report = report
  )
}
