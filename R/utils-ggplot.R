# ---------------------------------------------------------------------------
# Internal helpers for inspecting and copying ggplot objects. Not exported.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

is_ggplot <- function(x) inherits(x, "ggplot")

# Clone a layer instance so its `data` can be replaced without touching the
# original plot.
#
# ggplot2 stores layers as environments (ggproto instances), so plain list
# copies of a plot share layer state. Since ggplot2 4.0 even
# `ggplot2:::plot_clone()` no longer copies layers, so we clone explicitly.
# `rlang::env_clone()` preserves the parent (the ggproto class, where the
# methods live) but drops attributes, hence the attribute restore.
clone_layer <- function(layer) {
  if (!is.environment(layer)) {
    return(layer)
  }
  out <- rlang::env_clone(layer)
  attributes(out) <- attributes(layer)
  out
}

# Evaluate a mapping quosure on a data frame; NULL on failure.
eval_key <- function(quo, data) {
  if (is.null(quo)) {
    return(NULL)
  }
  tryCatch(rlang::eval_tidy(quo, data), error = function(e) NULL)
}

is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

# Ordered, non-missing unique values of a discrete variable, in the order
# ggplot2 would use for the axis (factor levels first, otherwise sorted).
disc_values <- function(x) {
  if (is.factor(x)) {
    lv <- levels(x)
    lv[lv %in% as.character(x)]
  } else {
    u <- unique(x[!is.na(x)])
    if (is.numeric(u) || is.logical(u)) sort(u) else sort(as.character(u))
  }
}

# Copy a mapping object (list-like "uneval" / S7 mapping) into a plain list so
# it can be merged without mutating the original plot (S7 mappings have
# reference semantics in ggplot2 >= 4.0).
map_to_list <- function(m) {
  if (is.null(m)) {
    return(list())
  }
  nms <- tryCatch(names(m), error = function(e) NULL)
  if (is.null(nms) || length(nms) == 0L) {
    return(list())
  }
  out <- vector("list", length(nms))
  names(out) <- nms
  for (nm in nms) {
    out[[nm]] <- tryCatch(m[[nm]], error = function(e) NULL)
  }
  out
}

# Global mapping overlaid with the first layer's local mapping (which wins).
combined_mapping <- function(p) {
  m <- map_to_list(p$mapping)
  if (length(p$layers) > 0L) {
    lm <- map_to_list(p$layers[[1]]$mapping)
    for (nm in names(lm)) {
      m[[nm]] <- lm[[nm]]
    }
  }
  m
}

# The data frame chunking should be based on: plot data, or the first
# non-empty layer data when the plot was created with an empty `ggplot()`.
primary_data <- function(p) {
  d <- p$data
  if (is.data.frame(d) && nrow(d) > 0L) {
    return(d)
  }
  for (l in p$layers) {
    ld <- l$data
    if (is.data.frame(ld) && nrow(ld) > 0L) {
      return(ld)
    }
    if (is.function(ld)) {
      ld2 <- tryCatch(ld(d), error = function(e) NULL)
      if (is.data.frame(ld2) && nrow(ld2) > 0L) {
        return(ld2)
      }
    }
  }
  if (is.data.frame(d)) {
    d
  } else {
    NULL
  }
}

# Some geoms, notably geom_col(), contribute values to the trained scale that
# are not present in the mapped data (its zero baseline is the important case).
# Build the complete plot once so shared limits match ggplot2's real training.
compute_perpendicular_limits <- function(p, axis) {
  dat <- primary_data(p)
  if (is.null(dat) || !is.data.frame(dat)) {
    return(NULL)
  }
  mapping <- combined_mapping(p)
  key <- mapping[[if (identical(axis, "x")) "y" else "x"]]
  values <- eval_key(key, dat)
  if (is.null(values) || !is.atomic(values) || length(values) != nrow(dat)) {
    return(NULL)
  }
  values <- values[!is.na(values)]
  if (length(values) == 0L) {
    return(NULL)
  }
  if (is_discrete(values)) {
    return(list(kind = "discrete", values = disc_values(values)))
  }
  if (!(is.numeric(values) || inherits(values, c("Date", "POSIXt")))) {
    return(NULL)
  }
  if (is.numeric(values)) {
    values <- values[is.finite(values)]
  }
  if (length(values) == 0L) {
    return(NULL)
  }

  built <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
  if (!is.null(built) && length(built$layout$panel_params) > 0L) {
    panel <- built$layout$panel_params[[1L]]
    scale <- panel[[if (identical(axis, "x")) "y" else "x"]]
    trained <- if (!is.null(scale)) scale$limits else NULL
    if (!is.null(trained) && length(trained) == 2L && all(is.finite(trained))) {
      if (inherits(values, "Date")) {
        trained <- as.Date(trained, origin = "1970-01-01")
      } else if (inherits(values, "POSIXt")) {
        trained <- as.POSIXct(
          trained, origin = "1970-01-01", tz = attr(values, "tzone") %||% ""
        )
      }
      return(list(kind = "continuous", values = trained))
    }
  }
  list(kind = "continuous", values = range(values))
}

# Per-layer original data: NULL means the layer inherits the plot data
# (waiver) or its data could not be captured (function data).
capture_layer_datas <- function(p) {
  lapply(p$layers, function(l) {
    d <- l$data
    if (is.null(d) || inherits(d, "waiver")) {
      return(NULL)
    }
    if (is.function(d)) {
      d <- tryCatch(d(p$data), error = function(e) NULL)
    }
    if (is.data.frame(d)) {
      d
    } else {
      NULL
    }
  })
}

# Subset a data frame to `rows` and drop unused factor levels on the columns
# that drive x-axis chunking, so each segment shows only its own axis levels.
# `rows = NULL` leaves the data untouched (layer inherits or is kept whole).
filter_rows <- function(df, rows, drop_syms = character(0)) {
  if (is.null(df) || !is.data.frame(df)) {
    return(df) # waiver()/function data: leave untouched
  }
  if (!is.null(rows) && !anyNA(rows) && nrow(df) > 0L) {
    df <- df[rows, , drop = FALSE]
    rownames(df) <- NULL
    for (s in drop_syms) {
      if (!is.null(df[[s]]) && is.factor(df[[s]])) {
        df[[s]] <- droplevels(df[[s]])
      }
    }
  }
  df
}

#' Info about the geoms used by a plot.
#'
#' @param p A ggplot object.
#' @return A data frame with one row per layer.
#' @keywords internal
extract_geom_info <- function(p) {
  n <- length(p$layers)
  if (n == 0L) {
    return(data.frame(
      layer = integer(0), geom = character(0), stat = character(0),
      position = character(0), data_rows = numeric(0)
    ))
  }
  data.frame(
    layer = seq_len(n),
    geom = vapply(p$layers, function(l) class(l$geom)[1], character(1)),
    stat = vapply(p$layers, function(l) class(l$stat)[1], character(1)),
    position = vapply(p$layers, function(l) class(l$position)[1], character(1)),
    data_rows = vapply(p$layers, function(l) {
      d <- l$data
      if (is.data.frame(d)) nrow(d) else NA_real_
    }, numeric(1))
  )
}

#' Info about the aesthetic mappings of a plot.
#'
#' @param p A ggplot object.
#' @return A list with a `mapping` data frame and the `x` label.
#' @keywords internal
extract_aes_info <- function(p) {
  m <- combined_mapping(p)
  dat <- primary_data(p)
  nms <- names(m)
  mapping <- data.frame(
    aesthetic = character(0), expression = character(0),
    type = character(0), n_levels = numeric(0)
  )
  for (nm in nms) {
    lab <- tryCatch(rlang::as_label(m[[nm]]), error = function(e) "<unevaluatable>")
    v <- eval_key(m[[nm]], dat)
    typ <- if (is.null(v)) NA_character_ else if (is_discrete(v)) "discrete" else "continuous"
    nl <- if (is.null(v)) NA_real_ else length(unique(v[!is.na(v)]))
    mapping <- rbind(mapping, data.frame(
      aesthetic = nm, expression = lab, type = typ, n_levels = nl
    ))
  }
  rownames(mapping) <- NULL
  list(mapping = mapping, x = m$x, x_label = if ("x" %in% names(m)) as_label_safe(m$x) else NULL)
}

as_label_safe <- function(quo) {
  if (is.null(quo)) {
    return(NULL)
  }
  tryCatch(rlang::as_label(quo), error = function(e) NULL)
}

# Convert a length/size value to inches for base devices.
to_inches <- function(x, units, dpi) {
  if (is.null(x) || is.na(x)) {
    return(NA_real_)
  }
  switch(units,
    "in" = x,
    "cm" = x / 2.54,
    "mm" = x / 25.4,
    "px" = x / dpi
  )
}

file_ext <- function(x) {
  tolower(sub(".*\\.([^.]+)$", "\\1", x))
}
