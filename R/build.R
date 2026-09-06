# ---------------------------------------------------------------------------
# Chunk plot construction. Each chunk plot is a fresh ggplot built from the
# original plot's components: new layer instances with chunk-filtered data,
# and shared (read-only) scales/facet/coordinates/theme/labels/guides. The
# plot object is never mutated, and per-build scale training is recomputed
# from the chunk data, while the optional perpendicular-axis limits remain
# shared across all chunks.
# ---------------------------------------------------------------------------

build_chunk_plot <- function(x, i) {
  st <- chunk_state(x)
  i <- as.integer(i)
  n_chunks_total <- length(st$chunks)
  if (is.na(i) || i < 1L || i > n_chunks_total) {
    stop(sprintf("`chunk` index must be between 1 and %d.", n_chunks_total), call. = FALSE)
  }
  key <- paste0("chunk", i)
  cached <- st$cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }

  ch <- st$chunks[[i]]
  spec <- st$spec

  pd <- filter_rows(x$data, ch$rows_plot, st$drop_syms)
  if (!is.data.frame(pd)) {
    pd <- NULL # waiver()/empty global data: let the layers carry the data
  }
  np <- ggplot2::ggplot(data = pd, mapping = x$mapping)

  layers <- vector("list", length(x$layers))
  for (j in seq_along(x$layers)) {
    lc <- clone_layer(x$layers[[j]])
    ld <- st$layer_datas[[j]]
    rows <- ch$rows_layers[[j]]
    if (!is.null(ld) && !is.null(rows) && !anyNA(rows)) {
      lc$data <- filter_rows(ld, rows, st$drop_syms)
    }
    layers[[j]] <- lc
  }
  np$layers <- layers

  # Share read-only components (each ggplot_build() re-derives scale training
  # from the data, and facet/coords/theme are stateless across builds).
  for (f in c("scales", "guides", "facet", "coordinates", "theme", "labels", "meta")) {
    v <- x[[f]]
    if (!is.null(v)) {
      np[[f]] <- v
    }
  }
  if (isTRUE(spec$fixed_perpendicular) && !is.null(st$perpendicular_limits)) {
    np <- apply_perpendicular_limits(np, st$axis, st$perpendicular_limits)
  }

  st$cache[[key]] <- np
  chunk_state(x) <- st
  np
}

apply_perpendicular_limits <- function(plot, axis, limits) {
  aesthetic <- if (identical(axis, "x")) "y" else "x"
  scale <- plot$scales$get_scales(aesthetic)
  if (!is.null(plot$coordinates$limits[[aesthetic]])) {
    return(plot)
  }
  if (limits$kind == "discrete") {
    if (is.null(scale)) {
      plot <- if (identical(aesthetic, "x")) {
        plot + ggplot2::scale_x_discrete(limits = limits$values)
      } else {
        plot + ggplot2::scale_y_discrete(limits = limits$values)
      }
    } else if (is.null(scale$limits)) {
      scales <- plot$scales$clone()
      index <- which(vapply(scales$scales, function(s) aesthetic %in% s$aesthetics, logical(1)))[1]
      fixed_scale <- scales$scales[[index]]$clone()
      fixed_scale$limits <- limits$values
      scales$scales[[index]] <- fixed_scale
      plot$scales <- scales
    }
    return(plot)
  }
  if (!is.null(scale)) {
    if (!is.null(scale$limits)) {
      return(plot)
    }
    scales <- plot$scales$clone()
    index <- which(vapply(
      scales$scales,
      function(s) aesthetic %in% s$aesthetics,
      logical(1)
    ))[1]
    fixed_scale <- scales$scales[[index]]$clone()
    fixed_scale$limits <- limits$values
    scales$scales[[index]] <- fixed_scale
    plot$scales <- scales
    return(plot)
  }

  fn <- if (identical(aesthetic, "x")) {
    if (inherits(limits$values, "Date")) {
      ggplot2::scale_x_date
    } else if (inherits(limits$values, "POSIXt")) {
      ggplot2::scale_x_datetime
    } else {
      ggplot2::scale_x_continuous
    }
  } else {
    if (inherits(limits$values, "Date")) {
      ggplot2::scale_y_date
    } else if (inherits(limits$values, "POSIXt")) {
      ggplot2::scale_y_datetime
    } else {
      ggplot2::scale_y_continuous
    }
  }
  plot + fn(limits = limits$values)
}
