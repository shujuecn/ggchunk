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
    lc$show.legend <- FALSE
    ld <- st$layer_datas[[j]]
    rows <- ch$rows_layers[[j]]
    if (!is.null(ld) && !is.null(rows) && !anyNA(rows)) {
      lc$data <- filter_rows(ld, rows, st$drop_syms)
    }
    layers[[j]] <- lc
  }
  np$layers <- layers

  legend_data <- primary_data(x)
  if (length(x$layers) > 0L && is.data.frame(legend_data)) {
    legend_layer <- clone_layer(x$layers[[1L]])
    legend_layer$data <- filter_rows(legend_data, ch$rows_plot, st$drop_syms)
    legend_layer$show.legend <- TRUE
    legend_layer$inherit.aes <- TRUE
    legend_layer$aes_params$alpha <- 0
    np$layers[[length(np$layers) + 1L]] <- legend_layer
  }

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
  np <- complete_guide_keys(np, x)

  st$cache[[key]] <- np
  chunk_state(x) <- st
  np
}

# Guide keys draw one glyph per scale break. Aesthetics that are mapped but not
# backed by a scale (e.g. `ymin`/`lower`/`middle`/`upper`/`ymax` of an
# identity-stat boxplot) are filled in from the layer data; a segment whose data
# lacks a given group leaves those columns NA and its key glyph cannot be drawn
# -- which is how a collected patchwork legend can end up showing a label
# without its icon. Injecting the missing values through the guide's
# `override.aes` completes every key without touching the drawn panel.
complete_guide_keys <- function(np, original) {
  mapping <- combined_mapping(original)
  legend_data <- primary_data(original)
  if (is.null(legend_data) || !is.data.frame(legend_data) || nrow(legend_data) == 0L) {
    return(np)
  }
  if (is.null(np$guides) || !is.list(np$guides$guides)) {
    return(np)
  }
  scale_aes <- unique(unlist(
    lapply(np$scales$scales, function(s) s$aesthetics),
    use.names = FALSE
  ))
  needed <- setdiff(names(mapping), c(scale_aes, "group"))
  if (length(needed) == 0L) {
    return(np)
  }
  row1 <- legend_data[1L, , drop = FALSE]
  injected <- list()
  for (nm in needed) {
    v <- eval_key(mapping[[nm]], row1)
    if (!is.null(v) && length(v) == 1L && !is.na(v)) {
      injected[[nm]] <- v
    }
  }
  if (length(injected) == 0L) {
    return(np)
  }
  # Guides and Guide objects are ggproto environments (reference semantics)
  # shared with the original plot: clone before mutating.
  guides <- rlang::env_clone(np$guides)
  attributes(guides) <- attributes(np$guides)
  guides$guides <- lapply(guides$guides, function(g) {
    if (!inherits(g, "Guide")) {
      return(g)
    }
    gc <- rlang::env_clone(g)
    attributes(gc) <- attributes(g)
    params <- gc$params %||% list()
    params[["override.aes"]] <- modifyList(injected, params[["override.aes"]] %||% list())
    gc$params <- params
    gc
  })
  np$guides <- guides
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
