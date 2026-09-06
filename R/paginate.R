# ---------------------------------------------------------------------------
# Single-figure layout: fold all segments into ONE figure, apply axis-aware
# compact styling, and draw with patchwork when available or grid viewports
# otherwise. There are no pages.
# ---------------------------------------------------------------------------

compute_layout <- function(n_segments, spec, axis = "x") {
  nrow <- spec$nrow
  ncol <- spec$ncol

  if (is.null(nrow) && is.null(ncol)) {
    if (identical(axis, "y")) {
      # A tall plot is folded left to right so its y range becomes readable
      # columns. The x axis remains the shared perpendicular axis.
      nrow <- 1L
      ncol <- n_segments
    } else {
      # A wide plot is folded top to bottom so its x range becomes readable
      # rows. This is the default layout for long categorical axes.
      nrow <- n_segments
      ncol <- 1L
    }
  } else if (!is.null(ncol) && is.null(nrow)) {
    nrow <- ceiling(n_segments / ncol)
  } else if (!is.null(nrow) && is.null(ncol)) {
    ncol <- ceiling(n_segments / nrow)
  } else if (nrow * ncol < n_segments) {
    stop(sprintf("nrow * ncol = %d is smaller than the %d segments.",
                 nrow * ncol, n_segments), call. = FALSE)
  }
  list(nrow = nrow, ncol = ncol)
}

has_patchwork <- function() {
  requireNamespace("patchwork", quietly = TRUE)
}

# Does the user's patchwork::plot_layout() take over the figure dimensions?
pw_layout_controls_dims <- function(pw_layout) {
  !is.null(pw_layout) && (!is.null(pw_layout$nrow) || !is.null(pw_layout$ncol))
}

# Axis-aware compact styling: zero plot margins; panels touch edge-to-edge;
# every segment keeps the scale it needs while shared titles are shown once.
compact_panel_theme <- function(axis, row, col, nrow_total, ncol_total = 1L,
                                default_theme = FALSE) {
  th <- if (isTRUE(default_theme)) {
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 13),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
  } else {
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  }
  first_col <- col == 1L
  last_row <- row == nrow_total
  if (identical(axis, "y")) {
    # y segments share the x axis; every column keeps its own y scale.
    if (!first_col) {
      th <- th + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    }
    if (!last_row) {
      th <- th + ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
    }
    if (!first_col) {
      th <- th + ggplot2::theme(axis.title.y = ggplot2::element_blank())
    }
  } else {
    # x segments share the y axis; every row keeps its own x scale.
    if (!last_row) {
      th <- th + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    }
    if (!first_col) {
      th <- th + ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
    } else if (row > 1L) {
      th <- th + ggplot2::theme(axis.title.y = ggplot2::element_blank())
    }
  }
  th
}

# A restrained continuation cue in a dedicated gutter plot, never inside a
# data panel. Only non-final segments receive a cue because the final segment
# closes the continuous reading direction.
continuation_indicator <- function(axis) {
  if (identical(axis, "x")) {
    grob <- grid::segmentsGrob(
      x0 = grid::unit(0.5, "npc"), x1 = grid::unit(0.5, "npc"),
      y0 = grid::unit(0.82, "npc"), y1 = grid::unit(0.18, "npc"),
      gp = grid::gpar(col = "grey55", lwd = 0.8),
      arrow = grid::arrow(length = grid::unit(1.4, "mm"), type = "closed")
    )
  } else {
    grob <- grid::segmentsGrob(
      x0 = grid::unit(0.18, "npc"), x1 = grid::unit(0.82, "npc"),
      y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"),
      gp = grid::gpar(col = "grey55", lwd = 0.8),
      arrow = grid::arrow(length = grid::unit(1.4, "mm"), type = "closed")
    )
  }
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::annotation_custom(grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
}

default_indicator_layout <- function(axis, plots) {
  n <- length(plots)
  gutters <- lapply(seq_len(max(0L, n - 1L)), function(i) continuation_indicator(axis))
  interleaved <- vector("list", n + length(gutters))
  for (i in seq_len(n)) {
    pos <- 2L * i - 1L
    interleaved[[pos]] <- plots[[i]]
    if (i < n) interleaved[[pos + 1L]] <- gutters[[i]]
  }
  if (identical(axis, "x")) {
    list(plots = interleaved, nrow = length(interleaved), ncol = 1L,
         heights = c(as.vector(rbind(rep(1, n - 1L), rep(0.12, n - 1L))), 1))
  } else {
    list(plots = interleaved, nrow = 1L, ncol = length(interleaved),
         widths = c(as.vector(rbind(rep(1, n - 1L), rep(0.12, n - 1L))), 1))
  }
}

# Build (and cache) the single figure holding every segment: one ggplot when
# there is a single segment, otherwise patchwork (or a grid-backed
# ggchunk_page).
build_figure <- function(x) {
  st <- chunk_state(x)
  key <- "figure"
  cached <- st$cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }

  lay <- st$layout
  plots <- lapply(seq_along(st$chunks), function(k) {
    pl <- build_chunk_plot(x, k)
    pl <- pl + compact_panel_theme(
      st$axis,
      row = (k - 1L) %/% lay$ncol + 1L,
      col = (k - 1L) %% lay$ncol + 1L,
      nrow_total = lay$nrow,
      ncol_total = lay$ncol,
      default_theme = length(x$theme) == 0L
    )
    if (k > 1L) {
      # figure-level labels belong on the first segment only
      pl <- pl + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL, tag = NULL)
    }
    pl
  })

  use_gutters <- isTRUE(st$spec$show_indicator) &&
    is.null(st$spec$nrow) && is.null(st$spec$ncol) &&
    !pw_layout_controls_dims(st$spec$pw_layout)
  figure_layout <- if (use_gutters) {
    default_indicator_layout(st$axis, plots)
  } else {
    list(plots = plots, nrow = lay$nrow, ncol = lay$ncol)
  }

  obj <- if (length(plots) == 1L) {
    plots[[1]]
  } else if (has_patchwork()) {
    if (pw_layout_controls_dims(st$spec$pw_layout)) {
      pw <- patchwork::wrap_plots(figure_layout$plots, guides = "collect")
    } else {
      pw <- patchwork::wrap_plots(
        figure_layout$plots, nrow = figure_layout$nrow, ncol = figure_layout$ncol,
        guides = "collect"
      )
    }
    if (!is.null(figure_layout$heights) || !is.null(figure_layout$widths)) {
      pw <- pw + patchwork::plot_layout(
        heights = figure_layout$heights, widths = figure_layout$widths
      )
    }
    if (!is.null(st$spec$pw_layout)) {
      pw <- pw + st$spec$pw_layout
    }
    if (!is.null(st$spec$pw_annotation)) {
      pw <- pw + st$spec$pw_annotation
    }
    pw
  } else {
    structure(list(
      plots = figure_layout$plots, nrow = figure_layout$nrow,
      ncol = figure_layout$ncol,
      heights = figure_layout$heights,
      widths = figure_layout$widths
    ), class = "ggchunk_page")
  }

  st$cache[[key]] <- obj
  chunk_state(x) <- st
  obj
}

# Multi-segment drawing without patchwork: row-major grid of viewports.
draw_page_panels <- function(page) {
  nrow <- page$nrow
  ncol <- page$ncol
  layout <- grid::grid.layout(
    nrow = nrow,
    ncol = ncol,
    heights = grid::unit(page$heights %||% rep(1, nrow), "null"),
    widths = grid::unit(page$widths %||% rep(1, ncol), "null")
  )
  grid::pushViewport(grid::viewport(layout = layout))
  on.exit(grid::popViewport(), add = TRUE)
  for (i in seq_along(page$plots)) {
    r <- (i - 1L) %/% ncol + 1L
    cc <- (i - 1L) %% ncol + 1L
    grid::pushViewport(grid::viewport(layout.pos.row = r, layout.pos.col = cc))
    draw_single(page$plots[[i]])
    grid::popViewport()
  }
}

draw_single <- function(pl) {
  if (inherits(pl, "ggchunk_page")) {
    return(draw_page_panels(pl))
  }
  tryCatch(
    print(pl, newpage = FALSE),
    error = function(e) print(pl)
  )
}

#' @export
print.ggchunk_page <- function(x, ...) {
  grid::grid.newpage()
  draw_page_panels(x)
  invisible(x)
}

# Make grid.draw() work so ggchunk figures can be saved through any device
# opened manually (and through ggsave() in ggplot2 versions that draw via
# grid.draw()).
#' Draw a ggchunk figure into the current grid viewport
#'
#' S3 method for [grid::grid.draw()] so `ggchunk_page` objects (the fallback
#' figure format when patchwork is not installed) can be rendered through any
#' open graphics device.
#'
#' @param x A `ggchunk_page` object.
#' @param recording Passed to grid.
#' @return Invisibly, `x`.
#' @export
grid.draw.ggchunk_page <- function(x, recording = TRUE) {
  draw_page_panels(x)
  invisible(x)
}
