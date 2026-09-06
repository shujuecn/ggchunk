test_that("default layout stacks segments into rows, like wrapped text", {
  expect_equal(ggchunk:::compute_layout(3, ggchunk(), "x"), list(nrow = 3L, ncol = 1L))
  expect_equal(ggchunk:::compute_layout(6, ggchunk(), "x"), list(nrow = 6L, ncol = 1L))
  expect_equal(ggchunk:::compute_layout(3, ggchunk(), "y"), list(nrow = 1L, ncol = 3L))
})

test_that("explicit nrow/ncol are honoured and validated", {
  expect_equal(ggchunk:::compute_layout(6, ggchunk(nrow = 2, ncol = 3)), list(nrow = 2L, ncol = 3L))
  expect_equal(ggchunk:::compute_layout(6, ggchunk(ncol = 2)), list(nrow = 3L, ncol = 2L))
  expect_equal(ggchunk:::compute_layout(6, ggchunk(nrow = 3)), list(nrow = 3L, ncol = 2L))
  expect_error(ggchunk:::compute_layout(6, ggchunk(nrow = 1, ncol = 2)), "smaller than")
})

test_that("continuation indicators are disabled by default and can be enabled", {
  expect_false(ggchunk()$show_indicator)
  expect_true(ggchunk(show_indicator = TRUE)$show_indicator)
  expect_error(ggchunk(show_indicator = "yes"), "show_indicator")
  expect_s3_class(ggchunk:::continuation_indicator("x"), "ggplot")
  expect_s3_class(ggchunk:::continuation_indicator("y"), "ggplot")
  expect_equal(ggchunk:::default_indicator_layout("x", list(1, 2, 3))$heights,
               c(1, 0.12, 1, 0.12, 1))
  expect_equal(ggchunk:::default_indicator_layout("y", list(1, 2, 3))$widths,
               c(1, 0.12, 1, 0.12, 1))
})

test_that("default typography is larger without overriding explicit themes", {
  d <- make_box_df(40, per = 5)
  p0 <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10)
  p1 <- p0 + ggplot2::theme_bw()

  th_default <- ggchunk:::compact_panel_theme(
    "x", row = 1, col = 1, nrow_total = 2, default_theme = TRUE
  )
  th_explicit <- ggchunk:::compact_panel_theme(
    "x", row = 1, col = 1, nrow_total = 2, default_theme = FALSE
  )
  expect_equal(th_default$text$size, 14)
  expect_equal(th_default$axis.text$size, 12)
  expect_null(th_explicit$text)

  expect_equal(length(p0$theme), 0L)
  expect_false(is.null(p1$theme))
})

test_that("fixed_perpendicular keeps the non-chunked axis shared", {
  d <- data.frame(
    x = factor(rep(sprintf("v%02d", 1:40), each = 5)),
    y = c(rnorm(100, 0, 1), rnorm(100, 100, 1))
  )
  p_fixed <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10, show_indicator = FALSE)
  p_free <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10, fixed_perpendicular = FALSE,
            show_indicator = FALSE)

  fixed_ranges <- lapply(seq_len(4), function(i) {
    ggplot2::ggplot_build(ggchunk:::build_chunk_plot(p_fixed, i))$layout$panel_params[[1]]$y.range
  })
  free_ranges <- lapply(seq_len(4), function(i) {
    ggplot2::ggplot_build(ggchunk:::build_chunk_plot(p_free, i))$layout$panel_params[[1]]$y.range
  })
  expect_true(all(vapply(fixed_ranges, identical, logical(1), fixed_ranges[[1]])))
  expect_false(all(vapply(free_ranges, identical, logical(1), free_ranges[[1]])))
  expect_true(ggchunk()$fixed_perpendicular)
  expect_error(ggchunk(fixed_perpendicular = "yes"), "fixed_perpendicular")
})

test_that("fixed_perpendicular also keeps x shared for y chunks", {
  d <- data.frame(
    x = c(rnorm(120, 0, 0.5), rnorm(120, 20, 0.5)),
    y = seq(0, 10, length.out = 240)
  )
  p_fixed <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggchunk(method = "y", n_chunks = 2, show_indicator = FALSE)
  p_free <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggchunk(method = "y", n_chunks = 2, fixed_perpendicular = FALSE,
            show_indicator = FALSE)

  fixed_ranges <- lapply(seq_len(2), function(i) {
    ggplot2::ggplot_build(ggchunk:::build_chunk_plot(p_fixed, i))$layout$panel_params[[1]]$x.range
  })
  free_ranges <- lapply(seq_len(2), function(i) {
    ggplot2::ggplot_build(ggchunk:::build_chunk_plot(p_free, i))$layout$panel_params[[1]]$x.range
  })
  expect_true(all(vapply(fixed_ranges, identical, logical(1), fixed_ranges[[1]])))
  expect_false(all(vapply(free_ranges, identical, logical(1), free_ranges[[1]])))
})

test_that("explicit perpendicular limits are respected", {
  d <- make_box_df(40, per = 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_y_continuous(limits = c(-20, 20), expand = ggplot2::expansion(mult = 0)) +
    ggchunk(method = "x", n_per_chunk = 10, show_indicator = FALSE)
  built <- ggplot2::ggplot_build(ggchunk:::build_chunk_plot(p, 1))
  expect_equal(built$layout$panel_params[[1]]$y.range, c(-20, 20))
})

test_that("fixed perpendicular limits preserve geom_col baselines", {
  d <- data.frame(
    category = factor(rep(sprintf("c%02d", 1:24), each = 2)),
    group = factor(rep(c("a", "b"), 24)),
    value = seq(20, 60, length.out = 48)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(category, value, fill = group)) +
    ggplot2::geom_col(position = "dodge") +
    ggchunk(method = "x", n_per_chunk = 8, show_indicator = FALSE)

  expect_true(all(vapply(seq_len(3), function(i) {
    built <- ggplot2::ggplot_build(ggchunk:::build_chunk_plot(p, i))
    nrow(built$data[[1]]) == 16L && min(built$layout$panel_params[[1]]$y.range) < 0
  }, logical(1))))
})

test_that("stacked rows show per-row x scales and shared y", {
  d <- make_box_df(60, per = 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10) # 6 rows
  expect_equal(ggchunk:::chunk_state(p)$layout, list(nrow = 6L, ncol = 1L))
  suppressMessages(print(chunk_plot(p)))
})

test_that("all segments render into ONE figure", {
  d <- make_box_df(60, per = 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10) # 6 segments
  fig <- chunk_plot(p)
  expect_s3_class(fig, "patchwork")
  expect_no_error(suppressMessages(print(fig)))
  expect_no_error(suppressMessages(print(p)))
})

test_that("all segments retain legends for collect", {
  d <- data.frame(
    variable = factor(rep(sprintf("v%02d", 1:40), each = 2)),
    batch = factor(rep(c("A", "B"), 40), levels = c("A", "B")),
    value = rnorm(80)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value, fill = batch)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 20) +
    ggplot2::scale_fill_manual(values = c(A = "#D55E5E", B = "#19A7A8"), drop = FALSE)

  panels <- lapply(seq_len(n_chunks(p)), function(i) {
    panel <- ggchunk:::build_chunk_plot(p, i)
    built <- ggplot2::ggplot_build(panel)
    expect_true(length(unique(built$data[[2]]$fill)) == 2L)
    expect_false(panel$layers[[1]]$show.legend)
    expect_true(panel$layers[[length(panel$layers)]]$show.legend)
    panel +
      ggchunk:::compact_panel_theme("x", i, 1L, n_chunks(p), 1L)
  })
  expect_true(all(vapply(panels, function(panel) {
    !identical(panel$theme$legend.position, "none")
  }, logical(1))))
})

test_that("a single segment falls back to the unchanged plot", {
  d <- make_box_df(24, per = 5)
  expect_message(
    p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
      ggplot2::geom_boxplot() +
      ggchunk(method = "x", n_per_chunk = 24),
    "single segment"
  )
  expect_identical(ggchunk:::chunk_state(p)$method, "none")
  expect_s3_class(chunk_plot(p), "ggplot")
})

test_that("compact styling is axis-aware", {
  # x truncation, stacked rows: every row keeps its y scale and its own x
  # scale; the x axis title only on the bottom row
  th1 <- ggchunk:::compact_panel_theme("x", row = 1, col = 1, nrow_total = 3)
  expect_null(th1$axis.text.y)
  expect_s3_class(th1$axis.title.x, "element_blank")
  th3 <- ggchunk:::compact_panel_theme("x", row = 3, col = 1, nrow_total = 3)
  expect_null(th3$axis.title.x)
  # multi-column layouts hide duplicated y scales
  th2 <- ggchunk:::compact_panel_theme("x", row = 1, col = 2, nrow_total = 1, ncol_total = 2)
  expect_s3_class(th2$axis.text.y, "element_blank")
  # y truncation: x scale is retained in the single default row
  thy <- ggchunk:::compact_panel_theme("y", row = 1, col = 1, nrow_total = 2)
  expect_s3_class(thy$axis.text.x, "element_blank")
  thy2 <- ggchunk:::compact_panel_theme("y", row = 2, col = 1, nrow_total = 2)
  expect_null(thy2$axis.text.x)

  # y truncation in its default layout uses columns; keep y labels but avoid
  # repeating the y-axis title.
  thy3 <- ggchunk:::compact_panel_theme("y", row = 1, col = 2, nrow_total = 1, ncol_total = 3)
  expect_null(thy3$axis.text.y)
  expect_s3_class(thy3$axis.title.y, "element_blank")
  expect_s3_class(thy3$axis.title.x, "element_blank")
})

test_that("plot_layout / plot_annotation style the figure", {
  skip_if_not_installed("patchwork")
  d <- make_box_df(60, per = 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10)
  p2 <- p + patchwork::plot_layout(ncol = 2) +
    patchwork::plot_annotation(title = "Folded axis")
  expect_s3_class(p2, "ggchunk")
  expect_false(is.null(ggchunk:::chunk_state(p2)$spec$pw_layout))
  expect_no_error(suppressMessages(print(p2)))
})

test_that("grid fallback (ggchunk_page) prints without patchwork", {
  d1 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  d2 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  pg <- structure(list(plots = list(d1, d2, d1), nrow = 2L, ncol = 2L), class = "ggchunk_page")
  expect_no_error(print(pg))
})
