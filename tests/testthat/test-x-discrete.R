test_that("discrete x folds into segments and drops unused axis levels", {
  d <- make_box_df(60, per = 10)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 15)

  expect_equal(n_chunks(p), 4L)
  cp <- ggchunk:::build_chunk_plot(p, 1)
  expect_s3_class(cp, "ggplot")

  kept <- levels(droplevels(cp$data$variable))
  expect_length(kept, 15)
  expect_identical(kept, sprintf("v%03d", 1:15))

  labs <- built_labels(cp)
  expect_length(labs, 15)
})

test_that("discrete x conserves all rows across segments (incl. NA keys)", {
  d <- make_box_df(30, per = 10)
  d$variable[c(1, 77)] <- NA
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10)

  rows <- vapply(seq_len(n_chunks(p)), function(i) {
    nrow(ggchunk:::build_chunk_plot(p, i)$data)
  }, numeric(1))
  expect_equal(sum(rows), nrow(d)) # NA-key rows kept in segment 1
})

test_that("discrete x works with character and reordered keys", {
  d <- make_box_df(45, per = 6)
  d$variable <- as.character(d$variable)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 15)
  expect_equal(n_chunks(p), 3L)
  expect_no_error(ggplot2::ggplot_build(ggchunk:::build_chunk_plot(p, 2)))

  d2 <- make_box_df(40, per = 5)
  p2 <- ggplot2::ggplot(d2, ggplot2::aes(stats::reorder(variable, value), value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10)
  expect_equal(n_chunks(p2), 4L)
  expect_no_error(ggplot2::ggplot_build(ggchunk:::build_chunk_plot(p2, 1)))
})

test_that("all major geoms fold over the discrete x axis", {
  d <- make_box_df(45, per = 8)

  pbar <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_col() +
    ggchunk(method = "x", n_per_chunk = 15)
  expect_equal(n_chunks(pbar), 3L)
  expect_no_error(ggplot2::ggplot_build(ggchunk:::build_chunk_plot(pbar, 1)))

  pviolin <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_violin() +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.4) +
    ggchunk(method = "x", n_per_chunk = 15)
  expect_equal(n_chunks(pviolin), 3L)
  expect_no_error(ggplot2::ggplot_build(ggchunk:::build_chunk_plot(pviolin, 2)))

  plabel <- ggplot2::ggplot(d, ggplot2::aes(variable, value, label = variable)) +
    ggplot2::geom_point() +
    ggchunk(method = "x", n_per_chunk = 15)
  expect_equal(n_chunks(plabel), 3L)
})

test_that("the original plot object is never mutated by truncation", {
  d <- make_box_df(40, per = 5)
  p0 <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot()
  before_data <- nrow(p0$data)
  before_layers <- length(p0$layers)

  p <- p0 + ggchunk(method = "x", n_per_chunk = 10)
  invisible(lapply(seq_len(4), function(i) ggchunk:::build_chunk_plot(p, i)))

  expect_equal(nrow(p0$data), before_data)
  expect_equal(length(p0$layers), before_layers)
  expect_null(p0$labels$subtitle)
  expect_true(is.null(p0$layers[[1]]$data) || inherits(p0$layers[[1]]$data, "waiver"))
})
