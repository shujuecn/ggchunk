test_that("ggchunk() validates its arguments", {
  expect_s3_class(ggchunk(), "ggchunk_spec")
  expect_error(ggchunk(method = "bogus"), 'one of "auto", "x" or "y"')
  expect_error(ggchunk(n_chunks = 0), "n_chunks")
  expect_error(ggchunk(nrow = -1), "nrow")
  expect_error(ggchunk(threshold_col = -3), "threshold_col")
  expect_error(ggchunk(even = "yes"), "even")
  expect_warning(ggchunk(unknown_arg = 1), "unknown_arg")
})

test_that("legacy method names are handled", {
  d <- make_box_df(40, per = 5)
  expect_warning(
    p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
      ggplot2::geom_boxplot() +
      ggchunk(method = "ncol", n_per_chunk = 10),
    'renamed to method = "x"'
  )
  expect_identical(ggchunk:::chunk_state(p)$method, "x")
  expect_error(ggchunk(method = "by_group"), "facet_wrap")
  expect_warning(
    p2 <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
      ggplot2::geom_point() +
      ggchunk(method = "n_points", n_per_chunk = 10),
    'folded into method = "x"'
  )
  expect_identical(ggchunk:::chunk_state(p2)$method, "x")
})

test_that("+ ggchunk() on a small plot is a no-op but stays a working ggplot", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggchunk()
  expect_s3_class(p, "ggchunk")
  expect_true(ggplot2::is_ggplot(p))
  expect_identical(ggchunk:::chunk_state(p)$method, "none")
  expect_equal(n_chunks(p), 1L)
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 32)
})

test_that("+ ggchunk() attaches the spec and computes segments", {
  d <- make_box_df(60)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 20)
  expect_s3_class(p, "ggchunk")
  expect_equal(n_chunks(p), 3L)
  st <- ggchunk:::chunk_state(p)
  expect_identical(st$axis, "x")
  expect_equal(st$layout, list(nrow = 3L, ncol = 1L)) # 3 stacked rows, ONE figure
})

test_that("ggplot additions after ggchunk() keep working and recompute", {
  d <- make_box_df(60, per = 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 20)
  p2 <- p + ggplot2::labs(title = "T") + ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(n.breaks = 4)
  expect_s3_class(p2, "ggchunk")
  expect_equal(n_chunks(p2), 3L)
  cp <- ggchunk:::build_chunk_plot(p2, 1)
  expect_identical(cp$labels$title, "T")
  expect_true(length(cp$theme) > 0)
})

test_that("verbose mode reports the decision", {
  d <- make_box_df(40, per = 5)
  expect_message(
    ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
      ggplot2::geom_boxplot() +
      ggchunk(method = "x", n_per_chunk = 10, verbose = TRUE),
    "truncated the x axis"
  )
  expect_message(
    ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
      ggplot2::geom_point() +
      ggchunk(verbose = TRUE),
    "no truncation"
  )
  expect_silent(
    ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
      ggplot2::geom_point() +
      ggchunk()
  )
})
