test_that("auto truncates x for many-level discrete data", {
  d <- make_box_df(40, per = 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk()
  expect_identical(ggchunk:::chunk_state(p)$method, "x")
  expect_equal(n_chunks(p), 2L) # 40 levels / default 20
})

test_that("auto truncates x for large continuous data", {
  set.seed(22)
  d <- data.frame(x = rnorm(2000), y = rnorm(2000))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggchunk(threshold_points = 500)
  expect_identical(ggchunk:::chunk_state(p)$method, "x")
  expect_equal(n_chunks(p), 3L) # default n_chunks
})

test_that("auto is a no-op for small plots", {
  p0 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth()
  p <- p0 + ggchunk()
  expect_identical(ggchunk:::chunk_state(p)$method, "none")
  expect_identical(p$labels, p0$labels)
  expect_identical(p$data, p0$data)
})
