test_that("y truncation folds segments into columns sharing the x axis", {
  set.seed(21)
  d <- data.frame(x = seq(0, 10, length.out = 600), y = rnorm(600, sd = 10))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggchunk(method = "y", n_chunks = 2)

  expect_equal(n_chunks(p), 2L)
  st <- ggchunk:::chunk_state(p)
  expect_identical(st$axis, "y")
  expect_equal(st$layout, list(nrow = 1L, ncol = 2L))

  sizes <- vapply(seq_len(2), function(i) {
    nrow(ggchunk:::build_chunk_plot(p, i)$data)
  }, numeric(1))
  expect_equal(sum(sizes), 600)
  suppressMessages(print(chunk_plot(p)))
})

test_that("y truncation needs a continuous y", {
  d <- data.frame(x = 1:50, y = factor(rep(letters[1:5], each = 10)))
  expect_error(
    ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggchunk(method = "y"),
    "continuous y"
  )
  expect_error(
    ggplot2::ggplot(d, ggplot2::aes(x)) +
      ggplot2::geom_histogram() +
      ggchunk(method = "y"),
    "y aesthetic"
  )
})
