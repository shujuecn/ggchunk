test_that("continuous x folds by equal-width ranges", {
  set.seed(11)
  d <- data.frame(x = c(rnorm(800, 0, 0.5), rnorm(400, 5, 0.5)), y = rnorm(1200))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggchunk(method = "x", n_chunks = 3)

  expect_equal(n_chunks(p), 3L)
  st <- ggchunk:::chunk_state(p)
  lo <- vapply(st$chunks, `[[`, numeric(1), "lo")
  hi <- vapply(st$chunks, `[[`, numeric(1), "hi")
  expect_equal(lo[2], lo[1] + (hi[1] - lo[1])) # adjacent segments
  expect_equal(hi[3], max(d$x))

  sizes <- vapply(seq_len(3), function(i) {
    nrow(ggchunk:::build_chunk_plot(p, i)$data)
  }, numeric(1))
  expect_equal(sum(sizes), nrow(d)) # no row lost, none duplicated
})

test_that("even = TRUE cuts at quantiles for equal density", {
  set.seed(12)
  d <- data.frame(x = c(rnorm(3000, 0, 0.5), rnorm(1000, 4, 0.5)), y = rnorm(4000))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggchunk(method = "x", n_chunks = 4, even = TRUE)
  sizes <- vapply(seq_len(4), function(i) {
    nrow(ggchunk:::build_chunk_plot(p, i)$data)
  }, numeric(1))
  expect_equal(sum(sizes), 4000)
  expect_true(max(sizes) - min(sizes) <= 2) # near-equal density
})

test_that("half-open segment boundaries keep every row exactly once", {
  set.seed(13)
  d <- data.frame(x = rep(c(0, 1, 2, 3), each = 25), y = rnorm(100))
  # range 0..3 in 2 equal segments: [0, 1.5) and [1.5, 3] -> 50 + 50 rows
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggchunk(method = "x", n_chunks = 2)
  sizes <- vapply(seq_len(2), function(i) {
    nrow(ggchunk:::build_chunk_plot(p, i)$data)
  }, numeric(1))
  expect_equal(sizes, c(50, 50))
})

test_that("layers with their own data are segmented by the same ranges", {
  set.seed(14)
  d <- data.frame(x = seq(0, 10, length.out = 400), y = rnorm(400))
  sub <- d[d$x > 7.5, ]
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = sub, colour = "red", size = 3) +
    ggchunk(method = "x", n_chunks = 2)

  cp <- ggchunk:::build_chunk_plot(p, 2)
  # the highlight layer's rows fall in segment 2 only
  expect_equal(nrow(cp$layers[[2]]$data), nrow(sub))
  expect_equal(nrow(ggchunk:::build_chunk_plot(p, 1)$layers[[2]]$data), 0)
})

test_that("geom_histogram and geom_line fold over continuous axes", {
  set.seed(15)
  d <- data.frame(x = runif(2000), g = rep(1:4, each = 500), y = rnorm(2000))

  ph <- ggplot2::ggplot(d, ggplot2::aes(x)) +
    ggplot2::geom_histogram(bins = 20) +
    ggchunk(method = "x", n_chunks = 2)
  expect_equal(n_chunks(ph), 2L)
  expect_no_error(ggplot2::ggplot_build(ggchunk:::build_chunk_plot(ph, 1)))

  pl <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = factor(g))) +
    ggplot2::geom_line() +
    ggchunk(method = "x", n_chunks = 2)
  expect_equal(n_chunks(pl), 2L)
  expect_no_error(ggplot2::ggplot_build(ggchunk:::build_chunk_plot(pl, 2)))
})

test_that("a plot built without global data folds via its layer data", {
  d <- make_box_df(30, per = 4)
  p <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = d, mapping = ggplot2::aes(variable, value)) +
    ggchunk(method = "x", n_per_chunk = 10)

  expect_equal(n_chunks(p), 3L)
  cp <- ggchunk:::build_chunk_plot(p, 1)
  expect_equal(nrow(cp$layers[[1]]$data), 40)
  expect_length(levels(droplevels(cp$layers[[1]]$data$variable)), 10)
})
