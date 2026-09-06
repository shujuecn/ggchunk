test_that("iris100 has the documented structure", {
  expect_equal(nrow(iris100), 5000)
  expect_equal(ncol(iris100), 5)
  expect_s3_class(iris100$Species, "factor")
  expect_equal(nlevels(iris100$Species), 100)
  expect_true(all(vapply(iris100[-1], is.numeric, logical(1))))
})

test_that("iris100 chunks into readable boxplot panels", {
  p <- ggplot2::ggplot(iris100, ggplot2::aes(Species, Sepal.Length)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 25)
  expect_equal(n_chunks(p), 4L)
  labs <- built_labels(ggchunk:::build_chunk_plot(p, 4))
  expect_length(labs, 25)
})

test_that("diamonds_sample folds along the x axis", {
  expect_equal(nrow(diamonds_sample), 5000)
  p <- ggplot2::ggplot(diamonds_sample, ggplot2::aes(carat, price)) +
    ggplot2::geom_point(alpha = 0.3) +
    ggchunk(method = "x", n_chunks = 2)
  expect_equal(n_chunks(p), 2L)
  # carat is right-skewed: equal-width segments hold unequal counts, but no
  # row is lost and even = TRUE balances them
  sizes <- vapply(seq_len(2), function(i) {
    nrow(ggchunk:::build_chunk_plot(p, i)$data)
  }, numeric(1))
  expect_equal(sum(sizes), 5000)

  pe <- ggplot2::ggplot(diamonds_sample, ggplot2::aes(carat, price)) +
    ggplot2::geom_point(alpha = 0.3) +
    ggchunk(method = "x", n_chunks = 2, even = TRUE)
  sizes_e <- vapply(seq_len(2), function(i) {
    nrow(ggchunk:::build_chunk_plot(pe, i)$data)
  }, numeric(1))
  # quantile cuts are near-balanced; ties at the median (172 rows of
  # carat == 0.7) shift the split a little
  expect_lt(max(sizes_e) / 5000, 0.6)
})
