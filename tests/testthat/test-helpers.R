test_that("extract_geom_info describes every layer", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth()
  gi <- ggchunk:::extract_geom_info(p)
  expect_equal(nrow(gi), 2L)
  expect_identical(gi$geom, c("GeomPoint", "GeomSmooth"))
  expect_true(anyNA(gi$data_rows)) # smooth layer has no own data
  expect_equal(nrow(ggchunk:::extract_geom_info(ggplot2::ggplot())), 0L)
})

test_that("extract_aes_info classifies mappings", {
  p <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length, colour = Species)) +
    ggplot2::geom_boxplot()
  ai <- ggchunk:::extract_aes_info(p)
  expect_true("x" %in% ai$mapping$aesthetic)
  xrow <- ai$mapping[ai$mapping$aesthetic == "x", ]
  expect_identical(xrow$type, "discrete")
  expect_equal(xrow$n_levels, 3)
})

test_that("internal chunk helpers behave", {
  expect_true(ggchunk:::is_discrete(factor("a")))
  expect_true(ggchunk:::is_discrete(c("a", "b")))
  expect_true(ggchunk:::is_discrete(TRUE))
  expect_false(ggchunk:::is_discrete(1.5))

  f <- factor(c("b", "a", "c", "a", NA), levels = c("a", "b", "c", "d"))
  expect_identical(ggchunk:::disc_values(f), c("a", "b", "c")) # unused level dropped
  expect_identical(ggchunk:::disc_values(c("z", "a")), c("a", "z"))

  dd <- data.frame(g = factor(c("a", "b", "a", "b")))
  fdd <- ggchunk:::filter_rows(dd, c(1, 3), drop_syms = "g")
  expect_identical(levels(fdd$g), "a")
  expect_identical(ggchunk:::filter_rows(dd, NULL)$g, dd$g)

  expect_equal(ggchunk:::to_inches(2.54, "cm", 300), 1)
  expect_equal(ggchunk:::to_inches(100, "px", 100), 1)
  expect_true(is.na(ggchunk:::to_inches(NA, "in", 300)))
})
