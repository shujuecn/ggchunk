test_that("ggsave_chunk writes the whole figure to one file", {
  d <- make_box_df(60, per = 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10)
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)
  res <- suppressMessages(ggsave_chunk(p, f, width = 12, height = 8))
  expect_identical(res, f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)

  fp <- tempfile(fileext = ".png")
  on.exit(unlink(fp), add = TRUE)
  suppressMessages(ggsave_chunk(p, fp, width = 12, height = 8, dpi = 100))
  expect_true(file.exists(fp))
  expect_false(grepl("_00", fp)) # one file, not page-numbered
})

test_that("ggsave_chunk leaves the caller's active device open", {
  d <- make_box_df(40, per = 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10)
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)

  grDevices::pdf(f, width = 4, height = 4)
  caller_device <- as.integer(grDevices::dev.cur())
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(ggsave_chunk(p, out, width = 8, height = 4))
  expect_equal(as.integer(grDevices::dev.cur()), caller_device)
  grDevices::dev.off()
})

test_that("ggsave_chunk passes through when no truncation happened", {
  p0 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  p <- p0 + ggchunk()
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)
  expect_no_error(ggchunk::ggsave_chunk(p, f))
  expect_true(file.exists(f))
})

test_that("ggsave_chunk rejects unknown devices", {
  d <- make_box_df(40, per = 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10)
  expect_error(ggsave_chunk(p, "foo.flarb"), "Unknown device")
})

test_that("facet_wrap survives truncation (stacked, not replaced)", {
  d <- data.frame(
    variable = factor(rep(sprintf("v%02d", 1:30), each = 12)),
    grp = factor(rep(c("a", "b"), 180)),
    value = rnorm(360)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~grp) +
    ggchunk(method = "x", n_per_chunk = 10)

  expect_equal(n_chunks(p), 3L)
  b <- ggplot2::ggplot_build(ggchunk:::build_chunk_plot(p, 1))
  expect_equal(length(unique(b$layout$layout$PANEL)), 2L)
  expect_no_error(ggplot2::ggplotGrob(ggchunk:::build_chunk_plot(p, 1)))
})

test_that("theme and labs given after ggchunk() reach every segment", {
  d <- make_box_df(30, per = 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Folded boxplots", x = "Category")
  for (i in seq_len(3)) {
    cp <- ggchunk:::build_chunk_plot(p, i)
    expect_identical(cp$labels$title, "Folded boxplots")
    expect_identical(cp$labels$x, "Category")
    expect_true(length(cp$theme) > 0)
  }
})

test_that("chunk_info and chunk_plot accessors behave", {
  d <- make_box_df(40, per = 5)
  p <- ggplot2::ggplot(d, ggplot2::aes(variable, value)) +
    ggplot2::geom_boxplot() +
    ggchunk(method = "x", n_per_chunk = 10)
  info <- chunk_info(p)
  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 4L)
  expect_true(all(info$n_items == 10))
  expect_output(chunk_info(p), "segments: 4 in 1 figure")

  seg <- chunk_plot(p, 2)
  expect_s3_class(seg, "ggplot")
  expect_length(built_labels(seg), 10)

  expect_output(chunk_info(ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggchunk()), "no truncation")
})
