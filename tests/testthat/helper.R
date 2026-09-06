# Helpers shared across test files

make_box_df <- function(n_levels = 60, per = 10, prefix = "v") {
  data.frame(
    variable = factor(rep(sprintf("%s%03d", prefix, seq_len(n_levels)), each = per)),
    value = rnorm(n_levels * per, sd = 5)
  )
}

built_labels <- function(plot_obj) {
  b <- ggplot2::ggplot_build(plot_obj)
  b$layout$panel_params[[1]]$x$get_labels()
}
