# Render targeted ggchunk examples for common manuscript figures.
# Run from the package root: Rscript examples/render_examples.R
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggchunk)
  set.seed(2026)
})

dir.create("examples", showWarnings = FALSE)
unlink(list.files("examples", pattern = "\\.(pdf|png)$", full.names = TRUE))

save_ex <- function(plot, name, width, height, dpi = 200) {
  suppressMessages(ggsave_chunk(
    plot, sprintf("examples/%s.pdf", name), width = width, height = height
  ))
  suppressMessages(ggsave_chunk(
    plot, sprintf("examples/%s.png", name), width = width, height = height,
    dpi = dpi
  ))
  invisible(NULL)
}

academic_theme <- theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 13, colour = "grey25"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
  )
rotated_x <- theme(
  axis.text.x = element_text(
    angle = 90, hjust = 1, vjust = 0.5, size = 12
  )
)

# 1. Discrete x: many experimental groups and a distribution summary.
p_box <- ggplot(iris100, aes(Species, Sepal.Length)) +
  geom_boxplot(fill = "grey82", colour = "grey25", outlier.size = 0.45) +
  labs(
    title = "Many experimental groups: boxplots folded into rows",
    subtitle = "Discrete x axis; 25 groups per segment",
    x = NULL, y = "Sepal length (cm)"
  ) +
  academic_theme +
  rotated_x +
  ggchunk(method = "x", n_per_chunk = 25)
save_ex(p_box, "01_categorical_boxplot", 10, 10.5)

# 2. Discrete x: grouped counts or effect sizes in a bar chart.
bar_data <- expand.grid(
  pathway = factor(sprintf("Pathway %02d", seq_len(48))),
  cohort = factor(c("Discovery", "Validation"))
)
bar_data$count <- rpois(nrow(bar_data), lambda = 35) +
  rep(seq(0, 18, length.out = 48), each = 2)
p_bar <- ggplot(bar_data, aes(pathway, count, fill = cohort)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.68) +
  scale_fill_manual(values = c("#3B7EA1", "#D97757")) +
  labs(
    title = "Grouped category summaries: bars folded into rows",
    subtitle = "Discrete x axis; 12 categories per segment",
    x = NULL, y = "Observed count", fill = NULL
  ) +
  academic_theme +
  rotated_x +
  ggchunk(method = "x", n_per_chunk = 12)
save_ex(p_bar, "02_categorical_bar", 11, 11)

# 3. Continuous x: a skewed predictor with equal-density segments.
p_scatter <- ggplot(diamonds_sample, aes(carat, price)) +
  geom_point(alpha = 0.28, size = 0.85, colour = "#3B7EA1") +
  geom_smooth(
    method = "lm", formula = y ~ x, se = FALSE,
    colour = "#B33E35", linewidth = 0.65
  ) +
  labs(
    title = "Skewed continuous predictor: scatterplot folded by density",
    subtitle = "Quantile cuts keep a similar number of observations per segment",
    x = "Carat", y = "Price (USD)"
  ) +
  academic_theme +
  ggchunk(method = "x", n_chunks = 4, even = TRUE)
save_ex(p_scatter, "03_continuous_scatter", 11, 9)

# 4. Continuous x: dates and repeated measurements in a longitudinal study.
dates <- seq.Date(as.Date("2024-01-01"), by = "day", length.out = 365)
time_data <- expand.grid(
  day = seq_along(dates),
  series = factor(c("Control", "Treatment A", "Treatment B"))
)
time_data$value <- with(
  time_data,
  10 +
    ifelse(series == "Control", 0, ifelse(series == "Treatment A", 1.2, 2.2)) +
    sin(day / 18) +
    rnorm(nrow(time_data), sd = 0.25)
)
p_time <- ggplot(time_data, aes(day, value, colour = series)) +
  geom_line(linewidth = 0.65) +
  scale_colour_manual(values = c("#333333", "#3B7EA1", "#B33E35")) +
  scale_x_continuous(
    breaks = seq(1, 365, by = 30),
    labels = format(dates[seq(1, 365, by = 30)], "%b")
  ) +
  labs(
    title = "Longitudinal measurements: dates folded into rows",
    subtitle = "Continuous time axis; equal-width temporal segments",
    x = "Month", y = "Measured response", colour = NULL
  ) +
  academic_theme +
  ggchunk(method = "x", n_chunks = 4)
save_ex(p_time, "04_time_series", 11, 10)

# 5. Continuous y: a response with a long lower tail, shown in columns.
y_data <- data.frame(
  predictor = seq(0, 10, length.out = 3000),
  response = c(rexp(1500, rate = 1), rnorm(1500, mean = 12, sd = 1))
)
p_y <- ggplot(y_data, aes(predictor, response)) +
  geom_point(alpha = 0.28, size = 0.7, colour = "#3B7EA1") +
  labs(
    title = "Long-tailed response: y axis folded into columns",
    subtitle = "Each column keeps its own y scale while sharing x",
    x = "Predictor", y = "Response"
  ) +
  academic_theme +
  ggchunk(method = "y", n_chunks = 3)
save_ex(p_y, "05_y_long_tail_scatter", 13, 6)

# 6. Facets plus a distribution geom: preserve biological or clinical groups.
violin_data <- data.frame(
  category = factor(rep(sprintf("Marker %02d", 1:36), each = 24)),
  group = factor(rep(c("Control", "Treatment"), each = 12, times = 36)),
  value = rnorm(36 * 24)
)
violin_data$value <- violin_data$value +
  ifelse(violin_data$group == "Treatment", 0.8, 0) +
  rep(seq(-1, 1, length.out = 36), each = 24)
p_violin <- ggplot(violin_data, aes(category, value, fill = group)) +
  geom_violin(trim = FALSE, colour = NA, alpha = 0.8) +
  geom_boxplot(width = 0.13, outlier.size = 0.25, colour = "grey20") +
  facet_wrap(~group, nrow = 2) +
  scale_fill_manual(values = c("#69B3A2", "#F28E65")) +
  labs(
    title = "Faceted distributions: facets remain inside each x segment",
    subtitle = "Violin and boxplot layers are chunked together",
    x = NULL, y = "Standardized measurement", fill = NULL
  ) +
  academic_theme +
  rotated_x +
  ggchunk(method = "x", n_per_chunk = 12)
save_ex(p_violin, "06_faceted_violin", 11, 12)

# 7. Continuous y: a matrix-like heatmap with many vertical positions.
heatmap_data <- expand.grid(
  time = factor(sprintf("T%02d", seq_len(24))),
  position = seq(1, 90)
)
heatmap_data$signal <- with(
  heatmap_data,
  sin(position / 8) + cos(as.numeric(time) / 3) + rnorm(nrow(heatmap_data), sd = 0.12)
)
p_heatmap <- ggplot(heatmap_data, aes(time, position, fill = signal)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  labs(
    title = "Matrix-like measurements: y positions folded into columns",
    subtitle = "Continuous y chunks preserve the shared time axis",
    x = "Time point", y = "Position", fill = "Signal"
  ) +
  academic_theme +
  theme(axis.text.x = element_text(angle = 90, size = 12)) +
  ggchunk(method = "y", n_chunks = 3)
save_ex(p_heatmap, "07_y_heatmap", 14, 6)

cat("targeted examples written to examples/\n")
