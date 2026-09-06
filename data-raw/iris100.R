# Source script for data/iris100.rda
# Simulates an "iris gone wild" dataset with 100 species x 50 observations,
# for testing/演示 the "ncol" chunking of long boxplots.
set.seed(2026)

n_species <- 100
species <- sprintf("sp_%03d", seq_len(n_species))

eff <- data.frame(
  Species = species,
  Sepal.Length = rnorm(n_species, 0, 0.35),
  Sepal.Width = rnorm(n_species, 0, 0.25),
  Petal.Length = rnorm(n_species, 0, 0.45),
  Petal.Width = rnorm(n_species, 0, 0.20)
)

cols <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
rows <- lapply(seq_len(n_species), function(i) {
  tmpl <- iris[sample.int(nrow(iris), 50, replace = TRUE), cols, drop = FALSE]
  for (cn in cols) {
    tmpl[[cn]] <- tmpl[[cn]] + eff[[cn]][i] + rnorm(50, 0, 0.15)
  }
  data.frame(Species = factor(species[i], levels = species), tmpl, row.names = NULL)
})

iris100 <- do.call(rbind, rows)
rownames(iris100) <- NULL
stopifnot(nrow(iris100) == 5000, nlevels(iris100$Species) == 100)

usethis::use_data(iris100, overwrite = TRUE, compress = "bzip2")
