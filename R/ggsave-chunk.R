# ---------------------------------------------------------------------------
# Saving the single figure.
#
# ggplot2 < 4.0 exports ggsave() as an S3 generic, so `ggsave("f.pdf", p)`
# dispatches to ggsave.ggchunk (registered dynamically in .onLoad). ggplot2
# 4.0+ inlined ggsave() (no S3 dispatch), so the documented entry point is
# ggsave_chunk(), which works on every ggplot2 version and always writes one
# file: the whole figure with all segments.
# ---------------------------------------------------------------------------

#' Save a ggchunk figure to a file
#'
#' Renders the complete figure -- every axis segment in one image -- to a
#' single PDF or raster/vector file. On ggplot2 older than 4.0, `ggsave()`
#' also works directly on `ggchunk` objects via S3 dispatch.
#'
#' @param x A `ggchunk` object.
#' @param filename File name; the extension selects the device unless
#'   `device` is given.
#' @param device Device name (`"pdf"`, `"cairo_pdf"`, `"png"`, ...); derived
#'   from `filename` when `NULL`.
#' @param path Directory in which to save; `NULL` (default) uses the working
#'   directory.
#' @param width,height Figure dimensions; `NA` uses the device default.
#' @param units One of `"in"`, `"cm"`, `"mm"`, `"px"`.
#' @param dpi Resolution, used for `"px"` units and raster devices.
#' @param limitsize Passed to ggplot2 where applicable.
#' @param bg Background colour where the device supports it.
#' @param ... Passed on to the device function.
#' @return Invisibly, the file path.
#' @importFrom ggplot2 ggsave
#' @examples
#' \donttest{
#' library(ggplot2)
#' df <- data.frame(
#'   variable = factor(rep(sprintf("v%02d", seq_len(40)), each = 5)),
#'   value = rnorm(200)
#' )
#' p <- ggplot(df, aes(variable, value)) +
#'   geom_boxplot() +
#'   ggchunk(method = "x", n_per_chunk = 10)
#' ggsave_chunk(p, tempfile(fileext = ".pdf"), width = 12, height = 4)
#' }
#' @export
ggsave_chunk <- function(x,
                         filename,
                         device = NULL,
                         path = NULL,
                         width = NA,
                         height = NA,
                         units = c("in", "cm", "mm", "px"),
                         dpi = 300,
                         limitsize = TRUE,
                         bg = NULL,
                         ...) {
  units <- match.arg(units)
  if (!inherits(x, "ggchunk")) {
    stop("`x` must be a ggchunk object; for plain ggplots use ggplot2::ggsave().", call. = FALSE)
  }
  x <- ensure_finalized(x)
  st <- chunk_state(x)
  if (st$method == "none") {
    return(ggplot2::ggsave(
      filename, plot = x, device = device, path = path,
      width = width, height = height, units = units, dpi = dpi,
      limitsize = limitsize, bg = bg, ...
    ))
  }

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  dev <- device %||% file_ext(filename)
  dev <- switch(dev, jpg = "jpeg", tif = "tiff", dev)
  caller_device <- as.integer(grDevices::dev.cur())
  opened_device <- NULL
  restore_device <- function() {
    if (!is.null(opened_device) && opened_device %in% grDevices::dev.list()) {
      grDevices::dev.set(opened_device)
      try(grDevices::dev.off(), silent = TRUE)
    }
    if (caller_device %in% grDevices::dev.list()) {
      grDevices::dev.set(caller_device)
    }
  }

  if (identical(dev, "pdf") || identical(dev, "cairo_pdf")) {
    w <- to_inches(width, units, dpi)
    h <- to_inches(height, units, dpi)
    dev_fun <- if (identical(dev, "pdf")) grDevices::pdf else grDevices::cairo_pdf
    args <- list(file = filename)
    if (!is.na(w)) args$width <- w
    if (!is.na(h)) args$height <- h
    if (!is.null(bg)) args$bg <- bg
    args <- c(args, list(...))
    do.call(dev_fun, args)
    opened_device <- as.integer(grDevices::dev.cur())
    on.exit(restore_device(), add = TRUE)
    print(build_figure(x))
    grDevices::dev.off()
    opened_device <- NULL
    grDevices::dev.set(caller_device)
    message(sprintf("ggchunk: figure saved to '%s'.", filename))
    invisible(filename)
  } else if (identical(dev, "png") || identical(dev, "jpeg") ||
    identical(dev, "bmp") || identical(dev, "tiff") || identical(dev, "svg")) {
    open_device(filename, dev, width, height, units, dpi, bg, list(...))
    opened_device <- as.integer(grDevices::dev.cur())
    on.exit(restore_device(), add = TRUE)
    print(build_figure(x))
    grDevices::dev.off()
    opened_device <- NULL
    grDevices::dev.set(caller_device)
    message(sprintf("ggchunk: figure saved to '%s'.", filename))
    invisible(filename)
  } else {
    stop(
      "Unknown device \"", dev, "\". Use pdf, cairo_pdf, png, jpeg, bmp, tiff or svg.",
      call. = FALSE
    )
  }
}

# Open a raster/svg device; retry without `bg` for devices that reject it.
open_device <- function(f, dev, width, height, units, dpi, bg, dot_args) {
  dev_fun <- get(dev, envir = asNamespace("grDevices"))
  has_units <- !identical(dev, "svg")
  args <- list(file = f)
  if (has_units && !is.na(width)) args$width <- width
  if (has_units && !is.na(height)) args$height <- height
  if (has_units) {
    args$units <- units
    if (!identical(units, "px")) args$res <- dpi
    if (identical(units, "px") && is.na(width)) args$width <- 480 # device default
    if (identical(units, "px") && is.na(height)) args$height <- 480
  } else {
    if (!is.na(width)) args$width <- to_inches(width, units, dpi)
    if (!is.na(height)) args$height <- to_inches(height, units, dpi)
  }
  if (!is.null(bg)) args$bg <- bg
  args <- c(args, dot_args)
  tryCatch(
    do.call(dev_fun, args),
    error = function(e) {
      args$bg <- NULL
      do.call(dev_fun, args)
    }
  )
}

# S3 method for ggplot2 < 4.0, where ggsave() is a real generic. Registered
# dynamically in .onLoad; intentionally not exported.
ggsave.ggchunk <- function(filename, plot, ...) {
  ggsave_chunk(plot, filename, ...)
}
