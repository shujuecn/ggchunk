# Dynamically register S3 methods for generics that live in soft-dependent
# packages (the vctrs/scales registration pattern), so ggchunk installs cleanly
# whether or not those packages are present. Methods are registered by name
# (late lookup inside the ggchunk namespace), which also allows registering
# against generics that are not exported by the soft dependency.

s3_register <- function(generic, class, pkgname) {
  stopifnot(is.character(generic), length(generic) == 1L)
  stopifnot(is.character(class), length(class) == 1L)

  pieces <- strsplit(generic, "::")[[1]]
  if (length(pieces) == 2L) {
    pkg <- pieces[[1]]
    genname <- pieces[[2]]
  } else {
    pkg <- NULL
    genname <- pieces[[1]]
  }

  if (is.null(pkg) || !requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(FALSE))
  }
  ns <- asNamespace(pkg)
  if (!exists(genname, envir = ns, inherits = FALSE)) {
    return(invisible(FALSE))
  }

  registerS3method(genname, class, paste0(genname, ".", class), envir = asNamespace(pkgname))
  invisible(TRUE)
}

is_s3_generic <- function(pkg, name) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return(FALSE)
  }
  ns <- asNamespace(pkg)
  if (!exists(name, envir = ns, inherits = FALSE)) {
    return(FALSE)
  }
  f <- get(name, envir = ns, inherits = FALSE)
  is.function(f) && grepl("UseMethod", paste(deparse(body(f)), collapse = " "))
}

.onLoad <- function(libname, pkgname) {
  # ggplot2 < 4.0 exports ggsave() as an S3 generic (and ggsave is imported
  # into this namespace, so registerS3method() can resolve it); 4.0+ inlined
  # ggsave(), in which case complete-figure saving goes through
  # ggsave_chunk().
  if (is_s3_generic("ggplot2", "ggsave")) {
    s3_register("ggplot2::ggsave", "ggchunk", pkgname)
  }
  invisible(NULL)
}
