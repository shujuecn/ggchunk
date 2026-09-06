# ---------------------------------------------------------------------------
# Segment computation: cut the truncation axis into consecutive segments and
# derive per-segment row indices for the plot data and for every layer that
# carries its own data frame.
# ---------------------------------------------------------------------------

compute_chunks <- function(p, spec, det) {
  axis <- det$method # "x" or "y"
  dat <- primary_data(p)
  keyquo <- det$keyquo
  key <- eval_key(keyquo, dat)
  if (is.null(key) || !is.atomic(key) || length(key) != nrow(dat)) {
    stop(
      "Could not evaluate the truncation axis on the primary plot data. ",
      "Provide a simpler mapping or check the data.",
      call. = FALSE
    )
  }

  if (is_discrete(key)) {
    n_pc <- spec$n_per_chunk %||% (spec$n_chunks %||% 20L)
    vals <- disc_values(key)
    if (length(vals) == 0L) {
      stop("The truncation axis has no non-missing values.", call. = FALSE)
    }
    groups <- split(seq_along(vals), ceiling(seq_along(vals) / n_pc))
    segments <- lapply(groups, function(ii) list(kind = "discrete", keep = vals[ii]))
    noun <- "levels"
  } else {
    nseg <- spec$n_chunks %||% 3L
    xv <- key[!is.na(key)]
    if (length(xv) == 0L) {
      stop("The truncation axis has no non-missing values.", call. = FALSE)
    }
    if (isTRUE(spec$even)) {
      brks <- unique(stats::quantile(
        xv, probs = seq(0, 1, length.out = nseg + 1),
        names = FALSE, type = 7
      ))
    } else {
      rng <- range(xv, na.rm = TRUE)
      brks <- seq(rng[1], rng[2], length.out = nseg + 1)
    }
    nseg_eff <- length(brks) - 1L
    segments <- lapply(seq_len(nseg_eff), function(i) {
      list(kind = "continuous", lo = brks[i], hi = brks[i + 1L], last = i == nseg_eff)
    })
    noun <- paste0(axis, " range")
  }

  # rows on the primary data for one segment
  rows_for <- function(seg) {
    if (seg$kind == "discrete") {
      rows <- which(!is.na(key) & key %in% seg$keep)
    } else {
      rows <- which(key >= seg$lo & (key < seg$hi | (seg$last & key <= seg$hi)))
    }
    rows
  }
  na_rows <- which(is.na(key))

  chunks <- lapply(seq_along(segments), function(i) {
    seg <- segments[[i]]
    rows <- rows_for(seg)
    if (i == 1L) {
      rows <- c(rows, na_rows) # rows with missing keys stay in segment 1
    }
    list(
      keep = seg$keep,
      lo = if (seg$kind == "continuous") seg$lo else NULL,
      hi = if (seg$kind == "continuous") seg$hi else NULL,
      rows_plot = rows
    )
  })

  empty <- !vapply(chunks, function(ch) length(ch$rows_plot) > 0L, logical(1))
  if (any(empty)) {
    warning(sprintf("ggchunk: dropping %d empty segment(s).", sum(empty)), call. = FALSE)
    chunks <- chunks[!empty]
  }

  n <- length(chunks)
  layer_datas <- capture_layer_datas(p)
  warned <- new.env(parent = emptyenv())

  for (i in seq_len(n)) {
    ch <- chunks[[i]]
    ch$rows_layers <- lapply(layer_datas, function(ld) {
      if (is.null(ld)) {
        return(NULL) # layer inherits the (already filtered) plot data
      }
      k <- eval_key(keyquo, ld)
      if (!is.null(k) && is.atomic(k) && length(k) == nrow(ld)) {
        if (!is.null(ch$keep)) {
          rows <- which(!is.na(k) & k %in% ch$keep)
        } else {
          rows <- which(k >= ch$lo & (k < ch$hi | (i == n & k <= ch$hi)))
        }
        if (i == 1L) {
          rows <- c(rows, which(is.na(k)))
        }
        rows
      } else if (nrow(ld) == nrow(dat)) {
        ch$rows_plot
      } else {
        if (!isTRUE(warned$layer)) {
          warning(
            "ggchunk: at least one layer carries data that could not be segmented; ",
            "it is drawn unfiltered in every segment.",
            call. = FALSE
          )
          warned$layer <- TRUE
        }
        NA_integer_ # marker: keep whole
      }
    })
    ch$index <- i
    ch$detail <- describe_chunk(ch, axis, noun)
    ch$tag <- sprintf("segment %d/%d - %s", i, n, ch$detail)
    chunks[[i]] <- ch
  }

  list(
    chunks = chunks,
    layer_datas = layer_datas,
    noun = noun,
    key_label = det$key_label,
    keyquo = keyquo,
    axis = axis
  )
}

describe_chunk <- function(ch, axis, noun) {
  if (!is.null(ch$keep)) {
    k <- ch$keep
    rng <- if (length(k) > 2L) sprintf("%s to %s", k[1], k[length(k)]) else paste(k, collapse = ", ")
    sprintf("%d %s: %s", length(k), noun, rng)
  } else {
    fmt <- function(v) format(v, digits = 4, trim = TRUE)
    sprintf("%s in [%s, %s]", axis, fmt(ch$lo), fmt(ch$hi))
  }
}
