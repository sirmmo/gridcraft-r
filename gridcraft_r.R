# GridCraft R: R in GridCraft's formulas and ribbon. main.R loads this file and calls main().
#
# GridCraft starts `Rscript --vanilla main.R` and talks to it over standard input
# and output: one JSON-RPC 2.0 message per line. See
# https://github.com/storytold/gridcraft/blob/main/docs/plugins.md
#
# Only base R (3.4 or later) is used: the JSON reader and writer are below, so the add-in behaves
# the same everywhere, with or without packages installed.
#
# Everything lives in the environment `.gridcraft_r`, so the R code people run (whose workspace
# is the global environment) doesn't trip over the add-in's own functions.

.gridcraft_r <- local({

PLUGIN_VERSION <- "0.2.0"
PLUGIN_ID <- "io.github.sirmmo.r"

# Limits. GridCraft sends at most 1,000,000 cells a call and takes as many back.
MAX_CELLS <- 1000000
MAX_TEXT <- 32767L          # longest text in a cell
MAX_DEPTH <- 64L            # deepest JSON nesting read
MAX_LINE <- 64 * 1024 * 1024 # longest message read (bytes)
FN_TIME_LIMIT <- 25         # seconds: R.EVAL/R.CALL (GridCraft waits 30)
local({ # tests set a shorter limit
  t <- suppressWarnings(as.numeric(Sys.getenv("GRIDCRAFT_R_TIME_LIMIT")))
  if (!is.na(t) && t > 0 && t < FN_TIME_LIMIT) FN_TIME_LIMIT <<- t
})
CMD_TIME_LIMIT <- 50        # seconds: commands (GridCraft waits 60)
MAX_PROMPT <- 3900L         # a message dialog holds 4,000 characters
MAX_ROWS <- 1048576
MAX_COLS <- 16384

ERR_VALUE <- '{"error":"#VALUE!"}'
ERR_NUM <- '{"error":"#NUM!"}'

# What `initialize` told us.
host <- new.env()
host$id <- "gridcraft"
host$features <- character(0)
host$version <- ""

# The workbook GridCraft is working on, as far as we know (see "Trust" below): what older
# GridCraft versions leave us to go by.
active <- new.env()
active$known <- FALSE
active$path <- NA_character_
active$name <- NA_character_

# The workbook whose formula is being calculated (functions/call's `document`, GridCraft 2.2).
calling <- new.env()
calling$has <- FALSE
calling$path <- NA_character_
calling$name <- NA_character_
calling$id <- NA_character_

# The workbooks GridCraft told us about by session id (protocol 2.2): id -> list(path, name).
docs <- new.env()

# The connection to GridCraft (standard input), for asking it things while we answer.
io <- new.env()
io$con <- NULL
io$queue <- character(0) # messages that arrived while we waited for an answer
io$next_id <- 0L

# Text typed in the dialogs, offered again next time.
last <- new.env()
last$script <- ""
last$plot <- "plot(x)"
last$console <- ""

ascii_out <- FALSE # escape everything that isn't ASCII (set in main(): non-UTF-8 sessions)

log_msg <- function(...) {
  text <- paste0(..., collapse = "")
  try(cat("gridcraft-r: ", text, "\n", sep = "", file = stderr()), silent = TRUE)
  invisible(NULL)
}

# A failure meant for the person: a command fails with this message, a function with #VALUE!.
plugin_error <- function(message) {
  stop(structure(class = c("plugin_error", "error", "condition"),
                 list(message = message, call = NULL)))
}

# A function's answer: a cell error such as #NUM! (as opposed to a failed call).
cell_error <- function(code) {
  stop(structure(class = c("cell_error", "error", "condition"),
                 list(message = code, call = NULL)))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ============================================================================================
# JSON
# ============================================================================================
#
# Reading: null is NA, true/false logical, numbers double, strings character (UTF-8), arrays
# unnamed lists, objects named lists. An array of rows of plain values (what a range looks like)
# comes back as a compact "json_rows" object instead (see json_rows_* below), which is how a
# million cells are read in about a second.

json_fail <- function(why) stop(structure(class = c("json_error", "error", "condition"),
                                           list(message = paste("bad JSON:", why), call = NULL)))

# Token types.
T_OPEN_A <- 1L; T_CLOSE_A <- 2L; T_OPEN_O <- 3L; T_CLOSE_O <- 4L; T_COLON <- 5L; T_COMMA <- 6L
T_STR <- 7L; T_NUM <- 8L; T_TRUE <- 9L; T_FALSE <- 10L; T_NULL <- 11L

BYTE_CLASS <- local({
  b <- integer(256)
  b[c(91L, 93L, 123L, 125L, 58L, 44L) + 1L] <- 1L
  b[c(32L, 9L, 10L, 13L) + 1L] <- 2L
  b
})

json_parse <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) json_fail("not text")
  s <- text
  Encoding(s) <- "bytes" # positions below are byte positions
  r <- charToRaw(s)
  n <- length(r)
  if (n == 0L) json_fail("empty")
  # Backslash escapes: the character after an odd run of backslashes is escaped.
  bs <- r == as.raw(92L)
  esc <- integer(0)
  if (any(bs)) {
    rl <- rle(bs)
    ends <- cumsum(rl$lengths)
    odd <- rl$values & (rl$lengths %% 2L == 1L)
    esc <- ends[odd] + 1L
    esc <- esc[esc <= n]
  }
  rm(bs)
  q <- r == as.raw(34L)
  if (length(esc)) q[esc] <- FALSE
  qi <- which(q)
  rm(q)
  if (length(qi) %% 2L == 1L) json_fail("a string isn't closed")
  so <- qi[seq_len(length(qi) %/% 2L) * 2L - 1L]
  sc <- qi[seq_len(length(qi) %/% 2L) * 2L]
  rm(qi)
  # Inside a string (quotes included)?
  d <- integer(n + 1L)
  d[so] <- d[so] + 1L
  d[sc + 1L] <- d[sc + 1L] - 1L
  outside <- cumsum(d)[seq_len(n)] == 0L
  rm(d)
  ri <- as.integer(r)
  # Byte classes outside strings: 1 punctuation, 2 white space, 0 anything else (a literal).
  cls <- BYTE_CLASS[ri + 1L]
  cls[!outside] <- 3L
  rm(outside)
  sp <- which(cls == 1L)
  edge <- diff(c(0L, cls == 0L, 0L))
  rm(cls)
  os <- which(edge == 1L)
  oe <- which(edge == -1L) - 1L
  rm(edge)
  starts <- c(sp, so, os)
  ends <- c(sp, sc, oe)
  kind <- c(rep.int(1L, length(sp)), rep.int(2L, length(so)), rep.int(3L, length(os)))
  o <- order(starts)
  starts <- starts[o]; ends <- ends[o]; kind <- kind[o]
  ntok <- length(starts)
  if (ntok == 0L) json_fail("empty")
  tt <- integer(ntok)
  numv <- rep(NA_real_, ntok)
  strv <- rep(NA_character_, ntok)
  k <- which(kind == 1L)
  tt[k] <- match(ri[starts[k]], c(91L, 93L, 123L, 125L, 58L, 44L))
  rm(ri)
  # The text of every string and literal at once (substring() is slow on long text): their
  # bytes, each followed by \001, split in one go.
  kt <- which(kind != 1L)
  texts <- character(0)
  if (length(kt)) {
    if (any(r == as.raw(1L))) json_fail("a control character outside an escape")
    str_tok <- kind[kt] == 2L
    cs <- starts[kt] + str_tok
    ce <- ends[kt] - str_tok
    len <- pmax(ce - cs + 1L, 0L)
    total <- sum(len)
    at <- if (total > 0) rep.int(cs, len) + (seq_len(total) - rep.int(cumsum(len) - len, len)) - 1L else integer(0)
    keys <- c(at, ce + 0.5)
    bytes <- c(r[at], rep(as.raw(1L), length(kt)))
    rm(at)
    texts <- strsplit(rawToChar(bytes[order(keys)]), "\001", fixed = TRUE, useBytes = TRUE)[[1L]]
    rm(keys, bytes)
    if (length(texts) < length(kt)) texts <- c(texts, rep("", length(kt) - length(texts)))
    if (length(texts) != length(kt)) json_fail("bad text")
  }
  rm(r)
  k <- which(kind == 2L)
  if (length(k)) {
    tt[k] <- T_STR
    strv[k] <- json_unescape(texts[kind[kt] == 2L])
  }
  k <- which(kind == 3L)
  if (length(k)) {
    w <- texts[kind[kt] == 3L]
    lit <- match(w, c("true", "false", "null"))
    isnum <- is.na(lit)
    if (any(isnum)) {
      ok <- grepl("^-?(0|[1-9][0-9]*)([.][0-9]+)?([eE][-+]?[0-9]+)?$", w[isnum], useBytes = TRUE)
      if (!all(ok)) json_fail(paste0("unexpected `", substr(w[isnum][!ok][1L], 1L, 20L), "`"))
    }
    tt[k] <- ifelse(isnum, T_NUM, T_NUM + lit)
    numv[k[isnum]] <- as.numeric(w[isnum])
  }
  rm(starts, ends, kind)
  # Brackets: depth after each token, and each opening bracket's closing one.
  op <- tt == T_OPEN_A | tt == T_OPEN_O
  cl <- tt == T_CLOSE_A | tt == T_CLOSE_O
  dep <- cumsum(op) - cumsum(cl)
  if (any(dep < 0L) || dep[ntok] != 0L) json_fail("brackets don't match")
  if (max(dep) > MAX_DEPTH) json_fail(paste("nested more than", MAX_DEPTH, "deep"))
  mate <- integer(ntok)
  oi <- which(op)
  ci <- which(cl)
  if (length(oi)) {
    oi <- oi[order(dep[oi], oi)]
    ci <- ci[order(dep[ci] + 1L, ci)]
    if (any(tt[ci] != tt[oi] + 1L)) json_fail("brackets don't match")
    mate[oi] <- ci
  }
  cs_open <- cumsum(op)
  cs_obj <- cumsum(tt == T_OPEN_O)
  rm(op, cl)
  pos <- 1L

  scalars <- function(idx) {
    out <- vector("list", length(idx))
    t <- tt[idx]
    w <- t == T_STR; if (any(w)) out[w] <- as.list(strv[idx[w]])
    w <- t == T_NUM; if (any(w)) out[w] <- as.list(numv[idx[w]])
    w <- t == T_TRUE; if (any(w)) out[w] <- list(TRUE)
    w <- t == T_FALSE; if (any(w)) out[w] <- list(FALSE)
    w <- t == T_NULL; if (any(w)) out[w] <- list(NA)
    out
  }

  # Members of the container opened at i (closing at j): the commas at its own level, plus one.
  members <- function(i, j) {
    sel <- (i + 1L):(j - 1L)
    sum(tt[sel] == T_COMMA & dep[sel] == dep[i]) + 1L
  }

  # An array whose elements are all flat arrays of plain values: a range. NULL if it isn't.
  rows_fast <- function(i, j) {
    if (tt[i + 1L] != T_OPEN_A || cs_obj[j] != cs_obj[i]) return(NULL)
    inner <- (i + 1L):(j - 1L)
    if (max(dep[inner]) > dep[i] + 1L) return(NULL)
    ro <- inner[tt[inner] == T_OPEN_A]
    rc <- mate[ro]
    m <- length(ro)
    if (rc[m] != j - 1L) return(NULL)
    if (m > 1L && (any(ro[-1L] != rc[-m] + 2L) || any(tt[rc[-m] + 1L] != T_COMMA))) return(NULL)
    rid <- cumsum(tt[inner] == T_OPEN_A)
    rel <- inner - ro[rid]
    rowlen <- rc - ro - 1L
    content <- rel >= 1L & rel <= rowlen[rid]
    if (any(rowlen > 0L & rowlen %% 2L == 0L)) json_fail("bad array")
    cp <- inner[content]
    crel <- rel[content]
    crid <- rid[content]
    isval <- crel %% 2L == 1L
    if (any(tt[cp[isval]] < T_STR) || any(tt[cp[!isval]] != T_COMMA)) json_fail("bad array")
    vp <- cp[isval]
    structure(list(nrow = m, rowlen = (rowlen + 1L) %/% 2L, row = crid[isval],
                   col = (crel[isval] + 1L) %/% 2L, type = tt[vp], num = numv[vp], str = strv[vp]),
              class = "json_rows")
  }

  value <- function() {
    if (pos > ntok) json_fail("it ends too soon")
    t <- tt[pos]
    if (t >= T_STR) {
      v <- switch(t - 6L, strv[pos], numv[pos], TRUE, FALSE, NA)
      pos <<- pos + 1L
      return(v)
    }
    if (t == T_OPEN_A) return(array_value())
    if (t == T_OPEN_O) return(object_value())
    json_fail("unexpected punctuation")
  }

  array_value <- function() {
    i <- pos
    j <- mate[i]
    if (j == i + 1L) {
      pos <<- j + 1L
      return(list())
    }
    if (cs_open[j] == cs_open[i]) {
      # Flat: value, comma, value...
      inner <- (i + 1L):(j - 1L)
      if (length(inner) %% 2L == 0L) json_fail("bad array")
      vi <- inner[seq_len((length(inner) + 1L) %/% 2L) * 2L - 1L]
      ci <- inner[seq_len(length(inner) %/% 2L) * 2L]
      if (any(tt[vi] < T_STR) || any(tt[ci] != T_COMMA)) json_fail("bad array")
      pos <<- j + 1L
      return(scalars(vi))
    }
    fast <- rows_fast(i, j)
    if (!is.null(fast)) {
      pos <<- j + 1L
      return(fast)
    }
    out <- vector("list", members(i, j))
    n <- 0L
    pos <<- i + 1L
    repeat {
      n <- n + 1L
      if (n > length(out)) json_fail("bad array")
      v <- value()
      out[n] <- list(v)
      t <- tt[pos]
      pos <<- pos + 1L
      if (t == T_CLOSE_A) break
      if (t != T_COMMA) json_fail("expected , or ]")
    }
    if (pos != j + 1L) json_fail("bad array")
    out[seq_len(n)]
  }

  object_value <- function() {
    i <- pos
    j <- mate[i]
    if (j == i + 1L) {
      pos <<- j + 1L
      return(structure(list(), names = character(0)))
    }
    size <- members(i, j)
    vals <- vector("list", size)
    keys <- character(size)
    n <- 0L
    pos <<- i + 1L
    repeat {
      n <- n + 1L
      if (n > size || pos + 1L >= j) json_fail("bad object")
      if (tt[pos] != T_STR || tt[pos + 1L] != T_COLON) json_fail("expected \"key\":")
      keys[n] <- strv[pos]
      pos <<- pos + 2L
      v <- value()
      vals[n] <- list(v)
      t <- tt[pos]
      pos <<- pos + 1L
      if (t == T_CLOSE_O) break
      if (t != T_COMMA) json_fail("expected , or }")
    }
    if (pos != j + 1L) json_fail("bad object")
    keys <- keys[seq_len(n)]
    keep <- !duplicated(keys, fromLast = TRUE)
    structure(vals[seq_len(n)][keep], names = keys[keep])
  }

  v <- value()
  if (pos != ntok + 1L) json_fail("there's more after the value")
  v
}

# The text of JSON string literals (without their quotes), as UTF-8.
json_unescape <- function(x) {
  hb <- grepl("\\", x, fixed = TRUE, useBytes = TRUE)
  if (any(hb)) {
    y <- x[hb]
    y <- gsub("\\\\", "\001", y, fixed = TRUE, useBytes = TRUE)
    y <- gsub("\\\"", "\"", y, fixed = TRUE, useBytes = TRUE)
    y <- gsub("\\/", "/", y, fixed = TRUE, useBytes = TRUE)
    y <- gsub("\\b", "\b", y, fixed = TRUE, useBytes = TRUE)
    y <- gsub("\\f", "\f", y, fixed = TRUE, useBytes = TRUE)
    y <- gsub("\\n", "\n", y, fixed = TRUE, useBytes = TRUE)
    y <- gsub("\\r", "\r", y, fixed = TRUE, useBytes = TRUE)
    y <- gsub("\\t", "\t", y, fixed = TRUE, useBytes = TRUE)
    # \001 stands for an escaped backslash: \uXXXX is decoded between them, so a decoded
    # U+0001 isn't mistaken for one.
    hu <- grepl("\\u", y, fixed = TRUE, useBytes = TRUE)
    if (any(hu)) y[hu] <- vapply(y[hu], function(t) {
      parts <- strsplit(t, "\001", fixed = TRUE, useBytes = TRUE)[[1L]]
      if (endsWith(t, "\001")) parts <- c(parts, "")
      paste(vapply(parts, unicode_escapes, "", USE.NAMES = FALSE), collapse = "\\")
    }, "", USE.NAMES = FALSE)
    if (any(!hu)) y[!hu] <- gsub("\001", "\\", y[!hu], fixed = TRUE, useBytes = TRUE)
    x[hb] <- y
  }
  Encoding(x) <- "UTF-8"
  x
}

# Replaces \uXXXX escapes (surrogate pairs joined, lone surrogates as U+FFFD, NUL dropped).
unicode_escapes <- function(y) {
  m <- gregexpr("\\\\u[0-9a-fA-F]{4}", y, useBytes = TRUE)[[1L]]
  if (m[1L] == -1L) return(y)
  st <- as.integer(m)
  b <- charToRaw(y)
  cu <- strtoi(vapply(st, function(p) rawToChar(b[(p + 2L):(p + 5L)]), ""), 16L)
  rep_raw <- vector("list", length(cu))
  i <- 1L
  while (i <= length(cu)) {
    c1 <- cu[i]
    if (c1 >= 0xD800 && c1 <= 0xDBFF && i < length(cu) && st[i + 1L] == st[i] + 6L &&
        cu[i + 1L] >= 0xDC00 && cu[i + 1L] <= 0xDFFF) {
      cp <- 0x10000 + (c1 - 0xD800) * 1024 + (cu[i + 1L] - 0xDC00)
      rep_raw[[i]] <- charToRaw(intToUtf8(cp))
      rep_raw[i + 1L] <- list(raw(0))
      i <- i + 2L
    } else {
      rep_raw[[i]] <- if (c1 >= 0xD800 && c1 <= 0xDFFF) charToRaw(intToUtf8(0xFFFD))
                      else if (c1 == 0) raw(0) else charToRaw(intToUtf8(c1))
      i <- i + 1L
    }
  }
  out <- vector("list", 2L * length(st) + 1L)
  prev <- 1L
  for (k in seq_along(st)) {
    if (st[k] > prev) out[[2L * k - 1L]] <- b[prev:(st[k] - 1L)]
    out[2L * k] <- list(rep_raw[[k]])
    prev <- st[k] + 6L
  }
  if (prev <= length(b)) out[[2L * length(st) + 1L]] <- b[prev:length(b)]
  s <- rawToChar(unlist(out))
  Encoding(s) <- "bytes"
  s
}

# --- writing ---------------------------------------------------------------------------------

# JSON string literals for a character vector (NA is null). ASCII-only output when ascii_out.
json_str <- function(x) {
  if (length(x) == 0L) return(character(0))
  x <- as.character(x)
  na <- is.na(x)
  x[na] <- ""
  x <- enc2utf8(x)
  x <- gsub("\\", "\\\\", x, fixed = TRUE, useBytes = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE, useBytes = TRUE)
  ctl <- grepl("[\001-\037\177]", x, useBytes = TRUE)
  if (any(ctl)) {
    y <- x[ctl]
    y <- gsub("\n", "\\n", y, fixed = TRUE, useBytes = TRUE)
    y <- gsub("\r", "\\r", y, fixed = TRUE, useBytes = TRUE)
    y <- gsub("\t", "\\t", y, fixed = TRUE, useBytes = TRUE)
    for (code in c(1:8, 11:12, 14:31, 127)) {
      ch <- rawToChar(as.raw(code))
      y <- gsub(ch, sprintf("\\u%04x", code), y, fixed = TRUE, useBytes = TRUE)
    }
    x[ctl] <- y
  }
  Encoding(x) <- "UTF-8"
  if (ascii_out) {
    wide <- grepl("[^\001-\177]", x, useBytes = TRUE)
    if (any(wide)) x[wide] <- vapply(x[wide], ascii_escape, "", USE.NAMES = FALSE)
  }
  out <- paste0("\"", x, "\"")
  out[na] <- "null"
  out
}

ascii_escape <- function(s) {
  cp <- utf8ToInt(s)
  if (length(cp) == 0L || anyNA(cp)) {
    # Not UTF-8 after all: keep the ASCII bytes.
    b <- charToRaw(s)
    return(rawToChar(b[b < as.raw(128L)]))
  }
  parts <- character(length(cp))
  small <- cp < 128L
  if (any(small)) parts[small] <- intToUtf8(cp[small], multiple = TRUE)
  big <- cp[!small]
  enc <- sprintf("\\u%04x", big)
  hi <- big > 0xFFFF
  if (any(hi)) {
    v <- big[hi] - 0x10000
    enc[hi] <- sprintf("\\u%04x\\u%04x", 0xD800 + v %/% 1024, 0xDC00 + v %% 1024)
  }
  parts[!small] <- enc
  paste0(parts, collapse = "")
}

# Numbers, shortest of 15 or 17 significant digits that reads back the same. Finite only.
json_num <- function(x) {
  x <- as.double(x)
  out <- sprintf("%.15g", x)
  back <- suppressWarnings(as.numeric(out))
  bad <- !is.na(back) & back != x
  if (any(bad)) out[bad] <- sprintf("%.17g", x[bad])
  out
}

# Text that is already JSON.
json_raw <- function(text) structure(text, class = "json_fragment")

# Any R value to JSON. Named lists are objects, other lists arrays; atomic vectors of length 1
# are scalars, others arrays (I(x) forces an array). Non-finite numbers are null.
to_json <- function(x) {
  if (is.null(x)) return("null")
  if (inherits(x, "json_fragment")) return(paste(unclass(x), collapse = ""))
  if (is.list(x)) {
    nm <- names(x)
    if (!is.null(nm)) {
      if (length(x) == 0L) return("{}")
      parts <- vapply(x, to_json, "", USE.NAMES = FALSE)
      return(paste0("{", paste0(json_str(nm), ":", parts, collapse = ","), "}"))
    }
    if (length(x) == 0L) return("[]")
    return(paste0("[", paste0(vapply(x, to_json, "", USE.NAMES = FALSE), collapse = ","), "]"))
  }
  if (is.factor(x)) x <- as.character(x)
  enc <- if (is.character(x)) {
    json_str(x)
  } else if (is.logical(x)) {
    ifelse(is.na(x), "null", ifelse(x, "true", "false"))
  } else if (is.numeric(x)) {
    out <- rep("null", length(x))
    ok <- is.finite(x)
    out[ok] <- json_num(x[ok])
    out
  } else {
    json_str(as.character(x))
  }
  if (length(x) == 1L && !inherits(x, "AsIs")) enc else paste0("[", paste0(enc, collapse = ","), "]")
}

# Reads `key` from a parsed object (NULL when it isn't there or isn't an object).
jget <- function(x, key) {
  if (!is.list(x) || is.null(names(x))) return(NULL)
  x[[key, exact = TRUE]]
}

is_null <- function(x) is.null(x) || (is.atomic(x) && length(x) == 1L && is.na(x) && is.logical(x))

jstring <- function(x) if (is.character(x) && length(x) == 1L && !is.na(x)) x else NULL

# ============================================================================================
# Cells
# ============================================================================================
#
# A range from GridCraft becomes a "grid": its size and, per cell, a type (0 empty, 1 number,
# 2 text, 3 logical, 4 error), the number, and the text (or error code). Grids then become R
# values (scalar, matrix, data frame) and R values become JSON cells.

C_EMPTY <- 0L; C_NUM <- 1L; C_TEXT <- 2L; C_BOOL <- 3L; C_ERR <- 4L

new_grid <- function(nr, nc) {
  list(nrow = nr, ncol = nc, type = matrix(C_EMPTY, nr, nc), num = matrix(NA_real_, nr, nc),
       str = matrix(NA_character_, nr, nc))
}

# An argument or `context.values` (scalar, json_rows, or rows as lists) as a grid.
as_grid <- function(a) {
  if (inherits(a, "json_rows")) {
    nr <- a$nrow
    nc <- max(c(1L, a$rowlen))
    if (as.double(nr) * nc > MAX_CELLS) cell_error("#VALUE!")
    g <- new_grid(nr, nc)
    idx <- a$row + (a$col - 1L) * nr
    t <- a$type
    w <- t == T_NUM; g$type[idx[w]] <- C_NUM; g$num[idx[w]] <- a$num[w]
    w <- t == T_STR; g$type[idx[w]] <- C_TEXT; g$str[idx[w]] <- a$str[w]
    w <- t == T_TRUE; g$type[idx[w]] <- C_BOOL; g$num[idx[w]] <- 1
    w <- t == T_FALSE; g$type[idx[w]] <- C_BOOL; g$num[idx[w]] <- 0
    # Ragged rows: the missing cells are #N/A, as GridCraft pads its own.
    short <- which(a$rowlen < nc)
    for (r in short) {
      cols <- (a$rowlen[r] + 1L):nc
      g$type[r, cols] <- C_ERR
      g$str[r, cols] <- "#N/A"
    }
    return(g)
  }
  if (is.list(a) && is.null(names(a))) {
    rows <- if (length(a) && all(vapply(a, function(r) is.list(r) && is.null(names(r)), NA))) a else list(a)
    nr <- length(rows)
    if (nr == 0L) return(new_grid(1L, 1L))
    nc <- max(c(1L, lengths(rows)))
    if (as.double(nr) * nc > MAX_CELLS) cell_error("#VALUE!")
    g <- new_grid(nr, nc)
    for (r in seq_len(nr)) {
      row <- rows[[r]]
      for (cc in seq_len(nc)) {
        if (cc > length(row)) {
          g$type[r, cc] <- C_ERR; g$str[r, cc] <- "#N/A"
        } else {
          g <- set_cell(g, r, cc, row[[cc]])
        }
      }
    }
    return(g)
  }
  g <- new_grid(1L, 1L)
  set_cell(g, 1L, 1L, a)
}

set_cell <- function(g, r, cc, v) {
  if (is.list(v)) {
    code <- jstring(jget(v, "error"))
    g$type[r, cc] <- C_ERR
    g$str[r, cc] <- if (is.null(code)) "#VALUE!" else code
  } else if (is_null(v)) {
    g$type[r, cc] <- C_EMPTY
  } else if (is.logical(v)) {
    g$type[r, cc] <- C_BOOL; g$num[r, cc] <- as.numeric(v)
  } else if (is.numeric(v)) {
    g$type[r, cc] <- C_NUM; g$num[r, cc] <- v
  } else {
    g$type[r, cc] <- C_TEXT; g$str[r, cc] <- as.character(v)
  }
  g
}

# The first error in a grid, or NULL.
grid_error <- function(g) {
  w <- which(g$type == C_ERR)
  if (length(w)) g$str[w[1L]] else NULL
}

# R values of a vector of cells (types t, numbers num, texts str): numeric if every non-empty
# cell is a number, logical if every one is a logical, else character. Empty cells are NA.
cells_vector <- function(t, num, str) {
  filled <- t != C_EMPTY
  if (!any(filled)) return(rep(NA, length(t)))
  if (all(t[filled] == C_NUM)) {
    v <- num
    v[!filled] <- NA_real_
    return(v)
  }
  if (all(t[filled] == C_BOOL)) {
    v <- num == 1
    v[!filled] <- NA
    return(v)
  }
  v <- rep(NA_character_, length(t))
  w <- t == C_NUM; v[w] <- as.character(num[w])
  w <- t == C_TEXT | t == C_ERR; v[w] <- str[w]
  w <- t == C_BOOL; v[w] <- ifelse(num[w] == 1, "TRUE", "FALSE")
  v
}

# A grid as an R value: one cell is a scalar, more a matrix.
grid_value <- function(g) {
  v <- cells_vector(as.vector(g$type), as.vector(g$num), as.vector(g$str))
  if (g$nrow == 1L && g$ncol == 1L) return(v[1L])
  matrix(v, g$nrow, g$ncol)
}

# A grid as a data frame, its first row the column names.
grid_frame <- function(g) {
  nr <- g$nrow - 1L
  heads <- cells_vector(g$type[1L, ], g$num[1L, ], g$str[1L, ])
  heads <- as.character(heads)
  blank <- is.na(heads) | heads == ""
  heads[blank] <- paste0("V", seq_len(g$ncol))[blank]
  cols <- lapply(seq_len(g$ncol), function(j) {
    if (nr < 1L) return(logical(0))
    cells_vector(g$type[-1L, j], g$num[-1L, j], g$str[-1L, j])
  })
  names(cols) <- make.unique(heads)
  as.data.frame(cols, stringsAsFactors = FALSE, optional = TRUE)
}

# The numbers of a grid (column by column), as Excel's statistics functions see a range: text,
# logicals and empty cells are left out; errors are returned.
grid_numbers <- function(g) {
  e <- grid_error(g)
  if (!is.null(e)) cell_error(e)
  g$num[g$type == C_NUM]
}

# A grid as a numeric matrix: numbers kept, everything else NA.
grid_matrix <- function(g) {
  e <- grid_error(g)
  if (!is.null(e)) cell_error(e)
  m <- g$num
  m[g$type != C_NUM] <- NA_real_
  m
}

# --- R values to cells -----------------------------------------------------------------------

# Marks a date cell in "literal" mode (see write_commands); never the start of real JSON.
DATE_MARK <- "\001"

# JSON cells for a vector. `quote_text`: TRUE: commands write values as typed input, so text gets
# a leading apostrophe to stay text (dates don't: they become dates). "literal": GridCraft writes
# text as text (range.setValues literal, protocol 2.2); date cells are marked with DATE_MARK, to be
# written as typed input.
cell_json <- function(x, quote_text = FALSE) {
  date_text <- FALSE
  if (is.factor(x)) {
    x <- as.character(x)
  } else if (inherits(x, "Date")) {
    x <- format(x, "%Y-%m-%d"); date_text <- TRUE
  } else if (inherits(x, "POSIXt")) {
    x <- format(x, "%Y-%m-%d %H:%M:%S"); date_text <- TRUE
  } else if (inherits(x, "difftime")) {
    x <- as.numeric(x)
  }
  if (is.raw(x)) x <- as.integer(x)
  if (is.complex(x)) x <- as.character(x)
  if (is.list(x)) {
    return(vapply(x, function(e) {
      if (is.atomic(e) && length(e) == 1L) cell_json(e, quote_text) else ERR_VALUE
    }, "", USE.NAMES = FALSE))
  }
  if (is.character(x)) {
    na <- is.na(x)
    len <- nchar(x, type = "chars", allowNA = TRUE)
    long <- !na & !is.na(len) & len > MAX_TEXT
    if (any(long)) x[long] <- substr(x[long], 1L, MAX_TEXT)
    if (isTRUE(quote_text) && !date_text) x[!na] <- paste0("'", x[!na])
    out <- json_str(x)
    if (identical(quote_text, "literal") && date_text) out[!na] <- paste0(DATE_MARK, out[!na])
    return(out)
  }
  if (is.logical(x)) return(ifelse(is.na(x), "null", ifelse(x, "true", "false")))
  if (is.numeric(x)) {
    x <- as.double(x)
    out <- rep("null", length(x))
    ok <- is.finite(x)
    out[ok] <- json_num(x[ok])
    out[is.nan(x) | is.infinite(x)] <- ERR_NUM
    return(out)
  }
  rep(ERR_VALUE, length(x))
}

too_big <- function(nr, nc) as.double(nr) * as.double(nc) > MAX_CELLS

printed <- function(v) {
  lines <- utils::capture.output(print(v))
  if (length(lines) > 10000L) lines <- c(lines[1:10000], "...")
  matrix(cell_json(lines), ncol = 1L)
}

# An R value as a character matrix of JSON cells. See the README's "Data conversion".
value_cells <- function(v, quote_text = FALSE) {
  if (inherits(v, "POSIXlt")) v <- as.POSIXct(v)
  if (is.null(v) || (length(v) == 0L && !is.data.frame(v))) return(matrix("null", 1L, 1L))
  if (is.data.frame(v)) {
    nc <- ncol(v)
    if (nc == 0L) return(matrix("null", 1L, 1L))
    if (too_big(nrow(v) + 1, nc)) cell_error("#NUM!")
    heads <- cell_json(names(v), quote_text)
    body <- vapply(seq_len(nc), function(j) {
      col <- v[[j]]
      if (is.matrix(col) || is.data.frame(col)) return(rep(ERR_VALUE, nrow(v)))
      cell_json(col, quote_text)
    }, character(nrow(v)))
    body <- matrix(body, nrow(v), nc)
    return(rbind(heads, body, deparse.level = 0))
  }
  if (is.matrix(v) || (is.array(v) && length(dim(v)) == 2L)) {
    if (too_big(nrow(v), ncol(v))) cell_error("#NUM!")
    if (nrow(v) == 0L || ncol(v) == 0L) return(matrix("null", 1L, 1L))
    vals <- if (is.factor(v)) as.character(v) else unclass(v)
    if (is.list(vals)) {
      out <- matrix(vapply(seq_along(vals), function(i) {
        e <- vals[[i]]
        if (is.atomic(e) && length(e) == 1L) cell_json(e, quote_text) else ERR_VALUE
      }, ""), nrow(v), ncol(v))
      return(out)
    }
    return(matrix(cell_json(as.vector(vals), quote_text), nrow(v), ncol(v)))
  }
  if (is.array(v) && length(dim(v)) == 1L) v <- structure(as.vector(v), names = names(v) %||% dimnames(v)[[1L]])
  if (is.array(v)) return(printed(v))
  is_vectorish <- is.atomic(v) || (is.list(v) && !is.object(v) &&
                                     all(vapply(v, function(e) is.atomic(e) && length(e) <= 1L, NA)))
  if (is_vectorish && (!is.object(v) || is.factor(v) || inherits(v, c("Date", "POSIXct", "difftime", "table", "noquote")))) {
    if (is.list(v)) v <- lapply(v, function(e) if (length(e) == 0L) NA else e)
    n <- length(v)
    if (too_big(n, 2)) cell_error("#NUM!")
    vals <- cell_json(if (inherits(v, "noquote")) unclass(v) else v, quote_text)
    nm <- names(v)
    if (n > 1L && !is.null(nm) && any(nzchar(nm))) {
      return(cbind(cell_json(nm, quote_text), vals, deparse.level = 0))
    }
    return(matrix(vals, ncol = 1L))
  }
  # A model, a function, a list of vectors: what R prints, one line a cell.
  printed(v)
}

# A matrix of JSON cells as a function's value: one cell alone, else rows.
# GridCraft shows a null in a formula's result as 0 (as Excel does), so empty cells (NA, NULL) are
# its explicit blank where it has one (protocol 2.2's emptyResult), else empty text.
cells_value <- function(m) {
  m[m == "null"] <- if (has_feature("emptyResult")) "{\"empty\":true}" else "\"\""
  if (nrow(m) == 1L && ncol(m) == 1L) return(m[1L, 1L])
  cells_rows(m)
}

cells_rows <- function(m) {
  rows <- if (ncol(m) == 1L) m[, 1L] else do.call(paste, c(lapply(seq_len(ncol(m)), function(j) m[, j]), sep = ","))
  paste0("[", paste0("[", rows, "]", collapse = ","), "]")
}

# A table of labels and values (or more columns) for the statistics functions: a list of rows.
table_cells <- function(rows) {
  nc <- max(lengths(rows))
  m <- matrix("null", length(rows), nc)
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    for (j in seq_along(r)) m[i, j] <- cell_json(r[[j]])
  }
  m
}

# ============================================================================================
# Running R code
# ============================================================================================

# Runs `f()` with a time limit, its printed output captured (it must never reach GridCraft) and
# its warnings logged. Returns list(value, visible, output) or stops with the R error.
run_user <- function(f, seconds) {
  output <- character(0)
  res <- NULL
  con <- textConnection("output", "w", local = TRUE)
  sink(con)
  on.exit({
    while (sink.number() > 0L) try(sink(NULL), silent = TRUE)
    try(close(con), silent = TRUE)
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  }, add = TRUE)
  setTimeLimit(elapsed = seconds, transient = TRUE)
  res <- withCallingHandlers(
    withVisible(f()),
    warning = function(w) {
      log_msg("R warning: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      log_msg("R message: ", sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    })
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  while (sink.number() > 0L) sink(NULL)
  try(close(con), silent = TRUE)
  list(value = res$value, visible = res$visible, output = output)
}

r_message <- function(e) {
  msg <- conditionMessage(e)
  if (grepl("time limit", msg, fixed = TRUE)) msg <- "it took too long (the R code was stopped)"
  msg <- gsub("[\r\n]+", " ", msg)
  substr(msg, 1L, 1000L)
}

parse_code <- function(code) {
  exprs <- tryCatch(parse(text = code, keep.source = FALSE),
                    error = function(e) plugin_error(paste("R can't read the code:", r_message(e))))
  exprs
}

# A function of `...` whose body is `exprs`, with x, y, z (the first three arguments) and args
# (all of them) bound; ..1, ..2, ... reach every argument. Its environment's parent is the global
# environment: the R workspace, with every attached package.
code_function <- function(exprs, argv) {
  f <- function(...) NULL
  body(f) <- as.call(c(list(as.name("{")), as.list(exprs)))
  fm <- alist(... = )
  extra <- list(args = argv)
  nm <- c("x", "y", "z")
  for (i in seq_len(min(3L, length(argv)))) extra[[nm[i]]] <- argv[[i]]
  # Values, not expressions, as the defaults (a NULL value would drop the formal: bind it later).
  for (k in names(extra)) fm[k] <- list(if (is.null(extra[[k]])) quote(NULL) else extra[[k]])
  formals(f) <- fm
  environment(f) <- globalenv()
  f
}

# ============================================================================================
# Trust
# ============================================================================================
#
# R.EVAL, R.CALL and the commands that run R code work only in trusted workbooks. Trusted
# workbooks are a list of absolute paths in trusted.json in the add-in's data folder; a new
# workbook that was never saved (path null, called Book1, Book2...) is trusted while unsaved.
# Formulas don't say which workbook they're in, so functions use the workbook GridCraft last
# told us about (a command's context.document, or the documentOpened/Saved/Closed events); none
# yet means untrusted. See the README for what this can't see.

trust <- new.env()
trust$paths <- character(0)
trust$mtime <- NA
trust$session <- character(0) # names of unsaved workbooks trusted for this session (older hosts)
trust$ids <- character(0)     # unsaved workbooks trusted for this session: names, by session id

data_dir <- function() {
  d <- Sys.getenv("GRIDCRAFT_R_DATA")
  if (nzchar(d)) return(d)
  if (.Platform$OS.type == "windows") {
    a <- Sys.getenv("APPDATA")
    if (!nzchar(a)) a <- file.path(Sys.getenv("USERPROFILE"), "AppData", "Roaming")
    return(file.path(a, "gridcraft-r"))
  }
  sys <- tryCatch(Sys.info()[["sysname"]], error = function(e) "")
  if (identical(sys, "Darwin")) {
    return(file.path(path.expand("~"), "Library", "Application Support", "gridcraft-r"))
  }
  x <- Sys.getenv("XDG_DATA_HOME")
  if (!nzchar(x)) x <- file.path(path.expand("~"), ".local", "share")
  file.path(x, "gridcraft-r")
}

trust_file <- function() file.path(data_dir(), "trusted.json")

norm_path <- function(p) {
  p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") p <- tolower(p)
  p
}

trust_load <- function() {
  f <- trust_file()
  mt <- suppressWarnings(file.info(f)$mtime)
  if (is.na(mt)) {
    trust$paths <- character(0)
    trust$mtime <- NA
    return(invisible())
  }
  if (!is.na(trust$mtime) && identical(as.numeric(mt), as.numeric(trust$mtime))) return(invisible())
  text <- tryCatch(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                   error = function(e) "")
  data <- tryCatch(json_parse(text), error = function(e) NULL)
  books <- jget(data, "workbooks")
  paths <- if (is.list(books)) unlist(lapply(books, jstring)) else character(0)
  trust$paths <- unique(as.character(paths))
  trust$mtime <- mt
  invisible()
}

trust_save <- function(paths) {
  dir <- data_dir()
  if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
    plugin_error(paste("Can't create", dir))
  }
  f <- trust_file()
  tmp <- paste0(f, ".tmp")
  body <- paste0("{\"version\":1,\"workbooks\":", to_json(I(as.character(paths))), "}")
  ok <- tryCatch({
    con <- file(tmp, open = "wb")
    writeBin(charToRaw(enc2utf8(body)), con)
    writeBin(charToRaw("\n"), con)
    close(con)
    file.rename(tmp, f) || (file.copy(tmp, f, overwrite = TRUE) && file.remove(tmp))
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) plugin_error(paste("Can't write", f))
  trust$paths <- unique(as.character(paths))
  trust$mtime <- NA
  invisible()
}

trust_all <- function() identical(tolower(Sys.getenv("GRIDCRAFT_R_TRUST")), "all")

is_new_book <- function(name) !is.na(name) && grepl("^Book[0-9]+$", name)

# A workbook is trusted by its file; one without a file by its session id (GridCraft 2.2) or,
# on older hosts, its name. A new BookN that was never saved is trusted: its formulas are yours.
doc_trusted <- function(path, name, id = NA_character_) {
  if (trust_all()) return(TRUE)
  if (!is.na(path)) {
    trust_load()
    return(norm_path(path) %in% trust$paths)
  }
  if (!is.na(id)) return(is_new_book(name) || id %in% names(trust$ids))
  !is.na(name) && (is_new_book(name) || name %in% trust$session)
}

# A `document` object (context.document, functions/call, the events) as list(path, name, id).
doc_of <- function(d, fallback_name = NULL) {
  path <- jstring(jget(d, "path")) %||% NA_character_
  name <- jstring(jget(d, "name")) %||% fallback_name %||% (if (is.na(path)) NA_character_ else basename(path))
  id <- jstring(jget(d, "id")) %||% NA_character_
  list(path = path, name = name, id = id)
}

# Remembers what GridCraft said about a workbook with a session id.
note_doc <- function(doc) {
  if (!is.na(doc$id)) assign(doc$id, list(path = doc$path, name = doc$name), envir = docs)
}

# The workbook a command runs in, from context.document (NULL: the host didn't say).
context_doc <- function(ctx) {
  d <- jget(ctx, "document")
  if (!is.list(d)) return(NULL)
  doc_of(d, jstring(jget(ctx, "workbook")))
}

set_active <- function(path, name) {
  active$known <- TRUE
  active$path <- path
  active$name <- name
}

# R code in a formula runs when the formula's own workbook is trusted (GridCraft 2.2 says which
# one it is); older hosts leave us the workbook we last heard of.
functions_trusted <- function() {
  if (trust_all()) return(TRUE)
  if (has_feature("functionDocument")) return(calling$has && doc_trusted(calling$path, calling$name, calling$id))
  active$known && doc_trusted(active$path, active$name)
}

# Which workbook the log should name when R is off.
off_where <- function() {
  if (has_feature("functionDocument")) {
    return(if (calling$has) paste0(" (", calling$path %|na|% calling$name, ")") else " (no workbook)")
  }
  if (active$known) paste0(" (", active$path %|na|% active$name, ")") else " (no workbook known yet)"
}

OFF_MESSAGE <- "R is turned off for this workbook: R > Trust This Workbook"

# ============================================================================================
# Functions
# ============================================================================================

# The arguments of functions/call as a list (a list of rows read as a range is undone first).
call_args <- function(a) {
  if (is.null(a) || is_null(a)) return(list())
  if (inherits(a, "json_rows")) {
    # Every argument was a flat list: each is one row.
    return(lapply(seq_len(a$nrow), function(r) {
      w <- a$row == r
      structure(list(nrow = 1L, rowlen = a$rowlen[r], row = rep(1L, sum(w)), col = a$col[w],
                     type = a$type[w], num = a$num[w], str = a$str[w]), class = "json_rows")
    }))
  }
  if (is.list(a) && is.null(names(a))) return(a)
  list(a)
}

arg_count <- function(a) {
  if (inherits(a, "json_rows")) return(as.double(length(a$type)))
  if (is.list(a) && is.null(names(a))) return(as.double(sum(vapply(a, function(r) if (is.list(r)) length(r) else 1L, 1L))))
  1
}

# An argument as an R value; an error value in it is the function's answer.
arg_value <- function(a) {
  g <- as_grid(a)
  e <- grid_error(g)
  if (!is.null(e)) cell_error(e)
  grid_value(g)
}

# An optional scalar argument (NULL when left out or empty).
opt_scalar <- function(args, i) {
  if (length(args) < i) return(NULL)
  g <- as_grid(args[[i]])
  e <- grid_error(g)
  if (!is.null(e)) cell_error(e)
  if (g$type[1L] == C_EMPTY) return(NULL)
  v <- grid_value(g)
  v[1L]
}

opt_bool <- function(args, i, default) {
  v <- opt_scalar(args, i)
  if (is.null(v) || is.na(v)) return(default)
  if (is.logical(v)) return(v)
  if (is.numeric(v)) return(v != 0)
  s <- toupper(as.character(v))
  if (s %in% c("TRUE", "YES", "1")) return(TRUE)
  if (s %in% c("FALSE", "NO", "0")) return(FALSE)
  cell_error("#VALUE!")
}

opt_number <- function(args, i, default) {
  v <- opt_scalar(args, i)
  if (is.null(v) || is.na(v)) return(default)
  if (!is.numeric(v)) cell_error("#VALUE!")
  v
}

opt_text <- function(args, i, default) {
  v <- opt_scalar(args, i)
  if (is.null(v) || is.na(v)) return(default)
  as.character(v)
}

numbers_of <- function(a) grid_numbers(as_grid(a))

# Runs user code for a function: errors become a failed call, which GridCraft logs and shows
# as #VALUE! (not cached, so fixing the cause and recalculating works).
user_value <- function(f) {
  res <- tryCatch(run_user(f, FN_TIME_LIMIT),
                  plugin_error = function(e) stop(e),
                  cell_error = function(e) stop(e),
                  error = function(e) plugin_error(paste("R error:", r_message(e))))
  if (length(res$output)) log_msg("R printed: ", paste(utils::head(res$output, 20L), collapse = " | "))
  cells_value(value_cells(res$value))
}

require_trust <- function() {
  if (!functions_trusted()) {
    log_msg(OFF_MESSAGE, off_where())
    plugin_error(OFF_MESSAGE)
  }
}

`%|na|%` <- function(a, b) if (is.na(a)) b else a

fn_eval <- function(args) {
  code <- arg_value(args[[1L]])
  if (!is.character(code) || length(code) != 1L || is.na(code)) cell_error("#VALUE!")
  require_trust()
  argv <- lapply(args[-1L], arg_value)
  exprs <- parse_code(code)
  f <- code_function(exprs, argv)
  user_value(function() do.call(f, unname(argv)))
}

FN_NAME <- "^([A-Za-z.][A-Za-z0-9._]*::)?[A-Za-z.][A-Za-z0-9._]*$"

find_function <- function(fname) {
  if (!grepl(FN_NAME, fname)) plugin_error(paste0("`", substr(fname, 1L, 80L), "` isn't the name of an R function"))
  parts <- strsplit(fname, "::", fixed = TRUE)[[1L]]
  f <- tryCatch({
    if (length(parts) == 2L) getExportedValue(parts[1L], parts[2L])
    else get(fname, envir = globalenv(), mode = "function")
  }, error = function(e) NULL)
  if (!is.function(f)) plugin_error(paste0("There's no R function called ", fname))
  f
}

fn_call <- function(args) {
  fname <- arg_value(args[[1L]])
  if (!is.character(fname) || length(fname) != 1L || is.na(fname)) cell_error("#VALUE!")
  require_trust()
  argv <- lapply(args[-1L], arg_value)
  f <- find_function(fname)
  user_value(function() do.call(f, unname(argv)))
}

fn_version <- function(args) {
  json_str(paste0("GridCraft R ", PLUGIN_VERSION, " on R ", R.version$major, ".", R.version$minor))
}

stat_value <- function(rows) cells_value(table_cells(rows))

fn_lm <- function(args) {
  yg <- as_grid(args[[1L]])
  xg <- as_grid(args[[2L]])
  const <- opt_bool(args, 3L, TRUE)
  ym <- grid_matrix(yg)
  if (nrow(ym) != 1L && ncol(ym) != 1L) cell_error("#VALUE!")
  y <- as.vector(ym)
  n <- length(y)
  xm <- grid_matrix(xg)
  if (nrow(xm) != n) {
    if (ncol(xm) == n) xm <- t(xm) else cell_error("#REF!")
  }
  k <- ncol(xm)
  colnames(xm) <- paste0("x", seq_len(k))
  df <- data.frame(y = y, xm)
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) < k + as.integer(const) + 1L) cell_error("#NUM!")
  fit <- stats::lm(if (const) y ~ . else y ~ . - 1, data = df)
  s <- summary(fit)
  co <- s$coefficients
  rows <- list(list("term", "estimate", "std. error", "t value", "p value"))
  for (i in seq_len(nrow(co))) {
    rows[[length(rows) + 1L]] <- list(rownames(co)[i], co[i, 1L], co[i, 2L], co[i, 3L], co[i, 4L])
  }
  fstat <- s$fstatistic
  fp <- if (is.null(fstat)) NA else stats::pf(fstat[1L], fstat[2L], fstat[3L], lower.tail = FALSE)
  rows <- c(rows, list(
    list(NA),
    list("R squared", s$r.squared),
    list("adjusted R squared", s$adj.r.squared),
    list("residual std. error", s$sigma),
    list("F statistic", if (is.null(fstat)) NA else unname(fstat[1L])),
    list("F p value", unname(fp)),
    list("observations", nrow(df)),
    list("residual df", fit$df.residual)))
  stat_value(rows)
}

fn_quantile <- function(args) {
  x <- numbers_of(args[[1L]])
  if (!length(x)) cell_error("#NUM!")
  probs <- if (length(args) >= 2L && !is_null(args[[2L]])) numbers_of(args[[2L]]) else c(0, 0.25, 0.5, 0.75, 1)
  if (!length(probs) || any(probs < 0 | probs > 1)) cell_error("#NUM!")
  type <- opt_number(args, 3L, 7)
  if (!(type %in% 1:9)) cell_error("#NUM!")
  q <- stats::quantile(x, probs, type = type, names = FALSE)
  if (length(probs) == 1L) return(cell_json(q))
  rows <- c(list(list("probability", "quantile")), lapply(seq_along(q), function(i) list(probs[i], q[i])))
  stat_value(rows)
}

frame_of <- function(a, headers) {
  g <- as_grid(a)
  e <- grid_error(g)
  if (!is.null(e)) cell_error(e)
  if (headers) {
    if (g$nrow < 2L) cell_error("#VALUE!")
    heads <- as.character(cells_vector(g$type[1L, ], g$num[1L, ], g$str[1L, ]))
    heads[is.na(heads) | heads == ""] <- paste0("x", seq_len(g$ncol))[is.na(heads) | heads == ""]
    g <- list(nrow = g$nrow - 1L, ncol = g$ncol, type = g$type[-1L, , drop = FALSE],
              num = g$num[-1L, , drop = FALSE], str = g$str[-1L, , drop = FALSE])
  } else {
    heads <- paste0("x", seq_len(g$ncol))
  }
  list(grid = g, names = heads)
}

fn_corr <- function(args) {
  method <- tolower(opt_text(args, 2L, "pearson"))
  if (!(method %in% c("pearson", "spearman", "kendall"))) cell_error("#VALUE!")
  fr <- frame_of(args[[1L]], opt_bool(args, 3L, FALSE))
  m <- grid_matrix(fr$grid)
  if (nrow(m) < 2L) cell_error("#DIV/0!")
  r <- suppressWarnings(stats::cor(m, use = "pairwise.complete.obs", method = method))
  rows <- c(list(c(list(NA), as.list(fr$names))),
            lapply(seq_len(ncol(m)), function(i) c(list(fr$names[i]), as.list(r[i, ]))))
  stat_value(rows)
}

fn_ttest <- function(args) {
  x <- numbers_of(args[[1L]])
  y <- if (length(args) >= 2L && !is_null(args[[2L]])) numbers_of(args[[2L]]) else NULL
  paired <- opt_bool(args, 3L, FALSE)
  mu <- opt_number(args, 4L, 0)
  alt <- tolower(opt_text(args, 5L, "two.sided"))
  alt <- switch(alt, "two.sided" = , "two-sided" = , "two sided" = , "both" = "two.sided",
                "less" = "less", "greater" = "greater", cell_error("#VALUE!"))
  if (length(x) < 2L || (!is.null(y) && length(y) < 2L)) cell_error("#DIV/0!")
  if (paired && (is.null(y) || length(x) != length(y))) cell_error("#N/A")
  tt <- tryCatch(stats::t.test(x, y, paired = paired, mu = mu, alternative = alt),
                 error = function(e) cell_error("#NUM!"))
  rows <- list(
    list("t", unname(tt$statistic)),
    list("df", unname(tt$parameter)),
    list("p value", tt$p.value),
    list("conf. low (95%)", tt$conf.int[1L]),
    list("conf. high (95%)", tt$conf.int[2L]))
  est <- tt$estimate
  for (i in seq_along(est)) rows[[length(rows) + 1L]] <- list(names(est)[i], unname(est[i]))
  rows[[length(rows) + 1L]] <- list("method", gsub("^ +| +$", "", tt$method))
  rows[[length(rows) + 1L]] <- list("alternative", alt)
  stat_value(rows)
}

fn_summary <- function(args) {
  fr <- frame_of(args[[1L]], opt_bool(args, 2L, FALSE))
  g <- fr$grid
  labels <- c("n", "missing", "min", "1st quartile", "median", "mean", "3rd quartile", "max", "std. dev.")
  cols <- lapply(seq_len(g$ncol), function(j) {
    t <- g$type[, j]
    x <- g$num[t == C_NUM, j]
    filled <- sum(t != C_EMPTY)
    out <- list(filled, sum(t == C_EMPTY))
    if (length(x)) {
      q <- stats::quantile(x, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
      out <- c(out, list(q[1L], q[2L], q[3L], mean(x), q[4L], q[5L], if (length(x) > 1L) stats::sd(x) else NA))
    } else {
      out <- c(out, rep(list(NA), 7L))
    }
    out
  })
  rows <- c(list(c(list(NA), as.list(fr$names))),
            lapply(seq_along(labels), function(i) c(list(labels[i]), lapply(cols, function(cc) cc[[i]]))))
  stat_value(rows)
}

FUNCTIONS <- list(
  "R.EVAL" = fn_eval,
  "R.CALL" = fn_call,
  "R.VERSION" = fn_version,
  "R.LM" = fn_lm,
  "R.QUANTILE" = fn_quantile,
  "R.CORR" = fn_corr,
  "R.TTEST" = fn_ttest,
  "R.SUMMARY" = fn_summary
)

call_function <- function(params) {
  name <- toupper(jstring(jget(params, "name")) %||% "")
  if (!(name %in% names(FUNCTIONS))) return(rpc_error(-32601L, paste("unknown function", name)))
  fn <- FUNCTIONS[[name]]
  d <- jget(params, "document")
  calling$has <- is.list(d) && !is.null(names(d))
  doc <- if (calling$has) doc_of(d) else list(path = NA_character_, name = NA_character_, id = NA_character_)
  calling$path <- doc$path
  calling$name <- doc$name
  calling$id <- doc$id
  if (calling$has) note_doc(doc)
  args <- call_args(jget(params, "args"))
  total <- sum(vapply(args, arg_count, 1))
  if (total > MAX_CELLS) return(json_raw(paste0("{\"value\":", ERR_VALUE, "}")))
  value <- tryCatch(fn(args),
                    cell_error = function(e) paste0("{\"error\":", json_str(conditionMessage(e)), "}"))
  json_raw(paste0("{\"value\":", value, "}"))
}

# ============================================================================================
# Commands
# ============================================================================================

has_feature <- function(f) f %in% host$features

# A1 references.
col_number <- function(letters) {
  n <- 0
  for (ch in strsplit(toupper(letters), "")[[1L]]) n <- n * 26 + (utf8ToInt(ch) - 64L)
  n
}
col_letters <- function(n) {
  s <- ""
  while (n > 0) {
    r <- (n - 1) %% 26
    s <- paste0(intToUtf8(65 + r), s)
    n <- (n - 1) %/% 26
  }
  s
}

# The selection's corners: list(c1, r1, c2, r2), or NULL.
parse_range <- function(ref) {
  if (is.null(ref)) return(NULL)
  ref <- sub("^.*!", "", ref)
  m <- regmatches(ref, regexec("^\\$?([A-Za-z]{1,3})\\$?([0-9]+)(:\\$?([A-Za-z]{1,3})\\$?([0-9]+))?$", ref))[[1L]]
  if (length(m) == 0L) return(NULL)
  c1 <- col_number(m[2L]); r1 <- as.numeric(m[3L])
  if (nzchar(m[4L])) {
    c2 <- col_number(m[5L]); r2 <- as.numeric(m[6L])
  } else {
    c2 <- c1; r2 <- r1
  }
  list(c1 = min(c1, c2), r1 = min(r1, r2), c2 = max(c1, c2), r2 = max(r1, r2))
}

fields_of <- function(params) {
  f <- jget(params, "fields")
  if (is.list(f) && !is.null(names(f))) f else params
}

field_text <- function(fields, key, default = "") {
  v <- jget(fields, key)
  if (is.character(v) && length(v) == 1L && !is.na(v)) v else default
}

field_bool <- function(fields, key, default = FALSE) {
  v <- jget(fields, key)
  if (is.logical(v) && length(v) == 1L && !is.na(v)) v else default
}

field_number <- function(fields, key, default) {
  v <- jget(fields, key)
  if (is.numeric(v) && length(v) == 1L && is.finite(v)) v else default
}

message_dialog <- function(title, prompt, command = NULL, params = NULL, buttons = NULL, ok = NULL) {
  d <- list(title = title, kind = "message", prompt = clip(prompt, MAX_PROMPT))
  if (!is.null(command)) d$command <- command
  if (!is.null(params)) d$params <- params
  if (!is.null(ok)) d$okLabel <- ok
  if (!is.null(buttons)) d$buttons <- buttons
  d
}

clip <- function(text, n) {
  if (nchar(text, allowNA = TRUE) %|na|% 0L > n) paste0(substr(text, 1L, n - 1L), "…") else text
}

# Refuses a command in a workbook that isn't trusted (offering to trust it when dialogs work).
check_command_trust <- function(ctx) {
  doc <- context_doc(ctx)
  if (trust_all()) return(NULL)
  if (is.null(doc)) {
    plugin_error("This GridCraft doesn't say which workbook is open, so R can't check that it's trusted. Update GridCraft (0.7 or later).")
  }
  if (doc_trusted(doc$path, doc$name, doc$id)) return(NULL)
  text <- paste0("R is turned off for ", doc$name %|na|% "this workbook", ": its R code could run on your computer. ",
                 "Trust the workbook first (R > Trust This Workbook).")
  if (has_feature("dialogs")) {
    return(list(dialog = message_dialog("R is turned off", text, command = "trust", ok = "Trust This Workbook...")))
  }
  plugin_error(text)
}

values_x <- function(ctx, header) {
  vals <- jget(ctx, "values")
  if (is.null(vals)) return(NULL)
  g <- as_grid(vals)
  if (header && g$nrow >= 1L) grid_frame(g) else grid_value(g)
}

elapsed_text <- function(t0) sprintf("%.2f s", as.numeric(proc.time()[["elapsed"]] - t0))

# The commands that write `cells` (a matrix of JSON cells) where `where` says. `literal`: the cells
# come from cell_json's "literal" mode: text is written as text (range.setValues literal), and the
# date cells (marked) are written again as typed input, so they become dates.
write_commands <- function(cells, where, ctx, bold_header, literal = FALSE) {
  nr <- nrow(cells); nc <- ncol(cells)
  dates <- matrix(FALSE, nr, nc)
  if (literal) {
    dates <- matrix(startsWith(cells, DATE_MARK), nr, nc)
    cells[dates] <- substring(cells[dates], 2L)
  }
  values <- json_raw(cells_rows(cells))
  set_values <- function(range) {
    p <- list(range = range, values = values)
    if (literal) p$literal <- TRUE
    list(command = "range.setValues", params = p)
  }
  sel <- parse_range(jstring(jget(ctx, "selection"))) %||% parse_range(jstring(jget(ctx, "active")))
  if (identical(where, "sheet") || is.null(sel)) {
    taken <- vapply(jget(ctx, "sheets") %||% list(), function(s) jstring(jget(s, "name")) %||% "", "")
    name <- "R Output"
    i <- 1L
    while (tolower(name) %in% tolower(taken)) {
      i <- i + 1L
      name <- paste("R Output", i)
    }
    cmds <- list(list(command = "home.insertSheet", params = list(name = name)), set_values("A1"))
    at <- paste0("'", name, "'!A1")
    first <- list(c1 = 1, r1 = 1)
  } else {
    first <- list(c1 = sel$c1, r1 = sel$r2 + 2)
    at <- paste0(col_letters(first$c1), first$r1)
    cmds <- list(set_values(at))
  }
  if (first$r1 + nr - 1 > MAX_ROWS || first$c1 + nc - 1 > MAX_COLS) {
    plugin_error(sprintf("The result (%d x %d) doesn't fit on the sheet from %s.", nr, nc, at))
  }
  for (j in which(colSums(dates) > 0)) {
    runs <- rle(dates[, j])
    ends <- cumsum(runs$lengths)
    for (k in which(runs$values)) {
      i2 <- ends[k]; i1 <- i2 - runs$lengths[k] + 1L
      cmds[[length(cmds) + 1L]] <- list(command = "range.setValues", params = list(
        range = paste0(col_letters(first$c1 + j - 1), first$r1 + i1 - 1),
        values = json_raw(cells_rows(cells[i1:i2, j, drop = FALSE]))))
    }
  }
  if (bold_header) {
    hdr <- paste0(col_letters(first$c1), first$r1, ":", col_letters(first$c1 + nc - 1), first$r1)
    cmds[[length(cmds) + 1L]] <- list(command = "home.bold", params = list(range = hdr))
  }
  list(commands = cmds, at = at)
}

script_form <- function() {
  list(dialog = list(
    title = "Run R Script",
    kind = "form",
    prompt = paste0("x holds the selection's values. The value of the last expression is written to the sheet.",
                    if (has_feature("multiline")) "" else " Separate statements with ;"),
    fields = list(
      list(id = "code", label = "R code", value = last$script, multiline = TRUE, code = TRUE, rows = 12L),
      list(id = "file", label = "...or an R file to run", value = ""),
      list(id = "header", label = "The first row holds column names (x is a data frame)", kind = "checkbox", value = FALSE),
      list(id = "output", label = "Write the result", kind = "choice", value = "below",
           options = list(list(id = "below", label = "Below the selection"),
                          list(id = "sheet", label = "On a new sheet")))),
    command = "runScript",
    okLabel = "Run"))
}

cmd_run_script <- function(params, ctx) {
  fields <- fields_of(params)
  code <- field_text(fields, "code")
  file <- field_text(fields, "file")
  if (!nzchar(trimws(code)) && !nzchar(trimws(file))) {
    if (has_feature("dialogs")) return(script_form())
    plugin_error("Give the R code to run: {\"code\": \"...\"}")
  }
  refused <- check_command_trust(ctx)
  if (!is.null(refused)) return(refused)
  if (nzchar(trimws(code))) last$script <- code
  header <- field_bool(fields, "header")
  where <- field_text(fields, "output", "below")
  x <- values_x(ctx, header)
  t0 <- proc.time()[["elapsed"]]
  f <- if (nzchar(trimws(code))) {
    code_function(parse_code(code), if (is.null(x)) list() else list(x))
  } else {
    path <- path.expand(trimws(file))
    if (!file.exists(path)) plugin_error(paste("There's no file", path))
    local_x <- x
    function(...) {
      env <- new.env(parent = globalenv())
      env$x <- local_x
      source(path, local = env, encoding = "UTF-8")$value
    }
  }
  res <- tryCatch(run_user(function() if (is.null(x)) f() else f(x), CMD_TIME_LIMIT),
                  plugin_error = function(e) stop(e),
                  error = function(e) plugin_error(paste("R error:", r_message(e))))
  took <- elapsed_text(t0)
  out <- list()
  if (is.null(res$value) || (!res$visible && is.null(res$value))) {
    out$message <- paste0("R ran in ", took, " (no result to write)")
  } else {
    literal <- has_feature("literalValues")
    cells <- tryCatch(value_cells(res$value, quote_text = if (literal) "literal" else TRUE),
                      cell_error = function(e) plugin_error("The result is too big for the sheet (at most 1,000,000 cells)."))
    w <- write_commands(cells, where, ctx, is.data.frame(res$value), literal)
    out$commands <- w$commands
    out$message <- sprintf("R ran in %s and wrote %d x %d cells at %s", took, nrow(cells), ncol(cells), w$at)
  }
  if (length(res$output) && has_feature("dialogs")) {
    out$dialog <- message_dialog("R output", paste(res$output, collapse = "\n"))
  }
  out$result <- list(output = I(res$output))
  out
}

# --- plot ------------------------------------------------------------------------------------

b64_chars <- c(LETTERS, letters, 0:9, "+", "/")

base64 <- function(r) {
  n <- length(r)
  if (n == 0L) return("")
  pad <- (3L - n %% 3L) %% 3L
  v <- c(as.integer(r), integer(pad))
  m <- matrix(v, nrow = 3L)
  n24 <- m[1L, ] * 65536L + m[2L, ] * 256L + m[3L, ]
  idx <- rbind(n24 %/% 262144L, (n24 %/% 4096L) %% 64L, (n24 %/% 64L) %% 64L, n24 %% 64L) + 1L
  ch <- b64_chars[idx]
  if (pad) ch[(length(ch) - pad + 1L):length(ch)] <- "="
  paste0(ch, collapse = "")
}

open_png <- function(file, width, height) {
  if (!isTRUE(unname(capabilities("png"))) && !isTRUE(unname(capabilities("cairo")))) {
    plugin_error("This R can't draw PNG pictures (capabilities(\"png\") is FALSE). Install an R built with cairo or libpng.")
  }
  types <- if (.Platform$OS.type == "windows") c("windows", "cairo")
           else if (isTRUE(unname(capabilities("cairo")))) c("cairo", "quartz", "Xlib")
           else c("quartz", "Xlib")
  for (type in types) {
    ok <- tryCatch({
      suppressWarnings(grDevices::png(file, width = width, height = height, type = type))
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(grDevices::dev.cur())
  }
  plugin_error("R couldn't open a PNG device (no cairo, quartz or X11 here).")
}

plot_form <- function() {
  list(dialog = list(
    title = "R Plot",
    kind = "form",
    prompt = "Base R graphics; x holds the selection's values. The picture goes next to the selection.",
    fields = list(
      list(id = "code", label = "R plotting code", value = last$plot, multiline = TRUE, code = TRUE, rows = 8L),
      list(id = "header", label = "The first row holds column names (x is a data frame)", kind = "checkbox", value = FALSE),
      list(id = "width", label = "Width (pixels)", kind = "number", value = 640),
      list(id = "height", label = "Height (pixels)", kind = "number", value = 480)),
    command = "plot",
    okLabel = "Plot"))
}

cmd_plot <- function(params, ctx) {
  fields <- fields_of(params)
  code <- field_text(fields, "code")
  if (!nzchar(trimws(code))) {
    if (has_feature("dialogs")) return(plot_form())
    plugin_error("Give the R plotting code: {\"code\": \"plot(x)\"}")
  }
  refused <- check_command_trust(ctx)
  if (!is.null(refused)) return(refused)
  last$plot <- code
  width <- round(min(max(field_number(fields, "width", 640), 50), 4000))
  height <- round(min(max(field_number(fields, "height", 480), 50), 4000))
  x <- values_x(ctx, field_bool(fields, "header"))
  f <- code_function(parse_code(code), if (is.null(x)) list() else list(x))
  file <- tempfile("gridcraft-r-", fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  t0 <- proc.time()[["elapsed"]]
  before <- grDevices::dev.list()
  dev <- open_png(file, width, height)
  res <- tryCatch(run_user(function() if (is.null(x)) f() else f(x), CMD_TIME_LIMIT),
                  plugin_error = function(e) e,
                  error = function(e) simpleError(paste("R error:", r_message(e))))
  # Close what the code opened, and ours.
  for (d in setdiff(grDevices::dev.list(), before)) try(grDevices::dev.off(d), silent = TRUE)
  if (inherits(res, "error")) plugin_error(conditionMessage(res))
  size <- suppressWarnings(file.info(file)$size)
  if (is.na(size) || size == 0) plugin_error("The R code drew nothing.")
  if (size > 48 * 1024 * 1024) plugin_error("The picture is too big.")
  bytes <- readBin(file, "raw", size)
  sel <- parse_range(jstring(jget(ctx, "selection"))) %||% parse_range(jstring(jget(ctx, "active")))
  at <- if (is.null(sel)) "A1" else paste0(col_letters(min(sel$c2 + 1, MAX_COLS)), sel$r1)
  list(
    commands = list(list(command = "insert.picture",
                         params = list(base64 = json_raw(json_str(base64(bytes))), mime = "image/png",
                                       at = at, alt = clip(paste("R plot:", code), 200L)))),
    message = sprintf("R drew a %d x %d picture in %s", width, height, elapsed_text(t0)))
}

# --- console ---------------------------------------------------------------------------------

console_form <- function() {
  list(dialog = list(
    title = "R Console",
    kind = "form",
    prompt = "Runs in R's workspace: what you define stays for R.EVAL, R.CALL and the next run. x holds the selection's values.",
    fields = list(list(id = "code", label = "R", value = last$console, multiline = TRUE, code = TRUE, rows = 6L)),
    command = "console",
    okLabel = "Run"))
}

cmd_console <- function(params, ctx) {
  fields <- fields_of(params)
  code <- field_text(fields, "code")
  if (!nzchar(trimws(code))) {
    if (has_feature("dialogs")) return(console_form())
    plugin_error("Give the R code to run: {\"code\": \"...\"}")
  }
  refused <- check_command_trust(ctx)
  if (!is.null(refused)) return(refused)
  last$console <- code
  x <- values_x(ctx, FALSE)
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) e)
  text <- if (inherits(exprs, "error")) {
    paste("Error:", r_message(exprs))
  } else {
    res <- tryCatch(run_user(function() {
      env <- globalenv()
      had_x <- exists("x", envir = env, inherits = FALSE)
      old_x <- if (had_x) get("x", envir = env) else NULL
      if (!is.null(x)) assign("x", x, envir = env)
      on.exit(if (!is.null(x)) {
        # x is the selection only while the code runs, unless the code set x itself.
        cur <- if (exists("x", envir = env, inherits = FALSE)) get("x", envir = env) else NULL
        if (identical(cur, x)) {
          if (had_x) assign("x", old_x, envir = env) else rm("x", envir = env)
        }
      })
      v <- NULL
      vis <- FALSE
      for (e in exprs) {
        r <- withVisible(eval(e, envir = env))
        v <- r$value
        vis <- r$visible
        if (vis) print(v)
      }
      invisible(v)
    }, CMD_TIME_LIMIT), error = function(e) e)
    if (inherits(res, "error")) paste("Error:", r_message(res))
    else if (length(res$output)) paste(res$output, collapse = "\n")
    else "(no output)"
  }
  shown <- paste0("> ", code, "\n", text)
  if (!has_feature("dialogs")) {
    return(list(message = clip(gsub("\n", " ", text), 200L), result = list(output = text)))
  }
  list(dialog = message_dialog("R Console", shown, command = "console", ok = "Again...",
                               params = structure(list(), names = character(0))),
       result = list(output = text))
}

# --- trust commands --------------------------------------------------------------------------

need_doc <- function(ctx) {
  doc <- context_doc(ctx)
  if (is.null(doc)) {
    plugin_error("This GridCraft doesn't say which workbook is open, so R can't trust it. Update GridCraft (0.7 or later).")
  }
  doc
}

RISK <- paste0(
  "R formulas (R.EVAL, R.CALL) and R scripts run R code when the workbook recalculates. ",
  "R code can do anything you can: read, change or delete your files, send them over the internet, ",
  "start programs. A workbook made by someone else can carry such formulas.\n\n",
  "Trust a workbook only if you made it, or you trust whoever did and know what its R code does.")

# Asks GridCraft something while we answer it (protocol 1.1 host requests; protocol 2.2's
# host/functions/invalidate also any time): sends the request and reads standard input until its
# answer comes. Other messages that arrive meanwhile are kept for the main loop. NULL when there's
# no GridCraft to ask (tests calling the functions directly) or it went away.
host_request <- function(method, params) {
  if (is.null(io$con)) return(NULL)
  io$next_id <- io$next_id + 1L
  id <- paste0("r", io$next_id)
  send(paste0("{\"jsonrpc\":\"2.0\",\"id\":", json_str(id), ",\"method\":", json_str(method), ",\"params\":", to_json(params), "}"))
  repeat {
    line <- tryCatch(readLines(io$con, n = 1L, warn = FALSE, encoding = "UTF-8", skipNul = TRUE),
                     error = function(e) character(0))
    if (length(line) == 0L) return(NULL)
    if (!grepl("[^ \t\r]", line, useBytes = TRUE)) next
    msg <- if (nchar(line, type = "bytes") > MAX_LINE) NULL else tryCatch(json_parse(line), error = function(e) NULL)
    if (is.list(msg) && !is.null(names(msg)) && is.null(jget(msg, "method")) && identical(jstring(jget(msg, "id")), id)) {
      return(msg)
    }
    # A request or notification of GridCraft's: the main loop handles it after this answer.
    if (is.list(msg) && !is.null(jget(msg, "method"))) io$queue <- c(io$queue, line)
  }
}

# The functions whose results depend on trust.
CODE_FUNCTIONS <- c("R.EVAL", "R.CALL")

# Trust changed: GridCraft forgets R.EVAL's and R.CALL's results and recalculates the formulas
# that use them (protocol 2.2). TRUE when it did; older hosts recalculate with Calculate Now.
invalidate_code <- function() {
  if (!has_feature("invalidate")) return(FALSE)
  a <- host_request("host/functions/invalidate", list(names = I(CODE_FUNCTIONS)))
  ok <- is.list(a) && !is.null(jget(a, "result"))
  if (!ok) log_msg("GridCraft didn't recalculate the R formulas: ", jstring(jget(jget(a, "error"), "message")) %||% "no answer")
  ok
}

CALC_NOW <- list(command = "formulas.calculateNow", params = structure(list(), names = character(0)))

# A result that brings the R formulas up to date after trust changed.
recalculated <- function(out) {
  if (!invalidate_code()) out$commands <- list(CALC_NOW)
  out
}

cmd_trust <- function(params, ctx) {
  doc <- need_doc(ctx)
  label <- doc$name %|na|% "this workbook"
  if (doc_trusted(doc$path, doc$name, doc$id)) {
    text <- if (is.na(doc$path) && is_new_book(doc$name)) {
      paste0(label, " is new and unsaved: R works in it", if (is.na(doc$id)) " until you save it. Then trust the saved file." else ", and stays on in the file you save it to.")
    } else {
      paste0("R is already on for ", label, ".")
    }
    return(recalculated(list(message = text)))
  }
  where <- if (is.na(doc$path)) {
    paste0("\n\n", label, " isn't saved: it will be trusted until GridCraft closes",
           if (is.na(doc$id)) "." else ", and so will the file you save it to.")
  } else {
    paste0("\n\nThe workbook: ", doc$path)
  }
  confirm <- list(path = doc$path, name = doc$name)
  if (!is.na(doc$id)) confirm$id <- doc$id
  if (!has_feature("dialogs")) return(cmd_trust_confirm(confirm, ctx))
  list(dialog = message_dialog(paste0("Trust ", label, "?"), paste0(RISK, where),
                               command = "trustConfirm", params = confirm, ok = "Trust"))
}

cmd_trust_confirm <- function(params, ctx) {
  doc <- need_doc(ctx)
  want <- jstring(jget(params, "path")) %||% NA_character_
  want_id <- jstring(jget(params, "id")) %||% NA_character_
  same <- if (!is.na(want)) {
    !is.na(doc$path) && norm_path(want) == norm_path(doc$path)
  } else if (!is.na(want_id) || !is.na(doc$id)) {
    is.na(doc$path) && identical(want_id, doc$id)
  } else {
    is.na(doc$path) && identical(jstring(jget(params, "name")), doc$name)
  }
  if (!same) plugin_error("The active workbook changed: run R > Trust This Workbook again.")
  if (!is.na(doc$path)) {
    trust_load()
    trust_save(unique(c(trust$paths, norm_path(doc$path))))
  } else if (!is.na(doc$id)) {
    trust$ids[doc$id] <- doc$name %|na|% doc$id
  } else {
    trust$session <- unique(c(trust$session, doc$name))
  }
  set_active(doc$path, doc$name)
  recalculated(list(message = paste0("R is on for ", doc$name %|na|% "this workbook")))
}

cmd_untrust <- function(params, ctx) {
  doc <- need_doc(ctx)
  label <- doc$name %|na|% "this workbook"
  if (is.na(doc$path)) {
    if (!is.na(doc$id) && doc$id %in% names(trust$ids)) {
      trust$ids <- trust$ids[names(trust$ids) != doc$id]
      return(recalculated_off(list(message = paste0("R is off for ", label))))
    }
    if (is.na(doc$id) && doc$name %in% trust$session) {
      trust$session <- setdiff(trust$session, doc$name)
      return(recalculated_off(list(message = paste0("R is off for ", label))))
    }
    if (is_new_book(doc$name)) {
      return(list(message = paste0(label, " is new and unsaved: R stays on in a new workbook you write")))
    }
    return(list(message = paste0("R was already off for ", label)))
  }
  trust_load()
  p <- norm_path(doc$path)
  if (!(p %in% trust$paths)) return(list(message = paste0("R was already off for ", label)))
  trust_save(setdiff(trust$paths, p))
  recalculated_off(list(message = paste0("R is off for ", label)))
}

# After R was turned off: GridCraft recalculates the R formulas (they show #VALUE!), where it can.
recalculated_off <- function(out) {
  if (!invalidate_code()) out$message <- paste0(out$message, " (results already calculated stay until their cells change)")
  out
}

trusted_dialog <- function(note = NULL) {
  trust_load()
  paths <- trust$paths
  if (!length(paths) && !length(trust$session) && !length(trust$ids)) {
    return(list(message = note %||% "No workbook is trusted",
                dialog = message_dialog("Trusted Workbooks", paste0(
                  "No workbook is trusted yet. R > Trust This Workbook turns R on for the active workbook.\n\n",
                  "The list is kept in ", trust_file()))))
  }
  unsaved <- "Unsaved: until GridCraft closes"
  items <- c(lapply(paths, function(p) list(id = p, label = basename(p), detail = dirname(p), icon = "workbook")),
             lapply(names(trust$ids), function(i) list(id = paste0("id:", i), label = trust$ids[[i]], detail = unsaved, icon = "workbook")),
             lapply(trust$session, function(n) list(id = paste0("session:", n), label = n, detail = unsaved, icon = "workbook")))
  out <- list(dialog = list(title = "Trusted Workbooks", kind = "list",
                            prompt = paste("R runs in these workbooks. Select one or more to take them off the list. Kept in", trust_file()),
                            items = items, multi = TRUE, command = "untrustPaths", okLabel = "Remove"),
              result = list(workbooks = I(paths), session = I(c(unname(trust$ids), trust$session))))
  if (!is.null(note)) out$message <- note
  out
}

cmd_trusted <- function(params, ctx) {
  if (!has_feature("dialogs")) {
    trust_load()
    return(list(message = sprintf("%d trusted workbook(s)", length(trust$paths)),
                result = list(workbooks = I(trust$paths), session = I(c(unname(trust$ids), trust$session)), file = trust_file())))
  }
  trusted_dialog()
}

cmd_untrust_paths <- function(params, ctx) {
  ids <- jget(params, "ids")
  ids <- if (is.list(ids)) unlist(lapply(ids, jstring)) else character(0)
  one <- jstring(jget(params, "id"))
  if (!is.null(one)) ids <- c(ids, one)
  if (!length(ids)) plugin_error("Select the workbooks to take off the list.")
  sess <- sub("^session:", "", ids[startsWith(ids, "session:")])
  sids <- sub("^id:", "", ids[startsWith(ids, "id:")])
  files <- ids[!startsWith(ids, "session:") & !startsWith(ids, "id:")]
  trust$session <- setdiff(trust$session, sess)
  trust$ids <- trust$ids[!(names(trust$ids) %in% sids)]
  trust_load()
  if (length(files)) trust_save(setdiff(trust$paths, norm_path(files)))
  invalidate_code()
  note <- sprintf("Took %d workbook(s) off the list", length(ids))
  if (!has_feature("dialogs")) return(list(message = note))
  trusted_dialog(note)
}

# --- about -----------------------------------------------------------------------------------

cmd_about <- function(params, ctx) {
  doc <- context_doc(ctx)
  pkgs <- tryCatch(sort(unique(rownames(utils::installed.packages()))), error = function(e) character(0))
  state <- if (is.null(doc)) "unknown (this GridCraft doesn't say which workbook is open)"
           else if (doc_trusted(doc$path, doc$name, doc$id)) paste0("on for ", doc$name %|na|% "this workbook")
           else paste0("off for ", doc$name %|na|% "this workbook", " (R > Trust This Workbook)")
  if (trust_all()) state <- paste(state, "- GRIDCRAFT_R_TRUST=all trusts every workbook")
  png_ok <- isTRUE(unname(capabilities("png"))) || isTRUE(unname(capabilities("cairo")))
  shown_pkgs <- if (length(pkgs) > 80L) c(utils::head(pkgs, 80L), sprintf("and %d more", length(pkgs) - 80L)) else pkgs
  text <- paste0(
    "GridCraft R ", PLUGIN_VERSION, "\n",
    R.version.string, " (", R.version$platform, ")\n",
    "R_HOME: ", R.home(), "\n",
    "Libraries: ", paste(.libPaths(), collapse = "; "), "\n",
    "PNG plots: ", if (png_ok) "yes" else "no", "\n",
    "R in this workbook: ", state, "\n",
    "Trusted workbooks: ", trust_file(), "\n\n",
    length(pkgs), " packages: ", paste(shown_pkgs, collapse = ", "))
  result <- list(plugin = PLUGIN_VERSION, r = R.version.string, rHome = R.home(),
                 packages = I(pkgs), trusted = !is.null(doc) && doc_trusted(doc$path, doc$name, doc$id),
                 trustFile = trust_file())
  if (!has_feature("dialogs")) return(list(message = paste("GridCraft R", PLUGIN_VERSION, "on", R.version.string), result = result))
  list(dialog = message_dialog("About GridCraft R", text), result = result)
}

COMMANDS <- list(
  runScript = cmd_run_script,
  plot = cmd_plot,
  console = cmd_console,
  trust = cmd_trust,
  trustConfirm = cmd_trust_confirm,
  untrust = cmd_untrust,
  trusted = cmd_trusted,
  untrustPaths = cmd_untrust_paths,
  about = cmd_about
)

run_command <- function(params) {
  id <- jstring(jget(params, "command")) %||% ""
  if (!(id %in% names(COMMANDS))) return(rpc_error(-32601L, paste("unknown command", id)))
  cmd <- COMMANDS[[id]]
  ctx <- jget(params, "context")
  if (!is.list(ctx)) ctx <- list()
  doc <- context_doc(ctx)
  if (!is.null(doc)) {
    set_active(doc$path, doc$name)
    note_doc(doc)
  }
  p <- jget(params, "params")
  if (!is.list(p)) p <- list()
  json_raw(to_json(cmd(p, ctx)))
}

# ============================================================================================
# Events (protocol 1.2): which workbook is active; a saved workbook keeps its trust (2.2)
# ============================================================================================

on_event <- function(event, params) {
  doc <- doc_of(params)
  path <- doc$path
  name <- doc$name
  if (event == "documentOpened") {
    set_active(path, name)
    note_doc(doc)
    # GridCraft before 2.2 calculated the workbook before telling us it opened, and doesn't say
    # which workbook a formula is in: its R formulas ran as "unknown workbook". Say how to fix it.
    if (!has_feature("functionDocument") && !has_feature("openedBeforeCalc") && !trust_all() && doc_trusted(path, name)) {
      return(json_raw(to_json(list(message = paste0("R: ", name, " is trusted; Formulas > Calculate Now (F9) runs its R formulas")))))
    }
    return(json_raw("{}"))
  }
  if (event == "documentSaved") {
    # The same workbook (session id) under a file: a trusted workbook stays trusted in it.
    before <- if (!is.na(doc$id) && exists(doc$id, envir = docs, inherits = FALSE)) get(doc$id, envir = docs) else NULL
    was_new <- active$known && is.na(active$path) && is_new_book(active$name)
    set_active(path, name)
    if (!is.null(before)) {
      was <- doc_trusted(before$path, before$name, doc$id)
      note_doc(doc)
      if (was && !is.na(path) && !trust_all() && !doc_trusted(path, name, doc$id)) {
        trust_load()
        trust_save(unique(c(trust$paths, norm_path(path))))
        return(json_raw(to_json(list(message = paste0("R: ", name, " stays trusted (it's the workbook you trusted)")))))
      }
      return(json_raw("{}"))
    }
    note_doc(doc)
    if (was_new && !doc_trusted(path, name, doc$id)) {
      return(json_raw(to_json(list(message = paste0("R: ", name, " is saved; R > Trust This Workbook keeps its R formulas working")))))
    }
    return(json_raw("{}"))
  }
  if (event == "documentClosed") {
    if (!is.na(doc$id) && exists(doc$id, envir = docs, inherits = FALSE)) rm(list = doc$id, envir = docs)
    if (active$known && !is.na(active$path) && !is.na(path) && norm_path(active$path) == norm_path(path)) {
      active$known <- FALSE
      active$path <- NA_character_
      active$name <- NA_character_
    }
    return(json_raw("{}"))
  }
  json_raw("{}")
}

# ============================================================================================
# Protocol
# ============================================================================================

rpc_error <- function(code, message) structure(list(code = code, message = message), class = "rpc_error")

initialize <- function(params) {
  h <- jget(params, "host")
  host$id <- tolower(jstring(jget(h, "id")) %||% jstring(jget(h, "name")) %||% "gridcraft")
  host$version <- jstring(jget(h, "version")) %||% ""
  f <- jget(h, "features")
  host$features <- if (is.list(f)) unlist(lapply(f, jstring)) %||% character(0) else character(0)
  json_raw("{\"protocolVersion\":1}")
}

dispatch <- function(method, params) {
  if (method == "initialize") return(initialize(params))
  if (method == "functions/call") return(call_function(params))
  if (method == "commands/run") return(run_command(params))
  if (startsWith(method, "events/")) return(on_event(sub("^events/", "", method), params))
  rpc_error(-32601L, paste("unknown method", method))
}

send <- function(text) {
  while (sink.number() > 0L) try(sink(NULL), silent = TRUE)
  cat(text, "\n", sep = "", file = stdout())
  flush(stdout())
}

reply_error <- function(id_json, code, message) {
  send(paste0("{\"jsonrpc\":\"2.0\",\"id\":", id_json, ",\"error\":{\"code\":", code,
              ",\"message\":", json_str(substr(message, 1L, 4000L)), "}}"))
}

# Handles one line; FALSE when it's time to stop.
handle_line <- function(line) {
  msg <- tryCatch(json_parse(line), error = function(e) e)
  if (inherits(msg, "error")) {
    log_msg("ignored a line that isn't JSON (", conditionMessage(msg), "): ", substr(line, 1L, 200L))
    return(TRUE)
  }
  if (!is.list(msg) || is.null(names(msg))) {
    log_msg("ignored a message that isn't an object")
    return(TRUE)
  }
  method <- jstring(jget(msg, "method"))
  id <- jget(msg, "id")
  has_id <- !is.null(id) && !is.list(id) && !is_null(id)
  if (!has_id) {
    if (identical(method, "shutdown") || identical(method, "exit")) return(FALSE)
    return(TRUE) # other notifications, and answers we never asked for
  }
  id_json <- to_json(id)
  if (is.null(method)) {
    if (!is.null(jget(msg, "result")) || !is.null(jget(msg, "error"))) return(TRUE)
    reply_error(id_json, -32600L, "no method")
    return(TRUE)
  }
  params <- jget(msg, "params")
  if (!is.list(params)) params <- structure(list(), names = character(0))
  result <- tryCatch(
    dispatch(method, params),
    plugin_error = function(e) rpc_error(1L, conditionMessage(e)),
    cell_error = function(e) rpc_error(1L, conditionMessage(e)),
    error = function(e) {
      log_msg("error in ", method, ": ", conditionMessage(e))
      rpc_error(-32603L, paste("internal error:", r_message(e)))
    },
    interrupt = function(e) rpc_error(-32603L, "interrupted"))
  if (inherits(result, "rpc_error")) {
    if (result$code == 1L) log_msg(method, ": ", result$message)
    reply_error(id_json, result$code, result$message)
  } else {
    text <- tryCatch(paste0("{\"jsonrpc\":\"2.0\",\"id\":", id_json, ",\"result\":", to_json(result), "}"),
                     error = function(e) NULL)
    if (is.null(text)) reply_error(id_json, -32603L, "internal error: the answer couldn't be written")
    else send(text)
  }
  TRUE
}

main <- function() {
  ascii_out <<- !isTRUE(l10n_info()[["UTF-8"]]) || nzchar(Sys.getenv("GRIDCRAFT_R_ASCII"))
  options(warn = 1, device = function(...) grDevices::pdf(file = NULL))
  # R code that calls quit() would stop the add-in mid-answer.
  assign("quit", function(...) stop("quit() is turned off in GridCraft R"), envir = globalenv())
  assign("q", function(...) stop("q() is turned off in GridCraft R"), envir = globalenv())
  con <- file("stdin", open = "r")
  io$con <- con
  repeat {
    line <- if (length(io$queue)) {
      first <- io$queue[1L]
      io$queue <- io$queue[-1L]
      first
    } else {
      tryCatch(readLines(con, n = 1L, warn = FALSE, encoding = "UTF-8", skipNul = TRUE),
               error = function(e) character(0))
    }
    if (length(line) == 0L) break
    if (!grepl("[^ \t\r]", line, useBytes = TRUE)) next
    if (nchar(line, type = "bytes") > MAX_LINE) {
      log_msg("ignored a message of ", nchar(line, type = "bytes"), " bytes")
      next
    }
    go <- tryCatch(handle_line(line), error = function(e) {
      log_msg("error: ", conditionMessage(e))
      TRUE
    }, interrupt = function(e) TRUE)
    if (!isTRUE(go)) break
  }
  invisible()
}

environment()
})

