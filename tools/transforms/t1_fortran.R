# Copyright 2026 Balasubramanian Narasimhan. GPL (>= 2); see inst/COPYRIGHTS.
#
# T1: Fortran I/O transform.
#
# Removes every statement that would make the compiled library write to
# stdout/stderr, touch files, stop the process or use the Fortran RNG, and
# fails loudly on anything it does not understand. The rules are:
#
#   WRITE/PRINT to an external unit  -> commented out; IOSTAT=v becomes v = 0
#   STOP, ERROR STOP                 -> CALL REXIT('MUMPS internal error in R')
#                                       (R is the enclosing routine)
#   INQUIRE(..., OPENED=x/EXIST=x)   -> x = .FALSE.
#   CALL RANDOM_NUMBER(x)            -> CALL RMUMPS_RANDOM_NUMBER(x), only at
#                                       sites listed in the manifest (x must
#                                       be a DOUBLE PRECISION scalar)
#   OPEN/CLOSE/READ/REWIND/BACKSPACE/ENDFILE/FLUSH and internal WRITE
#                                    -> commented out, only inside routines
#                                       listed in the manifest as IO-disabled
#                                       (a patch makes them unreachable);
#                                       IOSTAT=v becomes v = 1 for OPEN/READ
#                                       and v = 0 otherwise
#   anything else that is I/O         -> error
#
# A logical IF keeps its condition on the replacement statement. Commented
# lines get "C~" in columns 1-2, so line numbers are kept except where a
# replacement statement is inserted. Statements inside regions removed by
# the preprocessor are commented as well when they are WRITE/PRINT/STOP
# (harmless), and left alone otherwise.

T1_IO_RE <- paste0(
  "^(WRITE\\(|PRINT(\\*|'|\"|[0-9]|[A-Z_][A-Z0-9_]*,)|READ\\(|READ(\\*|'|\"|[0-9])|",
  "OPEN\\(|CLOSE\\(|INQUIRE\\(|FLUSH(\\(|[0-9A-Z_]+$)|CALLFLUSH\\(|",
  "(BACKSPACE|REWIND|ENDFILE)(\\(|[0-9A-Z_]+$)|STOP($|'|\"|[0-9])|ERRORSTOP|",
  "CALLRANDOM_(NUMBER|SEED)\\(|CALLEXIT\\b|CALLABORT\\b|CALLSYSTEM\\()")

t1_kind <- function(action) {
  if (!grepl(T1_IO_RE, action, perl = TRUE)) return("")
  k <- regmatches(action, regexpr(
    "^(WRITE|PRINT|READ|OPEN|CLOSE|INQUIRE|FLUSH|CALLFLUSH|BACKSPACE|REWIND|ENDFILE|ERRORSTOP|STOP|CALLRANDOM_NUMBER|CALLRANDOM_SEED|CALLEXIT|CALLABORT|CALLSYSTEM)",
    action))
  k
}

# Control-list items of an I/O statement whose squeezed action starts with
# `kw(`. Returns a named character vector (positional items are named
# "UNIT" and "FMT").
t1_control <- function(action, kw) {
  b <- substr(action, nchar(kw) + 1L, nchar(action))
  if (!startsWith(b, "(")) return(character())
  e <- ff_match_paren(b, 1L)
  items <- ff_split_top(substr(b, 2L, e - 1L))
  nm <- ifelse(grepl("^[A-Z]+=", items) & !grepl("^[A-Z]+==", items),
               sub("=.*$", "", items), "")
  val <- ifelse(nzchar(nm), sub("^[A-Z]+=", "", items), items)
  pos <- which(!nzchar(nm))
  if (length(pos) >= 1L) nm[pos[1L]] <- "UNIT"
  if (length(pos) >= 2L) nm[pos[2L]] <- "FMT"
  stats::setNames(val, nm)
}

# Condition text of a logical IF, taken from the raw text so that character
# literals survive.
t1_raw_cond <- function(st) {
  m <- st$masked
  lead <- regexpr("[^[:space:]]", m)
  # position of "(" after IF, allowing blanks between IF and "("
  rest <- substr(m, lead, nchar(m))
  p <- regexpr("\\(", rest)
  open <- lead + p - 1L
  close <- ff_match_paren(m, open)
  substr(st$raw, open + 1L, close - 1L)
}

# Fixed-form continuation of a statement text into lines of at most 72
# columns, breaking only outside character literals.
t1_emit <- function(text, sentinel = FALSE) {
  lead <- if (sentinel) "!$    " else "      "
  out <- character()
  first <- TRUE
  repeat {
    room <- 66L
    if (nchar(text) <= room) {
      out <- c(out, paste0(if (first) lead else paste0(substr(lead, 1, 5), "&"), text))
      break
    }
    inlit <- t1_in_literal(text)
    ch <- strsplit(text, "", fixed = TRUE)[[1]]
    word <- grepl("[A-Za-z0-9_.]", ch)
    cut <- room
    # break between cut and cut + 1: never inside a literal, never inside a
    # name or a dotted operator
    while (cut > 1L && (inlit[cut] || inlit[cut + 1L] || (word[cut] && word[cut + 1L])))
      cut <- cut - 1L
    # fixed form ignores line boundaries outside literals, so any position
    # outside a literal will do when there is no clean one
    if (cut <= 1L) {
      cut <- room
      while (cut > 1L && (inlit[cut] || inlit[cut + 1L])) cut <- cut - 1L
    }
    if (cut <= 1L) stop("cannot split statement: ", text, call. = FALSE)
    out <- c(out, paste0(if (first) lead else paste0(substr(lead, 1, 5), "&"),
                         substr(text, 1L, cut)))
    text <- substr(text, cut + 1L, nchar(text))
    first <- FALSE
  }
  out
}

# TRUE for each character of `text` that belongs to a character literal,
# quotes included.
t1_in_literal <- function(text) {
  ch <- strsplit(text, "", fixed = TRUE)[[1]]
  res <- logical(length(ch))
  q <- ""
  i <- 1L
  while (i <= length(ch)) {
    c <- ch[i]
    if (nzchar(q)) {
      res[i] <- TRUE
      if (c == q) {
        if (i < length(ch) && ch[i + 1L] == q) { res[i + 1L] <- TRUE; i <- i + 2L; next }
        q <- ""
      }
    } else if (c == "'" || c == "\"") {
      q <- c; res[i] <- TRUE
    }
    i <- i + 1L
  }
  res
}

t1_comment_lines <- function(lines, idx) {
  for (i in idx) {
    l <- lines[i]
    p <- substr(l, 1L, 2L)
    # columns 1-5 hold blanks or a label (which moves to the replacement)
    if (!(grepl("^[ 0-9]{1,5}", l) || grepl("^[!cC*]\\$", p) || nchar(l) < 2L))
      stop("cannot comment out line: ", l, call. = FALSE)
    lines[i] <- paste0("C~", substr(l, 3L, nchar(l)))
  }
  lines
}

# Transform one file. `file` is the file name used in messages and manifest
# keys; `man` is the parsed manifest (see vendor_manifest()).
t1_transform <- function(lines, file, man) {
  st <- ff_statements(lines)
  st$unit <- ff_units(st$squeezed)
  act <- ff_active(lines, man$defines, man$unknown_defines)
  cl <- ff_classify(lines)
  iscode <- cl$type %in% c("code", "cont")
  inserts <- list()
  log <- character()
  err <- character()
  fail <- function(i, msg)
    err <<- c(err, sprintf("%s:%d (%s): %s: %s", file, st$first[i], st$unit[i],
                           msg, substr(st$squeezed[i], 1L, 80L)))
  key <- function(i) paste(file, st$unit[i])
  for (i in seq_len(nrow(st))) {
    sq <- st$squeezed[i]
    ci <- ff_strip_if(sq)
    action <- ci[["action"]]
    kind <- t1_kind(action)
    if (!nzchar(kind)) next
    state <- act[st$first[i]]
    disabled <- key(i) %in% man$io_disabled || paste(file, "*") %in% man$io_disabled
    if (state == "off" && !(kind %in% c("WRITE", "PRINT", "STOP", "ERRORSTOP"))) next
    if (grepl(";", st$masked[i], fixed = TRUE)) { fail(i, "';' in I/O statement"); next }
    cond <- if (nzchar(ci[["cond"]])) t1_raw_cond(st[i, ]) else ""
    if (nzchar(cond) && grepl("['\"]", cond) && ff_mask(cond)$state != "")
      { fail(i, "condition with a continued character literal"); next }
    wrap <- function(stmt) if (nzchar(cond)) paste0("IF (", cond, ") ", stmt) else stmt
    repl <- character()
    if (kind %in% c("WRITE", "PRINT")) {
      if (kind == "WRITE") {
        ctl <- t1_control(action, "WRITE")
        u <- ctl[["UNIT"]]
        internal <- !(u %in% man$units)
        if (internal && !disabled && state != "off") {
          fail(i, paste0("WRITE to unknown unit '", u, "' (add it to Units or IO-Disabled)"))
          next
        }
        if ("IOSTAT" %in% names(ctl)) repl <- wrap(paste0(ctl[["IOSTAT"]], " = 0"))
        if (any(c("ERR", "END", "EOR") %in% names(ctl)) && state != "off")
          log <- c(log, sprintf("%s:%d: ERR=/END= on deleted WRITE", file, st$first[i]))
      }
      tag <- "write"
    } else if (kind %in% c("STOP", "ERRORSTOP")) {
      r <- if (nzchar(st$unit[i])) st$unit[i] else file
      repl <- wrap(paste0("CALL REXIT('MUMPS internal error in '//'", r, "')"))
      tag <- "stop"
    } else if (kind == "INQUIRE") {
      ctl <- t1_control(action, "INQUIRE")
      v <- ctl[names(ctl) %in% c("OPENED", "EXIST")]
      other <- setdiff(names(ctl), c("UNIT", "FILE", "OPENED", "EXIST", "IOSTAT"))
      if (!length(v) || length(other)) { fail(i, "unsupported INQUIRE"); next }
      repl <- vapply(v, function(x) wrap(paste0(x, " = .FALSE.")), "")
      if ("IOSTAT" %in% names(ctl)) repl <- c(repl, wrap(paste0(ctl[["IOSTAT"]], " = 0")))
      tag <- "inquire"
    } else if (kind == "CALLRANDOM_NUMBER") {
      if (!(key(i) %in% man$rng)) { fail(i, "RANDOM_NUMBER outside the RNG allowlist"); next }
      arg <- sub("^CALLRANDOM_NUMBER\\((.*)\\)$", "\\1", action)
      if (!grepl("^[A-Z_][A-Z0-9_]*$", arg)) { fail(i, "RANDOM_NUMBER argument is not a scalar name"); next }
      repl <- wrap(paste0("CALL RMUMPS_RANDOM_NUMBER(", arg, ")"))
      tag <- "rng"
    } else if (disabled) {
      kw <- switch(kind, CALLFLUSH = "CALLFLUSH", kind)
      ctl <- if (kind %in% c("OPEN", "CLOSE", "READ", "FLUSH", "CALLFLUSH", "REWIND", "BACKSPACE", "ENDFILE"))
        t1_control(action, kw) else character()
      if ("IOSTAT" %in% names(ctl))
        repl <- wrap(paste0(ctl[["IOSTAT"]], if (kind %in% c("OPEN", "READ")) " = 1" else " = 0"))
      tag <- "disabled"
    } else {
      fail(i, paste(kind, "outside an IO-disabled routine"))
      next
    }
    lab <- st$label[i]
    # a labeled statement may be a branch target or a DO terminator: the
    # label moves to the first replacement statement, or to a CONTINUE
    if (nzchar(lab) && !length(repl)) repl <- "CONTINUE"
    if (st$sentinel[i] && length(repl)) { fail(i, "replacement needed in an OpenMP sentinel line"); next }
    idx <- st$first[i]:st$last[i]
    lines <- t1_comment_lines(lines, idx[iscode[idx]])
    if (length(repl)) {
      em <- unlist(lapply(repl, t1_emit))
      if (nzchar(lab)) {
        if (nchar(lab) > 5L || !grepl("^[0-9]+$", lab)) { fail(i, "bad label"); next }
        substr(em[1L], 1L, 5L) <- sprintf("%-5s", lab)
      }
      inserts[[as.character(st$last[i])]] <- em
    }
    log <- c(log, sprintf("%s %s", tag, key(i)))
  }
  if (length(err)) stop(paste(c("T1 failed:", err), collapse = "\n  "), call. = FALSE)
  if (length(inserts)) {
    at <- sort(as.integer(names(inserts)), decreasing = TRUE)
    for (a in at) lines <- append(lines, inserts[[as.character(a)]], after = a)
  }
  list(lines = lines, log = log)
}
