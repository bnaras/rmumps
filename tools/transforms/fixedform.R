# Fixed-form Fortran statement scanner used by the T1 transform.
#
# A source file is split into physical lines, each classified as one of
#   "blank", "comment", "directive" (cpp line), "omp" (!$OMP-style directive,
#   a comment unless OpenMP is enabled), "code" (initial line), "cont"
#   (continuation line).
# Conditional-compilation sentinel lines (!$ in columns 1-2 followed by
# blanks or digits) are code lines with sentinel = TRUE.
# Code lines are then grouped into statements. Comment, blank and directive
# lines between an initial line and its continuations belong to the
# statement's line range but contribute no text.

ff_classify <- function(lines) {
  n <- length(lines)
  type <- character(n)
  sentinel <- logical(n)
  for (i in seq_len(n)) {
    l <- lines[i]
    c1 <- substr(l, 1, 1)
    if (grepl("^[[:space:]]*$", l)) {
      type[i] <- "blank"
    } else if (c1 == "#") {
      type[i] <- "directive"
    } else if (grepl("^[!cC*]\\$[ 0-9]{3}", l) || grepl("^[!cC*]\\$[ 0-9]{0,3}$", l)) {
      # conditional compilation sentinel (OpenMP); code when compiled with
      # OpenMP, comment otherwise
      sentinel[i] <- TRUE
      type[i] <- if (ff_is_cont(l)) "cont" else "code"
    } else if (grepl("^[!cC*]\\$", l)) {
      type[i] <- "omp"
    } else if (c1 %in% c("c", "C", "*", "!")) {
      type[i] <- "comment"
    } else if (grepl("^[[:space:]]*!", l)) {
      type[i] <- "comment"
    } else {
      if (grepl("\t", substr(l, 1, 6), fixed = TRUE))
        stop("tab in columns 1-6 is not supported: ", l, call. = FALSE)
      type[i] <- if (ff_is_cont(l)) "cont" else "code"
    }
  }
  list(type = type, sentinel = sentinel)
}

ff_is_cont <- function(l) {
  c6 <- substr(l, 6, 6)
  nchar(l) >= 6 && !(c6 %in% c(" ", "0"))
}

# Text of a code line between columns 7 and 72.
ff_body <- function(l) substr(l, 7, 72)

ff_label <- function(l) trimws(substr(sub("^[!cC*]\\$", "  ", l), 1, 5))

# Mask character literals and drop trailing ! comments. Returns the body with
# every character inside a literal replaced by "_" (quotes kept), so that
# keyword and delimiter searches never look inside strings. `state` carries an
# open literal across continuation lines ("" when none is open).
ff_mask <- function(body, state = "") {
  ch <- strsplit(body, "", fixed = TRUE)[[1]]
  out <- ch
  q <- state
  i <- 1L
  n <- length(ch)
  cut <- n
  while (i <= n) {
    c <- ch[i]
    if (nzchar(q)) {
      if (c == q) {
        if (i < n && ch[i + 1L] == q) {
          out[i] <- "_"; out[i + 1L] <- "_"; i <- i + 2L; next
        }
        q <- ""
      } else {
        out[i] <- "_"
      }
    } else if (c == "'" || c == "\"") {
      q <- c
    } else if (c == "!") {
      cut <- i - 1L
      break
    }
    i <- i + 1L
  }
  # the comment part is blanked rather than cut, so that the masked text is
  # aligned character by character with the raw text
  if (cut < n) out[(cut + 1L):n] <- " "
  list(text = paste(out, collapse = ""), state = q, comment = cut < n)
}

# Group lines into statements. Returns a data frame with one row per
# statement: first and last line, label, whether any directive or sentinel
# line is involved, the masked and squeezed uppercase text, and the raw
# concatenated body (columns 7-72 of each code line, pasted together; it is
# aligned character by character with `masked`). `open` is TRUE when a
# character literal is still open at the end of some line of the statement.
ff_statements <- function(lines) {
  cl <- ff_classify(lines)
  type <- cl$type
  n <- length(lines)
  starts <- which(type == "code")
  K <- length(starts)
  first <- starts
  last <- integer(K)
  label <- character(K)
  directive <- logical(K)
  sentinel <- logical(K)
  squeezed <- character(K)
  masked <- character(K)
  raw <- character(K)
  open <- logical(K)
  nxt <- c(starts[-1L], n + 1L)
  iscode <- type %in% c("code", "cont")
  for (k in seq_len(K)) {
    s <- starts[k]
    rng <- s:(nxt[k] - 1L)
    code <- rng[iscode[rng]]
    e <- code[length(code)]
    rng <- s:e
    state <- ""
    mk_txt <- character(length(code))
    for (m in seq_along(code)) {
      mk <- ff_mask(ff_body(lines[code[m]]), state)
      state <- mk$state
      if (nzchar(state)) open[k] <- TRUE
      mk_txt[m] <- mk$text
    }
    last[k] <- e
    label[k] <- ff_label(lines[s])
    directive[k] <- any(type[rng] == "directive")
    sentinel[k] <- any(cl$sentinel[code])
    masked[k] <- paste(mk_txt, collapse = "")
    raw[k] <- paste(ff_body(lines[code]), collapse = "")
  }
  squeezed <- toupper(gsub("[[:space:]]+", "", masked))
  data.frame(first = first, last = last, label = label,
             directive = directive, sentinel = sentinel,
             squeezed = squeezed, masked = masked, raw = raw, open = open,
             stringsAsFactors = FALSE)
}

# Preprocessor state of each line given the set of defined macro names.
# Returns "on", "off" or "unknown" per line (directive lines themselves are
# reported with the state of their enclosing region). Conditions understood:
# #if/#elif with defined(X), !, &&, || and parentheses; #ifdef; #ifndef.
# Anything else makes the region "unknown".
ff_active <- function(lines, defines, unknown = character()) {
  n <- length(lines)
  state <- character(n)
  # stack entries: list(parent, taken, cur)
  stack <- list()
  cur <- "on"
  for (i in seq_len(n)) {
    l <- lines[i]
    if (!startsWith(l, "#")) { state[i] <- cur; next }
    d <- sub("^#[[:space:]]*", "", l)
    d <- sub("/\\*.*$", "", d)
    kw <- sub("^([a-z]+).*", "\\1", d)
    arg <- trimws(sub("^[a-z]+", "", d))
    state[i] <- cur
    if (kw %in% c("if", "ifdef", "ifndef")) {
      v <- switch(kw,
                  ifdef = if (arg %in% unknown) "unknown" else if (arg %in% defines) "on" else "off",
                  ifndef = if (arg %in% unknown) "unknown" else if (arg %in% defines) "off" else "on",
                  "if" = ff_eval_cond(arg, defines, unknown))
      parent <- cur
      stack[[length(stack) + 1L]] <- list(parent = parent, taken = v)
      cur <- ff_and(parent, v)
    } else if (kw == "elif") {
      top <- stack[[length(stack)]]
      v <- ff_eval_cond(arg, defines, unknown)
      eff <- ff_and(ff_not(top$taken), v)
      stack[[length(stack)]]$taken <- ff_or(top$taken, v)
      cur <- ff_and(top$parent, eff)
    } else if (kw == "else") {
      top <- stack[[length(stack)]]
      cur <- ff_and(top$parent, ff_not(top$taken))
      stack[[length(stack)]]$taken <- "on"
    } else if (kw == "endif") {
      top <- stack[[length(stack)]]
      stack[[length(stack)]] <- NULL
      cur <- top$parent
    }
  }
  if (length(stack)) stop("unbalanced #if", call. = FALSE)
  state
}

ff_and <- function(a, b) if (a == "off" || b == "off") "off" else if (a == "on" && b == "on") "on" else "unknown"
ff_or <- function(a, b) if (a == "on" || b == "on") "on" else if (a == "off" && b == "off") "off" else "unknown"
ff_not <- function(a) switch(a, on = "off", off = "on", "unknown")

ff_eval_cond <- function(expr, defines, unknown = character()) {
  e <- gsub("defined[[:space:]]*\\(?[[:space:]]*([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\)?",
            "<\\1>", expr)
  toks <- regmatches(e, gregexpr("<[A-Za-z0-9_]+>|&&|\\|\\||!|\\(|\\)|[^[:space:]]", e))[[1]]
  pos <- 1L
  peek <- function() if (pos <= length(toks)) toks[pos] else ""
  bad <- FALSE
  p_or <- function() {
    v <- p_and()
    while (peek() == "||") { pos <<- pos + 1L; v <- ff_or(v, p_and()) }
    v
  }
  p_and <- function() {
    v <- p_un()
    while (peek() == "&&") { pos <<- pos + 1L; v <- ff_and(v, p_un()) }
    v
  }
  p_un <- function() {
    t <- peek()
    if (t == "!") { pos <<- pos + 1L; return(ff_not(p_un())) }
    if (t == "(") {
      pos <<- pos + 1L; v <- p_or()
      if (peek() == ")") pos <<- pos + 1L else bad <<- TRUE
      return(v)
    }
    if (grepl("^<.*>$", t)) {
      pos <<- pos + 1L
      nm <- gsub("[<>]", "", t)
      return(if (nm %in% unknown) "unknown" else if (nm %in% defines) "on" else "off")
    }
    bad <<- TRUE
    pos <<- pos + 1L
    "unknown"
  }
  v <- p_or()
  if (bad || pos <= length(toks)) "unknown" else v
}

# Index of the parenthesis matching the one at position `open` in `s`
# (a masked string), or NA.
ff_match_paren <- function(s, open) {
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  depth <- 0L
  for (i in seq.int(open, length(ch))) {
    if (ch[i] == "(") depth <- depth + 1L
    else if (ch[i] == ")") {
      depth <- depth - 1L
      if (depth == 0L) return(i)
    }
  }
  NA_integer_
}

# Split a squeezed statement into the condition of a logical IF (or "") and
# the action statement. "IF(c)THEN" is not a logical IF and is returned
# whole.
ff_strip_if <- function(sq) {
  if (!startsWith(sq, "IF(")) return(c(cond = "", action = sq))
  e <- ff_match_paren(sq, 3L)
  if (is.na(e)) return(c(cond = "", action = sq))
  rest <- substr(sq, e + 1L, nchar(sq))
  if (rest == "THEN" || rest == "" || grepl("^[0-9]+,[0-9]+,[0-9]+$", rest))
    return(c(cond = "", action = sq))
  c(cond = substr(sq, 4L, e - 1L), action = rest)
}

# Split `s` at top-level commas.
ff_split_top <- function(s) {
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  depth <- 0L
  parts <- character()
  cur <- character()
  for (c in ch) {
    if (c == "(") depth <- depth + 1L
    if (c == ")") depth <- depth - 1L
    if (c == "," && depth == 0L) {
      parts <- c(parts, paste(cur, collapse = "")); cur <- character()
    } else cur <- c(cur, c)
  }
  c(parts, paste(cur, collapse = ""))
}

# Program-unit tracking: name of the enclosing SUBROUTINE/FUNCTION/MODULE/
# PROGRAM for each statement (by squeezed text). Contained procedures take
# precedence until their END.
ff_units <- function(sq) {
  unit <- character(length(sq))
  stack <- character()
  re_start <- paste0(
    "^(RECURSIVE|PURE|ELEMENTAL|INTEGER(\\([^)]*\\)|\\*[0-9]+)?|",
    "REAL(\\([^)]*\\)|\\*[0-9]+)?|DOUBLEPRECISION|LOGICAL(\\([^)]*\\))?|",
    "COMPLEX(\\([^)]*\\))?|CHARACTER(\\([^)]*\\)|\\*[0-9]+)?)*",
    "(SUBROUTINE|FUNCTION|PROGRAM|MODULE(?!PROCEDURE)|BLOCKDATA)",
    "([A-Z0-9_]+)")
  for (i in seq_along(sq)) {
    s <- sq[i]
    m <- regmatches(s, regexec(re_start, s, perl = TRUE))[[1]]
    if (length(m) && !grepl("^MODULEPROCEDURE", s) &&
        !grepl("^(END)", s)) {
      stack <- c(stack, m[length(m)])
      unit[i] <- m[length(m)]
      next
    }
    unit[i] <- if (length(stack)) stack[length(stack)] else ""
    if (grepl("^END(SUBROUTINE|FUNCTION|PROGRAM|MODULE|BLOCKDATA)?[A-Z0-9_]*$", s) &&
        !grepl("^END(IF|DO|SELECT|TYPE|INTERFACE|WHERE|FORALL|ASSOCIATE|BLOCK$|CRITICAL)", s) &&
        length(stack)) {
      stack <- stack[-length(stack)]
    }
  }
  unit
}
