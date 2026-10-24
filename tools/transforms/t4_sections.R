# T4: element-to-section rewrites (Fortran conformance).
#
# MUMPS often passes an element of a pointer or assumed-shape array, P(I),
# to an explicit-shape or assumed-size dummy, relying on sequence
# association. The standard forbids that for pointer/assumed-shape
# elements, and gfortran >= 10 rejects it. The conforming equivalent is the
# section P(I:): when P is contiguous (always the case for MUMPS' allocated
# arrays) the callee receives the same address; otherwise the compiler
# makes a copy. A multi-dimensional element is rewritten only when every
# subscript is 1 (P(1,1) -> P(1:,1:), the whole array for lower bounds 1).
#
# The sites are listed in tools/conformance/<component>-sections.tsv
# (file, routine, callee, argument; built by tools/conformance-harvest.R
# from a compiler log and reviewed). In each listed routine, every CALL of
# the callee gets each top-level actual argument equal to the listed one
# (blanks and case ignored) rewritten. An entry that matches nothing is an
# error, so a changed upstream file cannot silently lose a fix.

t4_read_table <- function(path) {
  if (!file.exists(path)) return(data.frame(file = character(), routine = character(),
                                            callee = character(), arg = character()))
  l <- read_lines(path)
  l <- l[nzchar(trimws(l)) & !startsWith(l, "#")]
  r <- strsplit(l, "\t", fixed = TRUE)
  if (any(lengths(r) != 4L)) stop(path, ": every line needs 4 tab-separated fields", call. = FALSE)
  r <- do.call(rbind, r)
  data.frame(file = r[, 1], routine = toupper(r[, 2]), callee = toupper(r[, 3]),
             arg = toupper(gsub("[[:space:]]", "", r[, 4])))
}

# Positions (in `m`, 1-based) after which ':' must be inserted to turn the
# element designator m[a:b] into a section; NULL if it is not one T4 handles.
t4_colons <- function(m, a, b) {
  ch <- strsplit(m, "", fixed = TRUE)[[1]]
  # last non-blank character must close the subscript list
  e <- b
  while (e > a && ch[e] == " ") e <- e - 1L
  if (ch[e] != ")") return(NULL)
  d <- 0L
  open <- NA_integer_
  for (i in seq.int(e, a)) {
    if (ch[i] == ")") d <- d + 1L
    if (ch[i] == "(") { d <- d - 1L; if (d == 0L) { open <- i; break } }
  }
  if (is.na(open)) return(NULL)
  # the designator must not end at the opening parenthesis of a call
  pre <- trimws(paste(ch[a:max(a, open - 1L)], collapse = ""))
  if (open == a || !nzchar(pre)) return(NULL)
  inner <- paste(ch[(open + 1L):(e - 1L)], collapse = "")
  if (grepl(":", inner, fixed = TRUE)) return(NULL)
  # top-level commas of the subscript list
  commas <- integer()
  d <- 0L
  for (i in seq.int(open + 1L, e - 1L)) {
    if (ch[i] == "(") d <- d + 1L
    if (ch[i] == ")") d <- d - 1L
    if (ch[i] == "," && d == 0L) commas <- c(commas, i)
  }
  if (!length(commas)) {
    j <- e - 1L
    while (j > open && ch[j] == " ") j <- j - 1L
    return(j)
  }
  bounds <- c(open, commas, e)
  subs <- vapply(seq_len(length(bounds) - 1L), function(k)
    gsub(" ", "", paste(ch[(bounds[k] + 1L):(bounds[k + 1L] - 1L)], collapse = "")), "")
  if (!all(subs == "1")) return(NULL)
  vapply(seq_len(length(bounds) - 1L), function(k) {
    seg <- (bounds[k] + 1L):(bounds[k + 1L] - 1L)
    seg[ch[seg] == "1"][1L]
  }, 1L)
}

# Insert ':' after character `pos` of the statement body (columns 7-72 of
# the code lines `code`, concatenated), re-wrapping the physical line if it
# would pass column 72. Returns the new lines.
t4_insert <- function(lines, code, pos) {
  lens <- nchar(substr(lines[code], 7L, 72L))
  cum <- cumsum(lens)
  k <- which(cum >= pos)[1L]
  if (is.na(k)) stop("t4_insert: position outside the statement", call. = FALSE)
  li <- code[k]
  off <- pos - (if (k > 1L) cum[k - 1L] else 0L)
  l <- lines[li]
  col <- 6L + off
  new <- paste0(substr(l, 1L, col), ":", substr(l, col + 1L, nchar(l)))
  if (nchar(new) <= 72L) {
    lines[li] <- new
    return(lines)
  }
  # move the end of the line to a continuation line, preferably after the
  # last comma (outside character literals) that still fits in column 72,
  # otherwise right after the inserted ':'
  ch <- strsplit(new, "", fixed = TRUE)[[1]]
  inlit <- t1_in_literal(new)
  cut <- col + 1L
  cand <- which(ch == "," & !inlit & seq_along(ch) <= 72L & seq_along(ch) > 7L)
  if (length(cand)) cut <- max(cand)
  lines[li] <- substr(new, 1L, cut)
  append(lines, paste0("     &", substr(new, cut + 1L, nchar(new))), after = li)
}

t4_transform <- function(lines, file, table) {
  tab <- table[table$file == file, , drop = FALSE]
  if (!nrow(tab)) return(list(lines = lines, log = character()))
  used <- logical(nrow(tab))
  log <- character()
  st <- ff_statements(lines)
  st$unit <- ff_units(st$squeezed)
  iscode <- ff_classify(lines)$type %in% c("code", "cont")
  for (i in rev(seq_len(nrow(st)))) {
    act <- ff_strip_if(st$squeezed[i])[["action"]]
    if (!startsWith(act, "CALL")) next
    callee <- sub("^CALL([A-Z0-9_]+).*$", "\\1", act)
    rows <- which(tab$routine == st$unit[i] & tab$callee == callee)
    if (!length(rows)) next
    m <- st$masked[i]
    p0 <- regexpr("\\(", m)[[1]]
    if (p0 < 0) next
    p1 <- ff_match_paren(m, p0)
    ch <- strsplit(m, "", fixed = TRUE)[[1]]
    d <- 0L; start <- p0 + 1L; args <- list()
    for (j in seq.int(p0 + 1L, p1)) {
      c <- ch[j]
      if (c == "(") d <- d + 1L
      if (c == ")") {
        if (d == 0L) { args[[length(args) + 1L]] <- c(start, j - 1L); break }
        d <- d - 1L
      }
      if (c == "," && d == 0L) { args[[length(args) + 1L]] <- c(start, j - 1L); start <- j + 1L }
    }
    code <- st$first[i]:st$last[i]
    code <- code[iscode[code]]
    for (a in rev(args)) {
      txt <- toupper(gsub("[[:space:]]", "", substr(m, a[1], a[2])))
      r <- rows[tab$arg[rows] == txt]
      if (!length(r)) next
      at <- t4_colons(m, a[1], a[2])
      if (is.null(at))
        stop(sprintf("T4: %s:%d: cannot make a section of %s", file, st$first[i], txt), call. = FALSE)
      for (p in rev(at)) lines <- t4_insert(lines, code, p)
      used[r] <- TRUE
      log <- c(log, sprintf("section %s %s %s %s", file, st$unit[i], callee, txt))
    }
  }
  if (any(!used))
    stop(paste(c("T4: entries that match nothing (stale?):",
                 sprintf("  %s\t%s\t%s\t%s", tab$file[!used], tab$routine[!used],
                         tab$callee[!used], tab$arg[!used])),
               collapse = "\n"), call. = FALSE)
  list(lines = lines, log = log)
}
