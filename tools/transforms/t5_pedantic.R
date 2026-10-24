# T5: nonstandard type declarations (Fortran conformance, -pedantic).
#
# Declarations such as INTEGER*8 or REAL*4 are a common extension that
# gfortran reports under -pedantic ("GNU Extension: Nonstandard type
# declaration"), and CHARACTER*n is obsolescent. At the start of a
# declaration statement they become the standard forms:
#   INTEGER*n, LOGICAL*n, REAL*n  ->  INTEGER(n), LOGICAL(n), REAL(n)
#   COMPLEX*n                     ->  COMPLEX(n/2)
#   CHARACTER*n, CHARACTER*(*)    ->  CHARACTER(LEN=n), CHARACTER(LEN=*)
# Kind numbers equal to byte sizes hold for gfortran, LLVM flang and
# Intel ifx, the compilers R supports. Lines pushed past column 72 are
# re-wrapped.

T5_RE <- "^([ ]*)(INTEGER|LOGICAL|REAL|COMPLEX|CHARACTER)[ ]*\\*[ ]*([0-9]+|\\([ ]*\\*[ ]*\\))"

t5_rewrap <- function(lines, li) {
  l <- lines[li]
  if (nchar(l) <= 72L) return(lines)
  ch <- strsplit(l, "", fixed = TRUE)[[1]]
  inlit <- t1_in_literal(l)
  cand <- which(ch == "," & !inlit & seq_along(ch) <= 72L & seq_along(ch) > 7L)
  if (!length(cand)) stop("T5: cannot re-wrap: ", l, call. = FALSE)
  cut <- max(cand)
  lines[li] <- substr(l, 1L, cut)
  append(lines, paste0("     &", substr(l, cut + 1L, nchar(l))), after = li)
}

t5_transform <- function(lines, file) {
  cl <- ff_classify(lines)
  log <- character()
  for (li in rev(which(cl$type == "code" & !cl$sentinel))) {
    body <- substr(lines[li], 7L, nchar(lines[li]))
    m <- regmatches(body, regexec(T5_RE, toupper(body)))[[1]]
    if (!length(m)) next
    # m holds substrings of the original line (case preserved)
    type <- m[3]; size <- gsub("[ ]", "", m[4])
    new <- switch(toupper(type),
      COMPLEX = paste0(type, "(", as.integer(size) %/% 2L, ")"),
      CHARACTER = paste0(type, "(LEN=", if (size == "(*)") "*" else size, ")"),
      paste0(type, "(", size, ")"))
    rest <- substr(body, nchar(m[1]) + 1L, nchar(body))
    # a letter right after the size starts a name: keep them apart
    sep <- if (grepl("^[A-Za-z_]", rest)) " " else ""
    lines[li] <- paste0(substr(lines[li], 1L, 6L), m[2], new, sep, rest)
    lines <- t5_rewrap(lines, li)
    log <- c(log, sprintf("decl %s %s*%s", file, toupper(type), size))
  }
  list(lines = lines, log = log)
}
