# T2: C shim insertion.
#
# Every vendored .c file gets `#include "rmumps_cshim.h"` right after each of
# its #include lines (in the same conditional branch). Some files interleave
# includes and code, and some system headers (<assert.h>) redefine macros on
# every inclusion, so the shim, whose redefinitions are unguarded, is
# re-applied after each include. The shim turns printf-family output into
# no-ops, maps exit/abort/assert to rmumps_fatal(), rand/srand to a private
# generator and sprintf/vsprintf to vsnprintf; see src/shim/rmumps_cshim.h.
# Headers are left alone: macros they define expand in the .c file, after
# the shim.

T2_INCLUDE <- "#include \"rmumps_cshim.h\" /* rmumps: inserted by tools/vendor.R */"

# Line numbers of #include directives outside block comments.
t2_includes <- function(lines) {
  incomment <- FALSE
  at <- integer()
  for (i in seq_along(lines)) {
    l <- lines[i]
    if (incomment) {
      if (!grepl("*/", l, fixed = TRUE)) next
      incomment <- FALSE
      l <- sub("^.*?\\*/", "", l, perl = TRUE)
    }
    l2 <- gsub("/\\*.*?\\*/", "", l, perl = TRUE)
    if (grepl("^[[:space:]]*#[[:space:]]*include", l2)) at <- c(at, i)
    if (grepl("/*", l2, fixed = TRUE)) incomment <- TRUE
  }
  at
}

# Old-style empty parameter lists. `int f();` declares f without a
# prototype, which clang reports under -pedantic (-Wstrict-prototypes,
# deprecated in all versions of C); `int f(void)` is the same function with
# a prototype. Only declarations and definitions are rewritten: an
# identifier followed by () at brace depth 0 of the file, or of a macro
# body, where a call cannot occur. Comments and literals are skipped.
t2_void_params <- function(lines) {
  depth <- 0L         # brace depth in ordinary code
  mdepth <- 0L        # brace depth inside the current #define
  inmacro <- FALSE
  incomment <- FALSE
  mparams <- character()
  for (i in seq_along(lines)) {
    l <- lines[i]
    j <- 1L
    if (!inmacro && grepl("^[[:space:]]*#", l)) {
      if (!grepl("^[[:space:]]*#[[:space:]]*define", l)) next
      # a one-line macro is an expression or a token sequence, never one of
      # the multi-line function generators handled below
      if (!grepl("\\\\[[:space:]]*$", l)) next
      inmacro <- TRUE; mdepth <- 0L
      # skip "#define NAME" and the parameter list of a function-like macro
      hd <- regmatches(l, regexpr("^[[:space:]]*#[[:space:]]*define[[:space:]]+[A-Za-z_][A-Za-z0-9_]*(\\([^)]*\\))?", l))
      j <- nchar(hd) + 1L
      # a macro parameter followed by () may stand for any function
      mparams <- if (grepl("\\(", hd)) trimws(strsplit(sub("^[^(]*\\((.*)\\)$", "\\1", hd), ",")[[1]]) else character()
    }
    # the braces of extern "C" { ... } guards do not open a scope
    if (!inmacro && (grepl("^[[:space:]]*extern[[:space:]]+\"C\"[[:space:]]*\\{", l) ||
                     (grepl("^[[:space:]]*\\}[[:space:]]*$", l) && i > 1L &&
                      grepl("__cplusplus", lines[i - 1L])))) next
    ch <- strsplit(l, "", fixed = TRUE)[[1]]
    n <- length(ch)
    out <- ch
    mk <- ch          # ch with comments and literals blanked
    q <- ""
    while (j <= n) {
      c <- ch[j]
      if (incomment) {
        mk[j] <- " "
        if (c == "*" && j < n && ch[j + 1L] == "/") {
          mk[j + 1L] <- " "; incomment <- FALSE; j <- j + 2L; next
        }
        j <- j + 1L; next
      }
      if (nzchar(q)) {
        mk[j] <- " "
        if (c == "\\") { if (j < n) mk[j + 1L] <- " "; j <- j + 2L; next }
        if (c == q) q <- ""
        j <- j + 1L; next
      }
      if (c == "/" && j < n && ch[j + 1L] == "*") {
        mk[j] <- " "; mk[j + 1L] <- " "; incomment <- TRUE; j <- j + 2L; next
      }
      if (c == "/" && j < n && ch[j + 1L] == "/") { mk[j:n] <- " "; break }
      if (c == "\"" || c == "'") { mk[j] <- " "; q <- c; j <- j + 1L; next }
      if (c == "{") { if (inmacro) mdepth <- mdepth + 1L else depth <- depth + 1L }
      if (c == "}") { if (inmacro) mdepth <- mdepth - 1L else depth <- depth - 1L }
      if (c == "(" && (if (inmacro) mdepth else depth) == 0L) {
        # previous non-blank character ends an identifier, next non-blank is ')'
        k <- j - 1L
        while (k >= 1L && ch[k] %in% c(" ", "\t")) k <- k - 1L
        m <- j + 1L
        while (m <= n && ch[m] %in% c(" ", "\t")) m <- m + 1L
        if (k >= 1L && grepl("[A-Za-z0-9_]", ch[k]) && m <= n && ch[m] == ")") {
          # the identifier must not be a keyword like sizeof or a call in
          # an initializer (file-scope initializers are constant anyway)
          s <- k
          while (s > 1L && grepl("[A-Za-z0-9_]", ch[s - 1L])) s <- s - 1L
          id <- paste(ch[s:k], collapse = "")
          # declaration shape: only type-like tokens, '*' or '##' before the
          # name, and only ';', '{', '\' or the end of the line after '()'
          pre <- if (s > 1L) paste(mk[1L:(s - 1L)], collapse = "") else ""
          if (startsWith(sub("^[[:space:]]*", "", pre), "#")) pre <- sub("^[[:space:]]*#[[:space:]]*define[[:space:]]+[A-Za-z_][A-Za-z0-9_]*(\\([^)]*\\))?", "", pre)
          # the rest of the line is not scanned yet: blank its comments
          post <- if (m < n) paste(ch[(m + 1L):n], collapse = "") else ""
          post <- gsub("/\\*.*?(\\*/|$)", " ", sub("//.*$", "", post), perl = TRUE)
          decl <- grepl("^[[:space:]]*(([A-Za-z_][A-Za-z0-9_]*|\\*|##)[[:space:]]*)*$", pre) &&
            grepl("^[[:space:]]*(;|\\{|\\\\|$)", post)
          if (decl && !(inmacro && id %in% mparams) &&
              !(id %in% c("sizeof", "if", "while", "for", "switch", "return")))
            out[j] <- "(void"
        }
      }
      j <- j + 1L
    }
    lines[i] <- paste(out, collapse = "")
    if (inmacro && !grepl("\\\\[[:space:]]*$", l)) inmacro <- FALSE
  }
  lines
}

t2_transform <- function(lines, file, comp) {
  lines <- t2_void_params(lines)
  if (!grepl("\\.c$", file)) return(lines)
  at <- t2_includes(lines)
  if (!length(at)) return(c(T2_INCLUDE, lines))
  # an #include continued with a backslash is not expected in these sources
  if (any(grepl("\\\\$", lines[at]))) stop(file, ": continued #include", call. = FALSE)
  for (a in rev(at)) lines <- append(lines, T2_INCLUDE, after = a)
  lines
}
