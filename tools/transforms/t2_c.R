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

t2_transform <- function(lines, file, comp) {
  if (!grepl("\\.c$", file)) return(lines)
  at <- t2_includes(lines)
  if (!length(at)) return(c(T2_INCLUDE, lines))
  # an #include continued with a backslash is not expected in these sources
  if (any(grepl("\\\\$", lines[at]))) stop(file, ": continued #include", call. = FALSE)
  for (a in rev(at)) lines <- append(lines, T2_INCLUDE, after = a)
  lines
}
