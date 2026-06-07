#' @keywords internal
#' @noRd

# proporción de valores missing/problematic en un vector
missing_prop <- function(v) {
  n <- length(v)
  if (n == 0) {
    return(0)
  }

  bad <- rep(FALSE, n)

  # factor -> character para evaluar vacíos
  if (is.factor(v)) v <- as.character(v)

  # NA y NaN
  bad <- bad | is.na(v) | is.nan(v)

  # Inf y -Inf (solo numéricos/complex)
  if (is.numeric(v) || is.complex(v)) {
    bad <- bad | is.infinite(v)
  }

  # "" y " " (texto vacío o espacios)
  if (is.character(v)) {
    bad <- bad | trimws(v) == ""
  }

  # NULL dentro de list-columns
  if (is.list(v)) {
    bad <- bad | vapply(v, is.null, logical(1))
  }

  mean(bad)
}

# filtra valores missing/problematic (devuelve solo los "buenos")
filter_bad <- function(v) {
  n <- length(v)
  bad <- rep(FALSE, n)

  if (is.factor(v)) v <- as.character(v)

  # NA y NaN
  bad <- bad | is.na(v) | is.nan(v)

  # Inf y -Inf (solo numéricos/complex)
  if (is.numeric(v) || is.complex(v)) {
    bad <- bad | is.infinite(v)
  }

  # "" y " " (texto vacío o espacios)
  if (is.character(v)) {
    bad <- bad | trimws(v) == ""
  }

  # NULL dentro de list-columns
  if (is.list(v)) {
    bad <- bad | vapply(v, is.null, logical(1))
  }

  v[!bad]
}
