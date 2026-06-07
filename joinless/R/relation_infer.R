#' @keywords internal
#' @noRd

infer_relation <- function(vx, vy, missingness_tol, type_coerce) {
  tx <- typeof(vx)
  ty <- typeof(vy)

  # factor -> character para evitar líos
  if (is.factor(vx)) vx <- as.character(vx)
  if (is.factor(vy)) vy <- as.character(vy)

  # tipos diferentes
  if (tx != ty) {
    if (type_coerce) {
      vx <- as.character(vx)
      vy <- as.character(vy)
      tx <- typeof(vx)
      ty <- typeof(vy)
    } else {
      return(list(
        type = "error_type",
        match_rate = NA_real_,
        null_rate_x = missing_prop(vx),
        null_rate_y = missing_prop(vy),
        tx = tx, ty = ty,
        note = "Incompatible types"
      ))
    }
  }

  null_rate_x <- missing_prop(vx)
  null_rate_y <- missing_prop(vy)

  # tolerancia global de missing/problematic
  if (null_rate_x > missingness_tol || null_rate_y > missingness_tol) {
    return(list(
      type = "null",
      match_rate = NA_real_,
      null_rate_x = null_rate_x,
      null_rate_y = null_rate_y,
      tx = tx, ty = ty,
      note = "High missing/problematic rate"
    ))
  }

  # quitar missing/problematic antes de comparar
  vx2 <- filter_bad(vx)
  vy2 <- filter_bad(vy)

  if (length(vx2) == 0 || length(vy2) == 0) {
    return(list(
      type = "null",
      match_rate = NA_real_,
      null_rate_x = null_rate_x,
      null_rate_y = null_rate_y,
      tx = tx, ty = ty,
      note = "All missing/problematic after filter"
    ))
  }

  # match rate (existencia de claves en y)
  m <- match(vx2, vy2, nomatch = 0L)
  match_rate <- mean(m > 0L)

  if (match_rate < 0.05) {
    return(list(
      type = "unrelated",
      match_rate = match_rate,
      null_rate_x = null_rate_x,
      null_rate_y = null_rate_y,
      tx = tx, ty = ty,
      note = "Low match rate"
    ))
  }

  fx <- table(vx2)
  fy <- table(vy2)
  common_keys <- intersect(names(fx), names(fy))

  if (length(common_keys) == 0) {
    return(list(
      type = "unrelated",
      match_rate = match_rate,
      null_rate_x = null_rate_x,
      null_rate_y = null_rate_y,
      tx = tx, ty = ty,
      note = "No common keys"
    ))
  }

  cx <- as.integer(fx[common_keys])
  cy <- as.integer(fy[common_keys])

  x_unique <- all(cx == 1L)
  y_unique <- all(cy == 1L)

  rel_type <- if (x_unique && y_unique) {
    "one-one"
  } else if (!x_unique && y_unique) {
    "many-one"
  } else if (x_unique && !y_unique) {
    "one-many"
  } else {
    "many-many"
  }

  list(
    type = rel_type,
    match_rate = match_rate,
    null_rate_x = null_rate_x,
    null_rate_y = null_rate_y,
    tx = tx, ty = ty,
    note = ""
  )
}
