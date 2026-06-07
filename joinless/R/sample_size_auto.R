#' @keywords internal
#' @noRd

# helper: tamaño de muestra automático
sample_size_auto <- function(N, conf, error, p = 0.5) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  n0 <- (z^2 * p * (1 - p)) / (error^2)
  n_adj <- n0 / (1 + (n0 - 1) / N) # corrección finita
  ceiling(min(n_adj, N))
}
