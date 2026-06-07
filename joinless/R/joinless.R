#' Infer relationship types between variables in two datasets using sampling
#'
#' This function compares selected variables from two data frames and infers
#' their relational structure (e.g., one-to-one, many-to-one).
#' It uses random sampling—either automatic or user-defined—to estimate
#' match behavior, uniqueness patterns, and missingness characteristics.
#' The goal is to help diagnose potential join keys or detect unrelated fields
#' without performing full-table comparisons.
#'
#' @param x,y Data frames.
#'   Input datasets to be compared.
#'
#' @param x_vars,y_vars Character vectors specifying the column names to compare.
#'   If `NULL`, the function selects up to `max_vars` variables from each dataset.
#'
#' @param conf Numeric. Confidence level used to compute automatic sample sizes
#'   (default: `0.95`).
#'
#' @param error Numeric. Margin of error used in sample size calculation
#'   (default: `0.05`).
#'
#' @param n_x,n_y Optional fixed sample sizes for `x` and `y`.
#'   If not provided, sample sizes are computed automatically based on
#'   population size, `conf`, and `error`.
#'
#' @param max_vars Integer. Maximum number of variables to compare per dataset.
#'   Defaults to `20`.
#'
#' @param ignore Character vector of relation types to exclude from the output.
#'   By default, no types are excluded.
#'
#' @param missingness_tol Numeric. Maximum tolerated proportion of
#'   missing/problematic values within a variable (default: `0.1`).
#'   Problematic values include: `NA`, `NaN`, `NULL`, `Inf`, `-Inf`, empty strings,
#'   and whitespace-only strings.
#'
#' @param type_coerce Logical. If `TRUE` (default), attempts to coerce variables
#'   to a common type (typically character) when domains differ.
#'   If `FALSE`, type mismatches produce an `"error_type"` result.
#'
#' @param seed Optional integer. Random seed to make the sampling reproducible.
#'
#' @param verbose Logical. If `TRUE`, prints progress messages during execution.
#'
#' @param info Logical. If `FALSE` (default), returns only the inferred
#'   relationship type for each variable pair.
#'   If `TRUE`, returns additional diagnostics such as match rate,
#'   missingness rates, inferred types, and notes.
#'
#' @return A data frame summarizing the inferred relationship between every
#'   variable pair.
#'   If `info = FALSE`, the output contains:
#'   - `x_var`: variable name in `x`
#'   - `y_var`: variable name in `y`
#'   - `relation_type`: inferred relationship
#'
#'   If `info = TRUE`, additional columns include:
#'   - `n_used`: sample size used
#'   - `match_rate`: proportion of sampled values from `x` found in `y`
#'   - `null_rate_x`, `null_rate_y`: missingness/problematic rates
#'   - `type_x`, `type_y`: underlying storage types
#'   - `notes`: diagnostic messages
#'
#' @details
#' Relationship inference is determined using:
#' - **Match rate**: proportion of keys in `x` found in `y`
#' - **Key uniqueness**: frequency distribution of non-missing values
#'
#' Based on these, relationships are classified as:
#' - `"one-one"`
#' - `"many-one"`
#' - `"one-many"`
#' - `"many-many"`
#' - `"unrelated"` (very low or zero match rate)
#' - `"null"` (missingness above tolerance)
#' - `"error_type"` (incompatible types and coercion disabled)
#'
#' @examples
#' df1 <- data.frame(
#'   id    = 1:5,
#'   value = 1:5
#' )
#'
#' df2 <- data.frame(
#'   id    = 3:7,
#'   value = 3:7
#' )
#'
#' joinless(df1, df2, x_vars = "id", y_vars = "id")
#' joinless(df1, df2, conf = 0.99, error = 0.02, info = TRUE)
#' joinless(df1, df2, ignore = "unrelated")
#'
#' @export
joinless <- function(
  # --- input datasets ---
  x, y,
  # --- variables/columns to compare ---
  # if not provided, use all up to max_vars
  x_vars = NULL, y_vars = NULL,
  # --- confidence level and margin of error (for automatic n) ---
  conf = 0.95, error = 0.05,
  # --- manual sample sizes (if omitted, they are auto-calculated) ---
  n_x = NULL, n_y = NULL,
  # --- maximum number of variables to evaluate per dataset ---
  max_vars = 20,
  # --- which relation types to hide (optional) ---
  # by default, nothing is hidden
  ignore = character(0),
  # --- tolerance for missing/problematic values ---
  # includes: NA, NaN, NULL, Inf, -Inf, "", " "
  missingness_tol = 0.1,
  # --- attempt to coerce types when they differ ---
  type_coerce = TRUE,
  # --- seed for reproducible sampling ---
  seed = NULL,
  # --- print progress messages? ---
  verbose = FALSE,
  # --- if TRUE, return extra diagnostic info ---
  info = FALSE
) {
  if (!is.data.frame(x) || !is.data.frame(y)) {
    stop("x and y must be data.frames")
  }

  # if no variables selected, use all up to max_vars
  if (is.null(x_vars)) x_vars <- names(x)[1:min(max_vars, ncol(x))]
  if (is.null(y_vars)) y_vars <- names(y)[1:min(max_vars, ncol(y))]

  # if variables are selected, enforce max_vars
  if (length(x_vars) > max_vars) x_vars <- x_vars[1:max_vars]
  if (length(y_vars) > max_vars) y_vars <- y_vars[1:max_vars]

  if (!is.null(seed)) set.seed(seed)

  Nx <- nrow(x)
  Ny <- nrow(y)
  if (is.null(n_x)) n_x <- sample_size_auto(Nx, conf, error)
  if (is.null(n_y)) n_y <- sample_size_auto(Ny, conf, error)

  # sample each dataset once
  idx_x <- sample.int(Nx, n_x)
  idx_y <- sample.int(Ny, n_y)
  x_s <- x[idx_x, , drop = FALSE]
  y_s <- y[idx_y, , drop = FALSE]

  # full output (cartesian)
  total_pairs <- length(x_vars) * length(y_vars)
  out <- vector("list", total_pairs)
  k <- 1L

  for (xv in x_vars) {
    if (!xv %in% names(x_s)) {
      # still return an error row if variable does not exist
      for (yv in y_vars) {
        out[[k]] <- data.frame(
          x_var = xv, y_var = yv,
          relation_type = "error_type",
          n_used = NA_integer_,
          match_rate = NA_real_,
          null_rate_x = NA_real_, null_rate_y = NA_real_,
          type_x = NA_character_, type_y = NA_character_,
          notes = "x_var not found",
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
      next
    }

    vx <- x_s[[xv]]

    for (yv in y_vars) {
      if (!yv %in% names(y_s)) {
        out[[k]] <- data.frame(
          x_var = xv, y_var = yv,
          relation_type = "error_type",
          n_used = length(vx),
          match_rate = NA_real_,
          null_rate_x = missing_prop(vx),
          null_rate_y = NA_real_,
          type_x = typeof(vx),
          type_y = NA_character_,
          notes = "y_var not found",
          stringsAsFactors = FALSE
        )
        k <- k + 1L
        next
      }

      vy <- y_s[[yv]]
      if (verbose) message("Comparing ", xv, " vs ", yv)

      res <- infer_relation(vx, vy, missingness_tol, type_coerce)

      out[[k]] <- data.frame(
        x_var = xv,
        y_var = yv,
        relation_type = res$type,
        n_used = length(vx),
        match_rate = res$match_rate,
        null_rate_x = res$null_rate_x,
        null_rate_y = res$null_rate_y,
        type_x = res$tx,
        type_y = res$ty,
        notes = res$note,
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }

  out <- do.call(rbind, out)
  rownames(out) <- NULL

  # apply ignore only if provided
  if (length(ignore) > 0) {
    out <- out[!(out$relation_type %in% ignore), , drop = FALSE]
    rownames(out) <- NULL
  }

  # if info = FALSE, return only basic columns
  if (!info) {
    out <- out[, c("x_var", "y_var", "relation_type"), drop = FALSE]
  }

  out
}
