#' Infer relationship types between one dataset and multiple counterparts
#'
#' This function is a convenience wrapper around [joinless()] that compares
#' a single dataset `x` against multiple datasets supplied in a list `ys`.
#' Internally it calls [joinless()] once per dataset and row-binds the results,
#' adding an extra column that identifies the target dataset.
#'
#' For each dataset in `ys`, the function:
#' - optionally restricts the variables in `x` via `x_vars`,
#' - optionally restricts the variables in that dataset via `y_vars`,
#' - calls [joinless()] with the provided settings,
#' - tags the output with a dataset name.
#'
#' @param x A data frame. The reference dataset to compare from.
#'
#' @param ys A named or unnamed list of data frames. Each element is treated
#'   as a separate target dataset to compare `x` against.
#'
#' @param x_vars Optional character vector of column names in `x` to compare.
#'   Passed directly to [joinless()]. If `NULL`, the default behavior of
#'   [joinless()] is used (i.e., it selects up to `max_vars` variables).
#'
#' @param y_vars Optional character vector of column names to use in each
#'   target dataset. If not `NULL`, the function filters `y_vars` to those
#'   that exist in each dataset in `ys` before calling [joinless()]. If
#'   `NULL`, each dataset is allowed to use its own default variable selection
#'   in [joinless()] (up to `max_vars`).
#'
#' @param dataset_names Optional character vector with labels for each dataset
#'   in `ys`. If `NULL` and `ys` is a named list, the list names are used
#'   (empty names are replaced by `"y1"`, `"y2"`, …). If `ys` is unnamed,
#'   generic names `"y1"`, `"y2"`, … are generated. The length of
#'   `dataset_names` must match the length of `ys` if supplied.
#'
#' @param ... Additional arguments passed on to [joinless()], such as
#'   `conf`, `error`, `n_x`, `n_y`, `max_vars`, `ignore`, `missingness_tol`,
#'   `type_coerce`, `seed`, `verbose`, and `info`.
#'
#' @details
#' When `y_vars` is not `NULL`, the function intersects `y_vars` with the
#' column names of each dataset in `ys`. This means that:
#'
#' - Variables listed in `y_vars` but missing in a given dataset are silently
#'   dropped for that dataset.
#' - If *none* of the variables in `y_vars` exist in a particular dataset,
#'   that dataset is skipped and a warning is emitted.
#'
#' This behavior avoids producing `"error_type"` rows solely due to missing
#' columns in some of the target datasets.
#'
#' If all datasets are skipped (e.g., because none contain the requested
#' `y_vars`), the function returns an empty data frame.
#'
#' @return A data frame that row-binds the result of [joinless()] for all
#'   target datasets that were processed. It contains all columns returned by
#'   [joinless()] plus an additional column:
#'
#'   - `dataset`: identifier of the target dataset (one per element of `ys`).
#'
#'   If all datasets are skipped, an empty data frame is returned.
#'
#' @examples
#' df_base <- data.frame(id = 1:5, value = 1:5)
#' df_a <- data.frame(id = 3:7, value = 3:7)
#' df_b <- data.frame(id_alt = 1:5, value = 11:15)
#'
#' # Compare the same key from df_base against multiple datasets
#' res <- joinless_multiple(
#'   x = df_base,
#'   ys = list(a = df_a, b = df_b),
#'   x_vars = "id",
#'   y_vars = c("id", "id_alt"),
#'   info = TRUE
#' )
#'
#' @export
joinless_multiple <- function(
  x,
  ys,
  x_vars = NULL,
  y_vars = NULL,
  dataset_names = NULL,
  ...
) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data.frame")
  }

  if (!is.list(ys) || length(ys) == 0L) {
    stop("`ys` must be a non-empty list of data.frames")
  }

  bad <- !vapply(ys, is.data.frame, logical(1))
  if (any(bad)) {
    stop("All elements of `ys` must be data.frames")
  }

  # Resolve dataset_names
  if (is.null(dataset_names)) {
    if (!is.null(names(ys)) && any(nzchar(names(ys)))) {
      dataset_names <- names(ys)
      # Replace empty names, if any
      empty_idx <- which(!nzchar(dataset_names))
      if (length(empty_idx) > 0L) {
        dataset_names[empty_idx] <- paste0("y", empty_idx)
      }
    } else {
      dataset_names <- paste0("y", seq_along(ys))
    }
  } else if (length(dataset_names) != length(ys)) {
    stop("`dataset_names` must have the same length as `ys`")
  }

  out_list <- vector("list", length(ys))

  for (i in seq_along(ys)) {
    y_i <- ys[[i]]

    # Filter y_vars to only those that exist in this dataset
    y_vars_i <- y_vars
    if (!is.null(y_vars)) {
      y_vars_i <- intersect(y_vars, names(y_i))
      if (length(y_vars_i) == 0L) {
        warning("No y_vars found in dataset ", dataset_names[i], "; skipping.")
        next
      }
    }

    res_i <- joinless(
      x      = x,
      y      = y_i,
      x_vars = x_vars,
      y_vars = y_vars_i,
      ...
    )

    res_i$dataset <- dataset_names[i]
    out_list[[i]] <- res_i
  }

  # Drop datasets that were skipped
  out_list <- Filter(Negate(is.null), out_list)
  if (length(out_list) == 0L) {
    return(data.frame())
  }

  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}
