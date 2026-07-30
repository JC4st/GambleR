#' Validate chemistry inputs
#'
#' Performs structural validation only. It does not classify values as normal or
#' abnormal and does not generate diagnoses.
#'
#' @param sodium Sodium concentration in mmol/L.
#' @param potassium Potassium concentration in mmol/L.
#' @param chloride Chloride concentration in mmol/L.
#' @param bicarbonate Bicarbonate or total CO2 concentration in mmol/L.
#' @param albumin Albumin concentration in g/dL.
#' @param lactate Lactate concentration in mmol/L.
#' @param ionized_calcium Ionized calcium concentration in mmol/L.
#'
#' @return Invisibly returns `TRUE` when all inputs are valid.
#' @export
validate_chemistry_inputs <- function(
    sodium,
    potassium,
    chloride,
    bicarbonate,
    albumin,
    lactate,
    ionized_calcium
) {
  values <- c(
    sodium = sodium,
    potassium = potassium,
    chloride = chloride,
    bicarbonate = bicarbonate,
    albumin = albumin,
    lactate = lactate,
    ionized_calcium = ionized_calcium
  )

  if (!is.numeric(values) || length(values) != 7L) {
    stop("All chemistry inputs must be numeric scalars.", call. = FALSE)
  }

  if (any(!is.finite(values))) {
    stop("All chemistry inputs must be finite.", call. = FALSE)
  }

  if (any(values < 0)) {
    stop("Chemistry inputs cannot be negative.", call. = FALSE)
  }

  invisible(TRUE)
}
