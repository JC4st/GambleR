#' Build cation components
#'
#' @param sodium Sodium concentration in mmol/L.
#' @param potassium Potassium concentration in mmol/L.
#' @param ionized_calcium Ionized calcium concentration in mmol/L.
#' @param other_cations Illustrative residual cation compartment in mEq/L.
#'
#' @return A named numeric vector of cation charges.
#' @export
cation_components <- function(
    sodium,
    potassium,
    ionized_calcium,
    other_cations = 0
) {
  inputs <- c(
    sodium = sodium,
    potassium = potassium,
    ionized_calcium = ionized_calcium,
    other_cations = other_cations
  )

  if (any(!is.finite(inputs)) || any(inputs < 0)) {
    stop("Cation components must be finite and non-negative.", call. = FALSE)
  }

  c(
    sodium = sodium,
    potassium = potassium,
    ionized_calcium = 2 * ionized_calcium,
    other_cations = other_cations
  )
}

#' Build anion components
#'
#' @param chloride Chloride concentration in mmol/L.
#' @param bicarbonate Bicarbonate or total CO2 concentration in mmol/L.
#' @param lactate Lactate concentration in mmol/L.
#' @param albumin Albumin concentration in g/dL.
#' @param residual_anions Illustrative residual anion compartment in mEq/L.
#'
#' @return A named numeric vector of anion charges.
#' @export
anion_components <- function(
    chloride,
    bicarbonate,
    lactate,
    albumin,
    residual_anions = 0
) {
  inputs <- c(
    chloride = chloride,
    bicarbonate = bicarbonate,
    lactate = lactate,
    albumin = albumin,
    residual_anions = residual_anions
  )

  if (any(!is.finite(inputs)) || any(inputs < 0)) {
    stop("Anion components must be finite and non-negative.", call. = FALSE)
  }

  c(
    chloride = chloride,
    bicarbonate = bicarbonate,
    lactate = lactate,
    albumin = albumin_charge(albumin),
    residual_anions = residual_anions
  )
}
