#' Calculate the observed anion gap
#'
#' Uses the potassium-free convention:
#' `sodium - (chloride + bicarbonate)`.
#'
#' @param sodium Sodium concentration in mmol/L.
#' @param chloride Chloride concentration in mmol/L.
#' @param bicarbonate Bicarbonate or total CO2 concentration in mmol/L.
#'
#' @return Numeric observed anion gap in mmol/L.
#' @export
calculate_anion_gap <- function(sodium, chloride, bicarbonate) {
  inputs <- c(sodium = sodium, chloride = chloride, bicarbonate = bicarbonate)

  if (!is.numeric(inputs) || length(inputs) != 3L || any(!is.finite(inputs))) {
    stop("Inputs must be finite numeric scalars.", call. = FALSE)
  }

  sodium - (chloride + bicarbonate)
}

#' Calculate the albumin-corrected anion gap
#'
#' Uses:
#' `observed_ag + 2.5 * (4 - albumin_g_dl)`.
#'
#' @param observed_ag Observed anion gap in mmol/L.
#' @param albumin Albumin concentration in g/dL.
#'
#' @return Numeric corrected anion gap in mmol/L.
#' @export
calculate_corrected_anion_gap <- function(observed_ag, albumin) {
  inputs <- c(observed_ag = observed_ag, albumin = albumin)

  if (!is.numeric(inputs) || length(inputs) != 2L || any(!is.finite(inputs))) {
    stop("Inputs must be finite numeric scalars.", call. = FALSE)
  }

  observed_ag +
    ALBUMIN_AG_CORRECTION_FACTOR * (NORMAL_ALBUMIN_G_DL - albumin)
}
