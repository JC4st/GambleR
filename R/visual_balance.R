#' Calculate an electroneutral visual balance
#'
#' Residual compartments are illustrative. The function adds a residual charge
#' to the smaller side so both graphical bars have the same total height.
#'
#' @param sodium Sodium concentration in mmol/L.
#' @param potassium Potassium concentration in mmol/L.
#' @param chloride Chloride concentration in mmol/L.
#' @param bicarbonate Bicarbonate or total CO2 concentration in mmol/L.
#' @param albumin Albumin concentration in g/dL.
#' @param lactate Lactate concentration in mmol/L.
#' @param ionized_calcium Ionized calcium concentration in mmol/L.
#'
#' @return A list containing cation and anion named vectors, their common total,
#'   and observed and corrected anion gaps.
#' @export
calculate_visual_balance <- function(
    sodium,
    potassium,
    chloride,
    bicarbonate,
    albumin,
    lactate,
    ionized_calcium
) {
  validate_chemistry_inputs(
    sodium = sodium,
    potassium = potassium,
    chloride = chloride,
    bicarbonate = bicarbonate,
    albumin = albumin,
    lactate = lactate,
    ionized_calcium = ionized_calcium
  )

  cations <- cation_components(
    sodium = sodium,
    potassium = potassium,
    ionized_calcium = ionized_calcium
  )

  anions <- anion_components(
    chloride = chloride,
    bicarbonate = bicarbonate,
    lactate = lactate,
    albumin = albumin
  )

  cation_total <- sum(cations)
  anion_total <- sum(anions)

  if (cation_total > anion_total) {
    anions[["residual_anions"]] <- cation_total - anion_total
  } else if (anion_total > cation_total) {
    cations[["other_cations"]] <- anion_total - cation_total
  }

  observed_ag <- calculate_anion_gap(
    sodium = sodium,
    chloride = chloride,
    bicarbonate = bicarbonate
  )

  corrected_ag <- calculate_corrected_anion_gap(
    observed_ag = observed_ag,
    albumin = albumin
  )

  list(
    cations = cations,
    anions = anions,
    total_charge = sum(cations),
    observed_anion_gap = observed_ag,
    corrected_anion_gap = corrected_ag
  )
}
