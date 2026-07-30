#' Build canonical Gamblegram plotting data
#'
#' Converts the physiological balance output into a stable tabular structure
#' with one row per graphical segment.
#'
#' This function does not generate labels in a specific language. Instead, it
#' returns translation keys that can later be resolved by the interface.
#'
#' @param sodium Sodium concentration in mmol/L.
#' @param potassium Potassium concentration in mmol/L.
#' @param chloride Chloride concentration in mmol/L.
#' @param bicarbonate Bicarbonate or total CO2 concentration in mmol/L.
#' @param albumin Albumin concentration in g/dL.
#' @param lactate Lactate concentration in mmol/L.
#' @param ionized_calcium Ionized calcium concentration in mmol/L.
#'
#' @return A tibble with one row per Gamblegram segment.
#' @export
build_gamblegram_data <- function(
    sodium,
    potassium,
    chloride,
    bicarbonate,
    albumin,
    lactate,
    ionized_calcium
) {
  balance <- calculate_visual_balance(
    sodium = sodium,
    potassium = potassium,
    chloride = chloride,
    bicarbonate = bicarbonate,
    albumin = albumin,
    lactate = lactate,
    ionized_calcium = ionized_calcium
  )
  
  cation_data <- tibble::tibble(
    side = "cation",
    component = names(balance$cations),
    value = as.numeric(balance$cations),
    display_order = seq_along(balance$cations)
  )
  
  anion_data <- tibble::tibble(
    side = "anion",
    component = names(balance$anions),
    value = as.numeric(balance$anions),
    display_order = seq_along(balance$anions)
  )
  
  dplyr::bind_rows(
    cation_data,
    anion_data
  ) |>
    dplyr::mutate(
      translation_key = component,
      is_residual = component %in% c(
        "other_cations",
        "residual_anions"
      )
    ) |>
    dplyr::select(
      side,
      component,
      translation_key,
      value,
      display_order,
      is_residual
    )
}