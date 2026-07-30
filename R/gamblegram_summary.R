#' Build a Gamblegram textual summary
#'
#' Calculates the observed and albumin-corrected anion gaps and prepares
#' language-specific text for presentation.
#'
#' The function does not classify the anion gap, generate diagnoses, or produce
#' HTML. It returns structured data that can later be rendered by Shiny or
#' another interface.
#'
#' @param sodium Sodium concentration in mmol/L.
#' @param chloride Chloride concentration in mmol/L.
#' @param bicarbonate Bicarbonate or total CO2 concentration in mmol/L.
#' @param albumin Albumin concentration in g/dL.
#' @param language Language code. Supported values are `"es"` and `"en"`.
#' @param digits Number of decimal places used in formatted values.
#'
#' @return A list containing numeric results, formatted values, labels, and
#'   educational notes.
#' @export
build_gamblegram_summary <- function(
    sodium,
    chloride,
    bicarbonate,
    albumin,
    language = "es",
    digits = 1
) {
  validate_language(language)
  
  if (
    !is.numeric(digits) ||
    length(digits) != 1L ||
    !is.finite(digits) ||
    digits < 0 ||
    digits != as.integer(digits)
  ) {
    stop(
      "`digits` must be a single non-negative integer.",
      call. = FALSE
    )
  }
  
  inputs <- c(
    sodium = sodium,
    chloride = chloride,
    bicarbonate = bicarbonate,
    albumin = albumin
  )
  
  if (
    !is.numeric(inputs) ||
    length(inputs) != 4L ||
    any(!is.finite(inputs))
  ) {
    stop(
      "Summary inputs must be finite numeric scalars.",
      call. = FALSE
    )
  }
  
  if (any(inputs < 0)) {
    stop(
      "Summary inputs cannot be negative.",
      call. = FALSE
    )
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
  
  decimal_mark <- if (identical(language, "es")) "," else "."
  
  format_value <- function(value) {
    formatC(
      value,
      format = "f",
      digits = digits,
      decimal.mark = decimal_mark
    )
  }
  
  unit <- translate_text(
    key = "anion_gap_unit",
    language = language
  )
  
  list(
    language = language,
    title = translate_text(
      key = "summary_title",
      language = language
    ),
    values = list(
      observed_anion_gap = observed_ag,
      corrected_anion_gap = corrected_ag
    ),
    formatted_values = list(
      observed_anion_gap = format_value(observed_ag),
      corrected_anion_gap = format_value(corrected_ag)
    ),
    labels = list(
      observed_anion_gap = translate_text(
        key = "observed_ag",
        language = language
      ),
      corrected_anion_gap = translate_text(
        key = "corrected_ag",
        language = language
      )
    ),
    unit = unit,
    display_text = list(
      observed_anion_gap = paste(
        translate_text("observed_ag", language),
        paste0(format_value(observed_ag), " ", unit),
        sep = ": "
      ),
      corrected_anion_gap = paste(
        translate_text("corrected_ag", language),
        paste0(format_value(corrected_ag), " ", unit),
        sep = ": "
      )
    ),
    notes = list(
      albumin_correction = translate_text(
        key = "albumin_correction_note",
        language = language
      ),
      residual_compartments = translate_text(
        key = "residual_note",
        language = language
      ),
      disclaimer = translate_text(
        key = "educational_disclaimer",
        language = language
      )
    )
  )
}