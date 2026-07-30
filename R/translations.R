# Translation dictionaries and helpers.

translations <- list(
  en = list(
    app_title = "GambleR",
    educational_disclaimer = paste(
      "Educational visualization only.",
      "Not intended for clinical decision-making."
    ),
    summary_title = "Anion gap summary",
    anion_gap_unit = "mmol/L",
    albumin_correction_note = paste(
      "Albumin correction uses:",
      "AGc = AG + 2.5 \u00d7 (4 \u2212 albumin in g/dL)."
    ),
    residual_note = paste(
      "Residual graphical compartments are illustrative",
      "and only preserve visual electroneutrality."
    ),
    validation_title = "Check the entered values",
    validation_missing = "Complete all chemistry fields before generating the Gamblegram.",
    validation_invalid_number = "All chemistry values must be finite numeric values.",
    validation_negative = "Chemistry values cannot be negative.",
    cations = "Cations",
    anions = "Anions",
    charge_contribution = "Charge contribution (mEq/L)",
    sodium = "Sodium",
    potassium = "Potassium",
    chloride = "Chloride",
    bicarbonate = "Bicarbonate / total CO2",
    albumin = "Albumin",
    lactate = "Lactate",
    ionized_calcium = "Ionized calcium",
    other_cations = "Illustrative other cations",
    residual_anions = "Illustrative residual anions",
    observed_ag = "Observed anion gap",
    corrected_ag = "Albumin-corrected anion gap"
  ),
  es = list(
    app_title = "GambleR",
    educational_disclaimer = paste(
      "Visualizaci\u00f3n exclusivamente educativa.",
      "No est\u00e1 destinada a la toma de decisiones cl\u00ednicas."
    ),
    summary_title = "Resumen de la brecha ani\u00f3nica",
    anion_gap_unit = "mmol/L",
    albumin_correction_note = paste(
      "La correcci\u00f3n por alb\u00famina utiliza:",
      "AGc = AG + 2,5 \u00d7 (4 \u2212 alb\u00famina en g/dL)."
    ),
    residual_note = paste(
      "Los compartimentos gr\u00e1ficos residuales son ilustrativos",
      "y solo preservan la electroneutralidad visual."
    ),
    validation_title = "Revise los valores ingresados",
    validation_missing = "Complete todos los campos antes de generar el Gamblegram.",
    validation_invalid_number = "Todos los valores deben ser num\u00e9 ricos y finitos.",
    validation_negative = "Los valores no pueden ser negativos.",
    cations = "Cationes",
    anions = "Aniones",
    charge_contribution = "Contribuci\u00f3n de carga (mEq/L)",
    sodium = "Sodio",
    potassium = "Potasio",
    chloride = "Cloro",
    bicarbonate = "Bicarbonato / CO2 total",
    albumin = "Alb\u00famina",
    lactate = "Lactato",
    ionized_calcium = "Calcio ionizado",
    other_cations = "Otros cationes ilustrativos",
    residual_anions = "Aniones residuales ilustrativos",
    observed_ag = "Brecha ani\u00f3nica observada",
    corrected_ag = "Brecha ani\u00f3nica corregida por alb\u00famina"
  )
)

#' Validate a supported language
#'
#' @param language Language code.
#'
#' @return The validated language code.
validate_language <- function(language) {
  if (!is.character(language) || length(language) != 1L || is.na(language)) {
    stop("`language` must be a single character value.", call. = FALSE)
  }
  
  if (!language %in% names(translations)) {
    stop(
      sprintf(
        "Unsupported language: %s. Supported languages are: %s.",
        language,
        paste(names(translations), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  language
}

#' Translate a text key
#'
#' @param key Translation key.
#' @param language Language code. Supported values are `"es"` and `"en"`.
#' @param fallback Optional fallback value. By default, the key itself is used.
#'
#' @return A translated character value.
#' @export
translate_text <- function(
    key,
    language = "es",
    fallback = key
) {
  validate_language(language)
  
  if (!is.character(key) || length(key) != 1L || is.na(key)) {
    stop("`key` must be a single character value.", call. = FALSE)
  }
  
  translated_value <- translations[[language]][[key]]
  
  if (is.null(translated_value)) {
    return(fallback)
  }
  
  translated_value
}

#' Get translated component labels
#'
#' @param language Language code. Supported values are `"es"` and `"en"`.
#'
#' @return A named character vector.
#' @export
get_component_labels <- function(language = "es") {
  validate_language(language)
  
  component_keys <- c(
    "sodium",
    "potassium",
    "ionized_calcium",
    "other_cations",
    "chloride",
    "bicarbonate",
    "lactate",
    "albumin",
    "residual_anions"
  )
  
  labels <- vapply(
    component_keys,
    translate_text,
    character(1),
    language = language
  )
  
  stats::setNames(labels, component_keys)
} 

#' Get translated Gamblegram plot labels
#'
#' @param language Language code. Supported values are `"es"` and `"en"`.
#'
#' @return A list containing translated component labels, axis labels, and
#'   vertical-axis label.
#' @export
get_plot_labels <- function(language = "es") {
  validate_language(language)
  
  list(
    components = get_component_labels(language),
    sides = c(
      cation = translate_text("cations", language),
      anion = translate_text("anions", language)
    ),
    y_axis = translate_text(
      "charge_contribution",
      language
    )
  )
}