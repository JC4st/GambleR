#' Validate chemistry values for the user interface
#'
#' Returns translated validation messages without throwing an error.
#' Extreme but non-negative finite values are allowed.
#'
#' @param values Named list of chemistry values.
#' @param language Language code.
#'
#' @return A character vector. An empty vector means that validation passed.
#' @export
validate_chemistry_for_ui <- function(
    values,
    language = "es"
) {
  validate_language(language)
  
  expected_names <- c(
    "sodium",
    "potassium",
    "chloride",
    "bicarbonate",
    "albumin",
    "lactate",
    "ionized_calcium"
  )
  
  if (
    !is.list(values) ||
    !all(expected_names %in% names(values))
  ) {
    return(
      translate_text(
        "validation_missing",
        language
      )
    )
  }
  
  selected_values <- values[expected_names]
  
  is_complete_scalar <- vapply(
    selected_values,
    function(value) {
      length(value) == 1L &&
        !is.null(value) &&
        !is.na(value)
    },
    logical(1)
  )
  
  if (!all(is_complete_scalar)) {
    return(
      translate_text(
        "validation_missing",
        language
      )
    )
  }
  
  is_valid_number <- vapply(
    selected_values,
    function(value) {
      is.numeric(value) && is.finite(value)
    },
    logical(1)
  )
  
  if (!all(is_valid_number)) {
    return(
      translate_text(
        "validation_invalid_number",
        language
      )
    )
  }
  
  numeric_values <- unlist(
    selected_values,
    use.names = TRUE
  )
  
  if (any(numeric_values < 0)) {
    return(
      translate_text(
        "validation_negative",
        language
      )
    )
  }
  
  character(0)
}