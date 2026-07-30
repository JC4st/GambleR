#' Estimate graphical albumin charge
#'
#' This simplified estimate is used only for the Gamblegram visualization:
#' `2.8 * albumin_g_dl`.
#'
#' @param albumin Albumin concentration in g/dL.
#'
#' @return Estimated albumin charge in mEq/L.
#' @export
albumin_charge <- function(albumin) {
  if (!is.numeric(albumin) || length(albumin) != 1L || !is.finite(albumin)) {
    stop("Albumin must be a finite numeric scalar.", call. = FALSE)
  }

  if (albumin < 0) {
    stop("Albumin cannot be negative.", call. = FALSE)
  }

  ALBUMIN_CHARGE_FACTOR * albumin
}
