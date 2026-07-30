#' Plot a Gamblegram
#'
#' Creates a stacked bar chart from canonical Gamblegram data.
#'
#' The function is independent from Shiny and does not perform physiological
#' calculations. It expects the output of `build_gamblegram_data()`.
#'
#' @param data A tibble produced by `build_gamblegram_data()`.
#' @param labels Optional named character vector used to replace internal
#'   component names with display labels.
#' @param axis_labels Named character vector containing labels for `cation`
#'   and `anion`.
#' @param y_label Character label for the vertical axis.
#' @param show_values Logical. Should segment values be displayed?
#' @param minimum_label_value Minimum segment value required to display its
#'   numeric label. Smaller segments remain visible but are not labeled.
#'
#' @return A `ggplot` object.
#' @export
plot_gamblegram <- function(
    data,
    labels = NULL,
    axis_labels = c(
      cation = "Cations",
      anion = "Anions"
    ),
    y_label = "Charge contribution (mEq/L)",
    show_values = TRUE,
    minimum_label_value = 4
) {
  required_columns <- c(
    "side",
    "component",
    "translation_key",
    "value",
    "display_order",
    "is_residual"
  )
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0L) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  if (!is.numeric(data$value)) {
    stop("Column `value` must be numeric.", call. = FALSE)
  }
  
  if (any(!is.finite(data$value))) {
    stop("Column `value` must contain only finite values.", call. = FALSE)
  }
  
  if (any(data$value < 0)) {
    stop("Column `value` cannot contain negative values.", call. = FALSE)
  }
  
  if (!is.null(labels)) {
    if (!is.character(labels) || is.null(names(labels))) {
      stop("`labels` must be a named character vector.", call. = FALSE)
    }
  }
  
  if (
    !is.character(axis_labels) ||
    is.null(names(axis_labels)) ||
    !all(c("cation", "anion") %in% names(axis_labels))
  ) {
    stop(
      paste(
        "`axis_labels` must be a named character vector",
        "containing `cation` and `anion`."
      ),
      call. = FALSE
    )
  }
  
  if (
    !is.character(y_label) ||
    length(y_label) != 1L ||
    is.na(y_label)
  ) {
    stop("`y_label` must be a single character value.", call. = FALSE)
  }
  
  if (
    !is.numeric(minimum_label_value) ||
    length(minimum_label_value) != 1L ||
    !is.finite(minimum_label_value) ||
    minimum_label_value < 0
  ) {
    stop(
      "`minimum_label_value` must be a single non-negative numeric value.",
      call. = FALSE
    )
  }
  
  plot_data <- data |>
    dplyr::mutate(
      side = factor(
        side,
        levels = c("cation", "anion")
      ),
      component_label = component
    )
  
  if (!is.null(labels)) {
    plot_data <- plot_data |>
      dplyr::mutate(
        component_label = dplyr::coalesce(
          unname(labels[translation_key]),
          component
        )
      )
  }
  
  plot_data <- plot_data |>
    dplyr::group_by(side) |>
    dplyr::arrange(display_order, .by_group = TRUE) |>
    dplyr::ungroup()
  
  component_levels <- plot_data |>
    dplyr::arrange(side, display_order) |>
    dplyr::pull(component_label) |>
    unique()
  
  plot_data <- plot_data |>
    dplyr::mutate(
      component_label = factor(
        component_label,
        levels = component_levels
      )
    )
  
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = side,
      y = value,
      fill = component_label
    )
  ) +
    ggplot2::geom_col(
      width = 0.65,
      colour = "white",
      linewidth = 0.4
    ) +
    ggplot2::labs(
      x = NULL,
      y = y_label,
      fill = NULL
    ) +
    ggplot2::scale_x_discrete(
      labels = axis_labels
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"
    )
  
  if (isTRUE(show_values)) {
    plot <- plot +
      ggplot2::geom_text(
        ggplot2::aes(
          label = dplyr::if_else(
            value >= minimum_label_value,
            sprintf("%.1f", value),
            ""
          )
        ),
        position = ggplot2::position_stack(vjust = 0.5),
        size = 3.5
      )
  }
  
  plot
}