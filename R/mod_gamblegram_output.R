#' Gamblegram output module UI
#'
#' @param id Shiny module identifier.
#'
#' @return A Shiny tag list.
#' @export
mod_gamblegram_output_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::plotOutput(
      outputId = ns("gamblegram_plot"),
      height = "600px"
    ),

    shiny::uiOutput(
      outputId = ns("anion_gap_summary")
    ),

    shiny::uiOutput(
      outputId = ns("educational_notes")
    )
  )
}

#' Gamblegram output module server
#'
#' @param id Shiny module identifier.
#' @param chemistry Reactive expression containing chemistry values.
#' @param language Reactive expression containing `"es"` or `"en"`.
#'
#' @return Invisibly returns a list of internal reactive expressions, primarily
#'   for testing.
#' @export
mod_gamblegram_output_server <- function(
    id,
    chemistry,
    language
    ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      validated_chemistry <- shiny::reactive({
        values <- chemistry()
        
        issues <- validate_chemistry_for_ui(
          values = values,
          language = language()
        )
        
        shiny::validate(
          shiny::need(
            length(issues) == 0L,
            paste(issues, collapse = "\n")
          )
        )
        
        values
      })
      gamblegram_data <- shiny::reactive({
        values <- validated_chemistry()

        build_gamblegram_data(
          sodium = values$sodium,
          potassium = values$potassium,
          chloride = values$chloride,
          bicarbonate = values$bicarbonate,
          albumin = values$albumin,
          lactate = values$lactate,
          ionized_calcium = values$ionized_calcium
        )
      })

      gamblegram_summary <- shiny::reactive({
        values <- validated_chemistry()

        build_gamblegram_summary(
          sodium = values$sodium,
          chloride = values$chloride,
          bicarbonate = values$bicarbonate,
          albumin = values$albumin,
          language = language()
        )
      })

      plot_labels <- shiny::reactive({
        get_plot_labels(language())
      })

      output$gamblegram_plot <- shiny::renderPlot({
        labels <- plot_labels()

        plot_gamblegram(
          data = gamblegram_data(),
          labels = labels$components,
          axis_labels = labels$sides,
          y_label = labels$y_axis
        )
      })

      output$anion_gap_summary <- shiny::renderUI({
        summary <- gamblegram_summary()

        shiny::tags$section(
          class = "gamblegram-summary",

          shiny::tags$h3(summary$title),

          shiny::tags$p(
            shiny::tags$strong(
              summary$labels$observed_anion_gap
            ),
            paste0(
              ": ",
              summary$formatted_values$observed_anion_gap,
              " ",
              summary$unit
            )
          ),

          shiny::tags$p(
            shiny::tags$strong(
              summary$labels$corrected_anion_gap
            ),
            paste0(
              ": ",
              summary$formatted_values$corrected_anion_gap,
              " ",
              summary$unit
            )
          )
        )
      })

      output$educational_notes <- shiny::renderUI({
        summary <- gamblegram_summary()

        shiny::tags$aside(
          class = "gamblegram-notes",

          shiny::tags$p(
            summary$notes$albumin_correction
          ),

          shiny::tags$p(
            summary$notes$residual_compartments
          ),

          shiny::tags$p(
            shiny::tags$strong(
              summary$notes$disclaimer
            )
          )
        )
      })

      invisible(
        list(
          validated_chemistry = validated_chemistry,
          data = gamblegram_data,
          summary = gamblegram_summary,
          plot_labels = plot_labels
        )
      )
    }
  )
}