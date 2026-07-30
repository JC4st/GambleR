#' Gamblegram inputs module UI
#'
#' @param id Shiny module identifier.
#'
#' @return A Shiny tag list.
#' @export
mod_gamblegram_inputs_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::selectInput(
      inputId = ns("language"),
      label = NULL,
      choices = c(
        "Espa\u00f1ol" = "es",
        "English" = "en"
      ),
      selected = "es"
    ), 
    
    shiny::numericInput(
      inputId = ns("sodium"),
      label = "Sodio",
      value = 140,
      min = 0,
      step = 1
    ),
    
    shiny::numericInput(
      inputId = ns("potassium"),
      label = "Potasio",
      value = 4,
      min = 0,
      step = 0.1
    ),
    
    shiny::numericInput(
      inputId = ns("chloride"),
      label = "Cloro",
      value = 104,
      min = 0,
      step = 1
    ),
    
    shiny::numericInput(
      inputId = ns("bicarbonate"),
      label = "Bicarbonato / CO2 total",
      value = 24,
      min = 0,
      step = 1
    ),
    
    shiny::numericInput(
      inputId = ns("albumin"),
      label = "Alb\u00famina (g/dL)",
      value = 4,
      min = 0,
      step = 0.1
    ),
    
    shiny::numericInput(
      inputId = ns("lactate"),
      label = "Lactato (mmol/L)",
      value = 1,
      min = 0,
      step = 0.1
    ),
    
    shiny::numericInput(
      inputId = ns("ionized_calcium"),
      label = "Calcio ionizado (mmol/L)",
      value = 1.2,
      min = 0,
      step = 0.01
    )
  )
}

#' Gamblegram inputs module server
#'
#' @param id Shiny module identifier.
#'
#' @return A list containing reactive expressions for language and chemistry.
#' @export
mod_gamblegram_inputs_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      language <- shiny::reactive({
        input$language %||% "es"
      })
      
      shiny::observeEvent(
        language(),
        {
          language_value <- language()
          
          shiny::updateNumericInput(
            session = session,
            inputId = "sodium",
            label = translate_text("sodium", language_value)
          )
          
          shiny::updateNumericInput(
            session = session,
            inputId = "potassium",
            label = translate_text("potassium", language_value)
          )
          
          shiny::updateNumericInput(
            session = session,
            inputId = "chloride",
            label = translate_text("chloride", language_value)
          )
          
          shiny::updateNumericInput(
            session = session,
            inputId = "bicarbonate",
            label = translate_text("bicarbonate", language_value)
          )
          
          shiny::updateNumericInput(
            session = session,
            inputId = "albumin",
            label = paste0(
              translate_text("albumin", language_value),
              " (g/dL)"
            )
          )
          
          shiny::updateNumericInput(
            session = session,
            inputId = "lactate",
            label = paste0(
              translate_text("lactate", language_value),
              " (mmol/L)"
            )
          )
          
          shiny::updateNumericInput(
            session = session,
            inputId = "ionized_calcium",
            label = paste0(
              translate_text("ionized_calcium", language_value),
              " (mmol/L)"
            )
          )
        },
        ignoreInit = FALSE
      )
      
      chemistry <- shiny::reactive({
        list(
          sodium = input$sodium,
          potassium = input$potassium,
          chloride = input$chloride,
          bicarbonate = input$bicarbonate,
          albumin = input$albumin,
          lactate = input$lactate,
          ionized_calcium = input$ionized_calcium
        )
      })
        

        
        

      
      list(
        language = language,
        chemistry = chemistry
      )
    }
  )
}