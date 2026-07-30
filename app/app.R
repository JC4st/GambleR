library(shiny)
library(GambleR)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        .gamblegram-sidebar {
          padding: 1rem;
          border-right: 1px solid #dddddd;
        }

        .gamblegram-summary {
          margin-top: 1.5rem;
          padding: 1rem;
          border: 1px solid #dddddd;
          border-radius: 0.5rem;
        }

        .gamblegram-notes {
          margin-top: 1rem;
          padding: 1rem;
          background: #f5f5f5;
          border-radius: 0.5rem;
        }
        "
      )
    )
  ),
  
  titlePanel("GambleR"),
  
  sidebarLayout(
    sidebarPanel(
      class = "gamblegram-sidebar",
      mod_gamblegram_inputs_ui("inputs")
    ),
    
    mainPanel(
      mod_gamblegram_output_ui("output")
    )
  )
)

server <- function(input, output, session) {
  inputs <- mod_gamblegram_inputs_server("inputs")
  
  mod_gamblegram_output_server(
    id = "output",
    chemistry = inputs$chemistry,
    language = inputs$language
  )
}

shinyApp(
  ui = ui,
  server = server
)