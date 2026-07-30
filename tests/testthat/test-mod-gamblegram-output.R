test_that("output module builds Gamblegram data", {
  chemistry_values <- shiny::reactiveVal(
    list(
      sodium = 140,
      potassium = 4,
      chloride = 104,
      bicarbonate = 24,
      albumin = 4,
      lactate = 1,
      ionized_calcium = 1.2
    )
  )
  
  selected_language <- shiny::reactiveVal("es")
  
  shiny::testServer(
    mod_gamblegram_output_server,
    args = list(
      chemistry = chemistry_values,
      language = selected_language
    ),
    {
      result <- gamblegram_data()
      
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 9)
      
      totals <- result |>
        dplyr::group_by(side) |>
        dplyr::summarise(
          total = sum(value),
          .groups = "drop"
        )
      
      expect_equal(
        totals$total[totals$side == "cation"],
        totals$total[totals$side == "anion"]
      )
    }
  )
})

test_that("output module builds the expected anion gap summary", {
  chemistry_values <- shiny::reactiveVal(
    list(
      sodium = 140,
      potassium = 4,
      chloride = 104,
      bicarbonate = 24,
      albumin = 2,
      lactate = 1,
      ionized_calcium = 1.2
    )
  )
  
  selected_language <- shiny::reactiveVal("en")
  
  shiny::testServer(
    mod_gamblegram_output_server,
    args = list(
      chemistry = chemistry_values,
      language = selected_language
    ),
    {
      result <- gamblegram_summary()
      
      expect_equal(
        result$values$observed_anion_gap,
        12
      )
      
      expect_equal(
        result$values$corrected_anion_gap,
        17
      )
      
      expect_equal(
        result$title,
        "Anion gap summary"
      )
    }
  )
})

test_that("output module reacts to chemistry changes", {
  chemistry_values <- shiny::reactiveVal(
    list(
      sodium = 140,
      potassium = 4,
      chloride = 104,
      bicarbonate = 24,
      albumin = 4,
      lactate = 1,
      ionized_calcium = 1.2
    )
  )
  
  selected_language <- shiny::reactiveVal("es")
  
  shiny::testServer(
    mod_gamblegram_output_server,
    args = list(
      chemistry = chemistry_values,
      language = selected_language
    ),
    {
      expect_equal(
        gamblegram_summary()$values$observed_anion_gap,
        12
      )
      
      chemistry_values(
        list(
          sodium = 140,
          potassium = 4,
          chloride = 110,
          bicarbonate = 20,
          albumin = 4,
          lactate = 1,
          ionized_calcium = 1.2
        )
      )
      
      session$flushReact()
      
      expect_equal(
        gamblegram_summary()$values$observed_anion_gap,
        10
      )
    }
  )
})

test_that("output module reacts to language changes", {
  chemistry_values <- shiny::reactiveVal(
    list(
      sodium = 140,
      potassium = 4,
      chloride = 104,
      bicarbonate = 24,
      albumin = 4,
      lactate = 1,
      ionized_calcium = 1.2
    )
  )
  
  selected_language <- shiny::reactiveVal("es")
  
  shiny::testServer(
    mod_gamblegram_output_server,
    args = list(
      chemistry = chemistry_values,
      language = selected_language
    ),
    {
      expect_equal(
        gamblegram_summary()$title,
        "Resumen de la brecha aniónica"
      )
      
      expect_equal(
        plot_labels()$sides[["cation"]],
        "Cationes"
      )
      
      selected_language("en")
      session$flushReact()
      
      expect_equal(
        gamblegram_summary()$title,
        "Anion gap summary"
      )
      
      expect_equal(
        plot_labels()$sides[["cation"]],
        "Cations"
      )
    }
  )
})

test_that("output module generates a ggplot", {
  chemistry_values <- shiny::reactiveVal(
    list(
      sodium = 140,
      potassium = 4,
      chloride = 104,
      bicarbonate = 24,
      albumin = 4,
      lactate = 1,
      ionized_calcium = 1.2
    )
  )
  
  selected_language <- shiny::reactiveVal("es")
  
  shiny::testServer(
    mod_gamblegram_output_server,
    args = list(
      chemistry = chemistry_values,
      language = selected_language
    ),
    {
      labels <- plot_labels()
      
      plot <- plot_gamblegram(
        data = gamblegram_data(),
        labels = labels$components,
        axis_labels = labels$sides,
        y_label = labels$y_axis
      )
      
      expect_s3_class(plot, "ggplot")
    }
  )
})