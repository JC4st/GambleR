test_that("plot_gamblegram returns a ggplot object", {
  data <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  plot <- plot_gamblegram(data)
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_gamblegram accepts translated labels", {
  data <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  labels <- c(
    sodium = "Sodio",
    potassium = "Potasio",
    ionized_calcium = "Calcio ionizado",
    other_cations = "Otros cationes",
    chloride = "Cloro",
    bicarbonate = "Bicarbonato",
    lactate = "Lactato",
    albumin = "Albúmina",
    residual_anions = "Aniones residuales"
  )
  
  plot <- plot_gamblegram(
    data = data,
    labels = labels
  )
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_gamblegram detects missing columns", {
  incomplete_data <- tibble::tibble(
    side = "cation",
    value = 140
  )
  
  expect_error(
    plot_gamblegram(incomplete_data),
    "Missing required columns"
  )
})

test_that("plot_gamblegram rejects negative values", {
  data <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  data$value[1] <- -1
  
  expect_error(
    plot_gamblegram(data),
    "cannot contain negative"
  )
})

test_that("plot_gamblegram requires named labels", {
  data <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  expect_error(
    plot_gamblegram(
      data = data,
      labels = c("Sodio", "Potasio")
    ),
    "named character vector"
  )
})

test_that("plot_gamblegram can omit values", {
  data <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  plot <- plot_gamblegram(
    data = data,
    show_values = FALSE
  )
  
  expect_s3_class(plot, "ggplot")
  
  geom_classes <- vapply(
    plot$layers,
    function(layer) class(layer$geom)[1],
    character(1)
  )
  
  expect_false("GeomText" %in% geom_classes)
})

test_that("plot_gamblegram accepts translated plot labels", {
  data <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  plot_labels <- get_plot_labels("es")
  
  plot <- plot_gamblegram(
    data = data,
    labels = plot_labels$components,
    axis_labels = plot_labels$sides,
    y_label = plot_labels$y_axis
  )
  
  expect_s3_class(plot, "ggplot")
  
  expect_equal(
    plot$labels$y,
    "Contribución de carga (mEq/L)"
  )
})

test_that("plot_gamblegram validates side labels", {
  data <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  expect_error(
    plot_gamblegram(
      data = data,
      axis_labels = c(cation = "Cationes")
    ),
    "containing `cation` and `anion`"
  )
})

test_that("plot_gamblegram validates the y-axis label", {
  data <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  expect_error(
    plot_gamblegram(
      data = data,
      y_label = c("Carga", "Charge")
    ),
    "single character value"
  )
})

test_that("small segments do not receive internal labels", {
  data <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  plot <- plot_gamblegram(
    data = data,
    minimum_label_value = 4
  )
  
  built_plot <- ggplot2::ggplot_build(plot)
  text_layer <- built_plot$data[[2]]
  
  visible_labels <- text_layer$label[
    nzchar(text_layer$label)
  ]
  
  expect_false("1.0" %in% visible_labels)
  expect_false("2.4" %in% visible_labels)
  expect_true("4.0" %in% visible_labels)
  expect_true("24.0" %in% visible_labels)
})

test_that("minimum label value is configurable", {
  data <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  plot <- plot_gamblegram(
    data = data,
    minimum_label_value = 1
  )
  
  built_plot <- ggplot2::ggplot_build(plot)
  text_layer <- built_plot$data[[2]]
  
  visible_labels <- text_layer$label[
    nzchar(text_layer$label)
  ]
  
  expect_true("1.0" %in% visible_labels)
  expect_true("2.4" %in% visible_labels)
})

test_that("minimum label value is validated", {
  data <- build_gamblegram_data(
    sodium = 140,
    potassium = 4,
    chloride = 104,
    bicarbonate = 24,
    albumin = 4,
    lactate = 1,
    ionized_calcium = 1.2
  )
  
  expect_error(
    plot_gamblegram(
      data = data,
      minimum_label_value = -1
    ),
    "non-negative numeric"
  )
  
  expect_error(
    plot_gamblegram(
      data = data,
      minimum_label_value = c(2, 4)
    ),
    "non-negative numeric"
  )
})