
### SECTION 1 - BASIC LINE CHARTS ###############################################



test_that("line_chart produces a static chart", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    chart_title = "Detections per Month",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    x_axis_date_breaks = "2 months"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    chart_title = "Detections per Month",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    x_axis_date_breaks = "2 months"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart produces a static chart with grouped data", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    line_types = c("solid", "dashed", "dotted"),
    chart_title = "Detections per Month by Species",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    x_axis_date_breaks = "2 months"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with grouped data", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    line_types = c("solid", "dashed", "dotted"),
    chart_title = "Detections per Month by Species",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    x_axis_date_breaks = "2 months"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart static and dynamic outputs use the same data", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    chart_title = "Detections per Month",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create both outputs
  static_result <- line_chart(params = params, dynamic = FALSE)
  dynamic_result <- line_chart(params = params, dynamic = TRUE)

  # Extract data from ggplot
  gg_data <- ggplot2::ggplot_build(static_result)$data[[1]]

  # Extract data from plotly
  pl_data <- plotly::plotly_build(dynamic_result)$x$data[[1]]

  # Check that both outputs contain the same number of data points
  expect_equal(nrow(gg_data), length(pl_data$x))

})





### SECTION 2 - NAMED COLOURS ###################################################



test_that("line_chart produces a static chart with named line_colours", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list with named colour vector
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Detections per Month by Species",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with named line_colours", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list with named colour vector
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Detections per Month by Species",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 3 - HLINES ##########################################################



test_that("line_chart produces a static chart with grouped data and multiple hlines", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    chart_title = "Detections per Month by Species",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    hline = c(50, 100),
    hline_colour = c("blue", "red"),
    hline_label = c("threshold 1", "threshold 2"),
    hline_label_colour = c("blue", "red")
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with grouped data and multiple hlines", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    chart_title = "Detections per Month by Species",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    hline = c(50, 100),
    hline_colour = c("blue", "red"),
    hline_label = c("threshold 1", "threshold 2"),
    hline_label_colour = c("blue", "red")
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 4 - POINTS ##########################################################



test_that("line_chart produces a static chart with points", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    add_points = TRUE,
    add_points_size = 2,
    chart_title = "Detections per Month",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with points", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    add_points = TRUE,
    add_points_size = 2,
    chart_title = "Detections per Month",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart produces a static chart with grouped data and points", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    add_points = TRUE,
    add_points_size = 1.5,
    chart_title = "Detections per Month by Species",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with grouped data and points", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    add_points = TRUE,
    add_points_size = 1.5,
    chart_title = "Detections per Month by Species",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart does not show points when add_points is FALSE", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list with add_points = FALSE
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    add_points = FALSE
  )

  # Create static line chart
  static_result <- line_chart(params = params, dynamic = FALSE)

  # Check that no geom_point layer exists in the static chart
  layer_classes <- sapply(static_result$layers, function(l) class(l$geom)[1])
  expect_false("GeomPoint" %in% layer_classes)

  # Create dynamic line chart
  dynamic_result <- line_chart(params = params, dynamic = TRUE)

  # Check that the dynamic chart uses 'lines' mode (not 'lines+markers')
  pl_data <- plotly::plotly_build(dynamic_result)$x$data
  line_traces <- pl_data[sapply(pl_data, function(t) !is.null(t$mode))]
  expect_true(all(sapply(line_traces, function(t) t$mode == "lines")))

})





### SECTION 5 - CONFIDENCE INTERVALS: ERRORBARS #################################



test_that("line_chart produces a static chart with errorbars", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 20,
                  upper_limit = detections + 20)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    chart_title = "Detections per Month with Error Bars",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with errorbars", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 20,
                  upper_limit = detections + 20)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    chart_title = "Detections per Month with Error Bars",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart produces a static chart with grouped data and errorbars", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("red", "red", "red"),
    chart_title = "Detections per Month by Species with Error Bars",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with grouped data and errorbars", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("red", "red", "red"),
    chart_title = "Detections per Month by Species with Error Bars",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart produces a static chart with grouped errorbars and named ci_colours", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list with named ci_colours
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("KLEBSIELLA PNEUMONIAE" = "red",
                   "STAPHYLOCOCCUS AUREUS" = "blue",
                   "PSEUDOMONAS AERUGINOSA" = "green"),
    chart_title = "Detections with Named CI Colours",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with grouped errorbars and named ci_colours", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list with named ci_colours
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("KLEBSIELLA PNEUMONIAE" = "red",
                   "STAPHYLOCOCCUS AUREUS" = "blue",
                   "PSEUDOMONAS AERUGINOSA" = "green"),
    chart_title = "Detections with Named CI Colours",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart produces a static chart with grouped errorbars and single ci_colours", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list with single ci_colours value
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = "red",
    chart_title = "Detections with Single CI Colour",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with grouped errorbars and single ci_colours", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list with single ci_colours value
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = "red",
    chart_title = "Detections with Single CI Colour",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart produces a static chart with errorbars and ci_legend = TRUE", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("red", "blue", "green"),
    ci_legend = TRUE,
    ci_legend_title = "CI Bounds",
    chart_title = "Errorbars with CI Legend"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with errorbars and ci_legend = TRUE", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("red", "blue", "green"),
    ci_legend = TRUE,
    ci_legend_title = "CI Bounds",
    chart_title = "Errorbars with CI Legend"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

  # Note:- ci_legend not expected for dynamic chart when ci_legend = 'errorbar'
  #    as plotly does not support this

})



test_that("line_chart produces a static chart with errorbars and ci_legend = FALSE", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("red", "red", "red"),
    ci_legend = FALSE,
    chart_title = "Errorbars without CI Legend"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with errorbars and ci_legend = FALSE", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("red", "red", "red"),
    ci_legend = FALSE,
    chart_title = "Errorbars without CI Legend"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 6 - CONFIDENCE INTERVALS: RIBBONS ###################################



test_that("line_chart produces a static chart with ribbon", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 20,
                  upper_limit = detections + 20)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    ci = "ribbon",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = "#007C91",
    chart_title = "Detections per Month with Ribbon",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with ribbon", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 20,
                  upper_limit = detections + 20)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    ci = "ribbon",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = "#007C91",
    chart_title = "Detections per Month with Ribbon",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart produces a static chart with grouped data and ribbon", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    ci = "ribbon",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    chart_title = "Detections per Month by Species with Ribbon",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with grouped data and ribbon", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    ci = "ribbon",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    chart_title = "Detections per Month by Species with Ribbon",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart produces a static chart with grouped ribbon and named ci_colours", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list with named ci_colours
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = "ribbon",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                   "KLEBSIELLA PNEUMONIAE" = "#007C91",
                   "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Detections with Named CI Colours",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with grouped ribbon and named ci_colours", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list with named ci_colours
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = "ribbon",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                   "KLEBSIELLA PNEUMONIAE" = "#007C91",
                   "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Detections with Named CI Colours",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 7 - X-AXIS LIMITS ###################################################



test_that("line_chart produces a static chart with x-axis limits", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list with x-axis limits
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    x_limit_min = "2023-03-01",
    x_limit_max = "2023-09-30",
    chart_title = "Detections per Month (Limited Range)",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with x-axis limits", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list with x-axis limits
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    x_limit_min = "2023-03-01",
    x_limit_max = "2023-09-30",
    chart_title = "Detections per Month (Limited Range)",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart produces a static chart with x-axis limits and hline labels", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list with x-axis limits and hlines
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    x_limit_min = "2023-03-01",
    x_limit_max = "2023-09-30",
    hline = c(100, 200),
    hline_colour = c("red", "blue"),
    hline_label = c("Low threshold", "High threshold"),
    hline_label_colour = c("red", "blue"),
    chart_title = "Detections with Limits and HLines",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with x-axis limits and hline labels", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list with x-axis limits and hlines
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    x_limit_min = "2023-03-01",
    x_limit_max = "2023-09-30",
    hline = c(100, 200),
    hline_colour = c("red", "blue"),
    hline_label = c("Low threshold", "High threshold"),
    hline_label_colour = c("red", "blue"),
    chart_title = "Detections with Limits and HLines",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 8 - COMBINED FEATURES ###############################################



test_that("line_chart produces a static chart with grouped data, points, ribbon, and hlines", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list with all features
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    line_types = c("solid", "dashed", "dotted"),
    add_points = TRUE,
    ci = "ribbon",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    chart_title = "Detections per Month by Species",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    x_axis_date_breaks = "2 months",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    legend_title = "Species",
    hline = c(50, 100),
    hline_colour = c("blue", "red"),
    hline_label = c("threshold 1", "threshold 2"),
    hline_label_colour = c("blue", "red")
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with grouped data, points, ribbon, and hlines", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list with all features
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    line_types = c("solid", "dashed", "dotted"),
    add_points = TRUE,
    ci = "ribbon",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    chart_title = "Detections per Month by Species",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    x_axis_date_breaks = "2 months",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    legend_title = "Species",
    hline = c(50, 100),
    hline_colour = c("blue", "red"),
    hline_label = c("threshold 1", "threshold 2"),
    hline_label_colour = c("blue", "red")
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart produces a static chart with grouped data, points, errorbars, and x-axis limits", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    add_points = TRUE,
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("red", "red", "red"),
    x_limit_min = "2023-03-01",
    x_limit_max = "2023-09-30",
    chart_title = "Grouped Errorbars with Limits",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with grouped data, points, errorbars, and x-axis limits", {

  # Define test data with deterministic CI bounds
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    add_points = TRUE,
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("red", "red", "red"),
    x_limit_min = "2023-03-01",
    x_limit_max = "2023-09-30",
    chart_title = "Grouped Errorbars with Limits",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 9 - LEGEND AND DISPLAY OPTIONS ######################################



test_that("line_chart produces a static chart with legend_pos = 'bottom'", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    legend_pos = "bottom",
    legend_title = "Species",
    chart_title = "Legend at Bottom"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with legend_pos = 'bottom'", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    legend_pos = "bottom",
    legend_title = "Species",
    chart_title = "Legend at Bottom"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("line_chart produces a static chart with show_gridlines = FALSE", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    show_gridlines = FALSE,
    chart_title = "No Gridlines"
  )

  # Create static line chart
  result <- line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("line_chart produces a dynamic chart with show_gridlines = FALSE", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    show_gridlines = FALSE,
    chart_title = "No Gridlines"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})




test_that("line_chart produces a dynamic chart with hover_labels", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(hover_text = paste0("Month: ", specimen_month, "\nDetections: ", detections))

  # Create params list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    hover_labels = "hover_text",
    chart_title = "Chart with Hover Labels"
  )

  # Create dynamic line chart
  result <- line_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 10 - VALIDATION AND ERROR HANDLING ###################################



test_that("line_chart errors when df is not provided", {

  params <- list(
    x = "specimen_month",
    y = "detections"
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "A data frame argument is required")

})



test_that("line_chart errors when df is not a data frame", {

  params <- list(
    df = "not_a_dataframe",
    x = "specimen_month",
    y = "detections"
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "df is not a data frame object")

})



test_that("line_chart errors when df is empty", {

  params <- list(
    df = data.frame(),
    x = "specimen_month",
    y = "detections"
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "df is empty")

})



test_that("line_chart errors when x is not provided", {

  test_data <- lab_data |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  params <- list(
    df = test_data,
    y = "detections"
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "Please include a variable from df for x")

})



test_that("line_chart errors when y is not provided", {

  test_data <- lab_data |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  params <- list(
    df = test_data,
    x = "specimen_month"
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "Please include a variable from df for y")

})



test_that("line_chart errors when x variable is not in df", {

  test_data <- lab_data |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  params <- list(
    df = test_data,
    x = "nonexistent_column",
    y = "detections"
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "x not found within df")

})



test_that("line_chart errors when y variable is not in df", {

  test_data <- lab_data |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "nonexistent_column"
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "y not found within df")

})



test_that("line_chart errors when line_colours count does not match group count", {

  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61")
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "The number of line_colours provided must equal the number of unique groups")

})



test_that("line_chart errors when ci_colours count does not match group count", {

  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
                    organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(lower_limit = detections - 15,
                  upper_limit = detections + 15)

  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
    ci = "errorbar",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("red", "blue")
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "The number of ci_colours provided must equal the number of unique groups")

})



test_that("line_chart errors when ci is provided without ci_lower and ci_upper", {

  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    ci = "errorbar"
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "Please provide arguements for 'ci_upper' and 'ci_lower'")

})



test_that("line_chart errors with invalid line_types", {

  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    line_types = "invalid_type"
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "Invalid line type")

})



test_that("line_chart errors when x_axis_break_labels and x_axis_n_breaks both provided", {

  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    x_axis_break_labels = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "month"),
    x_axis_n_breaks = 6
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "x_axis_break_labels cannot be provided with x_axis_n_breaks")

})



test_that("line_chart errors when x_axis_break_labels and x_axis_date_breaks both provided", {

  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91",
    x_axis_break_labels = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "month"),
    x_axis_date_breaks = "2 months"
  )

  expect_error(line_chart(params = params, dynamic = FALSE),
               "x_axis_break_labels cannot be provided with x_axis_date_breaks")

})



test_that("line_chart errors when base is wrong type for dynamic output", {

  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91"
  )

  # Passing a ggplot base to a dynamic chart should error
  gg_base <- ggplot2::ggplot()
  expect_error(line_chart(params = params, dynamic = TRUE, base = gg_base),
               "base must be a plotly object if dynamic = TRUE")

})



test_that("line_chart errors when base is wrong type for static output", {

  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    line_colours = "#007C91"
  )

  # Passing a plotly base to a static chart should error
  pl_base <- plotly::plot_ly()
  expect_error(line_chart(params = params, dynamic = FALSE, base = pl_base),
               "base must be a ggplot object if dynamic = FALSE")

})
