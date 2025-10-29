

test_that("point_chart produces static chart", {
  library(epiviz)
  # Create test dataframe from epiviz::lab_data
  data <- epiviz::lab_data |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = data,
    x = "specimen_month",
    y = "detections",
    point_colours = "#007C91",
    point_size = 3,
    x_limit_min = "2022-01-01",
    x_limit_max = "2023-12-31",
    chart_title = "Detections per Month 2022-2023",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    x_axis_date_breaks = "2 months"
  )

  # Create static point chart
  result <- point_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})


# basic line chart with custom arguments not included in params
test_that("point_chart produces static chart", {
  library(epiviz)
  # Create test dataframe from epiviz::lab_data
  data <- epiviz::lab_data |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = data,
    x = "specimen_month",
    y = "detections",
    point_colours = "#007C91",
    point_size = 3,
    x_limit_min = "2022-01-01",
    x_limit_max = "2023-12-31",
    chart_title = "Detections per Month 2022-2023",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    x_axis_date_breaks = "2 months"
  )

  # Create static point chart
  result <- point_chart(params = params, dynamic = FALSE,
                        alpha = 0.7) # Test passing alpha through ...)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("point_chart produces dynamic chart", {
  library(epiviz)
  # Create test dataframe from epiviz::lab_data
  data <- epiviz::lab_data |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = data,
    x = "specimen_month",
    y = "detections",
    point_colours = "#007C91",
    point_size = 3,
    x_limit_min = "2022-01-01",
    x_limit_max = "2023-12-31",
    chart_title = "Detections per Month 2022-2023",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    x_axis_date_breaks = "2 months"
  )

  # Create static point chart
  result <- point_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("point_chart produces static chart with grouped data", {
  library(epiviz)
  # Create test dataframe from epiviz::lab_data
  data <- epiviz::lab_data |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
             organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:50,1),
           upper_limit = detections + sample(10:50,1)) |>
    ungroup()

  # Create params list
  params <- list(
    df = data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    point_colours = c("#007C91","#8A1B61","#FF7F32"),
    point_size = 3,
    x_limit_min = "2022-01-01",
    x_limit_max = "2023-12-31",
    chart_title = "Detections per Month 2022-2023",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    x_axis_date_breaks = "2 months",
    y_axis_break_labels = seq(0, 600, 100),
    x_axis_label_angle = 45,
    ci = "ribbon",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("#007C91","#8A1B61","#FF7F32"),
    hline = c(450,550),
    hline_colour = c("blue","red"),
    hline_label = c("threshold 1", "threshold 2"),
    hline_label_colour = c("blue","red")
  )

  # Create static point chart
  result <- point_chart(params = params, dynamic = FALSE)

  # check that the output is a plotly object
  expect_true(inherits(result, "ggplot"))

})





test_that("point_chart produces dynamic chart with grouped data", {
  library(epiviz)
  # Create test dataframe from epiviz::lab_data
  data <- epiviz::lab_data |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
             organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:50,1),
           upper_limit = detections + sample(10:50,1)) |>
    ungroup()

  # Create params list
  params <- list(
    df = data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    point_colours = c("#007C91","#8A1B61","#FF7F32"),
    point_size = 3,
    x_limit_min = "2022-01-01",
    x_limit_max = "2023-12-31",
    chart_title = "Detections per Month 2022-2023",
    x_axis_title = "Month of detection",
    y_axis_title = "Number of detections",
    x_axis_date_breaks = "2 months",
    y_axis_break_labels = seq(0, 600, 100),
    x_axis_label_angle = 45,
    ci = "ribbon",
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    ci_colours = c("#007C91","#8A1B61","#FF7F32"),
    hline = c(450,550),
    hline_colour = c("blue","red"),
    hline_label = c("threshold 1", "threshold 2"),
    hline_label_colour = c("blue","red")
  )

  # Create static point chart
  result <- point_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})




