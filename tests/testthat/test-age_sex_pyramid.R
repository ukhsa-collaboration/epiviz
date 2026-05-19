
### SECTION 1 - BASIC LINE LIST PYRAMIDS ########################################



test_that("age_sex_pyramid produces a static chart from line list (dob_var)", {

  # Create params list
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    fill_colours = c("#440154", "#2196F3"),
    chart_title = "Age-Sex Pyramid",
    x_axis_title = "Number of cases",
    y_axis_title = "Age group (years)"
  )

  # Create static pyramid
  result <- age_sex_pyramid(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("age_sex_pyramid produces a dynamic chart from line list (dob_var)", {

  # Create params list
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    fill_colours = c("#440154", "#2196F3"),
    chart_title = "Age-Sex Pyramid",
    x_axis_title = "Number of cases",
    y_axis_title = "Age group (years)"
  )

  # Create dynamic pyramid
  result <- age_sex_pyramid(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("age_sex_pyramid static and dynamic outputs use the same data (line list)", {

  # Create params list
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf)
  )

  # Create both outputs
  static_result <- age_sex_pyramid(params = params, dynamic = FALSE)
  dynamic_result <- age_sex_pyramid(params = params, dynamic = TRUE)

  # Extract data from ggplot (geom_col layer)
  gg_data <- ggplot2::ggplot_build(static_result)$data[[1]]

  # Extract data from plotly
  pl_data <- plotly::plotly_build(dynamic_result)$x$data

  # Get bar traces only (type == "bar")
  bar_traces <- pl_data[sapply(pl_data, function(t) identical(t$type, "bar"))]

  # Check that both outputs contain the same total number of bars
  total_plotly_bars <- sum(sapply(bar_traces, function(t) length(t$x)))
  expect_equal(nrow(gg_data), total_plotly_bars)

})





### SECTION 2 - PRE-GROUPED DATA ################################################



test_that("age_sex_pyramid produces a static chart from pre-grouped data", {

  # Define pre-grouped test data
  grouped_df <- data.frame(
    age_group = rep(c("0-4", "5-18", "19-64", "65+"), 2),
    sex = rep(c("Male", "Female"), each = 4),
    count = c(100, 120, 150, 80, 90, 110, 140, 70)
  )

  # Create params list
  params <- list(
    df = grouped_df,
    y = "count",
    age_group_var = "age_group",
    sex_var = "sex",
    grouped = TRUE,
    chart_title = "Age-Sex Pyramid (Grouped)",
    x_axis_title = "Number of cases",
    y_axis_title = "Age group (years)"
  )

  # Create static pyramid
  result <- age_sex_pyramid(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("age_sex_pyramid produces a dynamic chart from pre-grouped data", {

  # Define pre-grouped test data
  grouped_df <- data.frame(
    age_group = rep(c("0-4", "5-18", "19-64", "65+"), 2),
    sex = rep(c("Male", "Female"), each = 4),
    count = c(100, 120, 150, 80, 90, 110, 140, 70)
  )

  # Create params list
  params <- list(
    df = grouped_df,
    y = "count",
    age_group_var = "age_group",
    sex_var = "sex",
    grouped = TRUE,
    chart_title = "Age-Sex Pyramid (Grouped)",
    x_axis_title = "Number of cases",
    y_axis_title = "Age group (years)"
  )

  # Create dynamic pyramid
  result <- age_sex_pyramid(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 3 - CONFIDENCE INTERVALS ############################################


#### REDUNDANT TESTS, CI LIMITS ONLY POSSIBLE WITH PRE-AGGREGATED DATA
# test_that("age_sex_pyramid produces a static chart with error bars (line list)", {
#
#   # Create params list
#   params <- list(
#     df = lab_data,
#     dob_var = "date_of_birth",
#     sex_var = "sex",
#     age_breakpoints = c(0, 5, 19, 65, Inf),
#     ci = "errorbar",
#     ci_colours = "red",
#     errorbar_width = 0.5,
#     chart_title = "Age-Sex Pyramid with CI"
#   )
#
#   # Create static pyramid
#   result <- age_sex_pyramid(params = params, dynamic = FALSE)
#
#   # Check that the output is a ggplot object
#   expect_true(inherits(result, "ggplot"))
#
# })
#
#
#
# test_that("age_sex_pyramid produces a dynamic chart with error bars (line list)", {
#
#   # Create params list
#   params <- list(
#     df = lab_data,
#     dob_var = "date_of_birth",
#     sex_var = "sex",
#     age_breakpoints = c(0, 5, 19, 65, Inf),
#     ci = "errorbar",
#     ci_colours = "red",
#     errorbar_width = 0.5,
#     chart_title = "Age-Sex Pyramid with CI"
#   )
#
#   # Create dynamic pyramid
#   result <- age_sex_pyramid(params = params, dynamic = TRUE)
#
#   # Check that the output is a plotly object
#   expect_true(inherits(result, "plotly"))
#
# })



test_that("age_sex_pyramid produces a static chart with error bars (pre-grouped)", {

  # Define pre-grouped test data with CI columns
  grouped_df <- data.frame(
    age_group = rep(c("0-4", "5-18", "19-64", "65+"), 2),
    sex = rep(c("Male", "Female"), each = 4),
    count = c(100, 120, 150, 80, 90, 110, 140, 70),
    lower = c(90, 110, 140, 70, 80, 100, 130, 60),
    upper = c(110, 130, 160, 90, 100, 120, 150, 80)
  )

  # Create params list
  params <- list(
    df = grouped_df,
    y = "count",
    age_group_var = "age_group",
    sex_var = "sex",
    grouped = TRUE,
    ci = "errorbar",
    ci_lower = "lower",
    ci_upper = "upper",
    ci_colours = "red",
    chart_title = "Age-Sex Pyramid with CI (Grouped)"
  )

  # Create static pyramid
  result <- age_sex_pyramid(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("age_sex_pyramid produces a dynamic chart with error bars (pre-grouped)", {

  # Define pre-grouped test data with CI columns
  grouped_df <- data.frame(
    age_group = rep(c("0-4", "5-18", "19-64", "65+"), 2),
    sex = rep(c("Male", "Female"), each = 4),
    count = c(100, 120, 150, 80, 90, 110, 140, 70),
    lower = c(90, 110, 140, 70, 80, 100, 130, 60),
    upper = c(110, 130, 160, 90, 100, 120, 150, 80)
  )

  # Create params list
  params <- list(
    df = grouped_df,
    y = "count",
    age_group_var = "age_group",
    sex_var = "sex",
    grouped = TRUE,
    ci = "errorbar",
    ci_lower = "lower",
    ci_upper = "upper",
    ci_colours = "red",
    chart_title = "Age-Sex Pyramid with CI (Grouped)"
  )

  # Create dynamic pyramid
  result <- age_sex_pyramid(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 4 - BAR WIDTH #######################################################



test_that("age_sex_pyramid produces a static chart with custom bar_width", {

  # Create params list
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    bar_width = 0.6,
    chart_title = "Age-Sex Pyramid (Narrow Bars)"
  )

  # Create static pyramid
  result <- age_sex_pyramid(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("age_sex_pyramid produces a dynamic chart with custom bar_width", {

  # Create params list
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    bar_width = 0.6,
    chart_title = "Age-Sex Pyramid (Narrow Bars)"
  )

  # Create dynamic pyramid
  result <- age_sex_pyramid(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 5 - BAR LABELS ######################################################



test_that("age_sex_pyramid produces a static chart with bar labels (outside)", {

  # Create params list
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    bar_labels = TRUE,
    bar_labels_pos = "bar_outside",
    bar_labels_font_size = 8,
    bar_labels_font_colour = "black",
    chart_title = "Age-Sex Pyramid with Labels (Outside)"
  )

  # Create static pyramid
  result <- age_sex_pyramid(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("age_sex_pyramid produces a dynamic chart with bar labels (outside)", {

  # Create params list
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    bar_labels = TRUE,
    bar_labels_pos = "bar_outside",
    bar_labels_font_size = 8,
    bar_labels_font_colour = "black",
    chart_title = "Age-Sex Pyramid with Labels (Outside)"
  )

  # Create dynamic pyramid
  result <- age_sex_pyramid(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("age_sex_pyramid produces a static chart with bar labels (inside)", {

  # Create params list
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    bar_labels = TRUE,
    bar_labels_pos = "bar_inside",
    bar_labels_font_size = 10,
    bar_labels_font_colour = "white",
    chart_title = "Age-Sex Pyramid with Labels (Inside)"
  )

  # Create static pyramid
  result <- age_sex_pyramid(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("age_sex_pyramid produces a dynamic chart with bar labels (inside)", {

  # Create params list
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    bar_labels = TRUE,
    bar_labels_pos = "bar_inside",
    bar_labels_font_size = 10,
    bar_labels_font_colour = "white",
    chart_title = "Age-Sex Pyramid with Labels (Inside)"
  )

  # Create dynamic pyramid
  result <- age_sex_pyramid(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 6 - SHIFT ORIGIN ####################################################



test_that("age_sex_pyramid produces a static chart with shifted origin", {

  # Create params list
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    shift_origin = TRUE,
    chart_title = "Asymmetric Age-Sex Pyramid",
    fill_colours = c("#007C91", "#8A1B61")
  )

  # Create static pyramid
  result <- age_sex_pyramid(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("age_sex_pyramid produces a dynamic chart with shifted origin", {

  # Create params list
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    shift_origin = TRUE,
    chart_title = "Asymmetric Age-Sex Pyramid",
    fill_colours = c("#007C91", "#8A1B61")
  )

  # Create dynamic pyramid
  result <- age_sex_pyramid(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 7 - CHART CUSTOMISATION #############################################



test_that("age_sex_pyramid produces a static chart with full customisation", {

  # Create params list with all cosmetic options
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    fill_colours = c("#007C91", "#8A1B61"),
    bar_border_colour = "grey30",
    bar_width = 0.8,
    chart_title = "Fully Customised Pyramid",
    chart_title_size = 15,
    chart_title_colour = "#007C91",
    chart_footer = "Source: Simulated data.",
    chart_footer_size = 10,
    chart_footer_colour = "grey50",
    x_axis_title = "Count",
    y_axis_title = "Age band",
    x_axis_title_font_size = 12,
    y_axis_title_font_size = 12,
    x_axis_label_font_size = 10,
    y_axis_label_font_size = 10,
    x_axis_n_breaks = 8,
    show_gridlines = TRUE,
    show_axislines = TRUE,
    legend_title = "Sex",
    legend_pos = "bottom",
    legend_font_size = 10,
    legend_title_font_size = 10
  )

  # Create static pyramid
  result <- age_sex_pyramid(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("age_sex_pyramid produces a dynamic chart with full customisation", {

  # Create params list with all cosmetic options
  params <- list(
    df = lab_data,
    dob_var = "date_of_birth",
    sex_var = "sex",
    age_breakpoints = c(0, 5, 19, 65, Inf),
    fill_colours = c("#007C91", "#8A1B61"),
    bar_border_colour = "grey30",
    bar_width = 0.8,
    chart_title = "Fully Customised Pyramid",
    chart_title_size = 15,
    chart_title_colour = "#007C91",
    chart_footer = "Source: Simulated data.",
    chart_footer_size = 10,
    chart_footer_colour = "grey50",
    x_axis_title = "Count",
    y_axis_title = "Age band",
    x_axis_title_font_size = 12,
    y_axis_title_font_size = 12,
    x_axis_label_font_size = 10,
    y_axis_label_font_size = 10,
    x_axis_n_breaks = 8,
    show_gridlines = TRUE,
    show_axislines = TRUE,
    legend_title = "Sex",
    legend_pos = "bottom",
    legend_font_size = 10,
    legend_title_font_size = 10
  )

  # Create dynamic pyramid
  result <- age_sex_pyramid(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





### SECTION 8 - INPUT VALIDATION ################################################



test_that("age_sex_pyramid throws errors for invalid inputs", {

  # Missing df
  expect_error(
    age_sex_pyramid(params = list()),
    "df = NULL"
  )

  # df is not a data frame
  expect_error(
    age_sex_pyramid(params = list(df = "not_a_dataframe")),
    "df is not a data frame"
  )

  # Empty data frame
  expect_error(
    age_sex_pyramid(params = list(df = data.frame())),
    "df is empty"
  )

  # fill_colours has wrong length
  expect_error(
    age_sex_pyramid(params = list(
      df = lab_data,
      dob_var = "date_of_birth",
      fill_colours = c("red", "blue", "green")
    )),
    "fill_colours must be a vector of exactly 2 colours"
  )

  # Invalid bar_width
  expect_error(
    age_sex_pyramid(params = list(
      df = lab_data,
      dob_var = "date_of_birth",
      bar_width = 0
    )),
    "bar_width must be a numeric value"
  )

  # Grouped = TRUE but missing age_group_var
  expect_error(
    age_sex_pyramid(params = list(
      df = lab_data,
      grouped = TRUE,
      y = "count"
    )),
    "age_group_var"
  )

  # Grouped = TRUE but missing y
  expect_error(
    age_sex_pyramid(params = list(
      df = lab_data,
      grouped = TRUE,
      age_group_var = "age_group"
    )),
    "y"
  )

  # Ungrouped but missing both age_var and dob_var
  expect_error(
    age_sex_pyramid(params = list(
      df = lab_data
    )),
    "age_var.*dob_var"
  )

})

