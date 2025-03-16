test_that("col_chart basic functionality works", {
  library(epiviz)

  # Create summarized test data from lab_data
  test_data <- epiviz::lab_data |>
    dplyr::group_by(organism_species_name) |>
    dplyr::summarise(count = dplyr::n())

  # Basic column chart
  expect_true(inherits(
    col_chart(
      params = list(
        df = test_data,
        x = "organism_species_name",
        y = "count"
      )
    ), "ggplot"))

  # With custom fill color
  expect_true(inherits(
    col_chart(
      params = list(
        df = test_data,
        x = "organism_species_name",
        y = "count",
        fill = "red"
      )
    ), "ggplot"))

  # With x-axis label angle
  expect_true(inherits(
    col_chart(
      params = list(
        df = test_data,
        x = "organism_species_name",
        y = "count",
        x_label_angle = 45
      )
    ), "ggplot"))
})

test_that("col_chart grouping works", {
  library(epiviz)

  # Create grouped test data from lab_data
  test_data <- epiviz::lab_data |>
    dplyr::group_by(organism_species_name, region) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  # Test with grouping
  expect_true(inherits(
    col_chart(
      params = list(
        df = test_data,
        x = "organism_species_name",
        y = "count",
        group_var = "region",
        position = "dodge"
      )
    ), "ggplot"))
})

test_that("col_chart error handling works", {
  # Test missing data frame
  expect_error(col_chart(params = list(x = "test", y = "count")))

  # Test missing x variable
  expect_error(col_chart(params = list(df = data.frame(count = 1:5), y = "count")))

  # Test missing y variable
  expect_error(col_chart(params = list(df = data.frame(x = 1:5), x = "x")))

  # Test empty data frame
  expect_error(col_chart(params = list(df = data.frame(), x = "x", y = "y")))
})

test_that("col_chart axis customization works", {
  library(epiviz)

  test_data <- epiviz::lab_data |>
    dplyr::group_by(organism_species_name) |>
    dplyr::summarise(count = dplyr::n())

  # Test with custom labels
  p <- col_chart(
    params = list(
      df = test_data,
      x = "organism_species_name", 
      y = "count",
      x_label = "Species",
      y_label = "Count"
    )
  )
  expect_true(inherits(p, "ggplot"))

  # Test with percentage formatting
  p <- col_chart(
    params = list(
      df = test_data,
      x = "organism_species_name",
      y = "count",
      percent = TRUE
    )
  )
  expect_true(inherits(p, "ggplot"))
})

test_that("col_chart dynamic parameter works", {
  library(epiviz)

  test_data <- epiviz::lab_data |>
    dplyr::group_by(organism_species_name) |>
    dplyr::summarise(count = dplyr::n())

  # Test static output (default)
  result_static <- col_chart(
    dynamic = FALSE,
    params = list(
      df = test_data,
      x = "organism_species_name",
      y = "count"
    )
  )
  expect_true(inherits(result_static, "ggplot"))

  # Test dynamic output (when implemented)
  # Commented out until dynamic functionality is added
  # result_dynamic <- col_chart(
  #   dynamic = TRUE, 
  #   params = list(
  #     df = test_data,
  #     x = "organism_species_name",
  #     y = "count"
  #   )
  # )
  # expect_true(inherits(result_dynamic, "plotly"))
})

test_that("col_chart handles ellipsis arguments correctly", {
  library(epiviz)
  
  # Create test data
  test_data <- epiviz::lab_data |>
    dplyr::group_by(organism_species_name) |>
    dplyr::summarise(count = dplyr::n())

  # Test static plot with custom aesthetics that aren't in params
  result <- col_chart(
    params = list(
      df = test_data,
      x = "organism_species_name",
      y = "count"
    ),
    alpha = 0.7,            # Custom opacity
    linetype = "solid",     # Custom line type for bar borders
    color = "black"         # Custom border color
  )
  expect_true(inherits(result, "ggplot"))

  # Test dynamic plot with custom plotly parameters
  result_dynamic <- col_chart(
    dynamic = TRUE,
    params = list(
      df = test_data,
      x = "organism_species_name",
      y = "count"
    ),
    opacity = 0.8,               # Custom opacity for plotly
    hoverlabel = list(          # Custom hover label settings
      bgcolor = "white",
      bordercolor = "black",
      font = list(size = 14)
    ),
    showlegend = TRUE
  )
  expect_true(inherits(result_dynamic, "plotly"))

  # Test grouped plot with custom aesthetics
  grouped_data <- epiviz::lab_data |>
    dplyr::group_by(organism_species_name, region) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  result_grouped <- col_chart(
    params = list(
      df = grouped_data,
      x = "organism_species_name",
      y = "count",
      group_var = "region"
    ),
    alpha = 0.8,            # Custom opacity
    linewidth = 0.5,        # Custom border width
    color = "black"         # Custom border color
  )
  expect_true(inherits(result_grouped, "ggplot"))
})

test_that("col_chart handles NULL parameters correctly", {
  # Create test data
  test_data <- epiviz::lab_data |>
    dplyr::group_by(organism_species_name) |>
    dplyr::summarise(count = dplyr::n())

  # Test with NULL group_var
  result <- col_chart(
    params = list(
      df = test_data,
      x = "organism_species_name",
      y = "count",
      group_var = NULL,  # Explicitly test NULL group_var
      fill = "blue"
    )
  )
  expect_true(inherits(result, "ggplot"))

  # Test with NULL fill
  result2 <- col_chart(
    params = list(
      df = test_data,
      x = "organism_species_name",
      y = "count",
      group_var = "organism_species_name",
      fill = NULL  # Explicitly test NULL fill
    )
  )
  expect_true(inherits(result2, "ggplot"))
})
