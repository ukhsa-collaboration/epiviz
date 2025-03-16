test_that("line_chart works without group_var", {
  library(dplyr)
  library(epiviz)

  # Import df lab_data from epiviz and do some manipulation before passing for the test
  test_df <- epiviz::lab_data

  # Manipulating date within df
  test_df$specimen_date <- as.Date(test_df$specimen_date)

  # Setting start date and end date for filtering
  start_date <- as.Date("2023-01-01")
  end_date <- as.Date("2023-01-31")

  # Filter data for 'KLEBSIELLA PNEUMONIAE' and within the date range
  filtered_df <- test_df |>
    filter(organism_species_name == "KLEBSIELLA PNEUMONIAE",
           specimen_date >= start_date & specimen_date <= end_date) |>
    group_by(specimen_date) |>
    summarize(count = n(), .groups = 'drop')

  # Ensure that filtered_df is a data frame
  filtered_df <- as.data.frame(filtered_df)

  # Create params list without group_var
  params <- list(
    df = filtered_df,  # Ensure this is correctly referencing the data frame
    x = "specimen_date", # Ensure this matches the column name exactly
    y = "count",         # Ensure this matches the column name exactly
    line_colour = c("blue"),  # Single colour for single line
    line_type = c("solid")
  )

  # Generate the line chart without group_var
  result <- epiviz::line_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})


# test to chceck if the function returns a plotly object using the mandatory fields

test_that("line_chart static works", {
  library(dplyr)
  library(epiviz)

  # Import df lab_data from epiviz and do some manipulation before passing for the test
  test_df <- epiviz::lab_data

  # Manipulating date within df
  test_df$specimen_date <- as.Date(test_df$specimen_date)

  # Setting start date and end date for aggregation
  start_date <- as.Date("2023-01-01")
  end_date <- as.Date("2023-12-31")

  # Summarization
  summarised_df <- test_df |>
    group_by(organism_species_name, specimen_date) |>
    summarize(count = n(), .groups = 'drop') |>
    ungroup() |>
    filter(specimen_date >= start_date & specimen_date <= end_date)

  # Ensure that summarised_df is a data frame
  summarised_df <- as.data.frame(summarised_df)

  # Create params list
  params <- list(
    df = summarised_df,  # Ensure this is correctly referencing the data frame
    x = "specimen_date", # Ensure this matches the column name exactly
    y = "count",         # Ensure this matches the column name exactly
    group_var = "organism_species_name",  # Ensure this matches the column name exactly
    line_colour = c("blue","green","orange"),
    line_type = c("solid", "dotted", "dashed")
  )

   # Generate the line chart
  result <- epiviz::line_chart(params = params, dynamic = FALSE)


  # check that the output is a plotly object
  expect_true(inherits(result, "ggplot"))

})


# test to chceck if the function returns a plotly object using the mandatory fields

test_that("line_chart dynamic works", {
  library(dplyr)
  library(epiviz)

  # Import df lab_data from epiviz and do some manipulation before passing for the test
  test_df <- epiviz::lab_data

  # Manipulating date within df
  test_df$specimen_date <- as.Date(test_df$specimen_date)

  # Setting start date and end date for aggregation
  start_date <- as.Date("2023-01-01")
  end_date <- as.Date("2023-12-31")

  # Summarization
  summarised_df <- test_df |>
    group_by(organism_species_name, specimen_date) |>
    summarize(count = n(), .groups = 'drop') |>
    ungroup() |>
    filter(specimen_date >= start_date & specimen_date <= end_date)

  # Ensure that summarised_df is a data frame
  summarised_df <- as.data.frame(summarised_df)

  # Create params list
  params <- list(
    df = summarised_df,  # Ensure this is correctly referencing the data frame
    x = "specimen_date", # Ensure this matches the column name exactly
    y = "count",         # Ensure this matches the column name exactly
    group_var = "organism_species_name",  # Ensure this matches the column name exactly
    line_colour = c("blue","green","orange"),
    line_type = c("solid", "dotted", "dashed")
  )

  # Generate the line chart
  result <- epiviz::line_chart(params = params, dynamic = TRUE)


  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})

test_that("line_chart handles ellipsis arguments", {
  library(dplyr)
  library(epiviz)

  # Create test data
  test_df <- epiviz::lab_data |>
    filter(organism_species_name == "KLEBSIELLA PNEUMONIAE") |>
    mutate(specimen_date = as.Date(specimen_date)) |>
    group_by(specimen_date) |>
    summarize(count = n(), .groups = 'drop')

  # Basic params
  params <- list(
    df = test_df,
    x = "specimen_date",
    y = "count",
    line_colour = "blue"
  )

  # Test static plot with custom line properties
  result <- line_chart(
    params = params,
    dynamic = FALSE,
    alpha = 0.7  # Test passing alpha through ...
  )
  expect_true(inherits(result, "ggplot"))

  # Test dynamic plot with custom line properties
  result_dynamic <- line_chart(
    params = params,
    dynamic = TRUE,
    opacity = 0.7,    # Test passing opacity through ...
    line = list(     # Test passing complex attributes through ...
      dash = "dot",
      width = 3
    )
  )
  expect_true(inherits(result_dynamic, "plotly"))

  # Test grouped plot with custom aesthetics
  grouped_df <- epiviz::lab_data |>
    mutate(specimen_date = as.Date(specimen_date)) |>
    group_by(organism_species_name, specimen_date) |>
    summarize(count = n(), .groups = 'drop') |>
    filter(specimen_date >= as.Date("2023-01-01"))

  grouped_params <- list(
    df = grouped_df,
    x = "specimen_date",
    y = "count",
    group_var = "organism_species_name",
    line_colour = c("blue", "green", "red")
  )

  result_grouped <- line_chart(
    params = grouped_params,
    dynamic = FALSE,
    alpha = 0.8      # Test transparency
  )
  expect_true(inherits(result_grouped, "ggplot"))
})
