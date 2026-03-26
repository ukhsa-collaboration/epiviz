

### SECTION 1 - TESTS FOR BAR CHARTS WITH CATEGORICAL X-AXIS ###################



test_that("col_chart produces a static chart (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("col_chart produces a dynamic chart (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("col_chart produces a static chart with grouped data (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    show_gridlines = FALSE
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    show_gridlines = FALSE
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with grouped data and multiple hlines (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    hline = c(50,100),
    hline_colour = 'orange',
    hline_label = c('test1','test2'),
    hline_label_colour = 'orange'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data and multiple hlines (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    hline = c(50,100),
    hline_colour = 'orange',
    hline_label = c('test1','test2'),
    hline_label_colour = 'orange'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with grouped data and case boxes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-01-07")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    case_boxes = TRUE
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data and case boxes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-01-07")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    case_boxes = TRUE
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with bar labels (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45,
    bar_labels = 'detections',
    bar_labels_pos = 'bar_base',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with bar labels (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45,
    bar_labels = 'detections',
    bar_labels_pos = 'bar_base',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})




test_that("col_chart produces a static chart with stacked bars and bar labels (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with stacked bars and bar labels (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars and bar labels (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with dodged bars and bar labels (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with errorbars (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(50:200,1),
           upper_limit = detections + sample(50:200,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with errorbars (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(50:200,1),
           upper_limit = detections + sample(50:200,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with stacked bars and errorbars (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with stacked bars and errorbars (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars and errorbars (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with dodged bars and errorbars (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with stacked bars, bar labels, and errorbars (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with stacked bars, bar labels, and errorbars (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars, bar labels, and errorbars (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with dodged bars, bar labels, and errorbars (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



###

##### Repeat of all the above tests but with flipped axes for horizontal bar charts

###

test_that("col_chart produces a static chart, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("col_chart produces a dynamic chart, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("col_chart produces a static chart with grouped data, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    show_gridlines = FALSE
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    show_gridlines = FALSE
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with grouped data and multiple hlines, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    hline = c(50,100),
    hline_colour = 'orange',
    hline_label = c('test1','test2'),
    hline_label_colour = 'orange'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data and multiple hlines, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    hline = c(50,100),
    hline_colour = 'orange',
    hline_label = c('test1','test2'),
    hline_label_colour = 'orange'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with grouped data and case boxes, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-01-07")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    case_boxes = TRUE
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data and case boxes, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-01-07")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    case_boxes = TRUE
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})




test_that("col_chart produces a static chart with bar labels, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45,
    bar_labels = 'detections',
    bar_labels_pos = 'bar_base',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with bar labels, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    x_axis_label_angle = -45,
    bar_labels = 'detections',
    bar_labels_pos = 'bar_base',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})




test_that("col_chart produces a static chart with stacked bars and bar labels, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with stacked bars and bar labels, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars and bar labels, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with dodged bars and bar labels, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})






test_that("col_chart produces a static chart with errorbars, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(50:200,1),
           upper_limit = detections + sample(50:200,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    fill_colours = "#007C91",
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with errorbars, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(50:200,1),
           upper_limit = detections + sample(50:200,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    fill_colours = "#007C91",
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with stacked bars and errorbars, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with stacked bars and errorbars, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars and errorbars, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with dodged bars and errorbars, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with stacked bars, bar labels, and errorbars, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with stacked bars, bar labels, and errorbars, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars, bar labels, and errorbars, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with dodged bars, bar labels, and errorbars, flipped axes (categorical x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "region",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Region",
    y_axis_title = "Number of detections",
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})







### SECTION 2 - TESTS FOR BAR CHARTS WITH DATE X-AXIS ##########################

# Repeat of all the above tests for date x-axis


test_that("col_chart produces a static chart (date x-axis)", {

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
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = test_data$specimen_month,
    x_axis_label_angle = -45
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart (date x-axis)", {

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
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = test_data$specimen_month,
    x_axis_label_angle = -45
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})






test_that("col_chart produces a static chart with grouped data (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = test_data$specimen_month,
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    show_gridlines = FALSE
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = test_data$specimen_month,
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    show_gridlines = FALSE
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with grouped data and multiple hlines (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = test_data$specimen_month,
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    hline = c(50,100),
    hline_colour = 'orange',
    hline_label = c('test1','test2'),
    hline_label_colour = 'orange'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data and multiple hlines (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = test_data$specimen_month,
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    hline = c(50,100),
    hline_colour = 'orange',
    hline_label = c('test1','test2'),
    hline_label_colour = 'orange'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with grouped data and case boxes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-01-07")) |>
    group_by(specimen_date = lubridate::floor_date(specimen_date, 'day'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_date",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Date",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_date),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    case_boxes = TRUE
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data and case boxes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-01-07")) |>
    group_by(specimen_date = lubridate::floor_date(specimen_date, 'day'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_date",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Date",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_date),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    case_boxes = TRUE
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with bar labels (date x-axis)", {

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
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    x_axis_label_angle = -45,
    bar_labels = 'detections',
    bar_labels_pos = 'bar_base',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with bar labels (date x-axis)", {

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
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    x_axis_label_angle = -45,
    bar_labels = 'detections',
    bar_labels_pos = 'bar_base',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})




test_that("col_chart produces a static chart with stacked bars and bar labels (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with stacked bars and bar labels (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars and bar labels (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with dodged bars and bar labels (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with errorbars (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(50:200,1),
           upper_limit = detections + sample(50:200,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    fill_colours = "#007C91",
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with errorbars (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(50:200,1),
           upper_limit = detections + sample(50:200,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    fill_colours = "#007C91",
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with stacked bars and errorbars (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with stacked bars and errorbars (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars and errorbars (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with dodged bars and errorbars (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with stacked bars, bar labels, and errorbars (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with stacked bars, bar labels, and errorbars (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars, bar labels, and errorbars (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with dodged bars, bar labels, and errorbars (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



###

##### Repeat of all the above tests but with flipped axes for horizontal bar charts

###

test_that("col_chart produces a static chart, flipped axes (date x-axis)", {

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
    axis_flip = TRUE,
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    x_axis_label_angle = -45
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})



test_that("col_chart produces a dynamic chart, flipped axes (date x-axis)", {

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
    axis_flip = TRUE,
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    x_axis_label_angle = -45
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with grouped data, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    show_gridlines = FALSE
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    show_gridlines = FALSE
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with grouped data and multiple hlines, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    hline = c(50,100),
    hline_colour = 'orange',
    hline_label = c('test1','test2'),
    hline_label_colour = 'orange'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data and multiple hlines, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    hline = c(50,100),
    hline_colour = 'orange',
    hline_label = c('test1','test2'),
    hline_label_colour = 'orange'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with grouped data and case boxes, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-01-07")) |>
    group_by(specimen_date, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_date",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Date",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_date),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    case_boxes = TRUE
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with grouped data and case boxes, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-01-07")) |>
    group_by(specimen_date, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_date",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Date",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_date),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    case_boxes = TRUE
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with bar labels, flipped axes (date x-axis)", {

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
    axis_flip = TRUE,
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    x_axis_label_angle = -45,
    bar_labels = 'detections',
    bar_labels_pos = 'bar_base',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with bar labels, flipped axes (date x-axis)", {

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
    axis_flip = TRUE,
    fill_colours = "#007C91",
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    x_axis_label_angle = -45,
    bar_labels = 'detections',
    bar_labels_pos = 'bar_base',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})




test_that("col_chart produces a static chart with stacked bars and bar labels, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with stacked bars and bar labels, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars and bar labels, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with dodged bars and bar labels, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})






test_that("col_chart produces a static chart with errorbars, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(50:200,1),
           upper_limit = detections + sample(50:200,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    fill_colours = "#007C91",
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with errorbars, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(50:200,1),
           upper_limit = detections + sample(50:200,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    fill_colours = "#007C91",
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with stacked bars and errorbars, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})





test_that("col_chart produces a dynamic chart with stacked bars and errorbars, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars and errorbars, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with dodged bars and errorbars, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with stacked bars, bar labels, and errorbars, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with stacked bars, bar labels, and errorbars, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





test_that("col_chart produces a static chart with dodged bars, bar labels, and errorbars, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

})




test_that("col_chart produces a dynamic chart with dodged bars, bar labels, and errorbars, flipped axes (date x-axis)", {

  # Define test data
  test_data <- lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
    summarise(detections = n()) |>
    ungroup() |>
    rowwise() |>
    mutate(lower_limit = detections - sample(10:30,1),
           upper_limit = detections + sample(10:30,1)) |>
    ungroup()

  # Create parameter list
  params <- list(
    df = test_data,
    x = "specimen_month",
    y = "detections",
    axis_flip = TRUE,
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    ci = 'errorbar',
    ci_lower = "lower_limit",
    ci_upper = "upper_limit",
    errorbar_width = 0.2,
    chart_title = "Laboratory Detections by Region \nand Species 2023",
    chart_footer = "This chart has been created using simulated data.",
    x_axis_title = "Month",
    y_axis_title = "Number of detections",
    x_axis_break_labels = unique(test_data$specimen_month),
    chart_title_colour = "#007C91",
    chart_footer_colour = "#007C91",
    bar_labels = 'detections',
    bar_labels_pos = 'bar_centre',
    bar_labels_font_size = 8,
    bar_labels_font_colour = 'white'
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})





###

##### Tests for bar_width parameter

###

test_that("col_chart produces static chart with bar_width parameter", {
  library(dplyr)
  library(epiviz)

  # Create test dataframe from epiviz::lab_data
  test_df <- epiviz::lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list with bar_width

  params <- list(
    df = test_df,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    bar_width = 0.7,
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))

  # Extract the bar width from the ggplot object to verify it was applied
  build <- ggplot2::ggplot_build(result)
  bar_layer <- build$data[[1]]
  # Check that bar width is applied (xmax - xmin should be approximately bar_width)
  actual_width <- bar_layer$xmax[1] - bar_layer$xmin[1]
  expect_equal(actual_width, 0.7, tolerance = 0.01)
})


test_that("col_chart produces dynamic chart with bar_width parameter", {
  library(dplyr)
  library(epiviz)

  # Create test dataframe from epiviz::lab_data
  test_df <- epiviz::lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list with bar_width
  params <- list(
    df = test_df,
    x = "region",
    y = "detections",
    fill_colours = "#007C91",
    bar_width = 0.5,
    chart_title = "Laboratory Detections by Region 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections"
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})



test_that("col_chart produces static chart with grouped data and bar_width", {
  library(dplyr)
  library(epiviz)

  # Create test dataframe from epiviz::lab_data with grouped data
  test_df <- epiviz::lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list with bar_width and grouped data
  params <- list(
    df = test_df,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "group",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    bar_width = 0.8,
    chart_title = "Laboratory Detections by Region and Species 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections"
  )

  # Create static column chart
  result <- col_chart(params = params, dynamic = FALSE)

  # Check that the output is a ggplot object
  expect_true(inherits(result, "ggplot"))
})





test_that("col_chart produces dynamic chart with grouped data and bar_width", {
  library(dplyr)
  library(epiviz)

  # Create test dataframe from epiviz::lab_data with grouped data
  test_df <- epiviz::lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region, organism_species_name) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list with bar_width and grouped data
  params <- list(
    df = test_df,
    x = "region",
    y = "detections",
    group_var = "organism_species_name",
    group_var_barmode = "stack",
    fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
                     "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
                     "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
    bar_width = 0.6,
    chart_title = "Laboratory Detections by Region and Species 2023",
    x_axis_title = "Region",
    y_axis_title = "Number of detections"
  )

  # Create dynamic column chart
  result <- col_chart(params = params, dynamic = TRUE)

  # Check that the output is a plotly object
  expect_true(inherits(result, "plotly"))

})




test_that("col_chart uses default bar_width when not specified", {
  library(dplyr)
  library(epiviz)

  # Create test dataframe from epiviz::lab_data
  test_df <- epiviz::lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Create params list without bar_width (should use default of 0.9)
  params <- list(
    df = test_df,
    x = "region",
    y = "detections",
    fill_colours = "#007C91"
  )

  # Create static column chart
  result_static <- col_chart(params = params, dynamic = FALSE)
  expect_true(inherits(result_static, "ggplot"))

  # Check that default bar_width (0.9) was applied
  build <- ggplot2::ggplot_build(result_static)
  bar_layer <- build$data[[1]]
  actual_width <- bar_layer$xmax[1] - bar_layer$xmin[1]
  expect_equal(actual_width, 0.9, tolerance = 0.01)

  # Create dynamic column chart
  result_dynamic <- col_chart(params = params, dynamic = TRUE)
  expect_true(inherits(result_dynamic, "plotly"))


})





test_that("col_chart throws error for invalid bar_width values", {
  library(dplyr)
  library(epiviz)

  # Create test dataframe
  test_df <- epiviz::lab_data |>
    filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  # Test bar_width > 1 (should throw error)
  params_over <- list(
    df = test_df,
    x = "region",
    y = "detections",
    bar_width = 1.5
  )
  expect_error(col_chart(params = params_over, dynamic = FALSE))

  # Test bar_width <= 0 (should throw error)
  params_zero <- list(
    df = test_df,
    x = "region",
    y = "detections",
    bar_width = 0
  )
  expect_error(col_chart(params = params_zero, dynamic = FALSE))

  # Test bar_width negative (should throw error)
  params_neg <- list(
    df = test_df,
    x = "region",
    y = "detections",
    bar_width = -0.5
  )
  expect_error(col_chart(params = params_neg, dynamic = FALSE))

  # Test bar_width non-numeric (should throw error)
  params_char <- list(
    df = test_df,
    x = "region",
    y = "detections",
    bar_width = "wide"
  )
  expect_error(col_chart(params = params_char, dynamic = FALSE))
})






