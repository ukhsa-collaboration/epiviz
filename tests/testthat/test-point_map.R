### SECTION 1 - FUNCTIONAL OUTPUT #################################

# test that function returns a leaflet object for a continuous bubble map
test_that("point_map returns a leaflet object when provided with colour_var (continuous)", {

  library(dplyr)
  library(epiviz)

  region_detections <- epiviz::lab_data |>
    filter(organism_species_name == "KLEBSIELLA PNEUMONIAE") |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  region_points <- epiviz::PHEC_boundaries_2016 |>
    sf::st_drop_geometry() |>
    left_join(region_detections, by = c("phec16nm" = "region"))

  result <- point_map(
    params = list(
      df = region_points,
      lat = "lat",
      long = "long",
      colour_var = "detections",
      point_size = "detections",
      point_size_range = c(4, 20),
      point_colours = "YlOrRd",
      point_labels = "phec16nm",
      map_title = "Klebsiella Pneumoniae detections by region",
      legend_title = "Detections"
    )
  )

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})



# test that function returns a leaflet object for a categorical point map with a border
test_that("point_map returns a leaflet object when provided with colour_var (discrete) and border_shape_name", {

  library(dplyr)
  library(epiviz)

  region_detections <- epiviz::lab_data |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup() |>
    mutate(detection_level = ifelse(detections > median(detections), "High", "Low"))

  region_points <- epiviz::PHEC_boundaries_2016 |>
    sf::st_drop_geometry() |>
    left_join(region_detections, by = c("phec16nm" = "region"))

  expect_warning(
    result <- point_map(
      params = list(
        df = region_points,
        lat = "lat",
        long = "long",
        colour_var = "detection_level",
        point_colours = c("High" = "#8A1B61", "Low" = "#007C91"),
        map_title = "Detection levels by region",
        legend_title = "Detection level",
        border_shape_name = epiviz::UK_boundaries_2023
      )
    ),
    "border_code_col is not provided so all border shapefile areas will be included"
  )

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})



# test that function returns a leaflet object with layer control, clustering,
# a custom size legend, popups, and a measuring tool
test_that("point_map returns a leaflet object with all optional features enabled", {

  library(dplyr)
  library(epiviz)

  region_detections <- epiviz::lab_data |>
    group_by(region) |>
    summarise(detections = n()) |>
    ungroup()

  region_points <- epiviz::PHEC_boundaries_2016 |>
    sf::st_drop_geometry() |>
    left_join(region_detections, by = c("phec16nm" = "region")) |>
    mutate(
      detection_level = ifelse(detections > median(detections), "High", "Low"),
      popup_html = paste0("<strong>Region: </strong>", phec16nm)
    )

  result <- point_map(
    params = list(
      df = region_points,
      lat = "lat",
      long = "long",
      colour_var = "detection_level",
      point_colours = c("High" = "#8A1B61", "Low" = "#007C91"),
      cluster = TRUE,
      basemaps = c("Uncluttered" = "Esri.WorldTopoMap", "Detailed" = "OpenStreetMap"),
      popup_col = "popup_html",
      size_legend = TRUE,
      size_legend_sizes = c(13, 6),
      size_legend_labels = c("High", "Low"),
      measure_tool = TRUE,
      map_title = "Detections by region",
      legend_title = "Detection level",
      legend_pos = "topright",
      map_zoom = data.frame(LONG = -2.89479, LAT = 54.793409, zoom = c(5))
    )
  )

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})


# test that function returns a leaflet object when point_stroke is disabled
test_that("point_map returns a leaflet object when point_stroke = FALSE", {

  result <- point_map(
    params = list(
      df = data.frame(x = c(1, 2), y = c(1, 2)),
      lat = "y",
      long = "x",
      point_stroke = FALSE
    )
  )

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})


# test that colour_type can override auto-detection to force a numeric
# colour_var to be treated as discrete rather than continuous
test_that("point_map returns a leaflet object when colour_type forces a numeric colour_var to be discrete", {

  result <- point_map(
    params = list(
      df = data.frame(x = c(1, 2, 3), y = c(1, 2, 3), g = c(1, 2, 1)),
      lat = "y",
      long = "x",
      colour_var = "g",
      colour_type = "discrete"
    )
  )

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})


# test that an unnamed point_colours vector matching the number of unique
# groups is used directly, rather than being treated as a palette source
test_that("point_map returns a leaflet object when an unnamed point_colours vector matches the number of groups", {

  result <- point_map(
    params = list(
      df = data.frame(x = c(1, 2), y = c(1, 2), g = c("a", "b")),
      lat = "y",
      long = "x",
      colour_var = "g",
      point_colours = c("red", "blue")
    )
  )

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})


# test that a single point_colours value is used directly when colour_var is absent
test_that("point_map returns a leaflet object when point_colours is a single colour and colour_var is absent", {

  result <- point_map(
    params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      point_colours = "purple"
    )
  )

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})


# test that border_code_col/border_areaname filter the border_shape_name boundaries
test_that("point_map returns a leaflet object when border_shape_name is filtered by border_code_col/border_areaname", {

  result <- point_map(
    params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      border_shape_name = epiviz::UK_boundaries_2023,
      border_code_col = "CTRY23NM",
      border_areaname = "England"
    )
  )

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})


# test that cosmetic/pass-through styling and layout params are all accepted
test_that("point_map returns a leaflet object with all cosmetic and layout params set", {

  result <- point_map(
    params = list(
      df = data.frame(x = c(1, 2), y = c(1, 2), g = c("a", "b")),
      lat = "y",
      long = "x",
      colour_var = "g",
      point_opacity = 0.5,
      point_stroke_colour = "white",
      point_stroke_weight = 2,
      cluster = TRUE,
      cluster_max_radius = 40,
      points_group = "My cases",
      layers_control_pos = "topright",
      measure_tool = TRUE,
      measure_units = "miles",
      size_legend = TRUE,
      size_legend_sizes = c(10, 5),
      size_legend_labels = c("Large", "Small"),
      size_legend_title = "Size",
      size_legend_pos = "topleft",
      map_title = "Title",
      map_title_size = 16,
      map_title_colour = "red",
      map_footer = "Footer",
      map_footer_size = 10,
      map_footer_colour = "grey",
      legend_scrollable = FALSE,
      legend_max_height = 100
    )
  )

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})



### SECTION 2 - VALIDATION AND ERROR HANDLING #################################


test_that("point_map errors when df is not provided", {

  expect_error(
    point_map(params = list(lat = "y", long = "x")),
    "A data frame argument is required"
  )

})


test_that("point_map errors when df is not a data frame", {

  expect_error(
    point_map(params = list(df = "not_a_dataframe", lat = "y", long = "x")),
    "Argument df is not a data frame object"
  )

})


test_that("point_map errors when lat is missing", {

  expect_error(
    point_map(params = list(df = data.frame(x = 1), long = "x")),
    "Please include a variable from df for lat"
  )

})


test_that("point_map errors when long is missing", {

  expect_error(
    point_map(params = list(df = data.frame(y = 1), lat = "y")),
    "Please include a variable from df for long"
  )

})


test_that("point_map errors when lat column is not found in df", {

  expect_error(
    point_map(params = list(df = data.frame(x = 1, y = 1), lat = "missing_col", long = "x")),
    "lat not found within df"
  )

})


test_that("point_map errors when long column is not found in df", {

  expect_error(
    point_map(params = list(df = data.frame(x = 1, y = 1), lat = "y", long = "missing_col")),
    "long not found within df"
  )

})


test_that("point_map errors when colour_var is not found in df", {

  expect_error(
    point_map(params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      colour_var = "missing_col"
    )),
    "colour_var not found within df"
  )

})


test_that("point_map errors when colour_type is not a permissable value", {

  expect_error(
    point_map(params = list(
      df = data.frame(x = 1, y = 1, v = 1),
      lat = "y",
      long = "x",
      colour_var = "v",
      colour_type = "categorical"
    )),
    "colour_type must equal"
  )

})


test_that("point_map errors when named point_colours length does not match number of groups", {

  expect_error(
    point_map(params = list(
      df = data.frame(x = c(1, 2), y = c(1, 2), g = c("a", "b")),
      lat = "y",
      long = "x",
      colour_var = "g",
      point_colours = c("a" = "red")
    )),
    "point_colours provided must equal the number of unique groups"
  )

})


test_that("point_map errors when point_size variable is not found in df", {

  expect_error(
    point_map(params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      point_size = "missing_col"
    )),
    "point_size not found within df"
  )

})


test_that("point_map errors when legend_pos is not a leaflet permissable value", {

  expect_error(
    point_map(params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      legend_pos = "right"
    )),
    "legend_pos"
  )

})


test_that("point_map errors when size_legend is TRUE without size_legend_sizes/labels", {

  expect_error(
    point_map(params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      size_legend = TRUE
    )),
    "size_legend_sizes and size_legend_labels must both be provided"
  )

})


test_that("point_map errors when size_legend_sizes and size_legend_labels differ in length", {

  expect_error(
    point_map(params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      size_legend = TRUE,
      size_legend_sizes = c(5, 10),
      size_legend_labels = c("Low")
    )),
    "must be vectors of equal length"
  )

})


test_that("point_map errors when multiple basemaps are provided unnamed", {

  expect_error(
    point_map(params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      basemaps = c("Esri.WorldTopoMap", "OpenStreetMap")
    )),
    "basemaps must be a named character vector"
  )

})


test_that("point_map errors when map_zoom is missing required variables", {

  expect_error(
    point_map(params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      map_zoom = data.frame(LONG = -2, LAT = 53)
    )),
    "map_zoom must be a single-row df"
  )

})


test_that("point_map errors when map_zoom has more than one row", {

  expect_error(
    point_map(params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      map_zoom = data.frame(LONG = c(-2, -3), LAT = c(53, 54), zoom = c(5, 6))
    )),
    "map_zoom must be a single-row df"
  )

})



### SECTION 3 - WARNINGS #################################


test_that("point_map warns when multiple point_colours are provided without colour_var", {

  expect_warning(
    point_map(params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      point_colours = c("red", "blue")
    )),
    "point_colours have been provided but colour_var is absent"
  )

})


test_that("point_map warns when border_code_col is provided without border_shape_name", {

  expect_warning(
    point_map(params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      border_code_col = "area_code"
    )),
    "border_shape_name not included but border_code_col provided"
  )

})


test_that("point_map warns when border_areaname is provided without border_shape_name", {

  expect_warning(
    point_map(params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      border_areaname = "Area A"
    )),
    "border_shape_name not included but border_areaname provided"
  )

})
