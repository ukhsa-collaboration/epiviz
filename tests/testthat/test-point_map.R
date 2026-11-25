### SECTION 1 - FUNCTIONAL OUTPUT #################################

# Avoid downloading basemap tiles in tests: return a small grey raster covering
# the requested extent
fake_basemap_tiles <- function(x, ...) {
  bb <- sf::st_bbox(x)
  terra::rast(nrows = 2, ncols = 2, nlyrs = 3, crs = "EPSG:3857",
              extent = terra::ext(bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]]),
              vals = 200)
}
local_mocked_bindings(get_tiles = fake_basemap_tiles, .package = "maptiles")

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
    dynamic = TRUE,
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
      dynamic = TRUE,
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
    dynamic = TRUE,
    params = list(
      df = region_points,
      lat = "lat",
      long = "long",
      colour_var = "detection_level",
      point_colours = c("High" = "#8A1B61", "Low" = "#007C91"),
      cluster = TRUE,
      basemaps = c("Uncluttered" = "Esri.WorldTopoMap", "Detailed" = "OpenStreetMap"),
      click_popups_col = "popup_html",
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


# test that function returns a leaflet object when point_border is disabled
test_that("point_map returns a leaflet object when point_border = FALSE", {

  result <- point_map(
    dynamic = TRUE,
    params = list(
      df = data.frame(x = c(1, 2), y = c(1, 2)),
      lat = "y",
      long = "x",
      point_border = FALSE
    )
  )

  # check that the output is a leaflet object
  expect_true(inherits(result, "leaflet"))

})


# test that colour_type can override auto-detection to force a numeric
# colour_var to be treated as discrete rather than continuous
test_that("point_map returns a leaflet object when colour_type forces a numeric colour_var to be discrete", {

  result <- point_map(
    dynamic = TRUE,
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
    dynamic = TRUE,
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
    dynamic = TRUE,
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
    dynamic = TRUE,
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
    dynamic = TRUE,
    params = list(
      df = data.frame(x = c(1, 2), y = c(1, 2), g = c("a", "b")),
      lat = "y",
      long = "x",
      colour_var = "g",
      point_opacity = 0.5,
      point_border_colour = "white",
      point_border_weight = 2,
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


# test that a named continuous gradient is accepted (named vectors are only
# treated as group mappings for discrete colour variables)
test_that("point_map accepts a named point_colours gradient for a continuous colour_var", {

  result <- point_map(
    dynamic = TRUE,
    params = list(
      df = data.frame(x = c(-1, -2, -3), y = c(51, 52, 53), v = c(1, 5, 10)),
      lat = "y",
      long = "x",
      colour_var = "v",
      point_colours = c(low = "white", high = "red")
    )
  )

  expect_true(inherits(result, "leaflet"))

})


# test that the colour legend scroll CSS is scoped to each widget
test_that("point_map scopes legend scroll styling to each map's own legend", {

  df <- data.frame(x = c(-1, -2), y = c(51, 52), g = c("a", "b"))

  get_legend_class <- function(map) {
    legend_call <- Filter(function(cl) cl$method == "addLegend", map$x$calls)[[1]]
    sub("^info legend ", "", legend_call$args[[1]]$className)
  }
  get_css <- function(map) {
    title_call <- Filter(function(cl) cl$method == "addControl", map$x$calls)[[1]]
    as.character(title_call$args[[1]])
  }

  scroll_map <- point_map(dynamic = TRUE, params = list(
    df = df, lat = "y", long = "x", colour_var = "g", legend_max_height = 80))
  other_map <- point_map(dynamic = TRUE, params = list(
    df = df, lat = "y", long = "x", colour_var = "g", legend_max_height = 200))
  no_scroll_map <- point_map(dynamic = TRUE, params = list(
    df = df, lat = "y", long = "x", colour_var = "g", legend_scrollable = FALSE))

  scroll_class <- get_legend_class(scroll_map)
  expect_false(scroll_class == get_legend_class(other_map))
  expect_true(grepl(paste0(".leaflet-control.", scroll_class, " {\n    max-height:80px"),
                    get_css(scroll_map), fixed = TRUE))
  expect_false(grepl("max-height", get_css(no_scroll_map)))
  expect_false(grepl("map-legend {", get_css(scroll_map), fixed = TRUE))

})


# test that the size legend circles are drawn at the same diameter as the markers
test_that("point_map draws size legend circles at twice the supplied radius", {

  result <- point_map(dynamic = TRUE, params = list(
    df = data.frame(x = -1, y = 51), lat = "y", long = "x",
    size_legend = TRUE, size_legend_sizes = c(13), size_legend_labels = c("Big")))

  size_call <- Filter(function(cl) cl$method == "addControl", result$x$calls)
  size_html <- as.character(size_call[[length(size_call)]]$args[[1]])
  expect_true(grepl("width:26px;height:26px", size_html, fixed = TRUE))

})


# test that the static map returns a ggplot object for each colour/size mode
test_that("point_map returns a ggplot object when dynamic = FALSE", {

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

  discrete_map <- point_map(dynamic = FALSE, params = list(
    df = region_points, lat = "lat", long = "long",
    colour_var = "detection_level",
    point_colours = c("High" = "#8A1B61", "Low" = "#007C91"),
    point_size = "detections",
    size_legend_title = "Detections",
    border_shape_name = epiviz::UK_boundaries_2023,
    border_code_col = "CTRY23NM",
    border_areaname = "England",
    map_title = "Detections by region",
    map_footer = "Source: lab_data",
    legend_pos = "bottomleft"
  ))
  continuous_map <- point_map(dynamic = FALSE, params = list(
    df = region_points, lat = "lat", long = "long",
    colour_var = "detections", point_colours = "YlOrRd", point_border = FALSE,
    map_zoom = data.frame(LONG = -2.89479, LAT = 54.793409, zoom = 5)
  ))
  single_colour_map <- point_map(dynamic = FALSE, params = list(
    df = region_points, lat = "lat", long = "long", point_colours = "red"
  ))

  for (map in list(discrete_map, continuous_map, single_colour_map)) {
    expect_true(inherits(map, "ggplot"))
    expect_silent(ggplot2::ggplot_build(map))
  }

})


test_that("point_map draws basemap tiles and their credit on static maps", {

  map <- point_map(dynamic = FALSE, params = list(
    df = data.frame(x = c(-1, -2), y = c(51, 53)),
    lat = "y",
    long = "x",
    basemaps = c(Topo = "Esri.WorldTopoMap")
  ))

  layer_geoms <- vapply(map$layers, function(l) class(l$geom)[1], character(1))
  expect_true(grepl("raster", layer_geoms[1], ignore.case = TRUE))
  expect_equal(map$coordinates$crs, sf::st_crs(3857))
  expect_true(any(vapply(map$layers, function(l) identical(l$aes_params$label,
    stringr::str_wrap(maptiles::get_credit("Esri.WorldTopoMap"), 70)), logical(1))))
  expect_silent(ggplot2::ggplot_build(map))

})


test_that("point_map draws a static map without tiles when basemaps = NULL", {

  map <- point_map(dynamic = FALSE, params = list(
    df = data.frame(x = c(-1, -2), y = c(51, 53)),
    lat = "y",
    long = "x",
    basemaps = NULL
  ))

  layer_geoms <- vapply(map$layers, function(l) class(l$geom)[1], character(1))
  expect_false(any(grepl("raster", layer_geoms, ignore.case = TRUE)))
  expect_equal(map$coordinates$crs, sf::st_crs(4326))

})


test_that("point_map warns and drops the basemap when tiles cannot be downloaded", {

  local_mocked_bindings(get_tiles = function(...) stop("no connection"), .package = "maptiles")

  expect_warning(
    map <- point_map(dynamic = FALSE, params = list(df = data.frame(x = -1, y = 51), lat = "y", long = "x")),
    "Basemap tiles could not be downloaded.*no connection"
  )
  layer_geoms <- vapply(map$layers, function(l) class(l$geom)[1], character(1))
  expect_false(any(grepl("raster", layer_geoms, ignore.case = TRUE)))

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


test_that("point_map errors when named point_colours do not cover every group", {

  expect_error(
    point_map(params = list(
      df = data.frame(x = c(1, 2), y = c(1, 2), g = c("a", "b")),
      lat = "y",
      long = "x",
      colour_var = "g",
      point_colours = c("a" = "red")
    )),
    "Named point_colours must include a colour for every observed group"
  )

})


test_that("point_map errors when lat or long are not numeric", {

  expect_error(
    point_map(params = list(df = data.frame(x = 1, y = "a"), lat = "y", long = "x")),
    "lat must be a numeric variable"
  )
  expect_error(
    point_map(params = list(df = data.frame(x = "a", y = 1), lat = "y", long = "x")),
    "long must be a numeric variable"
  )

})


test_that("point_map errors when point_opacity is not between 0 and 1", {

  expect_error(
    point_map(params = list(df = data.frame(x = 1, y = 1), lat = "y", long = "x",
                            point_opacity = 1.5)),
    "point_opacity must be a single numeric value between 0 and 1"
  )

})


test_that("point_map errors when point_size_range is not an ascending numeric pair", {

  expect_error(
    point_map(params = list(df = data.frame(x = 1, y = 1), lat = "y", long = "x",
                            point_size_range = c(20, 4))),
    "point_size_range must be a numeric vector of length 2"
  )
  expect_error(
    point_map(params = list(df = data.frame(x = 1, y = 1), lat = "y", long = "x",
                            point_size_range = 4)),
    "point_size_range must be a numeric vector of length 2"
  )

})


test_that("point_map errors when point_size variable is not numeric", {

  expect_error(
    point_map(params = list(df = data.frame(x = 1, y = 1, s = "big"), lat = "y",
                            long = "x", point_size = "s")),
    "point_size variable must be numeric"
  )

})


test_that("point_map errors when click_popups_col or point_labels are not found in df", {

  expect_error(
    point_map(dynamic = TRUE,
              params = list(df = data.frame(x = 1, y = 1), lat = "y", long = "x",
                            click_popups_col = "missing_col")),
    "click_popups_col not found within df"
  )
  expect_error(
    point_map(dynamic = TRUE,
              params = list(df = data.frame(x = 1, y = 1), lat = "y", long = "x",
                            point_labels = "missing_col")),
    "point_labels not found within df"
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


test_that("point_map errors when a static basemap is not a maptiles provider", {

  expect_error(
    point_map(dynamic = FALSE, params = list(
      df = data.frame(x = 1, y = 1),
      lat = "y",
      long = "x",
      basemaps = "Not.AProvider"
    )),
    "basemap \"Not.AProvider\" is not available for static maps"
  )

})


test_that("point_map warns that static maps only use the first basemap", {

  expect_warning(
    map <- point_map(dynamic = FALSE, params = list(
      df = data.frame(x = -1, y = 51),
      lat = "y",
      long = "x",
      basemaps = c("Uncluttered" = "Esri.WorldTopoMap", "Detailed" = "OpenStreetMap")
    )),
    "only the first \\(\"Esri.WorldTopoMap\"\\) will be used"
  )
  expect_true(inherits(map, "ggplot"))

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


test_that("point_map warns and drops rows with missing lat/long", {

  expect_warning(
    result <- point_map(dynamic = TRUE, params = list(
      df = data.frame(x = c(-1, NA, -3), y = c(51, 52, 53)),
      lat = "y",
      long = "x"
    )),
    "1 row\\(s\\) with missing lat/long have been removed"
  )
  marker_call <- Filter(function(cl) cl$method == "addCircleMarkers", result$x$calls)[[1]]
  expect_equal(marker_call$args[[1]], c(51, 53))

})


test_that("point_map warns and plots NA point_size values at the minimum size", {

  expect_warning(
    result <- point_map(dynamic = TRUE, params = list(
      df = data.frame(x = c(-1, -2, -3), y = c(51, 52, 53), s = c(1, NA, 10)),
      lat = "y",
      long = "x",
      point_size = "s",
      point_size_range = c(4, 20)
    )),
    "1 point\\(s\\) with a missing point_size value will be plotted at the minimum size"
  )
  marker_call <- Filter(function(cl) cl$method == "addCircleMarkers", result$x$calls)[[1]]
  expect_equal(marker_call$args[[3]], c(4, 4, 20))

})


test_that("point_map warns when dynamic-only params are supplied for a static map", {

  expect_warning(
    result <- point_map(dynamic = FALSE, params = list(
      df = data.frame(x = -1, y = 51, lab = "A"),
      lat = "y",
      long = "x",
      point_labels = "lab",
      cluster = TRUE,
      measure_tool = FALSE
    )),
    "only apply when dynamic = TRUE and will be ignored: point_labels, cluster$"
  )
  expect_true(inherits(result, "ggplot"))

})
