#' Point Map
#'
#' @description A function for producing dynamic (leaflet) point (bubble) maps, used
#' to plot point locations (e.g. case locations, facilities, or area centroids) on a
#' map, optionally coloured by group or value, sized by a numeric variable, and
#' clustered.
#'
#' @param params A named list containing arguements used in map.
#' \describe{
#'    \item{df}{\strong{Mandatory.} Data frame containing the point locations
#'    and values to be plotted.}
#'    \item{lat}{\strong{Mandatory.} Name of the variable in df containing the
#'    latitude of each point.}
#'    \item{long}{\strong{Mandatory.} Name of the variable in df containing the
#'    longitude of each point.}
#'    \item{colour_var}{Name of a variable in df used to colour each point. By default
#'    (\code{colour_type = "auto"}), numeric variables are coloured on a continuous
#'    scale and all other variables are coloured by category. Use \code{colour_type}
#'    to override this detection.}
#'    \item{colour_type}{One of \code{"auto"} (default), \code{"continuous"}, or
#'    \code{"discrete"}. Overrides the automatic continuous/discrete detection of
#'    \code{colour_var}, e.g. for a numeric variable (such as an area code) that
#'    should be treated as a category rather than a gradient.}
#'    \item{point_colours}{Colours used for the plotted points. When \code{colour_var}
#'    is not provided, a single colour is used (default = \code{"blue"}). When
#'    \code{colour_var} resolves to a continuous variable, this is used as the colour
#'    gradient and can be provided as either the name of an RColorBrewer palette (e.g.
#'    \code{point_colours = "YlOrRd"}), a single rgb colour code, hexcode, or colour
#'    name, or a character vector of rgb codes, hexcodes, or colour names. When
#'    \code{colour_var} resolves to a category, an unnamed vector with a number of
#'    elements equal to the number of unique groups is used directly (in sorted group
#'    order); a named vector can be used to map specific colours to specific group
#'    values. Any other input (including a palette name, single colour, or a vector
#'    that doesn't match the number of groups) is instead treated as a palette source
#'    and automatically expanded to the required number of colours. Defaults to the
#'    RColorBrewer "Blues" palette when not supplied.}
#'    \item{point_opacity}{Numeric value between 0 and 1 to determine point opacity.
#'    Default = \code{0.9}.}
#'    \item{point_stroke}{Logical to draw a border around each point. Default =
#'    \code{TRUE}.}
#'    \item{point_stroke_colour}{Colour of the point border when \code{point_stroke =
#'    TRUE}. Default = \code{"black"}.}
#'    \item{point_stroke_weight}{Numeric line weight of the point border when
#'    \code{point_stroke = TRUE}. Default = \code{0.5}.}
#'    \item{point_size}{Size of the plotted points. If supplied as a number, all
#'    points will be plotted with this size. If supplied as the name of a numeric
#'    variable within df, then the size of each point will be scaled relative to
#'    the value of that variable (a bubble map). Default = \code{6}.}
#'    \item{point_size_range}{Numeric vector of length 2, giving the minimum and
#'    maximum plotted size of points when \code{point_size} is a variable name.
#'    Default = \code{c(4, 20)}.}
#'    \item{popup_col}{Name of a variable in df containing pre-built html popup
#'    content to display when a point is clicked. Default = \code{NULL}.}
#'    \item{point_labels}{Name of a variable in df containing text labels to apply
#'    as hover-labels for each point. Accepts html to format the output labels.
#'    Default = \code{NULL}.}
#'    \item{cluster}{Logical to cluster nearby points together. Default =
#'    \code{FALSE}.}
#'    \item{cluster_max_radius}{Numeric, the maximum radius (in pixels) that a
#'    cluster's "territory" can be when \code{cluster = TRUE}. Uses the leaflet
#'    default of 80 if not supplied.}
#'    \item{points_group}{Name used for the points layer group, used in
#'    \code{addLayersControl} when more than one basemap is provided. Default =
#'    \code{"Cases"}.}
#'    \item{basemaps}{Named character vector of leaflet provider tile IDs to use as
#'    base layers (e.g. \code{c("Uncluttered" = "Esri.WorldTopoMap", "Detailed" =
#'    "OpenStreetMap")}). Names are used as the base layer labels in the layer
#'    control. When more than one basemap is supplied, a layer control is
#'    automatically added. Default = \code{c(OpenStreetMap = "OpenStreetMap")}.}
#'    \item{layers_control_pos}{Position of the basemap layer control. Default =
#'    \code{"bottomleft"}.}
#'    \item{measure_tool}{Logical to add a distance-measuring tool to the map.
#'    Default = \code{FALSE}.}
#'    \item{measure_units}{Units used for the measuring tool when \code{measure_tool
#'    = TRUE}. Default = \code{"kilometers"}.}
#'    \item{size_legend}{Logical to add a custom html legend illustrating point
#'    sizes (e.g. to describe a categorical sizing choice that isn't captured by
#'    \code{colour_var}). Requires \code{size_legend_sizes} and
#'    \code{size_legend_labels} to be provided. Default = \code{FALSE}.}
#'    \item{size_legend_sizes}{\strong{Mandatory if \code{size_legend = TRUE}.}
#'    Numeric vector of circle pixel sizes to display in the size legend when
#'    \code{size_legend = TRUE}. Default = \code{NULL}.}
#'    \item{size_legend_labels}{\strong{Mandatory if \code{size_legend = TRUE}.}
#'    Character vector of labels corresponding to \code{size_legend_sizes}, must
#'    be of equal length. Default = \code{NULL}.}
#'    \item{size_legend_title}{Text used for the size legend title. Default = \code{""}.}
#'    \item{size_legend_pos}{Position of the size legend. Default = \code{"bottomleft"}.}
#'    \item{map_title}{string to determine map title. Default = \code{""}.}
#'    \item{map_title_size}{ font size of map title. Default = \code{13}.}
#'    \item{map_title_colour}{ string to determine map title colour. Default = \code{"black"}.}
#'    \item{map_footer}{string to determine map footer. Default = \code{""}.}
#'    \item{map_footer_size}{ font size of map footer. Default = \code{12}.}
#'    \item{map_footer_colour}{ string to determine map footer colour. Default = \code{"black"}.}
#'    \item{legend_title}{ string to determine legend title. Default = \code{""}.}
#'    \item{legend_pos}{ string to determine legend position. Permitted values =
#'    "topright", "bottomright", "bottomleft", or "topleft". Default = \code{"topright"}.}
#'    \item{legend_scrollable}{ logical, whether the colour legend should become
#'    scrollable (rather than grow unbounded) once it exceeds \code{legend_max_height}.
#'    Only applies to the colour legend (not the custom size legend). Default =
#'    \code{TRUE}.}
#'    \item{legend_max_height}{ numeric, the maximum height (in pixels) of the
#'    colour legend before it becomes scrollable. Only applies to the colour
#'    legend (not the custom size legend), and only when \code{legend_scrollable =
#'    TRUE}. Default = \code{300}.}
#'    \item{map_zoom}{ A single row data frame with variables of 'LAT', 'LONG', and 'zoom'
#'    that allows the map to be zoomed in on a specific region (e.g. data.frame(LONG =
#'    -2.547855, LAT = 53.00366, zoom = 6)). LAT = numerical latitude coordinate for
#'    the centre point of the zoom, LONG = numerical longitude coordinate for the
#'    centre point of the zoom, zoom = numerical value to represent the depth of zoom.
#'    Default = \code{NULL}.}
#'    \item{border_shape_name}{ Optional filepath for a shapefile containing boundaries
#'    to be included as background context for the plotted points. Only boundaries
#'    contained within border_shape_name will be used, areas will be unfilled.}
#'    \item{border_code_col}{ Variable name of the area code / name within border_shape_name.
#'    Required if a specific area within the border shapefile is required.}
#'    \item{border_areaname}{ Character vector containing the name of specific areas within
#'    border_code_col to be plotted. If supplied, only the boundaries included in border_areaname
#'    will be plotted. If not supplied, the boundaries of all areas within border_shape_name
#'    will be plotted.}
#'  }
#'
#'
#' @return A leaflet object.
#'
#' @import RColorBrewer
#' @import grDevices
#' @import sf
#' @import leaflet
#' @import htmltools
#' @import scales
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example 1: Dynamic map of cases coloured by a stratification variable, with
#' # two basemap choices, layer control, and clustering.
#'
#' library(dplyr)
#'
#' case_points <- lab_data |>
#'   filter(!is.na(region)) |>
#'   mutate(long = jitter(rep(-2.5, dplyr::n()), amount = 3),
#'          lat = jitter(rep(53, dplyr::n()), amount = 3))
#'
#' cases_map <- point_map(
#'   params = list(
#'     df = case_points,
#'     lat = "lat",
#'     long = "long",
#'     colour_var = "organism_species_name",
#'     cluster = TRUE,
#'     basemaps = c("Uncluttered" = "Esri.WorldTopoMap", "Detailed" = "OpenStreetMap"),
#'     points_group = "Cases",
#'     legend_title = "Stratification by selected variable",
#'     legend_pos = "bottomright"
#'   )
#' )
#'
#'
#'
#' # Example 2: Dynamic case-level map with popups, hover labels, bubble sizing
#' # randomised by lab test type (PCR/WGS), separate colour and size legends
#' # (one scrollable), a measuring tool, and a zoomed starting view.
#'
#' case_points <- case_points |>
#'   filter(format(specimen_date, "%Y-%m-%d") == "2023-01-01") |>
#'   mutate(
#'     test_type = sample(c("PCR", "WGS"), dplyr::n(), replace = TRUE),
#'     case_size = ifelse(test_type == "WGS", 13, 6),
#'     popup_html = paste0("<strong>Region: </strong>", region,
#'                         "<br><strong>Specimen date: </strong>", specimen_date)
#'   )
#'
#' detailed_map <- point_map(
#'   params = list(
#'     df = case_points,
#'     lat = "lat",
#'     long = "long",
#'     colour_var = "organism_species_name",
#'     point_size = "case_size",
#'     point_size_range = c(6, 13),
#'     popup_col = "popup_html",
#'     point_labels = "local_authority_name",
#'     legend_title = "Organism species",
#'     legend_scrollable = TRUE,
#'     legend_max_height = 60,
#'     size_legend = TRUE,
#'     size_legend_sizes = c(13, 6),
#'     size_legend_labels = c("WGS", "PCR"),
#'     size_legend_title = "Lab test type",
#'     measure_tool = TRUE,
#'     map_zoom = data.frame(LONG = -2.89479, LAT = 54.793409, zoom = c(5))
#'   )
#' )
#'
#' }
point_map <- function (params = list(
                         df = NULL,
                         lat = NULL,
                         long = NULL,
                         colour_var = NULL,
                         colour_type = "auto",
                         point_colours = NULL,
                         point_opacity = 0.9,
                         point_stroke = TRUE,
                         point_stroke_colour = "black",
                         point_stroke_weight = 0.5,
                         point_size = 6,
                         point_size_range = c(4, 20),
                         popup_col = NULL,
                         point_labels = NULL,
                         cluster = FALSE,
                         cluster_max_radius = NULL,
                         points_group = "Cases",
                         basemaps = c("OpenStreetMap" = "OpenStreetMap"),
                         layers_control_pos = "bottomleft",
                         measure_tool = FALSE,
                         measure_units = "kilometers",
                         size_legend = FALSE,
                         size_legend_sizes = NULL,
                         size_legend_labels = NULL,
                         size_legend_title = "",
                         size_legend_pos = "bottomleft",
                         map_title = "",
                         map_title_size = 13,
                         map_title_colour = "black",
                         map_footer = "",
                         map_footer_size = 12,
                         map_footer_colour = "black",
                         legend_title = "",
                         legend_pos = "topright",
                         legend_scrollable = TRUE,
                         legend_max_height = 300,
                         map_zoom = NULL,
                         border_shape_name = NULL,
                         border_code_col = NULL,
                         border_areaname = NULL
                       )
) {


  # Solve warnings regarding font family not found using utils/set_Arial() function
  #    -Sets chart_font variable
  set_Arial()


  # Assign any missing default args to params list
  if(!exists('colour_type',where=params)) params$colour_type <- "auto"
  if(!exists('point_opacity',where=params)) params$point_opacity <- 0.9
  if(!exists('point_stroke',where=params)) params$point_stroke <- TRUE
  if(!exists('point_stroke_colour',where=params)) params$point_stroke_colour <- "black"
  if(!exists('point_stroke_weight',where=params)) params$point_stroke_weight <- 0.5
  if(!exists('point_size',where=params)) params$point_size <- 6
  if(!exists('point_size_range',where=params)) params$point_size_range <- c(4, 20)
  if(!exists('cluster',where=params)) params$cluster <- FALSE
  if(!exists('points_group',where=params)) params$points_group <- "Cases"
  if(!exists('basemaps',where=params)) params$basemaps <- c("OpenStreetMap" = "OpenStreetMap")
  if(!exists('layers_control_pos',where=params)) params$layers_control_pos <- "bottomleft"
  if(!exists('measure_tool',where=params)) params$measure_tool <- FALSE
  if(!exists('measure_units',where=params)) params$measure_units <- "kilometers"
  if(!exists('size_legend',where=params)) params$size_legend <- FALSE
  if(!exists('size_legend_title',where=params)) params$size_legend_title <- ""
  if(!exists('size_legend_pos',where=params)) params$size_legend_pos <- "bottomleft"
  if(!exists('map_title',where=params)) params$map_title <- ""
  if(!exists('map_title_size',where=params)) params$map_title_size <- 13
  if(!exists('map_title_colour',where=params)) params$map_title_colour <- "black"
  if(!exists('map_footer',where=params)) params$map_footer <- ""
  if(!exists('map_footer_size',where=params)) params$map_footer_size <- 12
  if(!exists('map_footer_colour',where=params)) params$map_footer_colour <- "black"
  if(!exists('legend_title',where=params)) params$legend_title <- ""
  if(!exists('legend_pos',where=params)) params$legend_pos <- "topright"
  if(!exists('legend_scrollable',where=params)) params$legend_scrollable <- TRUE
  if(!exists('legend_max_height',where=params)) params$legend_max_height <- 300




  # Checks and warnings

  # Check if df is missing
  if (!exists('df',where=params)) stop("A data frame argument is required")

  # Check df is a df class
  if(!is.data.frame(params$df)) stop("Argument df is not a data frame object")

  # Check if lat argument is missing
  if ((is.null(params$lat)) | !exists('lat',where=params))
    stop("Please include a variable from df for lat, i.e. lat = \"variable_name\"")

  # Check if long argument is missing
  if ((is.null(params$long)) | !exists('long',where=params))
    stop("Please include a variable from df for long, i.e. long = \"variable_name\"")

  # Check if lat column name is in df
  if (!params$lat %in% colnames(params$df))
    stop("lat not found within df. Please include a variable from df for lat, i.e. lat = \"variable_name\"")

  # Check if long column name is in df
  if (!params$long %in% colnames(params$df))
    stop("long not found within df. Please include a variable from df for long, i.e. long = \"variable_name\"")

  # Check if colour_var column name is in df
  if (!is.null(params$colour_var)) {
    if (!params$colour_var %in% colnames(params$df))
      stop("colour_var not found within df. Please include a variable from df for colour_var, i.e. colour_var = \"variable_name\"")
  }

  # Check that colour_type is a permissable value
  if (!(params$colour_type %in% c("auto", "continuous", "discrete")))
    stop("colour_type must equal \"auto\", \"continuous\", or \"discrete\"")

  # Check if number of groups and number of point colours are the same, when point_colours
  # is supplied as a named (explicit) colour-to-group mapping
  if (!is.null(params$colour_var) & !is.null(names(params$point_colours))) {
    if (length(params$point_colours) != length(unique(params$df[[params$colour_var]])))
      stop("The number of point_colours provided must equal the number of unique groups in colour_var")
  }

  # Warn that multiple colours have been provided but colour_var absent
  if (length(params$point_colours) > 1 & is.null(params$colour_var))
    warning("Multiple point_colours have been provided but colour_var is absent")

  # Check if point_size variable name is in df when supplied as a character
  if (is.character(params$point_size)) {
    if (!params$point_size %in% colnames(params$df))
      stop("point_size not found within df. Please include a numeric variable from df, or a fixed numeric value")
  }

  # Check that legend_pos is a leaflet permissable value
  if (!(params$legend_pos %in% c("topright", "bottomright", "bottomleft", "topleft")))
    stop("legend_pos must equal \"topright\", \"bottomright\", \"bottomleft\", or \"topleft\"")

  # Check that size_legend_sizes and size_legend_labels are both provided and of equal length
  if (params$size_legend == TRUE) {
    if (is.null(params$size_legend_sizes) | is.null(params$size_legend_labels))
      stop("size_legend_sizes and size_legend_labels must both be provided when size_legend = TRUE")
    if (length(params$size_legend_sizes) != length(params$size_legend_labels))
      stop("size_legend_sizes and size_legend_labels must be vectors of equal length")
  }

  # Check that basemaps is a named vector so that base layer labels can be derived
  if (length(params$basemaps) > 1 & is.null(names(params$basemaps)))
    stop("basemaps must be a named character vector when more than one basemap is provided, e.g.
         c(\"Uncluttered\" = \"Esri.WorldTopoMap\", \"Detailed\" = \"OpenStreetMap\")")

  # Check that map_zoom df contains the 'LAT', 'LONG', and 'zoom' variables
  if (!is.null(params$map_zoom) & !all(c("LONG","LAT","zoom") %in% names(params$map_zoom)))
    stop("map_zoom must be a single-row df containing variables of 'LAT', 'LONG', and 'zoom',
         e.g. data.frame(LONG = -2.547855, LAT = 53.00366, zoom = 6)")

  # Check that map_zoom df contains only a single row
  if (!is.null(params$map_zoom)) {
    if(nrow(params$map_zoom) != 1)
      stop("map_zoom must be a single-row df containing variables of 'LAT', 'LONG', and 'zoom',
           e.g. data.frame(LONG = -2.547855, LAT = 53.00366, zoom = 6)")
  }

  # warn that border_shape_name not included but border_areaname or border_code_col are included
  if (is.null(params$border_shape_name) & !is.null(params$border_code_col))
    warning("border_shape_name not included but border_code_col provided")
  if (is.null(params$border_shape_name) & !is.null(params$border_areaname))
    warning("border_shape_name not included but border_areaname provided")




  ##### Parameter assignment

  # Define parameters as variables using utils/param_assign() function
  param_assign(params,
               c("df","lat","long","colour_var","colour_type","point_colours",
                 "point_opacity","point_stroke","point_stroke_colour","point_stroke_weight",
                 "point_size","point_size_range",
                 "popup_col","point_labels",
                 "cluster","cluster_max_radius","points_group","basemaps",
                 "layers_control_pos","measure_tool","measure_units","size_legend",
                 "size_legend_sizes","size_legend_labels","size_legend_title",
                 "size_legend_pos","map_title","map_title_size","map_title_colour",
                 "map_footer","map_footer_size","map_footer_colour","legend_title",
                 "legend_pos","legend_scrollable","legend_max_height","map_zoom","border_shape_name","border_code_col",
                 "border_areaname"))





  #################### POINT MAP #################################


  ##### DEFINE MAP DATA

  # Assign 'Lat' and 'Long' columns to dataframe
  df <- df |>
    mutate(Lat = get({{lat}}),
           Long = get({{long}}))

  # Assign 'Colour' column if colour_var supplied, and resolve whether it should
  # be treated as continuous or discrete
  is_discrete <- FALSE
  if (!is.null(colour_var)) {
    df <- df |> mutate(Colour = get({{colour_var}}))

    is_discrete <- switch(colour_type,
                          "discrete" = TRUE,
                          "continuous" = FALSE,
                          "auto" = !is.numeric(df$Colour))
  }

  # Assign 'Size' column and flag if point_size is a variable name
  size_is_var <- is.character(point_size)
  if (size_is_var) {
    df <- df |> mutate(Size = get({{point_size}}))
  }

  # Assign 'Label' column if point_labels supplied
  if (!is.null(point_labels)) {
    df <- df |> mutate(Label = get({{point_labels}}))
  }

  # Assign 'Popup' column if popup_col supplied
  if (!is.null(popup_col)) {
    df <- df |> mutate(Popup = get({{popup_col}}))
  }



  ##### Define additional map border if provided

  if (!is.null(border_shape_name)) {
    # Read in shapefile based on whether it's provided as a filepath or df
    if (is.data.frame(border_shape_name)) {
      border_sf <- st_as_sf(border_shape_name)
    } else {
      border_sf <- st_read(border_shape_name)
    }

    # Check if border_code_col has been provided to filter by specific areas, if not print message
    if (is.null(border_code_col)) {
      warning("border_code_col is not provided so all border shapefile areas will be included")
    }

    # Filter for specific area for border in shapefile
    if (!is.null(border_code_col)) {
      # Check if we have area to filter by, if not print message
      if (is.null(border_areaname)) {
        warning("border_areaname is not provided so all border shapefile areas will be included")

      } else {

        # Filter area
        border_sf <- border_sf |>
          filter(get({{border_code_col}}) %in% border_areaname)

      }

    }

  }




  ######################## PRODUCE MAP ##############################

  # Resolve colour legend scroll styling (only applied when legend_scrollable = TRUE;
  # the custom size legend is not affected by legend_max_height/legend_scrollable)

  legend_scroll_css <- if (legend_scrollable) {
    paste0("max-height:", legend_max_height, "px; overflow-y: auto;")
  } else {
    ""
  }


  # Add title controls

  tag.map.title <- htmltools::tags$style(htmltools::HTML(
    paste0(
      "
  .leaflet-control.map-title {
    transform: translate(30px,-0px);
    position: absolute !important;
    width: max-content;
    text-align: left;
    padding-left: 10px;
    padding-right: 10px;
    opacity: 1;
    font-weight: bold;
    font-size:", map_title_size, "px;
    font-family: Helvetica;
    color:", map_title_colour,";
  }
  .leaflet-control.map-legend {
    ", legend_scroll_css, "
  }
  ")))


  # Add footer controls

  tag.map.footer <- htmltools::tags$style(htmltools::HTML(
    paste0(
      "
.leaflet-control.map-footer {
  font-family: Helvetica;
  font-size:", map_footer_size, "px;
  color:", map_footer_colour,";
}
")))


  # Title text

  title <- htmltools::tags$div(
    tag.map.title, htmltools::HTML(gsub("\n","<br>",map_title))
  )


  # Footer text

  footer <- htmltools::tags$div(
    tag.map.footer, htmltools::HTML(gsub("\n","<br>",map_footer))
  )



  # Define list of hover labels, add default if not supplied

  if (!is.null(point_labels)) {

    labels <- df |>
      mutate(labs = gsub("\\n","<br>", Label))

  } else if (!is.null(colour_var) & !is_discrete) {

    labels <- df |>
      mutate(labs = paste0(Colour))

  } else {

    labels <- df |>
      mutate(labs = "")

  }

  labels <- as.list(labels$labs)



  # Resolve point colours

  if (!is.null(colour_var) & is_discrete) {

    group_levels <- sort(unique(df$Colour))

    # Named point_colours are an explicit value -> colour mapping and must cover
    # every group. An unnamed vector matching the number of groups is used
    # directly (in sorted group order). Anything else (NULL, a palette name, a
    # single colour, or a mismatched seed vector) is treated as a palette source
    # and auto-expanded to the required number of colours.
    if (!is.null(names(point_colours))) {
      group_palette <- point_colours
    } else if (!is.null(point_colours) && length(point_colours) == length(group_levels)) {
      group_palette <- point_colours
      names(group_palette) <- group_levels
    } else {
      palette_source <- if (is.null(point_colours)) "Blues" else point_colours
      group_palette <- palette_gen(palette_source, length(group_levels))
      names(group_palette) <- group_levels
    }

    colour_pal <- leaflet::colorFactor(group_palette, domain = df$Colour)
    point_colour_values <- colour_pal(df$Colour)

  } else if (!is.null(colour_var)) {

    palette_source <- if (is.null(point_colours)) "Blues" else point_colours
    gradient_colours <- palette_gen(palette_source, 100)
    colour_pal <- leaflet::colorNumeric(gradient_colours, domain = df$Colour)
    point_colour_values <- colour_pal(df$Colour)

  } else {
    point_colour_values <- if (!is.null(point_colours)) point_colours[1] else "blue"
  }


  # Resolve point radii

  if (size_is_var) {
    radius_values <- scales::rescale(df$Size, to = point_size_range)
  } else {
    radius_values <- point_size
  }


  # Resolve clustering options

  if (cluster) {
    if (!is.null(cluster_max_radius)) {
      cluster_opts <- leaflet::markerClusterOptions(maxClusterRadius = cluster_max_radius)
    } else {
      cluster_opts <- leaflet::markerClusterOptions()
    }
  } else {
    cluster_opts <- NULL
  }


  # Resolve basemap group names (fall back to the provider ID if unnamed)

  basemap_names <- names(basemaps)
  if (is.null(basemap_names) || any(basemap_names == "")) {
    basemap_names <- unname(basemaps)
  }


  # Create map, adding each basemap as its own tile layer

  map <- leaflet::leaflet(df)

  for (i in seq_along(basemaps)) {
    map <- map |>
      leaflet::addProviderTiles(basemaps[[i]], group = basemap_names[i])
  }


  # add points

  map <- map |>
    addCircleMarkers(lng = ~Long,
                     lat = ~Lat,
                     radius = radius_values,
                     stroke = point_stroke,
                     weight = point_stroke_weight,
                     color = point_stroke_colour,
                     fillColor = point_colour_values,
                     fillOpacity = point_opacity,
                     label = lapply(labels, htmltools::HTML),
                     popup = if (!is.null(popup_col)) lapply(df$Popup, htmltools::HTML) else NULL,
                     group = points_group,
                     clusterOptions = cluster_opts
    ) |>

    # Add title

    addControl(title, position = "topleft", className="map-title") |>

    # Add footer

    addControl(footer, position = "bottomleft", className="map-footer")


  # Add map border if provided

  if (!is.null(border_shape_name)) {

    map <- map |>
      addPolylines(data = border_sf,
                   color = "black",
                   weight = 0.5,
      )

  }


  # add basemap layer control if more than one basemap provided

  if (length(basemaps) > 1) {

    map <- map |>
      addLayersControl(baseGroups = basemap_names,
                       overlayGroups = points_group,
                       position = layers_control_pos,
                       options = leaflet::layersControlOptions(collapsed = FALSE))

  }


  # add legend if colour_var provided

  if (!is.null(colour_var)) {

    map <- map |>
      addLegend(pal = colour_pal,
                values = df$Colour,
                opacity = point_opacity,
                title = gsub("\n","<br>",legend_title),
                position = legend_pos,
                className = "info legend map-legend")

  }


  # add custom size legend if provided

  if (size_legend) {

    legend_items <- mapply(function(sz, lab) {
      paste0(
        "<div style='display:flex;align-items:center;margin-bottom:4px;",
        "font-size:11px;font-family:", chart_font, ";'>",
        "<div style='width:", sz, "px;height:", sz, "px;background-color:#A9A9A9;",
        "border-radius:50%;border:1px solid #707070;margin-right:6px;'></div>",
        "<span>", lab, "</span></div>"
      )
    }, size_legend_sizes, size_legend_labels)

    size_legend_html <- htmltools::HTML(paste0(
      "<div style='background:white;padding:8px;border:1px solid #ccc;",
      "font-family:", chart_font, ";'>",
      if (nzchar(size_legend_title)) paste0("<strong style='font-size:11px;'>", size_legend_title, "</strong>") else "",
      paste(legend_items, collapse = ""),
      "</div>"
    ))

    map <- map |>
      addControl(size_legend_html, position = size_legend_pos)

  }


  # add measuring tool if requested

  if (measure_tool) {

    map <- map |>
      addMeasure(primaryLengthUnit = measure_units, secondaryLengthUnit = measure_units)

  }


  ## set default position and zoom

  if(!is.null(map_zoom)){

    map <- map |>
      setView(lat = map_zoom$LAT,
              lng = map_zoom$LONG,
              zoom = map_zoom$zoom)
  }

  return(map)

}
