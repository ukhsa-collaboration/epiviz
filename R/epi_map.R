#' Epi Map
#'
#' @description A function for producing either static (ggplot) or dynamic (leaflet)
#' choropleth maps.
#'
#' @param dynamic Logical indicating whether to produce a dynamic (leaflet) output.
#' Default is \code{FALSE}, which will return a static ggplot output.
#' @param params A named list containing arguements used in map.
#' \describe{
#'    \item{df}{Data frame containing values used to fill areas on the output map.
#'          Can include pre-merged shapefile data if inc_shp = TRUE.}
#'    \item{value_col}{Name of the variable in df used to fill map areas.}
#'    \item{data_areacode}{Name of the variable in df containing the name or code of
#'          the map areas to be plotted. (Mandatory if shp_name argument passed).}
#'    \item{inc_shp}{boolean parameter to indicate whether df already includes
#'          shapefile data.}
#'    \item{shp_name}{ Data frame name or filepath of the shapefile containing the spatial information
#'          for the resultant map output. This will not be used if inc_shp = TRUE.}
#'    \item{shp_areacode}{ Name of the variable in shp_name containing the name
#'          or code of the map areas to be plotted. (Mandatory if shp_name argument passed).}
#'    \item{fill_palette}{Colour palette used to fill the map areas. Can be provided as either
#'    the name of an RColorBrewer palette (e.g. \code{fill_palette = "YlOrRd"}), a character containing
#'    a single rgb colour code, hexcode, or colour name that will be used to generate a colour range (e.g.
#'    \code{fill_palette = "#007C91"}), or a character vector containing multiple rgb codes, hexcodes,
#'    or colour names that will be used to generate a colour range (e.g. \code{c("#007C91","purple","red")}).
#'    Defaults to the RColorBrewer "Blues" palette.)}
#'    \item{fill_opacity}{numeric value between 0 and 1 to determine map fill-color opacity.}
#'    \item{break_intervals}{numeric vector of interval points for legend
#'    (Mandatory if break_labels argument is passed, break_intervals and break_labels must be
#'    of equal length).}
#'    \item{break_labels}{ vector of labels to include in the legend.
#'    (Mandatory if break_labels argument is passed, break_intervals and break_labels must be
#'    of equal length).}
#'    \item{force_cat}{ boolean parameter to determine whether all arguments passed
#'    in break_labels are used in the legend, even if there are no values present in the data.}
#'    \item{n_breaks}{ Number of break intervals. This argument is an alternative
#'    to supplying defined breaks via break_labels, and will provide a number of
#'    evenly distributed breaks as specified (default = 5). If break_labels argument is passed,
#'    n_breaks will be ignored.}
#'    \item{labels}{ name of string variable in df containing labels for each map area. If
#'    dynamic = FALSE, these labels will be positioned in the centre of each map area.
#'    If dynamic = TRUE, then these labels will appear as hover-over labels. If dynamic =
#'    TRUE, labels can include HTML.}
#'    \item{map_title}{string to determine map title.}
#'    \item{map_title_size}{ font size of map title.}
#'    \item{map_title_colour}{ string to determine map title colour.}
#'    \item{map_footer_size}{ font size of map footer.}
#'    \item{map_footer_colour}{ string to determine map title colour.}
#'    \item{area_labels boolean}{ parameter to add data_areacode as static area labels to the
#'    map areas. If dynamic = FALSE and a labels parameter has alredy been supplied, then area_labels
#'    will be ignored.}
#'    \item{area_labels_topn}{ numeric value to display only area_labels for areas with
#'    the top n values of value_col (e.g. if area_labels_topn = 5, only area_labels for map
#'    areas with the top 5 values of value_col will be displayed).}
#'    \item{legend_title}{ string to determine legend title.}
#'    \item{legend_pos}{ string to determine legend position. When dynamic = TRUE, both
#'    ggplot and leaflet permissable legend positions can be provided. When dynamic = FALSE,
#'    only leaflet permissable legend positions can be provided (i.e."topright", "bottomright",
#'    "bottomleft", or "topleft").}
#'    \item{map_zoom}{ A single row data frame with variables of 'LAT', 'LONG', and 'zoom'
#'    that allows the map to be zoomed in on a specific region (e.g. data.frame(LONG =
#'    -2.547855, LAT = 53.00366, zoom = 6)). LAT = numerical latitude coordinate for
#'    the centre point of the zoom, LONG = numerical longitude coordinate for the
#'    centre point of the zoom, zoom = numerical value to represent the depth of zoom.}
#'    \item{border_shape_name}{ Optional filepath for a shapefile containing additional
#'    borders to include in the output map. This should be a higher geography than the base
#'    map (e.g. if creating a map displaying UTLAs, a shapefile containing regional boundaries
#'    or higher should be used). Only boundaries contained within border_shape_name will
#'    be used, areas will be unfilled.}
#'    \item{border_code_col}{ Variable name of the area code / name within border_shape_name.
#'    Required if a specific area within the border shapefile is required.}
#'    \item{border_areaname}{ Character vector containing the name of specific areas within
#'    border_code_col to be plotted. If supplied, only the boundaries included in border_areaname
#'    will be plotted. If not supplied, the boundaries of all areas within border_shape_name
#'    will be plotted.}
#'  }
#'
#'
#' @return A ggplot or leaflet object.
#'
#' @import classInt
#' @import RColorBrewer
#' @import grDevices
#' @import sf
#' @import leaflet
#' @import htmltools
#' @import stringr
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example 1: Create a static map of Staphylococcus Aureus detections in London
#' # Local Authority Districts.
#'
#' # Define values for choropleth map using lab_data dataset
#' London_staph_detections <- lab_data |>
#'   filter(region == "London", organism_species_name == "STAPHYLOCOCCUS AUREUS") |>
#'   group_by(local_authority_name) |>
#'   summarise(detections = n())
#'
#' # Create static map using London_LA_boundaries_2023 data
#' London_staph_detections_map <- epi_map(
#'   params = list(
#'     df = London_staph_detections,
#'     value_col = "detections",
#'     data_areacode = "local_authority_name",
#'     inc_shp = FALSE,
#'     area_labels = TRUE,
#'     shp_name = London_LA_boundaries_2023,
#'     shp_areacode = "LAD23NM",
#'     map_title = "Staphylococcus Aureus detections in London Local Authority Districts",
#'     map_zoom = data.frame(LONG = c(-0.12776), LAT = c(51.50735), zoom = c(8.7)),
#'     legend_title = "Number of \nDetections",
#'     legend_pos = "right")
#' )
#'
#'
#'
#' # Example 2: Create a static map of Klebsiella Pneumoniae detections in England
#' # public health regions using data pre-merged with a shapefile.
#'
#' # Define values for choropleth map using the lab_data dataset
#' kleb_pneu_detections <- lab_data |>
#'   filter(organism_species_name == "KLEBSIELLA PNEUMONIAE") |>
#'   group_by(region) |>
#'   summarise(detections = n()) |>
#'   ungroup()
#'
#' # Add column defining labels to apply to map areas
#' kleb_pneu_detections <- kleb_pneu_detections |>
#'   mutate(map_labels = paste0(region,": \n",detections))
#'
#' # Join with the PHEC_boundaries_2016 shapefile data
#' kleb_pneu_detections_shp <- left_join(x = PHEC_boundaries_2016, y = kleb_pneu_detections,
#'                                       by = c("phec16nm" = "region"))
#'
#' # Define parameter list for map function
#' kleb_pneu_params <- list(
#'   df = kleb_pneu_detections_shp,
#'   value_col = "detections",
#'   data_areacode = "phec16nm",
#'   inc_shp = TRUE,
#'   fill_palette = "YlOrRd",
#'   fill_opacity = 0.7,
#'   labels = "map_labels",
#'   map_title = "Number of Klebsiella Pneumoniae detections \nin UK public health regions",
#'   map_title_size = 12,
#'   map_title_colour = "orangered",
#'   map_footer = "Map represents simulated test data only.",
#'   map_footer_size = 10,
#'   map_footer_colour = "black",
#'   legend_title = "Number of \nDetections",
#'   legend_pos = "topright",
#'   break_labels = c("0-499","500-999","1000-1499","1500-1999","2000-2499","2500+"),
#'   break_intervals = c(0,500,1000,1500,2000,2500),
#'   force_cat = TRUE
#' )
#'
#' # Create map
#' kleb_pneu_detections_map <- epi_map(dynamic = FALSE, params = kleb_pneu_params)
#'
#'
#'
#' # Example 3: Refactor the above map as dynamic map, only add area_labels for
#' # the top 5 areas by number of detections, and add an additional border using
#' # the UK_boundaries_2023 shapefile data.
#'
#' # Create list of additional parameters
#' kleb_pneu_params_add <- list(
#'   area_labels = TRUE,
#'   area_labels_topn = 5,
#'   map_zoom = data.frame(LONG = c(-2.89479), LAT = c(54.793409), zoom = c(5)),
#'   border_shape_name = UK_boundaries_2023
#' )
#'
#' # Combine existing parameters list with additional parameters list
#' kleb_pneu_params_dyn <- c(kleb_pneu_params, kleb_pneu_params_add)
#'
#' # Create map
#' kleb_pneu_map_dynamic <- epi_map(dynamic = TRUE, params = kleb_pneu_params_dyn)
#'
#' }
epi_map <- function (dynamic = FALSE,
                     params = list(
                       df = NULL,
                       value_col = NULL,
                       data_areacode = NULL,
                       inc_shp = TRUE,
                       shp_name = NULL,
                       shp_areacode = NULL,
                       fill_palette = "Blues",
                       fill_opacity = 1.0,
                       break_intervals = NULL,
                       break_labels = NULL,
                       force_cat = TRUE,
                       n_breaks = NULL,
                       labels = NULL,
                       map_title = "",
                       map_title_size = 13,
                       map_title_colour = "black",
                       map_footer = "",
                       map_footer_size = 12,
                       map_footer_colour = "black",
                       area_labels = FALSE,
                       area_labels_topn = NULL,
                       legend_title = "",
                       legend_pos = "topright",
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
  if(!exists('inc_shp',where=params)) params$inc_shp <- TRUE
  if(!exists('fill_palette',where=params)) params$fill_palette <- "Blues"
  if(!exists('fill_opacity',where=params)) params$fill_opacity <- 1.0
  if(!exists('force_cat',where=params)) params$force_cat <- TRUE
  if(!exists('map_title',where=params)) params$map_title <- ""
  if(!exists('map_title_size',where=params)) params$map_title_size <- 13
  if(!exists('map_title_colour',where=params)) params$map_title_colour <- "black"
  if(!exists('map_footer',where=params)) params$map_footer <- ""
  if(!exists('map_footer_size',where=params)) params$map_footer_size <- 12
  if(!exists('map_footer_colour',where=params)) params$map_footer_colour <- "black"
  if(!exists('area_labels',where=params)) params$area_labels <- FALSE
  if(!exists('legend_title',where=params)) params$legend_title <- ""
  if(!exists('legend_pos',where=params)) params$legend_pos <- "topright"




  # Checks and warnings

  # Check if df is missing
  if (!exists('df',where=params)) stop("A data frame argument is required")

  # Check df is a df class
  if(!is.data.frame(params$df)) stop("Argument df is not a data frame object")

  # Check if value_col argument is missing
  if ((is.null(params$value_col)) | !exists('value_col',where=params))
    stop("Please include a variable from df for value_col, i.e. value_col = \"variable_name\"")

  # Check if data_areacode argument is missing
  if ((is.null(params$data_areacode)) | !exists('data_areacode',where=params))
    stop("Please include a variable from df for data_areacode, i.e. data_areacode = \"variable_name\"")

  # Check if value_col column name is in df
  if (!params$value_col %in% colnames(params$df))
    stop("value_col not found within df. Please include a variable from df for value_col, i.e. data_areacode = \"variable_name\"")

  # Check if data_areacode column name is in df
  if (!params$data_areacode %in% colnames(params$df))
    stop("data_areacode not found within df. Please include a data frame variable for data_areacode, i.e. data_areacode = \"variable_name\"")


  # Check if inc_shp = FALSE but no shp_name provided
  if (params$inc_shp == FALSE & !exists('shp_name',where=params))
    stop("A shp_name arguement must be provided when inc_shp = FALSE")

  # Check that if break_intervals has been passed then break_labels has also been passed
  if (!is.null(params$break_intervals) & is.null(params$break_labels))
    stop("If break_intervals is provided then break_labels must also be provided")
  if (is.null(params$break_intervals) & !is.null(params$break_labels))
    stop("If break_labels is provided then break_intervals must also be provided")

  # Check that break_labels and break_intervals are the same length
  if (!is.null(params$break_intervals)) {
    if (length(params$break_labels) != length(params$break_intervals)) {
      stop("break_labels and break_intervals must be vectors of equal length")
    }
  }

  # Check if dynamic = TRUE but legend_pos not a leaflet permissable value
  if (dynamic == TRUE & !(params$legend_pos %in% c("topright", "bottomright", "bottomleft", "topleft")))
    stop("When dynamic = TRUE, legend_pos must equal \"topright\", \"bottomright\", \"bottomleft\", or \"topleft\"")

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

  # Warn that n_breaks will be ignored if break_intervals is set
  if (!is.null(params$break_intervals) & !is.null(params$n_breaks))
    warning("n_breaks will be ignored if break_intervals is set")

  # Warn that area_labels will not be used for static map if labels are provided
  if ((dynamic == FALSE) & (params$area_labels == TRUE) & !is.null(params$labels))
    warning("area_labels will not be used if labels are provided when dynamic = FALSE")

  # warn that shp_name included but inc_shp == TRUE
  if (!is.null(params$shp_name) & (params$inc_shp == TRUE))
    warning("shp_name provided but shape data already included in df (inc_shp == TRUE)")

  # warn that border_shape_name not included but border_areaname or border_code_col are included
  if (is.null(params$border_shape_name) & !is.null(params$border_code_col))
    warning("border_shape_name not included but border_code_col provided")
  if (is.null(params$border_shape_name) & !is.null(params$border_areaname))
    warning("border_shape_name not included but border_areaname provided")




  ##### Parameter assignment

  # Define parameters as variables using utils/param_assign() function
  #   -Takes input list, compares it ro a reference vector of expected
  #     list elements, assigns each element to a variable within the
  #     parent environment, and allocates a value of 'NULL' to anything
  #     it can't find within the reference list.
  param_assign(params,
               c("df","value_col","data_areacode","inc_shp","shp_name",
                 "shp_areacode","fill_palette","fill_opacity","break_intervals",
                 "break_labels","force_cat","n_breaks","labels","map_title",
                 "map_title_size","map_title_colour","map_footer","map_footer_size",
                 "map_footer_colour","area_labels","area_labels_topn","legend_title",
                 "legend_pos","map_zoom","border_shape_name","border_code_col","border_areaname"))





  #################### EPI MAP #################################


  ##### DEFINE MAP DATA


  ##### Read in shapefile and map-fill variable data

  # Read in data based on whether the df and shapefile are already merged
  if(inc_shp == TRUE) {

    # Read in shapefile, assign 'Area' variable
    df <- st_as_sf(df) |>
      mutate(Area = get({{data_areacode}}))

  } else {

    # Read in shapefile based on whether it's provided as a filepath or df
    if (is.data.frame(shp_name)) {
      area_sf <- st_as_sf(shp_name)
    } else {
      area_sf <- st_read(shp_name)
    }

    # Merge data and shapefile, assign 'Area' variable
    df <- merge(area_sf, df, by.x = shp_areacode, by.y = data_areacode) |>
      mutate(Area = get({{shp_areacode}}))

  }


  # Assign 'Value' column to dataframe
  df <- df |>
    mutate(Value = get({{value_col}}))



  ##### Define map labels

  # Define centroid long & lat coords for label positions in each map area

  # Temporarily turn off spherical geometry to eliminate 'Edge X has duplicate vertex' error
  #    Centroids only used to position chart labels, true spherical centroid not needed
  suppressMessages(
    sf_use_s2(FALSE)
  )
  # Above generates warnings with each run, suppress
  suppressWarnings(
    # Add centroid co-ords to df
    df <- df |>
      mutate(centroid_long = sf::st_coordinates(sf::st_centroid(df$geometry))[,1],
             centroid_lat = sf::st_coordinates(sf::st_centroid(df$geometry))[,2])
  )
  # Turn spherical geometry back on
  suppressMessages(
    sf_use_s2(TRUE)
  )


  # Define area label text for static chart

  if(area_labels == TRUE) {

    # Rank data to only show top n labels
    if (!is.null(area_labels_topn)) {

      df <- df |>
        mutate(ranks = rank(desc(Value)),
               labels_static = case_when(ranks <= area_labels_topn ~ Area,
                                         TRUE ~ ""))
    } else {

      df <- df |> mutate(labels_static = Area)

    }

  }


  ##### Define breaks and colour palette   (for map-fill and legend)

  ### Breaks

  # Set breaks and break labels depending upon whether they're pre-defined
  if (is.null(break_intervals)) {
    # Set default of n_breaks to 5 if this is missing
    if (is.null(n_breaks)) {
      n_breaks <- 5

    }

    # Set equal-interval breaks
    data_sf <- df |>
      mutate(value_cat = cut(unlist(Value), n_breaks, dig.lab=10)) |>
      # Re-format labels; remove ( and [  brackets and change ',' to ' - '
      mutate(value_cat = gsub("\\(|\\]", "", unlist(value_cat))) |>
      mutate(value_cat = gsub("\\,", " - ", unlist(value_cat)))


    # # Set quantile breaks. Add .00001 offset to catch the lowest value
    # breaks_qt <-
    #   classIntervals(c(min(unlist(df$Value)),# - .00001,
    #                    unlist(df$Value)),
    #                  n = n_breaks, style = "quantile")

    # data_sf <- df |>
    #   mutate(value_cat = cut(unlist(Value), breaks_qt$brks, dig.lab=10)) |>  # quantile breaks option, dig.lab=10 to eliminate scientific notation in legend
    #   # Re-format labels; remove ( and [  brackets and change ',' to ' - '
    #   mutate(value_cat = gsub("\\(|\\]", "", unlist(value_cat))) |>
    #   mutate(value_cat = gsub("\\,", " - ", unlist(value_cat)))


    # Force force_cat to be FALSE
    force_cat <- FALSE

  } else {
    # count number of interval groups
    interval_grp <- c(1:length(break_intervals))

    # Create breaks of break intervals and labels
    breaks_qt <-
      data.frame(interval = interval_grp , value_cat = break_labels)

    # Assign values to intervals- then join labels
    data_sf <- df |>
      mutate(interval = findInterval(unlist(Value), break_intervals)) |>
      left_join(breaks_qt, by = "interval")

    # Create order based on which categories are included
    order <-
      data.frame(interval = data_sf$interval,
                 value_cat = data_sf$value_cat) |>
      arrange(interval) |>
      distinct()

    # Re-order factor to correct order
    data_sf <- data_sf |>
      mutate(
        value_cat = factor(value_cat, order$value_cat),
        interval = as.factor(interval)
      )


  }


  ### Colour palette

  if (force_cat == TRUE) {
    # Count number of intervals for palette
    n_pal <- as.numeric(length(break_intervals))

    # Use utils/palette_gen() function to generate palette of fill colours for map
    pal <- palette_gen(fill_palette, n_pal)

    # Create df of colours + categories for legend
    pal <- data.frame(value_cat = break_labels,
                      fill_colour = pal)

    # To use in drop category argument for map
    drop_cat <- FALSE


  } else {

    # Count number of intervals for palette
    if(!is.null(n_breaks)) {
      n_pal <- n_breaks
    } else {
      n_pal <- as.numeric(length(unique(data_sf$value_cat)))
    }

    # Use utils/palette_gen() function to generate palette of fill colours for map
    pal <- palette_gen(fill_palette, n_pal)

    # Create df of colours + categories for legend
    pal <- data.frame(value_cat = levels(cut(unlist(df$Value), n_pal, dig.lab=10)),
                      fill_colour = pal) |>
      mutate(value_cat = gsub("\\(|\\]", "", unlist(value_cat))) |>
      mutate(value_cat = gsub("\\,", " - ", unlist(value_cat)))

    # To use in drop category argument for map
    drop_cat <- TRUE

  }

  # merge fill-colors back into df for use in leaflet
  df <- merge(data_sf, pal, by = "value_cat")





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
          filter(get({{border_code_col}}) %in% border_areaname) ###

      }

    }

  }




  ######################## PRODUCE MAP ##############################

  ##### CREATE STATIC MAP

  if (!dynamic) {
    # produce ggplot object if 'dynamic' is set to FALSE

    ### GGPLOT START

    # If legend title is missing set to blank
    if (is.null(legend_title)) {
      legend_title <- ""

    }

    # Create map
    map <- ggplot() +
      geom_sf(data = data_sf,
              alpha = fill_opacity,
              #key_glyph = "rect", # remove border around legend keys; NO LONGER WORKS AS OF GGPLOT 2 V4.0.0
              aes(fill = value_cat),
              show.legend = TRUE) +  # ensure unused legend keys are coloured in: https://github.com/tidyverse/ggplot2/issues/5728
      guides(fill = guide_legend(override.aes = list(colour = NA))) + # remove border around legend keys; ALTERNATIVE TO ABOVE FUNCTIONALITY FOR GGPLOT2 V4.0.0
      # Set colours
      scale_fill_manual(
        values = alpha(pal$fill_colour, fill_opacity),
        limits = pal$value_cat,
        # Drop or keep all category levels for legend based on selection
        drop = drop_cat,
        # Add legend title
        name = legend_title
      ) +
      # Set title
      ggtitle(map_title) +
      # Set footer
      labs(caption = map_footer)


    # Add user defined labels
    if(!is.null(labels)) {

      map <- map +
        geom_text(
          data = data_sf,
          size = 8/.pt,
          family = chart_font,
          aes(x = centroid_long,
              y = centroid_lat,
              label = get({{labels}}))
        )
    }


    # Add area name labels; overridden if other area labels are defined
    if(area_labels == TRUE & is.null(labels)) {

      map <- map +
        geom_text(
          data = data_sf,
          size = 8/.pt,
          family = chart_font,
          aes(x = centroid_long,
              y = centroid_lat,
              label = stringr::str_wrap(labels_static,12),
              fontface = 'italic')
        )
    }


    # Add map border if provided

    if (!is.null(border_shape_name)) {

      # Add border overlay
      map <- map +
        geom_sf(
          data = border_sf,
          fill = NA,
          size = 0.2,
          colour = "black"
        )

    }


    # Zoom in if map_zoom is specified

    if (!is.null(map_zoom)) {

      # Define new co-ordinate bounding box from zoom parameters
      # credit: https://www.r-bloggers.com/2019/04/zooming-in-on-maps-with-sf-and-ggplot2/
      lon_span <- 360 / 2^map_zoom$zoom
      lat_span <- 180 / 2^map_zoom$zoom

      lon_bounds <- c(map_zoom$LONG - lon_span / 2, map_zoom$LONG + lon_span / 2)
      lat_bounds <- c(map_zoom$LAT - lat_span / 2, map_zoom$LAT + lat_span / 2)

      # Apply zoomed co-rdinate limits
      map <- map +
        coord_sf(xlim = lon_bounds, ylim = lat_bounds)

    }


    # Add formatting
    map <- map +
      # # # Remove axis text
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # Remove gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # Set legend text formattting
        legend.text = element_text(size = 10, family = chart_font),
        legend.title = element_text(
          face = "bold",
          size = 10,
          family = chart_font
        ),
        # Set title formatting
        plot.title = element_text(
          face = "bold",
          size = map_title_size,
          family = chart_font,
          colour = map_title_colour),
        # Set footer formatting
        plot.caption = element_text(
          hjust = 0,
          size = map_footer_size,
          colour = map_footer_colour)
      )


    # Position legend if legend_pos matches leaflet-only inputs
    if (legend_pos == "bottomright") {
      map <- map +
        theme(
          legend.position='right',
          legend.justification='bottom'
        )
    } else if (legend_pos == "topright") {
      map <- map +
        theme(
          legend.position='right',
          legend.justification='top'
        )
    } else if (legend_pos == "bottomleft") {
      map <- map +
        theme(
          legend.position='left',
          legend.justification='bottom'
        )
    } else if (legend_pos == "topleft") {
      map <- map +
        theme(
          legend.position='left',
          legend.justification='top'
        )
    } else {
      map <- map +
        theme(legend.position = legend_pos)
    }

    return(map)

    ### GGPLOT END



  ##### CREATE DYNAMIC MAP

  } else {
    # produce leaflet object if 'dynamic' is set to TRUE

    ### LEAFLET START

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
      tag.map.title, htmltools::HTML(gsub("\n","<br>",map_title))  # sub R linebreak for html linebreak
    )


    # Footer text

    footer <- htmltools::tags$div(
      tag.map.footer, htmltools::HTML(gsub("\n","<br>",map_footer))   # sub R linebreak for html linebreak
    )



    # Define list of labels for leaflet hover, add if not supplied

    if(is.null(labels)) {

      labels <- df |>
        mutate(labs = paste0(Area, ": ",  Value))

      labels <- as.list(labels$labs)

    } else {

      labels <- df |>
        mutate(labs = gsub("\\n","<br>",get({{labels}})))   # sub R linebreak for html linebreak

      labels <- as.list(labels$labs)

    }



    # Create map

    map <- leaflet::leaflet(st_as_sf(df)) |>


      ## add polygons

      addPolygons(stroke = TRUE,
                  color = "black",
                  weight = "0.5",
                  smoothFactor = 0.3,
                  fillOpacity = fill_opacity,
                  fillColor = ~fill_colour,
                  label = lapply(labels, htmltools::HTML),
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)
      ) |>


      # Add title

      addControl(title, position = "topleft", className="map-title") |>

      # Add footer

      addControl(footer, position = "bottomleft", className="map-footer") |>

      # add legend

      addLegend(colors = pal$fill_colour,
                labels = pal$value_cat,
                values = ~df$Value,
                opacity = fill_opacity,
                title = gsub("\n","<br>",legend_title),  # sub R linebreak for html linebreak
                position = legend_pos)



    ## add static labels for top n Areas

    if(area_labels == TRUE) {

      map <- map |>
        addLabelOnlyMarkers(lng = ~centroid_long,
                            lat = ~centroid_lat,
                            label = ~lapply(gsub("\\n","<br>", # sub R linebreaks for html linebreaks
                                                 stringr::str_wrap(labels_static,12)
                            ), htmltools::HTML),
                            labelOptions = labelOptions(noHide = TRUE,
                                                        direction = 'center',
                                                        textOnly = TRUE,
                                                        style = list("font-size" = "12px",
                                                                     "font-style" = "italic",
                                                                     #"color" = "blue",
                                                                     "font-family" = "sans-serif")
                            )
        )
    }


    ## set default position and zoom

    if(!is.null(map_zoom)){

      map <- map |>
        setView(lat = map_zoom$LAT,
                lng = map_zoom$LONG,
                zoom = map_zoom$zoom)
    }


    # Add border if provided

    if (!is.null(border_shape_name)) {

      # Add border overlay
      map <- map |>
        addPolylines(data = border_sf,
                     color = "black",
                     weight = 0.5,
        )

    }

    return(map)

  }  ### LEAFLET END

}








