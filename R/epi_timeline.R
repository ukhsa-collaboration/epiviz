#' Epidemiological Timeline
#'
#' @description A function for producing either a static (ggplot) or dynamic (plotly)
#' epidemiological timeline chart. This chart displays patient movements across
#' locations over time, with optional overlays for symptoms and clinical events
#' (specimen dates, death dates).
#'
#' @param dynamic Logical indicating whether to produce a dynamic (plotly) output.
#' Default is \code{FALSE}, which will return a static ggplot output.
#' @param params A named list containing arguments used to create the plot.
#' \describe{
#'    \item{df}{A data frame containing timeline data. Must include columns for
#'    \code{patient}, \code{location}, \code{start_date}, and \code{end_date} at minimum.}
#'    \item{patient}{character, Name of the variable in \code{df} identifying each patient/case.}
#'    \item{location}{character, Name of the variable in \code{df} identifying the location
#'    (e.g. ward) for each time segment.}
#'    \item{start_date}{character, Name of the variable in \code{df} containing segment start dates.}
#'    \item{end_date}{character, Name of the variable in \code{df} containing segment end dates.}
#'    \item{sym_onset_date}{character, Name of the variable in \code{df} containing symptom onset dates. Optional.}
#'    \item{sym_end_date}{character, Name of the variable in \code{df} containing symptom end dates. Optional.}
#'    \item{spec_date}{character, Name of the variable in \code{df} containing positive specimen dates. Optional.}
#'    \item{neg_spec_date}{character, Name of the variable in \code{df} containing negative specimen dates. Optional.}
#'    \item{death_date}{character, Name of the variable in \code{df} containing death dates. Optional.}
#'    \item{case_def}{character, Name of the variable in \code{df} containing case definitions. Optional.}
#'    \item{bar_height}{Numeric, thickness of location bars. Default = \code{7}.}
#'    \item{symptom_height}{Numeric, thickness of symptom bars. Default = \code{5}.}
#'    \item{event_size}{Numeric, size of specimen/death point indicators. Default = \code{5}.}
#'    \item{x_axis_label_font_size}{Numeric, font size for date axis labels. Default = \code{11}.}
#'    \item{y_axis_label_font_size}{Numeric, font size for patient labels. Default = \code{14}.}
#'    \item{legend_font_size}{Numeric, font size for legend text. Default = \code{12}.}
#'    \item{case_def_font_size}{Numeric, font size for case definition legend. Default = \code{12}.}
#'    \item{positive_specimen_colour}{Colour for positive specimen date markers. Default = \code{"yellow"}.}
#'    \item{negative_specimen_colour}{Colour for negative specimen date markers. Default = \code{"blue"}.}
#'    \item{symptom_colour}{Colour for symptom bars. Default = \code{"#B3B3B3"}.}
#'    \item{death_colour}{Colour for death date markers. Default = \code{"black"}.}
#'    \item{major_gridline_colour}{Colour for major gridlines. Default = \code{"gray60"}.}
#'    \item{minor_gridline_colour}{Colour for minor gridlines. Default = \code{"gray80"}.}
#'    \item{fill_colours}{character, Colour scheme for locations. Options: \code{"Tableau 20"},
#'    \code{"Colourblind"}, or an RColorBrewer palette name, or a named character vector of
#'    colours mapping location names to colours. Default = \code{"Tableau 20"}.}
#'    \item{x_axis_date_breaks}{A string giving the distance between major x-axis breaks like
#'    \code{"1 week"} or \code{"2 months"}. Valid specifications are 'day', 'week', 'month' or
#'    'year', optionally preceded by a number and followed by 's'. Default = \code{"1 week"}.}
#'    \item{x_axis_minor_date_breaks}{A string giving the distance between minor x-axis breaks.
#'    Same format as \code{x_axis_date_breaks}. Default = \code{"1 day"}.}
#'    \item{x_axis_break_labels}{Vector of specific Date values to use as x-axis tick marks. If
#'    provided, overrides \code{x_axis_date_breaks}.}
#'    \item{x_axis_label_angle}{Angle for x-axis label text. Permitted values: \code{"horizontal"},
#'    \code{"vertical"}, \code{"slanted"}, or a numeric angle. Default = \code{"vertical"}.}
#'    \item{suppress_gap}{A string indicating the minimum gap duration to suppress (e.g.
#'    \code{"6 months"}). Set to \code{"Never"} to disable. Only applies to static output.
#'    Default = \code{"Never"}.}
#'    \item{sort_by}{character, Name of the column in \code{df} to sort patients by.
#'    Default = \code{"startdate"}.}
#'    \item{show_case_def}{Logical, whether to show case definition symbols appended to
#'    patient labels. Default = \code{FALSE}.}
#'    \item{show_symptoms}{Logical, whether to show symptom bars. Default = \code{TRUE}.}
#'    \item{show_events}{Logical, whether to show specimen/death date markers. Default = \code{TRUE}.}
#'    \item{x_limit_min}{Date, lower limit for the x-axis date range. Default uses data range.}
#'    \item{x_limit_max}{Date, upper limit for the x-axis date range. Default uses data range.}
#'    \item{filter_locations}{Character vector of location names to filter to. Only patients
#'    who visited any of the specified locations will be shown. Default = \code{NULL} (all locations).}
#'    \item{filter_case_defs}{Character vector of case definitions to filter to.
#'    Default = \code{NULL} (all).}
#'    \item{chart_title}{Text to use as the chart title.}
#'    \item{chart_title_size}{Font size of chart title. Default = \code{13}.}
#'    \item{chart_title_colour}{Font colour of chart title. Default = \code{"black"}.}
#'    \item{chart_footer}{Text to use as chart footer.}
#'    \item{chart_footer_size}{Font size of chart footer. Default = \code{12}.}
#'    \item{chart_footer_colour}{Font colour of chart footer. Default = \code{"black"}.}
#'    \item{hover_labels}{string, Text to be used in the hover-over labels in a dynamic chart.}
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import scales
#' @import ggplot2
#' @import stringr
#' @importFrom assertthat not_empty
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @return A ggplot or plotly object.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example: Basic timeline chart
#'
#' library(epiviz)
#'
#' # Load example timeline data
#' timeline_data <- read.csv("path/to/timeline_data.csv")
#'
#' # Create static timeline
#' chart <- epi_timeline(
#'   params = list(
#'     df = timeline_data,
#'     patient = "patient",
#'     location = "location",
#'     start_date = "startdate",
#'     end_date = "enddate",
#'     spec_date = "specdate",
#'     death_date = "deathdate",
#'     sym_onset_date = "symonsdate",
#'     sym_end_date = "symenddate",
#'     neg_spec_date = "neg_specdate",
#'     case_def = "case_def",
#'     x_axis_date_breaks = "1 week",
#'     sort_by = "startdate",
#'     fill_colours = "Tableau 20"
#'   )
#' )
#'
#' chart
#'
#' # Create dynamic timeline
#' dynamic_chart <- epi_timeline(
#'   dynamic = TRUE,
#'   params = list(
#'     df = timeline_data,
#'     patient = "patient",
#'     location = "location",
#'     start_date = "startdate",
#'     end_date = "enddate",
#'     fill_colours = "Tableau 20"
#'   )
#' )
#'
#' dynamic_chart
#'
#' }
#'
epi_timeline <- function(
    dynamic = FALSE,
    params = list(
      df = NULL,
      patient = "patient",
      location = "location",
      start_date = "startdate",
      end_date = "enddate",
      sym_onset_date = NULL,
      sym_end_date = NULL,
      spec_date = NULL,
      neg_spec_date = NULL,
      death_date = NULL,
      case_def = NULL,
      bar_height = 7,
      symptom_height = 5,
      event_size = 5,
      x_axis_label_font_size = 11,
      y_axis_label_font_size = 14,
      legend_font_size = 12,
      case_def_font_size = 12,
      positive_specimen_colour = "yellow",
      negative_specimen_colour = "blue",
      symptom_colour = "#B3B3B3",
      death_colour = "black",
      major_gridline_colour = "gray60",
      minor_gridline_colour = "gray80",
      fill_colours = "Tableau 20",
      x_axis_date_breaks = "1 week",
      x_axis_minor_date_breaks = "1 day",
      x_axis_break_labels = NULL,
      x_axis_label_angle = "vertical",
      suppress_gap = "Never",
      sort_by = "startdate",
      show_case_def = FALSE,
      show_symptoms = TRUE,
      show_events = TRUE,
      x_limit_min = NULL,
      x_limit_max = NULL,
      filter_locations = NULL,
      filter_case_defs = NULL,
      chart_title = NULL,
      chart_title_size = 13,
      chart_title_colour = "black",
      chart_footer = NULL,
      chart_footer_size = 12,
      chart_footer_colour = "black",
      hover_labels = NULL
    )
) {

  # Solve warnings regarding font family
  set_Arial()

  # ============================================================================
  # ASSIGN DEFAULTS
  # ============================================================================

  if (!exists("patient", where = params)) params$patient <- "patient"
  if (!exists("location", where = params)) params$location <- "location"
  if (!exists("start_date", where = params)) params$start_date <- "startdate"
  if (!exists("end_date", where = params)) params$end_date <- "enddate"
  if (!exists("bar_height", where = params)) params$bar_height <- 7
  if (!exists("symptom_height", where = params)) params$symptom_height <- 5
  if (!exists("event_size", where = params)) params$event_size <- 5
  if (!exists("x_axis_label_font_size", where = params)) params$x_axis_label_font_size <- 11
  if (!exists("y_axis_label_font_size", where = params)) params$y_axis_label_font_size <- 14
  if (!exists("legend_font_size", where = params)) params$legend_font_size <- 12
  if (!exists("case_def_font_size", where = params)) params$case_def_font_size <- 12
  if (!exists("positive_specimen_colour", where = params)) params$positive_specimen_colour <- "yellow"
  if (!exists("negative_specimen_colour", where = params)) params$negative_specimen_colour <- "blue"
  if (!exists("symptom_colour", where = params)) params$symptom_colour <- "#B3B3B3"
  if (!exists("death_colour", where = params)) params$death_colour <- "black"
  if (!exists("major_gridline_colour", where = params)) params$major_gridline_colour <- "gray60"
  if (!exists("minor_gridline_colour", where = params)) params$minor_gridline_colour <- "gray80"
  if (!exists("fill_colours", where = params)) params$fill_colours <- "Tableau 20"
  if (!exists("x_axis_date_breaks", where = params)) params$x_axis_date_breaks <- "1 week"
  if (!exists("x_axis_minor_date_breaks", where = params)) params$x_axis_minor_date_breaks <- "1 day"
  if (!exists("x_axis_label_angle", where = params)) params$x_axis_label_angle <- "vertical"
  if (!exists("suppress_gap", where = params)) params$suppress_gap <- "Never"
  if (!exists("sort_by", where = params)) params$sort_by <- "startdate"
  if (!exists("show_case_def", where = params)) params$show_case_def <- FALSE
  if (!exists("show_symptoms", where = params)) params$show_symptoms <- TRUE
  if (!exists("show_events", where = params)) params$show_events <- TRUE
  if (!exists("chart_title_size", where = params)) params$chart_title_size <- 13
  if (!exists("chart_title_colour", where = params)) params$chart_title_colour <- "black"
  if (!exists("chart_footer_size", where = params)) params$chart_footer_size <- 12
  if (!exists("chart_footer_colour", where = params)) params$chart_footer_colour <- "black"


  # ============================================================================
  # VALIDATION
  # ============================================================================

  if (!exists("df", where = params)) stop("A data frame argument is required")
  if (!is.data.frame(params$df)) stop("df is not a data frame object")
  if (!assertthat::not_empty(params$df)) stop("df is empty")

  # Check required column mappings exist in df
  required_cols <- c("patient", "location", "start_date", "end_date")
  for (col_param in required_cols) {
    col_name <- params[[col_param]]
    if (!col_name %in% colnames(params$df))
      stop(paste0("Column '", col_name, "' (specified by '", col_param, "') not found in df"))
  }


  # ============================================================================
  # PARAMETER ASSIGNMENT
  # ============================================================================

  param_assign(params,
               c("df", "patient", "location", "start_date", "end_date",
                 "sym_onset_date", "sym_end_date", "spec_date", "neg_spec_date",
                 "death_date", "case_def",
                 "bar_height", "symptom_height", "event_size",
                 "x_axis_label_font_size", "y_axis_label_font_size",
                 "legend_font_size", "case_def_font_size",
                 "positive_specimen_colour", "negative_specimen_colour",
                 "symptom_colour", "death_colour",
                 "major_gridline_colour", "minor_gridline_colour",
                 "fill_colours", "x_axis_date_breaks", "x_axis_minor_date_breaks",
                 "x_axis_break_labels", "x_axis_label_angle",
                 "suppress_gap", "sort_by", "show_case_def",
                 "show_symptoms", "show_events",
                 "x_limit_min", "x_limit_max",
                 "filter_locations", "filter_case_defs",
                 "chart_title", "chart_title_size", "chart_title_colour",
                 "chart_footer", "chart_footer_size", "chart_footer_colour",
                 "hover_labels"))


  # ============================================================================
  # DATA PREPARATION
  # ============================================================================

  # Work with a copy and ensure location is character
  dftl <- df |>
    mutate(!!location := as.character(.data[[location]]))

  # Ensure date columns are POSIXct for sub-day precision
  date_cols <- c(start_date, end_date)
  optional_date_cols <- c(sym_onset_date, sym_end_date, spec_date, neg_spec_date, death_date)
  optional_date_cols <- optional_date_cols[!is.null(optional_date_cols) & optional_date_cols %in% colnames(dftl)]
  all_date_cols <- c(date_cols, optional_date_cols)

  for (col in all_date_cols) {
    if (col %in% colnames(dftl)) {
      if (lubridate::is.POSIXct(dftl[[col]])) next
      if (lubridate::is.Date(dftl[[col]])) {
        dftl[[col]] <- as.POSIXct(dftl[[col]], tz = "UTC")
      } else {
        dftl[[col]] <- as.POSIXct(dftl[[col]], tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d", "%d/%m/%Y"))
      }
    }
  }

  # Calculate data date range
  start_cols <- intersect(c(start_date, sym_onset_date, spec_date, neg_spec_date), colnames(dftl))
  end_cols <- intersect(c(end_date, sym_end_date, spec_date, neg_spec_date, death_date), colnames(dftl))

  # Gather all non-NA start/end values to find the data range
  all_start_vals <- do.call(c, lapply(start_cols, function(col) dftl[[col]][!is.na(dftl[[col]])]))
  all_end_vals <- do.call(c, lapply(end_cols, function(col) dftl[[col]][!is.na(dftl[[col]])]))

  if (length(all_start_vals) == 0 || length(all_end_vals) == 0) {
    stop("No valid date values found in the data")
  }

  mindate <- min(all_start_vals)
  maxdate <- max(all_end_vals)

  # Apply user date range or default to data range
  date_range_start <- if (!is.null(x_limit_min)) as.POSIXct(x_limit_min, tz = "UTC") else mindate
  date_range_end <- if (!is.null(x_limit_max)) as.POSIXct(x_limit_max, tz = "UTC") else maxdate

  # Check if date range cuts off data for visual indication
  xstart_expand <- if (mindate < date_range_start) 0 else 3
  xend_expand <- if (maxdate > date_range_end) 0 else 3

  # Apply date clipping and single-day adjustment
  dftl <- dftl |>
    mutate(
      !!start_date := pmax(.data[[start_date]], date_range_start),
      !!end_date := pmin(.data[[end_date]], date_range_end)
    ) |>
    mutate(
      single_day = as.Date(.data[[start_date]]) == as.Date(.data[[end_date]])
    ) |>
    mutate(
      !!start_date := if_else(single_day, .data[[start_date]] - lubridate::hours(6), .data[[start_date]]),
      !!end_date := if_else(single_day, .data[[end_date]] + lubridate::hours(6), .data[[end_date]])
    )

  # Filter: valid date data and overlap with date range

  dftl <- dftl |>
    group_by(.data[[patient]]) |>
    filter(
      n() > 0 &
        !all(is.na(.data[[start_date]])) &
        !all(is.na(.data[[end_date]]))
    ) |>
    filter(
      !(min(.data[[start_date]], na.rm = TRUE) > date_range_end) &
        !(max(.data[[end_date]], na.rm = TRUE) < date_range_start)
    ) |>
    ungroup()

  # Filter: Locations
  if (!is.null(filter_locations)) {
    dftl <- dftl |>
      group_by(.data[[patient]]) |>
      filter(any(.data[[location]] %in% filter_locations)) |>
      ungroup()
  }

  # Filter: Case definitions
  if (!is.null(filter_case_defs) && !is.null(case_def) && case_def %in% colnames(dftl)) {
    dftl <- dftl |>
      filter(.data[[case_def]] %in% filter_case_defs)
  }

  # Stop if no data remains after filtering
  if (nrow(dftl) == 0) {
    stop("No data remains after applying filters and date range")
  }


  # ============================================================================
  # CASE DEFINITION SYMBOLS
  # ============================================================================

  case_defs_df <- NULL
  if (!is.null(case_def) && case_def %in% colnames(dftl) && show_case_def) {
    all_syms <- c(
      "\u25A0", "\u25B2", "\u25EF", "\u25BC",
      "\u25C6", "\u25A3", "\u25D1", "\u25E9"
    )

    case_defs_unique <- sort(unique(dftl[[case_def]]))
    case_defs_syms <- all_syms[seq_along(case_defs_unique)]

    case_defs_df <- data.frame(
      case_def_val = case_defs_unique,
      case_def_sym = case_defs_syms,
      stringsAsFactors = FALSE
    )
    names(case_defs_df)[1] <- case_def

    dftl <- dftl |>
      left_join(case_defs_df, by = case_def) |>
      mutate(!!patient := paste(.data[[patient]], case_def_sym))
  }


  # ============================================================================
  # PREPARE EVENT DATA
  # ============================================================================

  dftl_events <- NULL
  event_cols <- c()
  if (!is.null(death_date) && death_date %in% colnames(dftl)) event_cols <- c(event_cols, death_date)
  if (!is.null(spec_date) && spec_date %in% colnames(dftl)) event_cols <- c(event_cols, spec_date)
  if (!is.null(neg_spec_date) && neg_spec_date %in% colnames(dftl)) event_cols <- c(event_cols, neg_spec_date)

  if (show_events && length(event_cols) > 0) {
    dftl_events <- dftl |>
      select(all_of(c(patient, event_cols))) |>
      pivot_longer(cols = -all_of(patient),
                   names_to = "event",
                   values_to = "date") |>
      filter(!is.na(date))

    # Map column names to display labels
    if (!is.null(neg_spec_date)) {
      dftl_events$event <- gsub(paste0("^", neg_spec_date, "$"), "Date of negative specimen", dftl_events$event)
    }
    if (!is.null(spec_date)) {
      dftl_events$event <- gsub(paste0("^", spec_date, "$"), "Date of positive specimen", dftl_events$event)
    }
    if (!is.null(death_date)) {
      dftl_events$event <- gsub(paste0("^", death_date, "$"), "Date of death", dftl_events$event)
    }

    if (nrow(dftl_events) == 0) dftl_events <- NULL
  }


  # ============================================================================
  # PREPARE SYMPTOMS DATA
  # ============================================================================

  dftl_sym <- NULL
  if (show_symptoms &&
      !is.null(sym_onset_date) && sym_onset_date %in% colnames(dftl) &&
      !is.null(sym_end_date) && sym_end_date %in% colnames(dftl)) {
    dftl_sym <- dftl |>
      select(all_of(c(patient, sym_onset_date, sym_end_date))) |>
      filter(!is.na(.data[[sym_onset_date]]) & !is.na(.data[[sym_end_date]]))

    if (nrow(dftl_sym) == 0) dftl_sym <- NULL
  }


  # ============================================================================
  # SETUP COLOURS
  # ============================================================================

  actual_locations <- dftl |>
    pull(.data[[location]]) |>
    unique() |>
    sort()
  actual_locations <- actual_locations[actual_locations != "" & !is.na(actual_locations)]
  nloc <- length(actual_locations)

  # Tableau 20 palette (reordered for better contrast)
  tabcol <- ggthemes::tableau_color_pal("Tableau 20")(20)[
    c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
  ]

  okabe_ito <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999"
  )

  # Determine location colours
  if (is.character(fill_colours) && length(fill_colours) == 1 && !grepl("^#", fill_colours)) {
    # fill_colours is a palette name
    if (nloc > 0) {
      if (fill_colours == "Tableau 20") {
        bcolloc <- tabcol[seq_len(nloc)]
      } else if (fill_colours == "Colourblind") {
        bcolloc <- rep(okabe_ito, length.out = nloc)
      } else {
        bcolloc <- RColorBrewer::brewer.pal(max(3, nloc), fill_colours)[seq_len(nloc)]
      }
    } else {
      bcolloc <- "#999999"
    }
    names(bcolloc) <- actual_locations
  } else if (is.character(fill_colours) && length(fill_colours) > 1) {
    # fill_colours is a vector (possibly named)
    if (!is.null(names(fill_colours))) {
      bcolloc <- fill_colours
    } else {
      bcolloc <- fill_colours[seq_len(nloc)]
      names(bcolloc) <- actual_locations
    }
  } else {
    bcolloc <- tabcol[seq_len(max(1, nloc))]
    names(bcolloc) <- actual_locations
  }

  # Build colour mapping including symptoms
  location_names <- actual_locations
  colplot <- bcolloc

  has_symptoms <- !is.null(dftl_sym) && nrow(dftl_sym) > 0
  if (has_symptoms) {
    location_names <- c(actual_locations, "Symptoms")
    colplot <- c(bcolloc, setNames(symptom_colour, "Symptoms"))
  }

  # Event shapes and colours
  event_order <- c("Date of positive specimen",
                   "Date of negative specimen",
                   "Date of death")

  eventplot_shape <- c("Date of positive specimen" = 21,
                       "Date of negative specimen" = 24,
                       "Date of death" = 23)

  eventplot_colour <- c("Date of positive specimen" = positive_specimen_colour,
                        "Date of negative specimen" = negative_specimen_colour,
                        "Date of death" = death_colour)


  # ============================================================================
  # X-AXIS LABEL ROTATION
  # ============================================================================

  if (is.character(x_axis_label_angle)) {
    if (x_axis_label_angle == "horizontal") {
      xlabang <- 0; xlabhjust <- 0.5; xlabvjust <- 1
    } else if (x_axis_label_angle == "vertical") {
      xlabang <- 90; xlabhjust <- 0; xlabvjust <- 0.5
    } else if (x_axis_label_angle == "slanted") {
      xlabang <- 45; xlabhjust <- 1; xlabvjust <- 1
    } else {
      xlabang <- as.numeric(x_axis_label_angle)
      xlabhjust <- 1; xlabvjust <- 1
    }
  } else {
    xlabang <- x_axis_label_angle
    xlabhjust <- if (xlabang == 90) 0 else if (xlabang == 0) 0.5 else 1
    xlabvjust <- if (xlabang == 90) 0.5 else 1
  }


  # ============================================================================
  # HANDLE TIME GAPS (SUPPRESS PERIODS) - static only
  # ============================================================================

  gaps <- data.frame()

  if (suppress_gap != "Never" && !dynamic) {
    dftl_valid <- dftl |>
      filter(
        !is.na(.data[[start_date]]) &
          !is.na(.data[[end_date]]) &
          .data[[start_date]] < .data[[end_date]])

    if (nrow(dftl_valid) > 0) {
      merged_intervals <- ivs::iv(dftl_valid[[start_date]], dftl_valid[[end_date]]) |>
        ivs::iv_groups()

      gaps <- data.frame(
        gap_startdate = ivs::iv_start(merged_intervals),
        gap_enddate = ivs::iv_end(merged_intervals)
      ) |>
        mutate(
          gap_start = lag(gap_enddate),
          gap = gap_startdate - gap_start,
          nudge = floor(0.02 * gap),
          void_start = gap_start + nudge,
          void_end = gap_startdate - nudge
        ) |>
        filter(gap > lubridate::period(suppress_gap))
    }
  }


  # ============================================================================
  # SORT PATIENTS
  # ============================================================================

  sort_data_internal <- function(df_sort, sort_var, pat_var) {
    if (nrow(df_sort) == 0) return(character(0))
    df_sort[[pat_var]] <- as.character(df_sort[[pat_var]])
    sort_col <- if (!is.null(sort_var) && sort_var %in% colnames(df_sort)) sort_var else pat_var
    df_sort |>
      dplyr::arrange(.data[[sort_col]]) |>
      dplyr::distinct(.data[[pat_var]]) |>
      dplyr::pull(.data[[pat_var]]) |>
      rev()
  }

  if (!is.null(sort_by) && nrow(dftl) > 0) {
    sortnum_lookup <- sort_data_internal(dftl, sort_by, patient)
    if (length(sortnum_lookup) > 0) {
      dftl <- dftl |>
        mutate(!!patient := factor(.data[[patient]], levels = sortnum_lookup))
    }
  } else {
    dftl <- dftl |>
      mutate(!!patient := factor(.data[[patient]]))
  }

  # Also factor patient in event/symptom data
  patient_levels <- levels(dftl[[patient]])
  if (!is.null(dftl_events)) {
    dftl_events <- dftl_events |>
      mutate(!!patient := factor(.data[[patient]], levels = patient_levels),
             event = factor(event, levels = event_order))
  }
  if (!is.null(dftl_sym)) {
    dftl_sym <- dftl_sym |>
      mutate(!!patient := factor(.data[[patient]], levels = patient_levels))
  }


  # ============================================================================
  # CREATE PLOT
  # ============================================================================

  if (!dynamic) {

    # ==== STATIC (ggplot) ====

    suppressMessages({

      g <- ggplot(data = dftl) +
        geom_segment(aes(x = .data[[start_date]],
                         xend = .data[[end_date]],
                         y = .data[[patient]],
                         yend = .data[[patient]],
                         colour = .data[[location]],
                         group = .data[[patient]]),
                     linewidth = bar_height, na.rm = TRUE) +
        scale_color_manual(values = colplot) +
        ylab("") +
        guides(colour = guide_legend(order = 1,
                                     nrow = ceiling(nloc / 10))) +
        theme_bw() +
        theme(
          legend.text = element_text(size = legend_font_size),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.spacing.y = grid::unit(0, "pt"),
          legend.box.spacing = grid::unit(0, "pt"),
          legend.margin = ggplot2::margin(0, 0, 0, 0),
          legend.title = element_blank(),
          legend.key = element_blank(),
          plot.caption = element_text(size = case_def_font_size, hjust = 0),
          axis.text.y = element_text(size = y_axis_label_font_size),
          axis.text.x = element_text(angle = xlabang, hjust = xlabhjust,
                                     vjust = xlabvjust, size = x_axis_label_font_size),
          panel.grid.minor = element_line(colour = minor_gridline_colour),
          panel.grid.major = element_line(colour = major_gridline_colour),
          panel.grid.major.x = element_line(linetype = "dashed"),
          panel.grid.minor.x = element_line(linetype = "dotted"),
          axis.text.x.top = element_blank(),
          axis.ticks.x.top = element_blank(),
          axis.line.x.top = element_blank()
        )

      # Apply x-axis scale
      if (!is.null(x_axis_break_labels)) {
        g <- g + scale_x_datetime(name = "",
                              breaks = as.POSIXct(as.character(x_axis_break_labels), tz = "UTC"),
                              labels = date_format("%d-%m-%Y"),
                              limits = c(date_range_start, date_range_end),
                              expand = expansion(add = c(xstart_expand * 86400, xend_expand * 86400)))
      } else {
        g <- g + scale_x_datetime(name = "",
                              date_breaks = x_axis_date_breaks,
                              date_minor_breaks = x_axis_minor_date_breaks,
                              labels = date_format("%d-%m-%Y"),
                              limits = c(date_range_start, date_range_end),
                              expand = expansion(add = c(xstart_expand * 86400, xend_expand * 86400)))
      }

      g <- g + coord_cartesian(xlim = c(date_range_start, date_range_end))

      # Add title
      if (!is.null(chart_title)) {
        g <- g + ggplot2::labs(title = chart_title) +
          theme(plot.title = element_text(hjust = 0.5,
                                          size = chart_title_size,
                                          colour = chart_title_colour,
                                          family = chart_font,
                                          face = "bold"))
      }

      # Add footer
      if (!is.null(chart_footer)) {
        g <- g + ggplot2::labs(caption = chart_footer) +
          theme(plot.caption = element_text(size = chart_footer_size,
                                            colour = chart_footer_colour,
                                            family = chart_font))
      }

      # Add symptoms
      if (!is.null(dftl_sym) && nrow(dftl_sym) > 0) {
        g <- g + geom_segment(data = dftl_sym,
                              aes(x = .data[[sym_onset_date]],
                                  xend = .data[[sym_end_date]],
                                  y = .data[[patient]],
                                  colour = "Symptoms",
                                  group = .data[[patient]]),
                              linewidth = symptom_height,
                              na.rm = TRUE,
                              lineend = "round")
      }

      # Add events
      if (!is.null(dftl_events) && nrow(dftl_events) > 0) {
        g <- g + geom_point(data = dftl_events,
                            aes(x = date,
                                y = .data[[patient]],
                                shape = event,
                                fill = event),
                            size = event_size,
                            colour = "gray30",
                            stroke = 0.5,
                            na.rm = TRUE) +
          scale_shape_manual(values = eventplot_shape) +
          scale_fill_manual(values = eventplot_colour) +
          guides(
            shape = guide_legend(order = 2,
                                 breaks = event_order,
                                 nrow = 1,
                                 override.aes = list(
                                   fill = unname(eventplot_colour[event_order]),
                                   colour = "gray30",
                                   size = event_size
                                 )),
            fill = "none"
          )
      }

      # Add case definition caption
      if (!is.null(case_defs_df) && show_case_def) {
        g <- g + labs(caption = paste("Case definition:  ",
                                      paste(case_defs_df$case_def_sym,
                                            case_defs_df[[case_def]],
                                            collapse = "   ")))
      }

      # Add breaks for suppressed time periods
      if (nrow(gaps) > 0) {
        for (i in seq_len(nrow(gaps))) {
          g <- g + ggbreak::scale_x_break(breaks = c(
            gaps$void_start[i],
            gaps$void_end[i]))
        }
      }

    }) # suppressMessages end

    return(clean_gg_labels(g))


  } else {

    # ==== DYNAMIC (plotly) ====

    # Build a static reference chart to harvest exact measurements
    gg_ref <- ggplot(data = dftl) +
      geom_segment(aes(x = .data[[start_date]],
                       xend = .data[[end_date]],
                       y = .data[[patient]],
                       yend = .data[[patient]]),
                   linewidth = bar_height, na.rm = TRUE)
    if (!is.null(x_axis_break_labels)) {
      gg_ref <- gg_ref + scale_x_datetime(
        breaks = as.POSIXct(as.character(x_axis_break_labels), tz = "UTC"),
        labels = date_format("%d-%m-%Y"),
        limits = c(date_range_start, date_range_end),
        expand = expansion(add = c(xstart_expand * 86400, xend_expand * 86400)))
    } else {
      gg_ref <- gg_ref + scale_x_datetime(
        date_breaks = x_axis_date_breaks,
        date_minor_breaks = x_axis_minor_date_breaks,
        labels = date_format("%d-%m-%Y"),
        limits = c(date_range_start, date_range_end),
        expand = expansion(add = c(xstart_expand * 86400, xend_expand * 86400)))
    }
    gg_built <- ggplot_build(gg_ref)

    # Count patients for dynamic bar-width calculation via onRender
    n_patients <- length(levels(dftl[[patient]]))

    # Ratio of bar_height to symptom_height for proportional scaling
    symptom_ratio <- symptom_height / bar_height

    # Convert R colour names to hex for plotly compatibility
    major_grid_hex <- do.call(rgb, as.list(c(col2rgb(major_gridline_colour) / 255, 1)))
    minor_grid_hex <- do.call(rgb, as.list(c(col2rgb(minor_gridline_colour) / 255, 1)))

    # Extract x-axis range from the ggplot build for exact parity
    gg_x_range <- gg_built$layout$panel_params[[1]]$x.range
    plotly_x_range <- c(as.POSIXct(gg_x_range[1], origin = "1970-01-01", tz = "UTC"),
                        as.POSIXct(gg_x_range[2], origin = "1970-01-01", tz = "UTC"))

    # Extract major and minor tick positions from the ggplot build
    gg_major_breaks <- gg_built$layout$panel_params[[1]]$x.sec$breaks
    gg_major_breaks <- gg_major_breaks[!is.na(gg_major_breaks)]
    gg_minor_breaks <- gg_built$layout$panel_params[[1]]$x.sec$minor_breaks
    gg_minor_breaks <- gg_minor_breaks[!is.na(gg_minor_breaks)]

    # Build plotly traces for location segments
    p <- plot_ly()

    # Track which locations have been added to the legend
    legend_added <- c()

    for (i in seq_len(nrow(dftl))) {
      row <- dftl[i, ]
      loc <- as.character(row[[location]])
      pat <- as.character(row[[patient]])
      show_legend <- !(loc %in% legend_added)
      if (show_legend) legend_added <- c(legend_added, loc)

      loc_colour <- if (loc %in% names(colplot)) colplot[[loc]] else "#999999"

      hover_text <- if (!is.null(hover_labels)) {
        hover_labels
      } else {
        paste0("<b>", pat, "</b><br>",
               loc, "<br>",
               format(row[[start_date]], "%d-%m-%Y"), " to ",
               format(row[[end_date]], "%d-%m-%Y"))
      }

      # Add intermediate points so hover triggers across the full segment, not just ends
      x_hover <- seq(row[[start_date]], row[[end_date]], length.out = 25)

      p <- p |>
        add_trace(
          x = x_hover,
          y = rep(pat, length(x_hover)),
          type = "scatter",
          mode = "lines",
          line = list(width = bar_height, color = loc_colour),
          legendgroup = loc,
          name = loc,
          showlegend = show_legend,
          hovertext = hover_text,
          hoverinfo = "text"
        )
    }

    # Add symptoms
    if (!is.null(dftl_sym) && nrow(dftl_sym) > 0) {
      sym_legend_added <- FALSE
      for (i in seq_len(nrow(dftl_sym))) {
        row <- dftl_sym[i, ]
        pat <- as.character(row[[patient]])

        # Add intermediate points so hover triggers across the full symptom segment
        x_hover <- seq(row[[sym_onset_date]], row[[sym_end_date]], length.out = 25)

        p <- p |>
          add_trace(
            x = x_hover,
            y = rep(pat, length(x_hover)),
            type = "scatter",
            mode = "lines",
            line = list(width = symptom_height, color = symptom_colour),
            legendgroup = "Symptoms",
            name = "Symptoms",
            showlegend = !sym_legend_added,
            hovertext = paste0("<b>", pat, "</b><br>Symptoms<br>",
                               format(row[[sym_onset_date]], "%d-%m-%Y"), " to ",
                               format(row[[sym_end_date]], "%d-%m-%Y")),
            hoverinfo = "text"
          )
        sym_legend_added <- TRUE
      }
    }

    # Add events
    if (!is.null(dftl_events) && nrow(dftl_events) > 0) {
      event_types <- event_order[event_order %in% as.character(unique(dftl_events$event))]

      plotly_event_shapes <- c("Date of positive specimen" = "circle",
                               "Date of negative specimen" = "triangle-up",
                               "Date of death" = "diamond")

      for (evt in event_types) {
        evt_data <- dftl_events |> filter(event == evt)
        evt_colour <- if (evt %in% names(eventplot_colour)) eventplot_colour[[evt]] else "black"
        evt_shape <- if (evt %in% names(plotly_event_shapes)) plotly_event_shapes[[evt]] else "circle"

        p <- p |>
          add_trace(
            data = evt_data,
            x = ~date,
            y = as.formula(paste0("~`", patient, "`")),
            type = "scatter",
            mode = "markers",
            marker = list(
              size = event_size * 2,
              color = evt_colour,
              symbol = evt_shape,
              line = list(color = "gray30", width = 1)
            ),
            legend = "legend2",
            name = evt,
            showlegend = TRUE,
            hovertext = paste0("<b>", evt_data[[patient]], "</b><br>",
                               evt, "<br>",
                               format(evt_data$date, "%d-%m-%Y")),
            hoverinfo = "text"
          )
      }
    }

    # Determine plotly x-axis label angle
    plotly_xlabang <- if (is.character(params$x_axis_label_angle)) {
      switch(params$x_axis_label_angle,
             "horizontal" = 0,
             "vertical" = -90,
             "slanted" = -45,
             0)
    } else {
      -params$x_axis_label_angle
    }

    # Title font
    title_font <- list(family = chart_font, size = chart_title_size, color = chart_title_colour)
    footer_font <- list(family = chart_font, size = chart_footer_size, color = chart_footer_colour)

    # Build tick values from ggplot-extracted breaks for exact parity
    plotly_major_tickvals <- as.POSIXct(gg_major_breaks, origin = "1970-01-01", tz = "UTC")
    plotly_minor_tickvals <- as.POSIXct(gg_minor_breaks, origin = "1970-01-01", tz = "UTC")

    # Layout
    layout_args <- list(
      p = p,
      xaxis = list(
        type = "date",
        range = c(as.character(plotly_x_range[1]),
                  as.character(plotly_x_range[2])),
        tickformat = "%d-%m-%Y",
        tickangle = plotly_xlabang,
        tickfont = list(size = x_axis_label_font_size),
        tickmode = "array",
        tickvals = plotly_major_tickvals,
        gridcolor = major_grid_hex,
        griddash = "dash",
        gridwidth = 1,
        showgrid = TRUE,
        ticks = "outside",
        ticklen = 5,
        tickcolor = "black",
        showline = TRUE,
        linecolor = "black",
        linewidth = 1,
        mirror = TRUE,
        title = ""
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = y_axis_label_font_size),
        gridcolor = major_grid_hex,
        griddash = "dash",
        gridwidth = 1,
        showgrid = TRUE,
        showline = TRUE,
        linecolor = "black",
        linewidth = 1,
        mirror = TRUE,
        ticks = "outside",
        ticklen = 5,
        tickcolor = "black",
        categoryorder = "array",
        categoryarray = levels(dftl[[patient]])
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.05,
        yanchor = "bottom",
        font = list(size = legend_font_size)
      ),
      legend2 = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.01,
        yanchor = "bottom",
        font = list(size = legend_font_size)
      ),
      margin = list(t = 80, b = 80),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )

    # Add title
    if (!is.null(chart_title)) {
      layout_args$title <- list(
        text = html_bold(chart_title),
        font = title_font,
        x = 0.5,
        xanchor = "center"
      )
    }

    # Add footer as annotation
    if (!is.null(chart_footer)) {
      layout_args$annotations <- list(
        list(
          x = 1, y = -0.15,
          text = chart_footer,
          xanchor = "right", yanchor = "middle",
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = footer_font
        )
      )
    }

    # Add case def caption
    if (!is.null(case_defs_df) && show_case_def) {
      caption_text <- paste("Case definition:  ",
                            paste(case_defs_df$case_def_sym,
                                  case_defs_df[[case_def]],
                                  collapse = "   "))
      caption_annotation <- list(
        x = 0, y = -0.15,
        text = caption_text,
        xanchor = "left", yanchor = "middle",
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = case_def_font_size)
      )
      if (is.null(layout_args$annotations)) {
        layout_args$annotations <- list(caption_annotation)
      } else {
        layout_args$annotations <- c(layout_args$annotations, list(caption_annotation))
      }
    }

    p <- do.call(layout, layout_args)

    # Add minor x-axis gridlines as explicit shapes (dotted vertical lines)
    # since plotly's minor axis config may not render reliably across versions
    if (length(plotly_minor_tickvals) > 0) {
      minor_shapes <- lapply(plotly_minor_tickvals, function(tv) {
        list(
          type = "line",
          x0 = as.character(tv), x1 = as.character(tv),
          y0 = 0, y1 = 1,
          xref = "x", yref = "paper",
          line = list(color = "#EBEBEB", width = 0.5, dash = "solid"),
          layer = "below"
        )
      })
      p <- layout(p, shapes = minor_shapes)
    }

    # Dynamically size bar widths to match static ggplot rendering.
    # Plotly line width is in pixels and fixed, while ggplot linewidth scales
    # with the plot area. onRender calculates the correct pixel width from
    # the actual rendered plot height and number of patients, keeping bar
    # thickness consistent at any display size and matching the static output.
    p <- htmlwidgets::onRender(p, sprintf("
      function(el) {
        var plotHeight = el._fullLayout._size.h;
        var nPatients  = %d;
        var barFrac    = %f;
        var symFrac    = %f;
        var barWidth   = barFrac * plotHeight / nPatients;
        var symWidth   = symFrac * plotHeight / nPatients;
        var barIdx = []; var symIdx = [];
        el.data.forEach(function(trace, i) {
          if (trace.mode === 'lines' && trace.legendgroup === 'Symptoms') {
            symIdx.push(i);
          } else if (trace.mode === 'lines') {
            barIdx.push(i);
          }
        });
        if (barIdx.length > 0) Plotly.restyle(el, {'line.width': barWidth}, barIdx);
        if (symIdx.length > 0) Plotly.restyle(el, {'line.width': symWidth}, symIdx);
      }
    ", n_patients, bar_height / 10, symptom_height / 10))

    return(p)

  }

}
