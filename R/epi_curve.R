#' epi_curve
#'
#' @description A function for producing either a static (ggplot) or dynamic (plotly)
#' epidemic curve.
#'
#' @param dynamic Logical indicating whether to produce a dynamic (plotly) output.
#' Default is \code{FALSE}, which will return a static ggplot output.
#' @param params A named list containing arguments used to create the plot.
#' \describe{
#'    \item{df}{A data frame containing data used to create the epi curve.}
#'    \item{date_var}{character, Name of the variable in \code{df} containing the dates used
#'    to populate the x-axis.}
#'    \item{y}{If data is pre-aggregated, the name of the variable in \code{df} containing the
#'    aggregated values (i.e. the values used to populate the y-axis.)}
#'    \item{date_start}{A date that will determine the minimum value along the x-axis. Any rows
#'    with \code{date_var < date_start} will be excluded from aggregates.}
#'    \item{date_end}{A date that will determine the maximum value along the x-axis. Any rows
#'    with \code{date_var > date_end} will be excluded from aggregates.}
#'    \item{time_period}{The time period to be used along the x-axis. Options include
#'    \code{c("day","year","month","quarter","year_month","year_quarter",
#'      "iso_year","iso_week","start_iso_year_week","iso_year_week",
#'      "use_date_var")}. Default = \code{"use_date_var"}, which indicates that the dates
#'      as they appear in the \code{date_var} column will be used to populate the x-axis.}
#'    \item{group_var}{Name of the variable in df used to define separate groups within each bar,
#'    e.g. species or region.}
#'    \item{group_var_barmode}{Indicates how grouped bar data should be plotted. Options include
#'    \code{c("group","stack")}. Default = \code{"stack"}.}
#'    \item{fill_colours}{Colours used to fill bars on chart. If \code{group_var} has not been
#'    provided, then \code{fill_colours} must be a character containing a single colour (default =
#'    \code{"lightblue"}). If \code{group_var} has been provided, then \code{fill_colours} must be
#'    a character vector of colours with a number of elements equal to the number of unique groups
#'    in \code{group_var}. If a named character vector is provided where the names are values within
#'    \code{group_var}, then each colour will be mapped to it's corresponding value in \code{group_var}
#'    on the output chart and legend (e.g. \code{c("KLEBSIELLA PNEUMONIAE" = "#007C91", "STAPHYLOCOCCUS
#'    AUREUS" = "#8A1B61", "PSEUDOMONAS AERUGINOSA" = "#FF7F32")} or \code{setNames(c("#007C91",
#'    "#8A1B61","#FF7F32"), c("KLEBSIELLA PNEUMONIAE","STAPHYLOCOCCUS AUREUS","PSEUDOMONAS AERUGINOSA"))})}
#'    \item{bar_border_colour}{Colour of the border around each bar. No border colour is drawn as default.}
#'    \item{case_boxes}{boolean, If \code{case_boxes = TRUE} then a boundary box will be drawn around
#'    each case within each bar. Defaults to \code{case_boxes = FALSE}.}
#'    \item{case_boxes_colour}{The colour of the border around each case box if if \code{case_boxes =
#'    TRUE}. Default = \code{"white"}.}
#'    \item{rolling_average_line}{boolean, If \code{rolling_average_line = TRUE}, then a line showing the
#'    rolling mean will be added to the plot. Default = \code{FALSE}.}
#'    \item{rolling_average_line_lookback}{Integer denoting the lookback window across which the rolling
#'    mean will be calculated (including the current time interval). Each integer denotes a division
#'    within \code{time_period}, e.g. if \code{time_period = "year_month"} and
#'    \code{rolling_average_line_lookback = 3} then the rolling mean will be calculated using values from
#'    the current month and the previous 2 months, and if \code{time_period = "day"} and
#'    \code{rolling_average_line_lookback = 7} then the rolling mean will be calculated using values from
#'    the previous 7 days including the current day. If there are less values within the lookback window
#'    than \code{rolling_average_line_lookback}, then the mean will be calculated from an incomplete
#'    window using the values available (i.e. if \code{rolling_average_line_lookback = 7} and
#'    \code{time_period = "day"} but there are only 4 values within the previous 7 days, then the rolling
#'    mean will be calculated from the 4 available values.)}
#'    \item{rolling_average_line_colour}{character Colour of the rolling average line. Default = \code{"red"}.}
#'    \item{rolling_average_line_width}{numeric Width of the rolling average line. Default = 1.}
#'    \item{rolling_average_line_legend_label}{character Label to be used for the rolling average line
#'    in the chart legend.}
#'    \item{cumulative_sum_line}{boolean, If \code{cumulative_sum_line_line = TRUE}, then a line showing
#'    the cumulative sum will be added to the plot. Default = \code{FALSE}. Values for the cumulative sum
#'    will be plotted on the secondary y-axis.}
#'    \item{cumulative_sum_line_colour}{character Colour of the cumulative line. Default = \code{"darkblue"}.}
#'    \item{cumulative_sum_line_width}{numeric Width of the cumulative sum line. Default = 1.}
#'    \item{cumulative_sum_line_legend_label}{character Label to be used for the cumulative sum line
#'    in the chart legend.}
#'    \item{cumulative_sum_line_axis_title}{character Axis title for the cumulative sum line secondary
#'    axis.}
#'    \item{chart_title}{Text to use as chart title.}
#'    \item{chart_title_size}{Font size of chart title.}
#'    \item{chart_title_colour}{Font colour of chart title.}
#'    \item{chart_footer}{Text to use as chart footer.}
#'    \item{chart_footer_size}{Font size of chart footer.}
#'    \item{chart_footer_colour}{Font colour of chart footer.}
#'    \item{x_axis_title}{Text used for x-axis title. Defaults to name of x-variable if
#'    not stated.}
#'    \item{y_axis_title}{Text used for y-axis title. Defaults to name of y-variable if
#'    not stated.}
#'    \item{x_axis_title_font_size}{Font size of the x-axis title.}
#'    \item{y_axis_title_font_size}{Font size of the y-axis title.}
#'    \item{x_axis_label_angle}{Angle for x-axis label text.}
#'    \item{y_axis_label_angle}{Angle for y-axis label text.}
#'    \item{x_axis_label_font_size}{Font size for the x-axis tick labels.}
#'    \item{y_axis_label_font_size}{Font size for the y-axis tick labels.}
#'    \item{y_limit_min}{Lower limit for the y-axis. Default used if not provided.}
#'    \item{y_limit_max}{Upper limit for the y-axis. Default used if not provided.}
#'    \item{x_axis_break_labels}{Vector of values to use for x-axis breaks. Defaults
#'    used if not provided. Values provided must match the formatting of \code{time_period}.}
#'    \item{y_axis_break_labels}{Vector of values to use for y-axis breaks. Defaults
#'    used if not provided.}
#'    \item{y_axis_n_breaks}{Scales y-axis with approximately n breaks. Cannot be used
#'    if \code{y_axis_break_labels} is also provided.}
#'    \item{show_gridlines}{Logical to show chart gridlines. Default = \code{FALSE}.}
#'    \item{show_axislines}{Logical to show chart axis lines. Default = \code{TRUE}.}
#'    \item{legend_title}{Text used for legend title.}
#'    \item{legend_pos}{Position of the legend. Permitted values = c("top","bottom","right","left")}
#'    \item{legend_font_size}{Font size used in the legend.}
#'    \item{legend_title_font_size}{Font size used for the legend title.}
#'    \item{hline}{Adds horizontal line across the chart at the corresponding y-value. Multiple
#'    values may be provided as a vector to add multiple horizontal lines.}
#'    \item{hline_colour}{Colour of the horizontal lines if \code{hline} is provided. A vector of colours
#'    can be provided to colour individual hlines if multiple hlines have been provided.}
#'    \item{hline_width}{Numerical width of the horizontal lines if \code{hline} is provided. A vector of numerical widths
#'    can be provided for individual hlines if multiple hlines have been provided.}
#'    \item{hline_type}{Line style of the horizontal lines if \code{hline} is provided. A vector of line styles
#'    can be provided to style hlines if multiple hlines have been provided. Permitted values = c("solid", "dotted",
#'    "dashed", "longdash", "dotdash").}
#'    \item{hline_label}{Text to label the horizontal lines if \code{hline} is provided. A vector of text strings
#'    can be provided to label individual hlines if multiple hlines have been provided.}
#'    \item{hline_label_colour}{Colour of the horizontal line labels if \code{hline_labels} is provided.
#'    A vector of colours can be provided to colour individual hline_labels if multiple hline_labels have been provided.}
#'    \item{hover_labels}{string, Text to be used in the hover-over labels in a dynamic chart.
#'    Accepts html, use \code{'\%{x}'} to reference corresponding x-axis values (i.e. date intervals)
#'    and \code{'\%{y}'} to reference y-axis values, e.g. \code{hover_labels = "<b>Date:</b>
#'    \%{x}<br><b>Count:</b> \%{y}"}.}
#'
#'  }
#'
#'
#'
#' @import dplyr
#' @import grDevices
#' @import scales
#' @import tidyr
#' @import lubridate
#' @import ISOweek
#' @import forcats
#' @import slider
#' @importFrom graphics box
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @return A ggplot or plotly object.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example 1: Basic epi curve
#'
#' # Create a basic epi curve using the epiviz::lab_data dataset
#' library(epiviz)
#'
#' basic_epi_curve <- epi_curve(
#'   params = list(
#'     df = lab_data,
#'     date_var = "specimen_date",
#'     date_start = "2020-01-01",
#'     date_end = "2023-12-31",
#'     time_period = "year_month",
#'     fill_colours = "#007C91",
#'     rolling_average_line = TRUE,
#'     rolling_average_line_lookback = 3,
#'     rolling_average_line_legend_label = "3-month rolling average",
#'     chart_title = "Laboratory Detections per Month",
#'     x_axis_title = "Year - Month",
#'     y_axis_title = "Number of detections",
#'     x_axis_label_angle = -90
#'   )
#' )
#'
#' basic_epi_curve
#'
#'
#'
#'
#' # Example 2: Create both static and dynamic epi curves using grouped data
#'
#' library(epiviz)
#'
#' # Define list of date breaks for x-axis; use every other ISO week in date range
#' week_seq <- seq(as.Date("2021-01-01"),as.Date("2022-05-31"), by = '2 week')
#' week_breaks <- paste0(lubridate::isoyear(week_seq),'-W',lubridate::isoweek(week_seq))
#'
#' # Create parameter list
#' params_list <- list(
#'   df = lab_data,
#'   date_var = "specimen_date",
#'   date_start = "2021-01-01",
#'   date_end = "2022-05-31",
#'   time_period = "iso_year_week",
#'   group_var = "organism_species_name",
#'   group_var_barmode = "stack",
#'   fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
#'                    "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
#'                    "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
#'   rolling_average_line = TRUE,
#'   rolling_average_line_legend_label = "7-week rolling average",
#'   chart_title = "Laboratory detections by species \n 2021-01 - 2022-05",
#'   chart_footer = "This chart has been created using simulated data.",
#'   x_axis_title = "Year - ISO Week",
#'   y_axis_title = "Number of detections",
#'   x_axis_label_angle = -90,
#'   x_axis_break_labels = week_breaks,
#'   y_axis_break_labels = seq(0, 250, 20),
#'   chart_title_colour = "#007C91",
#'   chart_footer_colour = "#007C91"
#' )
#'
#' # Create static epi curve
#' static_curve <- epi_curve(params = params_list, dynamic = FALSE)
#'
#' # Create dynamic epi curve
#' dynamic_curve <- epi_curve(params = params_list, dynamic = TRUE)
#'
#' # View both simultaneously using shiny app
#' library(shiny)
#' library(plotly)
#' ui <- fluidPage(
#'   plotOutput('static_curve'),
#'   plotlyOutput('dynamic_curve')
#' )
#' server <- function(input, output, session) {
#'   output$static_curve <- renderPlot(static_curve)
#'   output$dynamic_curve <- renderPlotly(dynamic_curve)
#' }
#' shinyApp(ui, server)
#'
#'
#'
#'
#' # Example 3: Create static and dynamic curves using grouped data, include cumulative
#' # sum line and boxes around each case
#'
#' library(epiviz)
#'
#' # Create parameter list
#' params_cases <- list(
#'   df = lab_data,
#'   date_var = "specimen_date",
#'   date_start = "2021-06-01",
#'   date_end = "2021-07-31",
#'   time_period = "day",
#'   group_var = "organism_species_name",
#'   group_var_barmode = "stack",
#'   fill_colours = c("#007C91","#8A1B61","#FF7F32"),
#'   case_boxes = TRUE,
#'   rolling_average_line = TRUE,
#'   rolling_average_line_legend_label = "7-day rolling average",
#'   cumulative_sum_line = TRUE,
#'   chart_title = "Laboratory detections by species \n June - July 2021",
#'   chart_title_colour = "#007C91",
#'   hline = c(35),
#'   hline_label = "Threshold",
#'   hline_width = 0.5,
#'   hline_colour = "orange",
#'   hline_label_colour = "orange",
#'   hline_type = "dotdash",
#'   legend_title = "Detected organisms",
#'   legend_pos = "right",
#'   y_limit_max = 40,
#'   x_axis_break_labels = as.character(seq(as.Date("2021-06-01"),
#'                                          as.Date("2021-07-31"),
#'                                          by = '2 days')),
#'   y_axis_break_labels = seq(0, 40, 5),
#'   x_axis_title = "Date",
#'   y_axis_title = "Number of detections",
#'   x_axis_label_angle = -90,
#'   y_axis_label_angle = 90
#' )
#'
#'
#' # Create static and dynamic curves
#' static_curve <- epi_curve(params = params_cases, dynamic = FALSE)
#' dynamic_curve <- epi_curve(params = params_cases, dynamic = TRUE)
#'
#' # View both simultaneously using shiny app
#' library(shiny)
#' library(plotly)
#' ui <- fluidPage(
#'   plotOutput('static_curve'),
#'   plotlyOutput('dynamic_curve')
#' )
#' server <- function(input, output, session) {
#'   output$static_curve <- renderPlot(static_curve)
#'   output$dynamic_curve <- renderPlotly(dynamic_curve)
#' }
#' shinyApp(ui, server)
#'
#'
#'
#'
#' # Example 4: Create static and dynamic curves using pre-aggregated data
#'
#' library(epiviz)
#'
#' # Define a dataframe containing the number of detections by region
#' regional_detections <- lab_data |>
#'   group_by(specimen_date, region) |>
#'   summarise(detections = n()) |>
#'   ungroup()
#'
#'
#' # Create parameter list
#' params_regions <- list(
#'   df = regional_detections,
#'   y = "detections",
#'   date_var = "specimen_date",
#'   date_start = "2021-10-01",
#'   date_end = "2022-03-31",
#'   time_period = "iso_year_week",
#'   group_var = "region",
#'   group_var_barmode = "stack",
#'   rolling_average_line = TRUE,
#'   rolling_average_line_lookback = 3,
#'   rolling_average_line_legend_label = "3-week rolling average",
#'   rolling_average_line_colour = "#007C91",
#'   rolling_average_line_width = 1.5,
#'   cumulative_sum_line = TRUE,
#'   cumulative_sum_line_colour = "orange",
#'   chart_title = "Laboratory Detections by Region \nWinter 2021-22",
#'   chart_title_colour = "#007C91",
#'   legend_title = "Region",
#'   legend_pos = "right",
#'   y_axis_break_labels = seq(0, 300, 50),
#'   x_axis_title = "ISO Week",
#'   y_axis_title = "Number of detections",
#'   x_axis_label_angle = -90,
#'   hover_labels = "<b>Week:</b> %{x}<br><b>Count:</b> %{y}"
#' )
#'
#'
#' # Create static and dynamic curves
#' static_curve <- epi_curve(params = params_regions, dynamic = FALSE)
#' dynamic_curve <- epi_curve(params = params_regions, dynamic = TRUE)
#'
#' # View both simultaneously using shiny app
#' library(shiny)
#' library(plotly)
#' ui <- fluidPage(
#'   plotOutput('static_curve'),
#'   plotlyOutput('dynamic_curve')
#' )
#' server <- function(input, output, session) {
#'   output$static_curve <- renderPlot(static_curve)
#'   output$dynamic_curve <- renderPlotly(dynamic_curve)
#' }
#' shinyApp(ui, server)
#'
#'
#' }
#'
epi_curve <- function(
                        dynamic = FALSE,
                        params = list(
                          df = NULL,
                          y = NULL,                      # for pre-aggregated data, requires implementation
                          date_var = NULL,
                          date_start = NULL,
                          date_end = NULL,
                          time_period = "use_date_var",
                          group_var = NULL,
                          group_var_barmode = 'stack',
                          fill_colours = "lightblue",
                          bar_border_colour = "transparent",
                          case_boxes = FALSE,
                          case_boxes_colour = "white",
                          rolling_average_line = FALSE,
                          rolling_average_line_lookback = 7,
                          rolling_average_line_colour = "red",
                          rolling_average_line_width = 1,
                          rolling_average_line_legend_label = "Rolling average",
                          cumulative_sum_line = FALSE,
                          cumulative_sum_line_colour = "darkblue",
                          cumulative_sum_line_width = 1,
                          cumulative_sum_line_legend_label = "Cumulative sum",
                          cumulative_sum_line_axis_title = "Cumulative Sum",
                          chart_title = NULL,
                          chart_title_size = 13,
                          chart_title_colour = "black",
                          chart_footer = NULL,
                          chart_footer_size = 12,
                          chart_footer_colour = "black",
                          x_axis_title = NULL,
                          y_axis_title = NULL,
                          x_axis_title_font_size = 11,
                          y_axis_title_font_size = 11,
                          x_axis_label_angle = NULL,
                          y_axis_label_angle = NULL,
                          x_axis_label_font_size = 9,
                          y_axis_label_font_size = 9,
                          y_limit_min = NULL,
                          y_limit_max = NULL,
                          x_axis_break_labels = NULL,
                          y_axis_break_labels = NULL,
                          y_axis_n_breaks = NULL,
                          show_gridlines = FALSE,
                          show_axislines = TRUE,
                          legend_title = "",
                          legend_pos = "right",
                          legend_font_size = 8,
                          legend_title_font_size = 8,
                          hline = NULL,
                          hline_colour = "black",
                          hline_width = 0.5,
                          hline_type = "dashed",
                          hline_label = NULL,
                          hline_label_colour = "black",
                          hover_labels = NULL
                        )
                  ) {


  # Solve warnings regarding font family not found using utils/set_Arial() function
  set_Arial()


  # Where relevant, assign defaults to any parameters not specified by the user
  if(!exists('group_var_barmode',where=params)) params$group_var_barmode <- "stack"
  if(!exists('time_period',where=params)) params$time_period <- "use_date_var"
  if(!exists('fill_colours',where=params)) params$fill_colours <- "lightblue"
  if(!exists('bar_border_colour',where=params)) params$bar_border_colour <- "transparent"
  if(!exists('case_boxes',where=params)) params$case_boxes <- FALSE
  if(!exists('case_boxes_colour',where=params)) params$case_boxes_colour <- "white"
  if(!exists('rolling_average_line',where=params)) params$rolling_average_line <- FALSE
  if(!exists('rolling_average_line_lookback',where=params)) params$rolling_average_line_lookback <- 7
  if(!exists('rolling_average_line_colour',where=params)) params$rolling_average_line_colour <- "red"
  if(!exists('rolling_average_line_width',where=params)) params$rolling_average_line_width <- 1
  if(!exists('rolling_average_line_legend_label',where=params)) params$rolling_average_line_legend_label <- "Rolling average"
  if(!exists('cumulative_sum_line',where=params)) params$cumulative_sum_line <- FALSE
  if(!exists('cumulative_sum_line_colour',where=params)) params$cumulative_sum_line_colour <- "darkblue"
  if(!exists('cumulative_sum_line_width',where=params)) params$cumulative_sum_line_width <- 1
  if(!exists('cumulative_sum_line_legend_label',where=params)) params$cumulative_sum_line_legend_label <- "Cumulative sum"
  if(!exists('cumulative_sum_line_axis_title',where=params)) params$cumulative_sum_line_axis_title <- "Cumulative Sum"
  if(!exists('chart_title_size',where=params)) params$chart_title_size <- 12
  if(!exists('chart_title_colour',where=params)) params$chart_title_colour <- "black"
  if(!exists('chart_footer_size',where=params)) params$chart_footer_size <- 10
  if(!exists('chart_footer_colour',where=params)) params$chart_footer_colour <- "black"
  if(!exists('x_axis_title_font_size',where=params)) params$x_axis_title_font_size <- 11
  if(!exists('y_axis_title_font_size',where=params)) params$y_axis_title_font_size <- 11
  if(!exists('x_axis_label_angle',where=params)) params$x_axis_label_angle <- 0
  if(!exists('y_axis_label_angle',where=params)) params$y_axis_label_angle <- 0
  if(!exists('x_axis_label_font_size',where=params)) params$x_axis_label_font_size <- 9
  if(!exists('y_axis_label_font_size',where=params)) params$y_axis_label_font_size <- 9
  if(!exists('x_axis_reverse',where=params)) params$x_axis_reverse <- FALSE
  if(!exists('show_gridlines',where=params)) params$show_gridlines <- FALSE
  if(!exists('show_axislines',where=params)) params$show_axislines <- TRUE
  if(!exists('legend_title',where=params)) params$legend_title <- ""
  if(!exists('legend_pos',where=params)) params$legend_pos <- "right"
  if(!exists('legend_font_size',where=params)) params$legend_font_size <- 8
  if(!exists('legend_title_font_size',where=params)) params$legend_title_font_size <- 8
  if(!exists('hline_colour',where=params)) params$hline_colour <- "black"
  if(!exists('hline_width',where=params)) params$hline_width <- 0.5
  if(!exists('hline_type',where=params)) params$hline_type <- "dashed"
  if(!exists('hline_label_colour',where=params)) params$hline_label_colour <- "black"
  # The following are not input parameters for epi_curve, but are needed
  #   for base_gg and base_plotly so set defaults
  params$y_percent <- FALSE
  params$y_sec_axis <- FALSE
  params$y_sec_axis_no_shift <- TRUE
  params$y_sec_axis_percent_full <- FALSE


  # Rename certain parameters so that they will be recognised
  #   by base_gg() and base_plotly()
  params <- param_rename(params,"date_var","x")
  params <- param_rename(params,"date_start","x_limit_min")
  params <- param_rename(params,"date_end","x_limit_max")


  # 'base' not a user define arguement for epi_curve, so set to NULL
  #   for base_gg() and base_plotly()
  base <- NULL





  ##### Checks and warnings

  # Check if df is is.null
  if (!exists('df',where=params)) stop("A data frame argument is required")

  # Check df is a df class
  if(!is.data.frame(params$df)) stop("df is not a data frame object")

  # Check df is empty
  if(!not_empty(params$df)) stop("df is empty")

  # Check if date_var argument is null (date_var renamed to 'x' at this stage)
  if ((is.null(params$x)) | !exists('x',where=params))
    stop("Please include a variable from df for date_var, i.e. date_var = \"variable_name\"")

  # Check if date_var is in df (date_var renamed to 'x' at this stage)
  if (!params$x %in% colnames(params$df))
    stop("date_var not found within df. Please include a variable from df for date_var, i.e. date_var = \"variable_name\"")

  # Check group_var_barmode valid
  if (!is.null(params$group_var) & !(params$group_var_barmode %in% c('stack', 'group'))) {
    stop("group_var_barmode must equal 'stack' or 'group'")
  }

  # Check time_period valid
  if (!(params$time_period %in% c("day","year","month","quarter","year_month","year_quarter",
                                  "iso_year","iso_week","start_iso_year_week","iso_year_week",
                                  "use_date_var"))) {
    stop("time_period must equal 'day', 'year', 'month', 'quarter', 'year_month', 'year_quarter',
              'iso_year', 'iso_week', 'start_iso_year_week', 'iso_year_week', or 'use_date_var'")
  }


  # If provided, check if y is in df
  if (exists('y',where=params)) {
    if (!params$y %in% colnames(params$df))
      stop("y not found within df. Please include a variable from df for y, i.e. y = \"variable_name\"")
  }

  # If provided, check if group_var is in df
  if ("group_var" %in% names(params)) {
    if (!params$group_var %in% colnames(params$df))
      stop("group_var not found within df. Please include a variable from df for group_var, i.e. group_var = \"variable_name\"")
  }

  # Check if number of groups and number of point colours are the same
  if (exists('group_var', where=params) & length(params$fill_colours) > 1) {
      if (length(params$fill_colours) != length(unique(params$df[[params$group_var]])))
        stop("The number of fill_colours provided must equal the number of unique groups in group_var")
  }


  # Warn that multiple colours have been provided but group_var absent
  if (length(params$fill_colours) >1 & !exists('group_var',where=params))
    warning("Multiple fill_colours have been provided but group_var is absent")










  ##### Parameter assignment

  # Define parameters as variables using utils/param_assign() function
  #   -Takes input list, compares it ro a reference vector of expected
  #     list elements, assigns each element to a variable within the
  #     parent environment, and allocates a value of 'NULL' to anything
  #     it can't find within the reference list.
  param_assign(params,
               c("df",
                 "x",
                 "y",
                 "time_period",
                 "group_var",
                 "group_var_barmode",
                 "fill_colours",
                 "bar_border_colour",
                 "case_boxes",
                 "case_boxes_colour",
                 "rolling_average_line",
                 "rolling_average_line_lookback",
                 "rolling_average_line_colour",
                 "rolling_average_line_width",
                 "rolling_average_line_legend_label",
                 "cumulative_sum_line",
                 "cumulative_sum_line_colour",
                 "cumulative_sum_line_width",
                 "cumulative_sum_line_legend_label",
                 "cumulative_sum_line_axis_title",
                 "chart_title",
                 "chart_footer",
                 "chart_title_size",
                 "chart_title_colour",
                 "chart_footer_size",
                 "chart_footer_colour",
                 "x_axis_title",
                 "y_axis_title",
                 "x_axis_title_font_size",
                 "y_axis_title_font_size",
                 "x_axis_label_angle",
                 "y_axis_label_angle",
                 "x_axis_label_font_size",
                 "y_axis_label_font_size",
                 "st_theme",
                 "x_axis_reverse",
                 "y_limit_min",
                 "y_limit_max",
                 "x_limit_min",
                 "x_limit_max",
                 "x_axis_break_labels",
                 "y_axis_break_labels",
                 "x_axis_n_breaks",
                 "y_axis_n_breaks",
                 "x_axis_date_breaks",
                 "show_gridlines",
                 "show_axislines",
                 "legend_title",
                 "legend_pos",
                 "legend_font_size",
                 "legend_title_font_size",
                 "hline",
                 "hline_colour",
                 "hline_width",
                 "hline_type",
                 "hline_label",
                 "hline_label_colour",
                 "hover_labels",
                 "ci_lower",   # required as NULL for base_gg() / base_plotly()
                 "ci_upper"    # required as NULL for base_gg() / base_plotly()
                 )
               )


  # Set a default colour palette when group_var is set but fill_colour is not
  if(!is.null(group_var) & length(fill_colours) == 1) {
    fill_colours <- hue_pal()(length(unique(df[[group_var]])))
  }




  #################### DATA MANIPULATION #########################


  ### Define full date range

  # Define start and end dates if not provided
  if(is.null(x_limit_min)){
    date_start <- min(df[[x]]) - 5
  } else {
    date_start <- as.Date(x_limit_min)
  }

  if(is.null(x_limit_max)){
    date_end <- max(df[[x]]) + 5
  } else {
    date_end <- as.Date(x_limit_max)
  }

  # Filter out any rows outside date range so that they aren't included in aggregates
  df <- df |>
    filter(get(x) >= date_start & get(x) <= date_end) # & date_factor <= x_limit_max




  ### Add time periods to df

  if (time_period != "use_date_var") {

    # Use utils/adorn_dates() function to add additional date variables to df
    df <- adorn_dates(df, x)

    # Redefine x so that it points towards the relevant date column
    x <- time_period

  }


  ### Create date factor for x-axis

  # Define sequence of all dates in full date range
  all_dates_seq <- data.frame(date_seq = seq(date_start, date_end, 1))

  # Expand with additional time periods using utils/adorn_dates()
  all_dates <- adorn_dates(all_dates_seq, "date_seq")

  # Pull only values corresponding to x (i.e. user specified time_period), turn
  #    into vector of unique values for use as factor levels
  unique_dates <- unique(all_dates[[x]])

  # Add date factor column to df for use in chart
  df <- df |>
    mutate(date_factor = factor(as.character(get(x)), levels = unique_dates)) |>
    filter(!is.na(date_factor)) # filter out NAs, i.e. dates that fall outside of range

  # Redefine x so that it points towards date_factor
  x <- "date_factor"



  ### Change x_limit_max and x_limit_min to match time_period choice

  # Create df of limits and apply utils/adorn_dates() to find time_period limits
  df_x_limits <- data.frame(x_limit = as.Date(c(x_limit_min,x_limit_max))) |>
    adorn_dates("x_limit") |>
    select(x_limit, any_of(time_period))

  # Define limits converted to time_period
  x_limit_min <- df_x_limits[[time_period]][1]
  x_limit_max <- df_x_limits[[time_period]][2]





  ### Add totals by date to df (i.e. defining y-axis)

  if (is.null(y)) {

    # Count rows by date if y is not specified (i.e. count rows by date if data is not pre-aggregated)

    if(is.null(group_var)) {

      # Un-grouped
      df <- df |>
        group_by(date_factor) |>
        summarise(n = n()) |>
        ungroup()

    } else {

      # Grouped
      df <- df |>
        group_by(date_factor, .data[[group_var]]) |>
        summarise(n = n()) |>
        ungroup()

    }

  } else {
    # Sum values by date if y is specified (i.e. sum values by date if data is pre-aggregated)

    if(is.null(group_var)) {

      # Un-grouped
      df <- df |>
        group_by(date_factor) |>
        summarise(n = sum(.data[[y]])) |>
        ungroup()

    } else {

      # Grouped
      df <- df |>
        group_by(date_factor, .data[[group_var]]) |>
        summarise(n = sum(.data[[y]])) |>
        ungroup()

    }

  }

  # Re-define y so that it points towards n
  y <- "n"




  # Add rolling average df for rolling average line if specified
  if (rolling_average_line == TRUE) {

      df_rolling_average <- df |>
        group_by(date_factor) |>
        summarise(n = sum(.data[[y]])) |>
        ungroup() |>
        mutate(rolling_average = slider::slide_dbl(
                  .x = n,
                  .f = mean,
                  .before = rolling_average_line_lookback - 1, # -1 as .before includes current time interval
                  .complete = FALSE
                )
              )

  }



  # Add cumulative sum df for cumulative sum line if specified
  if (cumulative_sum_line == TRUE) {

      df_cumulative_sum <- df |>
        group_by(date_factor) |>
        summarise(n = sum(.data[[y]])) |>
        ungroup() |>
        mutate(cumulative_sum = cumsum(n))

  }






  #################### EPI CURVE #################################

  ##### CREATE STATIC CHART

  if (!dynamic) {
    # produce ggplot object if 'dynamic' is set to FALSE




  ##### Create base ggplot object

  # Define base ggplot object using R/base_gg() function
  #    -Force base_gg() to run in calling environment so it can find variables
  #     (lexical scoping will cause it to look for unfound variables in /R where
  #     it's stored rather than within the calling function's environment)
  environment(base_gg) <- environment()
  base_return <- base_gg()

  # base_gg() returns a list containing base and df; extract here
  base <- base_return$base
  df <- base_return$df




  # Supress spurious 'Scale for X is already present' messages
  #   -Will suppress other messages
  suppressMessages({



  ##### Build epi curve

  # Build according to whether plotting variables are grouped or not
  if(is.null(group_var)) {

    # Create epi curve without groups

      base <-
        base + geom_bar(
          data = df,
          mapping = aes(x = .data[[x]],
                        y = .data[[y]]
                        ),
          fill = fill_colours[1],
          stat = 'identity',
          color = bar_border_colour,
          linewidth = 0.25
        )

  } else {

    # Create epi curve with groups

      # Adjust group_var_barmode for ggplot
      if(group_var_barmode == "group") {group_var_barmode <- "dodge"}

      base <-
        base + geom_bar(
          data = df,
          mapping = aes(x = .data[[x]],
                        y = .data[[y]],
                        group = .data[[group_var]],
                        fill = .data[[group_var]]
          ),
          stat = 'identity',
          color = bar_border_colour,
          linewidth = 0.25,
          position = group_var_barmode
        ) +
        scale_fill_manual(values = fill_colours)

    }



    ##### Apply legend parameters

    # Legend title
    if (!is.null(legend_title)) {
      base <-  base + labs(name = legend_title,
                           fill = legend_title,
                           colour = legend_title)
    }

    # Legend position
    if (!is.null(legend_pos)) {
      base <-  base + theme(legend.position = legend_pos)
    }



  ##### Add boxes around each case if specified

  if (case_boxes == TRUE) {

    # Add transparent stacked bar plot with external borders to create
    #   boxes around each case.
    base <-
      base + geom_bar(
        # Uncount df to get 1 row per case, add box = 1 column for y-vals to
        #   create 1 box per case
        data = df |> uncount(get(y)) |> mutate(box = 1),
        mapping = aes(x = .data[[x]],
                      y = box
        ),
        fill = "transparent",
        stat = 'identity',
        color = case_boxes_colour,
        linewidth = 0.5
      )

  }



  ##### Add rolling average line if specified

  if (rolling_average_line == TRUE) {

    base <-
      base + geom_line(
        data = df_rolling_average,
        mapping = aes(x = .data[[x]],
                      y = rolling_average,
                      color = rolling_average_line_legend_label,
                      group = 1   # group = 1 as x is a factor
        ),
        linewidth = rolling_average_line_width
      )

  }



  ##### Add cumulative sum line if specified

  if (cumulative_sum_line == TRUE) {

    # Calculate scale factor

    # Get limits of current plotted data, use y_limits instead if provided.
    current_plotted_data_max <-
      if (!is.null(y_limit_max)) {y_limit_max} else {max(layer_scales(base)$y$range$range)}
    current_plotted_data_min <-
      if (!is.null(y_limit_min)) {y_limit_min} else {min(layer_scales(base)$y$range$range)}

    # Get upper limit of cumsum for secondary axis scale
    cumsum_max <- max(df_cumulative_sum$cumulative_sum, na.rm=T)

    # Define scale
    scale <- (current_plotted_data_max - current_plotted_data_min) / (cumsum_max)

    # Plot cumulative sum line
    base <-
      base + geom_line(
        data = df_cumulative_sum,
        mapping = aes(x = .data[[x]],
                      y = (cumulative_sum * scale) + current_plotted_data_min, #shift so that cumsum scale always starts at 0
                      color = cumulative_sum_line_legend_label,
                      group = 1   # group = 1 as x is a factor
        ),
        linewidth = cumulative_sum_line_width
      )

    # Define variable for use in scale_y_continuous below
    sec_axis_var <- sec_axis(~ . / scale - (current_plotted_data_min / scale),
                             name = cumulative_sum_line_axis_title)

  }



  ##### Add rolling average + cumsum line legends

  # Rolling average only
  if (rolling_average_line == TRUE & cumulative_sum_line == FALSE) {

    base <- base +
      scale_color_manual(name = "", values = rolling_average_line_colour)

  # Cumulative sum only
  } else if (rolling_average_line == FALSE & cumulative_sum_line == TRUE) {

    base <- base +
      scale_color_manual(name = "", values = cumulative_sum_line_colour)

  # Both
  } else if (rolling_average_line == TRUE & cumulative_sum_line == TRUE) {

    base <- base +
      scale_color_manual(name = "",
                         values = c(rolling_average_line_colour, cumulative_sum_line_colour))

  }



  ##### Redefine elements of base_gg specific to epi_curve

  # Redefine x-axis label so that default = time_period
  if (!is.null(x_axis_title)) {
    base <- base + labs(x = x_axis_title)
  } else {
    base <- base + labs(x = time_period) # default to time_period if label not provided
  }


  # Redefine y-axis scale to remove gap between bars and x-axis

  # Waiver secondary y-axis scale if no cumulative sum line specified
  if (cumulative_sum_line == FALSE) {sec_axis_var <- waiver()}

  if (!is.null(y_axis_break_labels)) {
    base <- base + scale_y_continuous(breaks = y_axis_break_labels,
                                      expand = c(0,0),
                                      sec.axis = sec_axis_var)
  } else {
    base <- base + scale_y_continuous(expand = c(0,0),
                                      sec.axis = sec_axis_var)
  }


  # Re-apply hline so that it doesn't appear behind the bars

  # Adds hline
  if (!is.null(hline)) {

    base <-
      base + geom_hline(yintercept = hline,
                        colour = hline_colour,
                        linewidth = hline_width,
                        linetype = hline_type)

  }

  # Apply hline label to plot
  if (!is.null(hline) && !is.null(hline_label)) {

    # Get hline_xpos from base_return list from base_gg
    hline_xpos <- base_return$hline_xpos

    base <- base +
      geom_text(
        aes(
          x = hline_xpos,
          y = hline,
          label = hline_label,
          vjust = -0.5,
          hjust = 0
        ),
        colour = hline_label_colour)

  }



  ##### Return final output
  return(base)

  }) # suppressMessage() end

  ### STATIC CHART END



  } else {

  ##### CREATE DYNAMIC CHART

  # Produce plotly object if 'dynamic' is set to TRUE


    ##### Define base min/max x & y values for axis ranges

    #   -It is not currently possible to access range/autorange values from
    #    a plotly object, so define a ggplot object showing the same information
    #    and use it's autoranges as a basis. This also keeps the formatting the
    #    same as the static chart.
    #   -Must be done outside of base_plotly(), as the ggplot geoms will be
    #      different depending upon the nature of the chart

    # Define ggplot object to harvest axis ranges from
    #ggobj <- ggplot() + geom_bar(data=df, aes(x=.data[[x]],y=.data[[y]])) + geom_hline(yintercept = hline)
    if(is.null(group_var)) {
      ggobj <- ggplot() +
        geom_bar(data = df, aes(x = .data[[x]],y = .data[[y]]), stat = 'identity') +
        geom_hline(yintercept = hline)
    } else {
      if(group_var_barmode == "group") {group_var_barmode <- "dodge"}
      ggobj <- ggplot() +
        geom_bar(data = df, aes(x=.data[[x]],y=.data[[y]],group=.data[[group_var]]),
                 stat = 'identity',position = group_var_barmode) +
        geom_hline(yintercept = hline)
    }
    x_min <- ggplot_build(ggobj)$layout$panel_params[[1]]$x.range[1]
    x_max <- ggplot_build(ggobj)$layout$panel_params[[1]]$x.range[2]
    #y_min <- ggplot_build(ggobj)$layout$panel_params[[1]]$y.range[1]
    y_min <- 0
    y_max <- ggplot_build(ggobj)$layout$panel_params[[1]]$y.range[2]


    # # Handle dates converting to numeric when extracted from ggplot axis range
    # x_min <- if(lubridate::is.Date(df[[x]])) {as.Date.numeric(x_min)} else {x_min}
    # x_max <- if(lubridate::is.Date(df[[x]])) {as.Date.numeric(x_max)} else {x_max}
    # y_min <- if(lubridate::is.Date(df[[y]])) {as.Date.numeric(y_min)} else {y_min}
    # y_max <- if(lubridate::is.Date(df[[y]])) {as.Date.numeric(y_max)} else {y_max}




    ##### Create base plotly object

    # Define base plotly object using R/base_plotly() function
    #    -Force base_plotly() to run in calling environment so it can find variables
    #     (lexical scoping will cause it to look for unfound variables in /R where
    #     it's stored rather than within the calling function's environment)
    environment(base_plotly) <- environment()
    base_return <- base_plotly()

    # base_plotly() returns a list containing base, df, y_axis_choice, and axis_label_font; extract here
    base <- base_return$base
    df <- base_return$df
    y_axis_choice <- base_return$y_axis_choice
    x_axis_title_font <- base_return$x_axis_title_font
    y_axis_title_font <- base_return$y_axis_title_font


    ##### Define colour parameters for bar plot

    # Ungrouped data
    if (is.null(group_var)) {

      # Define colour map for single colour
      colormap <- setNames(fill_colours[1], fill_colours[1])

      # Add colour field to df
      df <- df |>
        mutate(fill_colour = fill_colours[1])

    # Grouped data
    } else {

      # Define colour map for multiple colours if not already defined by user
      if (is.null(names(fill_colours))) {
        colormap <- setNames(fill_colours, unique(df[[group_var]]))
      } else {
        colormap <- fill_colours
      }

      # Add colour field to df
      df <- df |>
        mutate(fill_colour = if (is.factor(get(group_var))) {get(group_var)} else {as.factor(get(group_var))} )

    }

    # Define colour_field parameter for bar plot
    #    Note:- Stacked bar colours need reversing to match with ggplot output
    if (is.factor(df$fill_colour) & group_var_barmode == "stack") {
      colour_field <- forcats::fct_rev(df$fill_colour)
    } else {
      colour_field <- df$fill_colour
    }




    ##### Define hover labels

    # Define defaults if user does not define hover_labels
    if (is.null(hover_labels)) {

      # Define total to display depending upon whether case boxes are enabled
      hover_n <- if (case_boxes == FALSE) {'{y}'} else {'{customdata}'}

      # Ungrouped
      if (!is.null(group_var)) {
        hoverlabels <- paste0('<b>%{x}</b>',
                              '<br>%',hover_n)
      # Grouped
      } else {
        hoverlabels <- paste0('<b>%{x}</b>',
                              '<br>%',hover_n,
                              '<extra></extra>') # Remove tooltip for ungrouped data
      }

    } else {

      # Hover labels specified by user input parameter hover_labels
      if (case_boxes == FALSE) {
        hoverlabels <- hover_labels
      } else {
        hoverlabels <- gsub('\\{y\\}','{customdata}',hover_labels)
      }

    }





    ##### Build epi curve

    if (case_boxes == FALSE) {

      # Add bar plot without boxes around each case
      base <- base |>
        add_trace(
          df,
          x = ~ df[[x]],
          y = ~ df[[y]],
          type = 'bar',
          color = ~ colour_field, #df$fill_colour,
          colors = colormap,
          marker = list(
            #color = fill_colours[1]
            line = list(color = bar_border_colour,
                        width = 0.5)
            ),
          hovertemplate = hoverlabels,
          #legendgroup = 'bars',
          showlegend = if (is.null(group_var)) {F} else {T}
        ) |>
        layout(barmode = group_var_barmode)



    # Add bar plot with boxes around each case
    } else if (case_boxes == TRUE) {

        # Uncount data to get one row per case for one box per case.
        df_case_boxes <- df |> uncount(get(y)) |> mutate(box = 1)

        # Re-define colour_field parameter for bar plot
        #    Note:- Stacked bar colours need reversing to match with ggplot output
        if (is.factor(df_case_boxes$fill_colour) & group_var_barmode == "stack") {
          colour_field <- forcats::fct_rev(df_case_boxes$fill_colour)
        } else {
          colour_field <- df_case_boxes$fill_colour
        }

        # Bar plot with boxes
        base <- base |>
          add_trace(
            df_case_boxes,
            x = ~ df_case_boxes[[x]],
            y = ~ df_case_boxes$box,
            type = 'bar',
            color = ~ colour_field, #df$fill_colour,
            colors = colormap,
            marker = list(
              line = list(color = case_boxes_colour,
                          width = 0.5)
            ),
            customdata = df_case_boxes$n, # for hoverlabels
            hovertemplate = hoverlabels,
            #legendgroup = 'bars',
            showlegend = if (is.null(group_var)) {F} else {T}
          ) |>
          layout(barmode = group_var_barmode)

      }



    ##### Add rolling average line if specified

    if (rolling_average_line == TRUE) {

      base <- base |>
        add_trace(
          df_rolling_average,
          x = ~ df_rolling_average[[x]],
          y = ~ df_rolling_average$rolling_average,
          type = 'scatter',
          mode = 'line',
          #mode = 'lines+markers',
          line = list(color = rolling_average_line_colour,
                      width = rolling_average_line_width*2), # *2 to match ggplot formatting),
          #marker = list(color = 'transparent', size = 0),
          name = rolling_average_line_legend_label,
          #legendgroup = 'lines',
          hovertemplate = paste0('<b>%{x}</b>',
                                 '<br>%{y}',
                                 '<extra>',rolling_average_line_legend_label,'</extra>')
        )

    }



    ##### Add cumulative sum line if specified

    if (cumulative_sum_line == TRUE) {

      # Create secondary axis
      base <- base |>
        layout(
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            showline = TRUE,
            zeroline = FALSE,
            linewidth = 1,
            ticks="outside",
            ticklen=3,
            tickangle = -y_axis_label_angle,
            # autorange = TRUE,
            title = list(text = html_bold(cumulative_sum_line_axis_title),
                         font = y_axis_title_font)
          )
        )


        base <- base |>
          add_trace(
            df_cumulative_sum,
            x = ~ df_cumulative_sum[[x]],
            y = ~ df_cumulative_sum$cumulative_sum,
            yaxis = "y2",
            type = 'scatter',
            mode = 'line',
            line = list(color = cumulative_sum_line_colour,
                        width = cumulative_sum_line_width*2), # *2 to match ggplot formatting
            name = cumulative_sum_line_legend_label,
            #legendgroup = 'lines',
            hovertemplate = paste0('<b>%{x}</b>',
                                   '<br>%{y}',
                                   '<extra>',cumulative_sum_line_legend_label,'</extra>')
          )

      }



    ##### Apply legend parameters

    # Legend position
    if (!is.null(legend_pos)) {

      # Let utils/plotly_legend_pos() function run in calling environment so that
      #    it can access point_chart arguements
      environment(plotly_legend_pos) <- environment()

      if (legend_pos != "none") {
        base <- base |> layout(legend = plotly_legend_pos(legend_pos))  # use utils/plotly_legend_pos() function to switch between ggplot and plotly legend params
      } else {
        base <- base |> layout(showlegend = F)
      }

    }

    # Legend title + font
    base <- base |>
      layout(
        legend = list(
          #traceorder = "grouped+reversed",
          title=list(text = legend_title, font = list(size = legend_title_font_size)),
          font=list(size = legend_font_size)
        )
      )


  # return base plot
  return(base)

    } ### DYNAMIC CHART END


}






