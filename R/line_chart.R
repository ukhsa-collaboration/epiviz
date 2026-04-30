#' Line Chart
#'
#' @description A function for producing either a static (ggplot) or dynamic (plotly)
#' line chart.
#'
#' @param dynamic Logical indicating whether to produce a dynamic (plotly) output.
#' Default is \code{FALSE}, which will return a static ggplot output.
#' @param base A base ggplot or plotly object that the output will be applied to. If
#' \code{dynamic = TRUE} then \code{base} must be a plotly object, and if \code{dynamic = FALSE}
#' then \code{base} must be a ggplot object.
#' @param params A named list containing arguements used to create the plot.
#' \describe{
#'    \item{df}{A data frame containing data used to create the line chart.}
#'    \item{x}{character, Name of the variable in \code{df} containing the values used
#'    to populate the x-axis.}
#'    \item{y}{character, Name of the variable in \code{df} containing the values used
#'    to populate the y-axis.}
#'    \item{group_var}{Name of the variable in df used to define separate groups of lines
#'    in the chart.}
#'    \item{line_colours}{Colour(s) of the lines to be plotted (default = \code{"blue"}).
#'    When \code{group_var} is provided, \code{line_colours} can be set as a
#'    character vector to define colours for each group. If a named character vector
#'    is provided where the names are values within \code{group_var}, then each colour
#'    will be mapped to its corresponding value.}
#'    \item{line_types}{Line type for the plotted lines. Permitted values:
#'    \code{"solid"}, \code{"dotted"}, \code{"dashed"}, \code{"longdash"}, \code{"dotdash"}.
#'    Default = \code{"solid"}. When \code{group_var} is provided, a vector of line types
#'    can be supplied to define a different type for each group.}
#'    \item{line_width}{Numeric, width of the plotted lines. Default = \code{1}.}
#'    \item{add_points}{Logical. If \code{TRUE}, points will be added to the line chart.
#'    Default = \code{FALSE}.}
#'    \item{add_points_size}{Numeric, size of the plotted points when \code{add_points = TRUE}.
#'    Default = \code{1.5}.}
#'    \item{ci}{Confidence interval. If \code{ci = "errorbar"} then confidence intervals
#'    will be plotted with each point as errorbars, and if \code{ci = "ribbon"} then
#'    confidence intervals will be added to the chart as a ribbon plot for each group.
#'    If \code{ci} is provided, then \code{ci_upper} and \code{ci_lower} must also be provided.}
#'    \item{ci_upper}{character, Name of the variable in df used as the upper confidence limit.
#'    Mandatory when \code{ci} is provided.}
#'    \item{ci_lower}{character, Name of the variable in df used as the lower confidence limit.
#'    Mandatory when \code{ci} is provided.}
#'    \item{ci_legend}{Logical indicating whether a separate legend should be included
#'    in the chart for confidence interval parameters. Only applies when \code{group_var}
#'    is provided. Defaults to \code{FALSE}.}
#'    \item{ci_legend_title}{Text to use as title for separate legend when \code{ci_legend = TRUE}.
#'    Default = \code{"Confidence interval"}.}
#'    \item{ci_colours}{Colour(s) used for plotting confidence intervals. When \code{ci =
#'    "errorbar"} this will determine the colour of the plotted errorbars, when \code{ci =
#'    "ribbon"} this will determine the colour of the plotted ribbons.}
#'    \item{errorbar_width}{Horizontal width of the plotted error bars when \code{ci =
#'    "errorbar"}.}
#'    \item{y_sec_axis}{Logical to indicate whether data should be plotted on the
#'    secondary (right) y-axis. Default = \code{FALSE}.}
#'    \item{y_sec_axis_no_shift}{Forces the secondary y-axis scale to begin at 0. Default = \code{TRUE}.}
#'    \item{y_sec_axis_percent_full}{Forces the secondary y-axis scale to range from 0-100\%
#'    when \code{y_percent = TRUE}}
#'    \item{chart_title}{Text to use as the chart title.}
#'    \item{chart_title_size}{Font size of chart title. Default = \code{13}.}
#'    \item{chart_title_colour}{Font colour of chart title. Default = \code{"black"}.}
#'    \item{chart_footer}{Text to use as chart footer.}
#'    \item{chart_footer_size}{Font size of chart footer. Default = \code{12}.}
#'    \item{chart_footer_colour}{Font colour of chart footer. Default = \code{"black"}.}
#'    \item{x_axis_title}{Text used for x-axis title. Defaults to name of x-variable if
#'    not stated.}
#'    \item{y_axis_title}{Text used for y-axis title. Defaults to name of y-variable if
#'    not stated.}
#'    \item{x_axis_title_font_size}{Font size of the x-axis title. Default = \code{11}.}
#'    \item{y_axis_title_font_size}{Font size of the y-axis title. Default = \code{11}.}
#'    \item{x_axis_label_angle}{Angle for x-axis label text.}
#'    \item{y_axis_label_angle}{Angle for y-axis label text.}
#'    \item{x_axis_label_font_size}{Font size for the x-axis tick labels. Default = \code{9}.}
#'    \item{y_axis_label_font_size}{Font size for the y-axis tick labels. Default = \code{9}.}
#'    \item{x_axis_reverse}{Reverses x-axis scale if \code{x_axis_reverse = TRUE}.}
#'    \item{y_percent}{Converts y-axis to percentage scale if \code{y_percent = TRUE}.}
#'    \item{x_limit_min}{Lower limit for the x-axis. Default used if not provided.}
#'    \item{x_limit_max}{Upper limit for the x-axis. Default used if not provided.}
#'    \item{y_limit_min}{Lower limit for the y-axis. Default used if not provided.}
#'    \item{y_limit_max}{Upper limit for the y-axis. Default used if not provided.}
#'    \item{x_axis_break_labels}{Vector of values to use for x-axis breaks. Defaults
#'    used if not provided.}
#'    \item{y_axis_break_labels}{Vector of values to use for y-axis breaks. Defaults
#'    used if not provided.}
#'    \item{x_axis_n_breaks}{Scales x-axis with approximately n breaks. Cannot be provided
#'    if \code{x_axis_break_labels} has also been provided.}
#'    \item{y_axis_n_breaks}{Scales y-axis with approximately n breaks. Cannot be used
#'    if \code{y_axis_break_labels} has also been provided.}
#'    \item{x_axis_date_breaks}{A string giving the distance between breaks like "2 weeks",
#'    or "10 years". Valid specifications are 'sec', 'min', 'hour', 'day', 'week',
#'    'month' or 'year', optionally followed by 's'. Matches ggplot scale_date() conventions.
#'    Cannot be used if \code{x_axis_break_labels} is also provided.}
#'    \item{st_theme}{Name of a ggplot theme to be applied to a static plot. Can only be provided
#'    when \code{dynamic = FALSE}.}
#'    \item{show_gridlines}{Logical to show chart gridlines. Default = \code{TRUE}.}
#'    \item{show_axislines}{Logical to show chart axis lines. Default = \code{TRUE}.}
#'    \item{legend_title}{Text used for legend title.}
#'    \item{legend_pos}{Position of the legend. Permitted values = c("top","bottom","right","left")}
#'    \item{legend_font_size}{Font size used in the legend. Default = \code{8}.}
#'    \item{legend_title_font_size}{Font size used for the legend title. Default = \code{8}.}
#'    \item{hline}{Adds horizontal line across the chart at the corresponding y-value. Multiple
#'    values may be provided as a vector to add multiple horizontal lines.}
#'    \item{hline_colour}{Colour of the horizontal lines if \code{hline} is provided. A vector of colours
#'    can be provided to colour individual hlines if multiple hlines have been provided. Default = \code{"black"}.}
#'    \item{hline_width}{Numerical width of the horizontal lines if \code{hline} is provided. A vector of numerical widths
#'    can be provided for individual hlines if multiple hlines have been provided. Default = \code{0.5}.}
#'    \item{hline_type}{Line style of the horizontal lines if \code{hline} is provided. A vector of line styles
#'    can be provided to style hlines if multiple hlines have been provided. Permitted values = c("solid", "dotted",
#'    "dashed", "longdash", "dotdash"). Default = \code{"dashed"}.}
#'    \item{hline_label}{Text to label the horizontal lines if \code{hline} is provided. A vector of text strings
#'    can be provided to label individual hlines if multiple hlines have been provided.}
#'    \item{hline_label_colour}{Colour of the horizontal line labels if \code{hline_label} is provided.
#'    A vector of colours can be provided to colour individual hline_labels if multiple hline_labels have been
#'    provided. Default = \code{"black"}.}
#'    \item{hover_labels}{string, Text to be used in the hover-over labels in a dynamic chart.
#'    Accepts html, use \code{'\%{x}'} to reference corresponding x-axis values
#'    and \code{'\%{y}'} to reference y-axis values.}
#'  }
#'
#'
#' @import dplyr
#' @import grDevices
#' @import scales
#' @import tidyr
#' @import lubridate
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
#' # Example 1: Basic line chart
#'
#' library(epiviz)
#'
#' # Summarise detections per month
#' detections_per_month <- epiviz::lab_data |>
#'   group_by(specimen_month = lubridate::floor_date(specimen_date, 'month')) |>
#'   summarise(detections = n()) |>
#'   ungroup()
#'
#' # Create static line chart
#' chart <- line_chart(
#'   params = list(
#'     df = detections_per_month,
#'     x = "specimen_month",
#'     y = "detections",
#'     line_colours = "#007C91",
#'     chart_title = "Detections per Month",
#'     x_axis_title = "Month of detection",
#'     y_axis_title = "Number of detections",
#'     x_axis_label_angle = 45,
#'     x_axis_date_breaks = "2 months"
#'   )
#' )
#'
#' chart
#'
#'
#'
#' # Example 2: Line chart with grouped data and confidence ribbon
#'
#' library(epiviz)
#'
#' # Define detections per month by species with error limits
#' species_by_month <- epiviz::lab_data |>
#'   group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'),
#'            organism_species_name) |>
#'   summarise(detections = n()) |>
#'   ungroup() |>
#'   rowwise() |>
#'   mutate(lower_limit = detections - sample(10:50,1),
#'          upper_limit = detections + sample(10:50,1)) |>
#'   ungroup()
#'
#' # Define parameters
#' species_params <- list(
#'   df = species_by_month,
#'   x = "specimen_month",
#'   y = "detections",
#'   group_var = "organism_species_name",
#'   line_colours = c("#007C91", "#8A1B61", "#FF7F32"),
#'   line_types = c("solid", "dashed", "dotted"),
#'   add_points = TRUE,
#'   chart_title = "Detections per Month by Species",
#'   x_axis_title = "Month of detection",
#'   y_axis_title = "Number of detections",
#'   x_axis_label_angle = 45,
#'   x_axis_date_breaks = "2 months",
#'   ci = "ribbon",
#'   ci_lower = "lower_limit",
#'   ci_upper = "upper_limit",
#'   ci_colours = c("#007C91", "#8A1B61", "#FF7F32"),
#'   hline = c(450, 550),
#'   hline_colour = c("blue", "red"),
#'   hline_label = c("threshold 1", "threshold 2"),
#'   hline_label_colour = c("blue", "red")
#' )
#'
#' # Create static and dynamic charts
#' static_chart <- line_chart(params = species_params, dynamic = FALSE)
#' dynamic_chart <- line_chart(params = species_params, dynamic = TRUE)
#'
#' # View both simultaneously using shiny app
#' library(shiny)
#' library(plotly)
#' ui <- fluidPage(
#'   plotOutput('static_chart'),
#'   plotlyOutput('dynamic_chart')
#' )
#' server <- function(input, output, session) {
#'   output$static_chart <- renderPlot(static_chart)
#'   output$dynamic_chart <- renderPlotly(dynamic_chart)
#' }
#' shinyApp(ui, server)
#'
#' }
#'
line_chart <- function(
    dynamic = FALSE,
    base = NULL,
    params = list(
      df = NULL,
      x = NULL,
      y = NULL,
      group_var = NULL,
      line_colours = "blue",
      line_types = "solid",
      line_width = 1,
      add_points = FALSE,
      add_points_size = 1.5,
      ci = NULL,
      ci_upper = NULL,
      ci_lower = NULL,
      ci_legend = TRUE,
      ci_legend_title = "Confidence interval",
      ci_colours = "red",
      errorbar_width = NULL,
      y_sec_axis = FALSE,
      y_sec_axis_no_shift = TRUE,
      y_sec_axis_percent_full = FALSE,
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
      x_axis_reverse = FALSE,
      y_percent = FALSE,
      x_limit_min = NULL,
      x_limit_max = NULL,
      y_limit_min = NULL,
      y_limit_max = NULL,
      x_axis_break_labels = NULL,
      y_axis_break_labels = NULL,
      x_axis_n_breaks = NULL,
      y_axis_n_breaks = NULL,
      x_axis_date_breaks = NULL,
      st_theme = NULL,
      show_gridlines = TRUE,
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
  if(!exists('line_colours',where=params)) params$line_colours <- "blue"
  if(!exists('line_types',where=params)) params$line_types <- "solid"
  if(!exists('line_width',where=params)) params$line_width <- 1
  if(!exists('add_points',where=params)) params$add_points <- FALSE
  if(!exists('add_points_size',where=params)) params$add_points_size <- 1.5
  if(!exists('ci_legend',where=params)) params$ci_legend <- TRUE
  if(!exists('ci_legend_title',where=params)) params$ci_legend_title <- "Confidence interval"
  if(!exists('ci_colours',where=params)) params$ci_colours <- "red"
  if(!exists('errorbar_width',where=params)) params$errorbar_width <- NULL
  if(!exists('y_percent',where=params)) params$y_percent <- FALSE
  if(!exists('y_sec_axis',where=params)) params$y_sec_axis <- FALSE
  if(!exists('y_sec_axis_no_shift',where=params)) params$y_sec_axis_no_shift <- TRUE
  if(!exists('y_sec_axis_percent_full',where=params)) params$y_sec_axis_percent_full <- FALSE
  if(!exists('chart_title_size',where=params)) params$chart_title_size <- 13
  if(!exists('chart_title_colour',where=params)) params$chart_title_colour <- "black"
  if(!exists('chart_footer_size',where=params)) params$chart_footer_size <- 12
  if(!exists('chart_footer_colour',where=params)) params$chart_footer_colour <- "black"
  if(!exists('x_axis_title_font_size',where=params)) params$x_axis_title_font_size <- 11
  if(!exists('y_axis_title_font_size',where=params)) params$y_axis_title_font_size <- 11
  if(!exists('x_axis_label_angle',where=params)) params$x_axis_label_angle <- 0
  if(!exists('y_axis_label_angle',where=params)) params$y_axis_label_angle <- 0
  if(!exists('x_axis_label_font_size',where=params)) params$x_axis_label_font_size <- 9
  if(!exists('y_axis_label_font_size',where=params)) params$y_axis_label_font_size <- 9
  if(!exists('x_axis_reverse',where=params)) params$x_axis_reverse <- FALSE
  if(!exists('show_gridlines',where=params)) params$show_gridlines <- TRUE
  if(!exists('show_axislines',where=params)) params$show_axislines <- TRUE
  if(!exists('legend_title',where=params)) params$legend_title <- ""
  if(!exists('legend_pos',where=params)) params$legend_pos <- "right"
  if(!exists('legend_font_size',where=params)) params$legend_font_size <- 8
  if(!exists('legend_title_font_size',where=params)) params$legend_title_font_size <- 8
  if(!exists('hline_colour',where=params)) params$hline_colour <- "black"
  if(!exists('hline_width',where=params)) params$hline_width <- 0.5
  if(!exists('hline_type',where=params)) params$hline_type <- "dashed"
  if(!exists('hline_label_colour',where=params)) params$hline_label_colour <- "black"



  ##### Checks and warnings

  # Check if df is is.null
  if (!exists('df',where=params)) stop("A data frame argument is required")

  # Check df is a df class
  if(!is.data.frame(params$df)) stop("df is not a data frame object")

  # Check df is empty
  if(!assertthat::not_empty(params$df)) stop("df is empty")

  # Check if x argument is is.null
  if ((is.null(params$x)) | !exists('x',where=params))
    stop("Please include a variable from df for x, i.e. x = \"variable_name\"")

  # Check if y argument is is.null
  if ((is.null(params$y)) | !exists('y',where=params))
    stop("Please include a variable from df for y, i.e. y = \"variable_name\"")

  # Check if x is in df
  if (!params$x %in% colnames(params$df))
    stop("x not found within df. Please include a variable from df for x, i.e. x = \"variable_name\"")

  # Check if y is in df
  if (!params$y %in% colnames(params$df))
    stop("y not found within df. Please include a variable from df for y, i.e. y = \"variable_name\"")

  # Check if number of groups and number of line colours are the same
  if (exists('group_var', where=params)) {
    if (length(params$line_colours) > 1) {
      if (length(params$line_colours) != length(unique(params$df[[params$group_var]])))
        stop("The number of line_colours provided must equal the number of unique groups in group_var")
    }
  }

  # Check if number of groups and number of ci colours are the same
  if (exists('group_var', where=params) & exists('ci_colours', where=params) & (exists('ci', where=params))) {
    if (length(params$ci_colours) > 1) {
      if (length(params$ci_colours) != length(unique(params$df[[params$group_var]])))
        stop("The number of ci_colours provided must equal the number of unique groups in group_var")
    }
  }

  # Validate line types
  valid_line_types <- c("solid", "dotted", "dashed", "longdash", "dotdash")
  if (!all(params$line_types %in% valid_line_types)) {
    stop("Invalid line type. Permitted values: 'solid', 'dotted', 'dashed', 'longdash', 'dotdash'")
  }

  # Warn that multiple colours have been provided but group_var absent
  if (length(params$line_colours) > 1 & !exists('group_var',where=params))
    warning("Multiple line_colours have been provided but group_var is absent")

  # Allow axis_break_labels or axis_n_breaks
  if ((!is.null(params$x_axis_break_labels)) & (!is.null(params$x_axis_n_breaks)))
    stop("x_axis_break_labels cannot be provided with x_axis_n_breaks, please provide
         x_axis_break_labels OR x_axis_n_breaks")
  if ((!is.null(params$y_axis_break_labels)) & (!is.null(params$y_axis_n_breaks)))
    stop("y_axis_break_labels cannot be provided with y_axis_n_breaks, please provide
           y_axis_break_labels OR y_axis_n_breaks")

  # Allow x_axis_break_labels or x_axis_date_breaks
  if ((!is.null(params$x_axis_break_labels)) & (!is.null(params$x_axis_date_breaks)))
    stop("x_axis_break_labels cannot be provided with x_axis_date_breaks, please provide
           x_axis_break_labels OR x_axis_date_breaks")

  # Warn that x_axis_date_breaks cannot be used with a reversed x-axis
  if ((params$x_axis_reverse == TRUE) & (!is.null(params$x_axis_date_breaks)))
    warning("x_axis_date_breaks cannot be used with a reversed x-axis, consider using
              x_axis_break_labels instead")

  # Error that base must be provided if sec_axis = TRUE
  if ((params$y_sec_axis == TRUE) & is.null(base)) {
    stop("base must be provided if y_sec_axis = TRUE")
  }

  # Error that base must align with output type
  if (!is.null(base)) {
    if ((dynamic == TRUE) & !("plotly" %in% class(base))) {
      stop("base must be a plotly object if dynamic = TRUE")
    }
    if ((dynamic == FALSE) & !(ggplot2::is_ggplot(base))) {
      stop("base must be a ggplot object if dynamic = FALSE")
    }
  }



  ##### Parameter assignment

  # Define parameters as variables using utils/param_assign() function
  param_assign(params,
               c("df",
                 "x",
                 "y",
                 "group_var",
                 "line_colours",
                 "line_types",
                 "line_width",
                 "add_points",
                 "add_points_size",
                 "ci",
                 "ci_legend",
                 "ci_legend_title",
                 "ci_lower",
                 "ci_upper",
                 "ci_colours",
                 "errorbar_width",
                 "y_sec_axis",
                 "y_sec_axis_no_shift",
                 "y_sec_axis_percent_full",
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
                 "y_percent",
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
                 "hover_labels"))


  # Set a default colour palette when group_var is set but only 1 colour is provided
  if(!is.null(group_var) & length(line_colours) == 1) {
    line_colours <- hue_pal()(length(unique(df[[group_var]])))
  }

  # Set default line types when group_var is set but only 1 type is provided
  if(!is.null(group_var) & length(line_types) == 1) {
    line_types <- rep(line_types, length(unique(df[[group_var]])))
  }

  # Expand ci_colours if group_var is provided but only 1 colour is provided
  # (matching col_chart approach)
  if(!is.null(ci) & !is.null(group_var)) {
    if(length(ci_colours) == 1) {
      ci_colours <- rep(ci_colours, each = length(unique(df[[group_var]])))
    }
  }



  #################### LINE CHART #################################

  ##### CREATE STATIC CHART

  if (!dynamic) {
    # produce ggplot object if 'dynamic' is set to FALSE


    # Suppress spurious 'Scale for X is already present' messages
    suppressMessages({


      ##### Create base ggplot object

      # Define base ggplot object using R/base_gg() function
      environment(base_gg) <- environment()
      base_return <- base_gg()

      # base_gg() returns a list containing base and df; extract here
      base <- base_return$base
      df <- base_return$df



      ##### Apply confidence intervals

      # Apply before main line-plot so that lines appear in front of ribbons.

      # Add conf intervals if arguments for ci and ci_upper+ci_lower bounds are provided.
      if(!is.null(ci)) {

        if(!is.null(ci_lower) && !is.null(ci_upper)) {

          # If errorbar_width not provided; define default based on x-axis limits
          if (is.null(errorbar_width)) {
            if (is.null(base$coordinates$limits$x)) {
              errorbar_width <- as.numeric((max(df[[x]]) - min(df[[x]])) / 100)
            } else {
              errorbar_width <- as.numeric((base$coordinates$limits$x[[2]] - base$coordinates$limits$x[[1]]) / 100)
            }
          }

          # Account for geom_ribbon show.legend parameter accepting values of 'NA' or 'FALSE'
          show_ci_leg <- ifelse(ci_legend == TRUE, NA, FALSE)

          # Plot for no group_var
          if (is.null(group_var)) {

            # Add error bars without grouping variable
            if(ci == 'errorbar') {

              base <-
                base + ggplot2::geom_errorbar(
                  data = df,
                  mapping = aes(
                    x = .data[[x]],
                    ymin = .data[[ci_lower]],
                    ymax = .data[[ci_upper]],
                    colour = ci_legend_title
                  ),
                  width = errorbar_width,
                  linewidth = 0.5,
                  show.legend = show_ci_leg
                ) +
                scale_color_manual("",values=ci_colours[[1]])

              # Add ribbon without grouping variable
            } else if (ci == 'ribbon') {

              base <-
                base + ggplot2::geom_ribbon(
                  data = df,
                  mapping = aes(
                    x = .data[[x]],
                    ymin = .data[[ci_lower]],
                    ymax = .data[[ci_upper]],
                    fill = ci_legend_title,
                    group = 1
                  ),
                  alpha = .5,
                  show.legend = show_ci_leg
                ) +
                scale_fill_manual("",values=ci_colours[[1]])
            }

            # Plot for group_var provided
          } else if (!is.null(group_var)) {

            # Grouped errorbars are added AFTER lines/points using
            # ggnewscale::new_scale_colour() to avoid colour scale conflict.
            # Only grouped ribbons (which use 'fill', not 'colour') are added here.

            if (ci == 'ribbon') {

              base <-
                base +
                ggplot2::geom_ribbon(
                  data = df,
                  mapping = aes(
                    x = .data[[x]],
                    ymin = .data[[ci_lower]],
                    ymax = .data[[ci_upper]],
                    group = .data[[group_var]],
                    fill = .data[[group_var]]
                  ),
                  alpha = .5,
                  show.legend = show_ci_leg
                ) +
                labs(fill = ci_legend_title)

              # Add ci_colours if provided
              if (length(ci_colours) > 1) {
                base <- base +
                  scale_fill_manual(values = ci_colours)
              }

            }
          }

          # Stop if ci_upper and/or ci_lower limit isn't provided
        } else {
          stop("Please provide arguements for 'ci_upper' and 'ci_lower' when ci is specified.")
        }

      }



      ##### Build line chart

      # Build according to whether plotting variables are grouped or not
      if(is.null(group_var)) {

        # Create line chart without groups
        base <-
          base + ggplot2::geom_line(
            data = df,
            mapping = aes(
              x = .data[[x]],
              y = .data[[y]]
            ),
            colour = line_colours[[1]],
            linetype = line_types[[1]],
            linewidth = line_width
          )

      } else {

        # Create line chart with groups
        base <-
          base + ggplot2::geom_line(
            data = df,
            mapping = aes(
              x = .data[[x]],
              y = .data[[y]],
              group = .data[[group_var]],
              colour = .data[[group_var]],
              linetype = .data[[group_var]]
            ),
            linewidth = line_width
          ) +
          scale_colour_manual(values = line_colours) +
          scale_linetype_manual(values = line_types)


        ##### Apply legend parameters

        # Legend title
        if (!is.null(legend_title)) {
          base <-  base + labs(name = legend_title,
                               colour = legend_title,
                               linetype = legend_title)
        }

        # Legend position
        if (!is.null(legend_pos)) {
          base <-  base + theme(legend.position = legend_pos)
        }

      }



      ##### Add points to lines if specified

      if (add_points) {
        if (!is.null(group_var)) {
          base <-
            base + ggplot2::geom_point(
              data = df,
              mapping = aes(
                x = .data[[x]],
                y = .data[[y]],
                colour = .data[[group_var]]
              ),
              size = add_points_size
            ) +
            guides(size = "none")
        } else {
          base <-
            base + ggplot2::geom_point(
              data = df,
              mapping = aes(
                x = .data[[x]],
                y = .data[[y]]
              ),
              colour = line_colours[[1]],
              size = add_points_size
            ) +
            guides(size = "none")
        }
      }



      ##### Add grouped errorbars (after lines/points to allow separate colour scale)

      if (!is.null(ci) && ci == 'errorbar' && !is.null(group_var)) {
        # new_scale_colour() internally renames the original colour aesthetic,
        # which prevents ggplot2 from merging the colour and linetype legends.
        # Fold linetype visuals into the colour guide and suppress the separate
        # linetype legend to avoid a duplicate with black entries.
        base <- base +
          guides(
            colour = guide_legend(override.aes = list(linetype = line_types)),
            linetype = "none"
          )

        # new_scale_colour() must be added as a separate step so it is
        # registered in the plot before subsequent layers reference it.
        base <- base + ggnewscale::new_scale_colour()

        base <- base +
          ggplot2::geom_errorbar(
            data = df,
            mapping = aes(
              x = .data[[x]],
              ymin = .data[[ci_lower]],
              ymax = .data[[ci_upper]],
              colour = .data[[group_var]]
            ),
            width = errorbar_width,
            linewidth = .5,
            show.legend = ifelse(ci_legend == TRUE, NA, FALSE)
          ) +
          scale_colour_manual(
            name = ci_legend_title,
            values = if (length(ci_colours) > 1) ci_colours else rep(ci_colours, length(line_colours)),
            guide = if (ci_legend) "legend" else "none"
          )

      }



      ##### Return final output
      return(clean_gg_labels(base))  # use utils/clean_gg_labels() to prevent 'Ignoring unknown labels:' messages

    }) # suppressMessage() end

    ### STATIC CHART END



  } else {

    ##### CREATE DYNAMIC CHART

    # Produce plotly object if 'dynamic' is set to TRUE


    ##### Define base min/max x & y values for axis ranges

    #   -It is not currently possible to access range/autorange values from
    #    a plotly object, so define a ggplot object showing the same information
    #    and use its autoranges as a basis. This also keeps the formatting the
    #    same as the static chart.

    # Build xlim and ylim for coord_cartesian (matching base_gg approach)
    xlim <- c(
      if (!is.null(x_limit_min)) { if (lubridate::is.Date(df[[x]])) as.Date(x_limit_min) else x_limit_min } else { NA },
      if (!is.null(x_limit_max)) { if (lubridate::is.Date(df[[x]])) as.Date(x_limit_max) else x_limit_max } else { NA }
    )
    ylim <- c(
      if (!is.null(y_limit_min)) { y_limit_min } else { NA },
      if (!is.null(y_limit_max)) { y_limit_max } else { NA }
    )

    # Define ggplot object to harvest axis ranges from (with coord_cartesian to match static chart)
    ggobj <- ggplot() +
      geom_line(data=df, aes(x=.data[[x]], y=.data[[y]])) +
      geom_hline(yintercept = hline) +
      coord_cartesian(xlim = if (all(is.na(xlim))) NULL else xlim,
                      ylim = if (all(is.na(ylim))) NULL else ylim)

    x_min <- ggplot_build(ggobj)$layout$panel_params[[1]]$x.range[1]
    x_max <- ggplot_build(ggobj)$layout$panel_params[[1]]$x.range[2]
    y_min <- ggplot_build(ggobj)$layout$panel_params[[1]]$y.range[1]
    y_max <- ggplot_build(ggobj)$layout$panel_params[[1]]$y.range[2]

    # Handle dates converting to numeric when extracted from ggplot axis range
    x_min <- if(lubridate::is.Date(df[[x]])) {as.Date.numeric(x_min)} else {x_min}
    x_max <- if(lubridate::is.Date(df[[x]])) {as.Date.numeric(x_max)} else {x_max}
    y_min <- if(lubridate::is.Date(df[[y]])) {as.Date.numeric(y_min)} else {y_min}
    y_max <- if(lubridate::is.Date(df[[y]])) {as.Date.numeric(y_max)} else {y_max}

    # Compensate for the 5% padding that base_plotly adds to date axes
    # base_plotly adds: xpad = 0.05 * range, then x_min -= xpad, x_max += xpad
    # To counteract this, we pre-shrink by: compensation = range / 22
    if (lubridate::is.Date(df[[x]])) {
      x_range_days <- as.numeric(difftime(x_max, x_min, units = "days"))
      compensation_days <- round(x_range_days / 22, digits = 0)
      x_min <- x_min + compensation_days
      x_max <- x_max - compensation_days
    } else if (is.numeric(df[[x]])) {
      x_range <- x_max - x_min
      compensation <- x_range / 22
      x_min <- x_min + compensation
      x_max <- x_max - compensation
    }

    # Replace x_limit_min/max with compensated x_min/x_max so that base_plotly:
    # 1) Assigns x_min <- x_limit_min (no-op, since the value already equals x_min)
    # 2) Uses x_limit_min for hline label positioning (at the visible left edge)
    x_limit_min <- x_min
    x_limit_max <- x_max



    ##### Create base plotly object

    # Define base plotly object using R/base_plotly() function
    environment(base_plotly) <- environment()
    base_return <- base_plotly()

    # base_plotly() returns a list containing base, df, and the y_axis_choice variable; extract here
    base <- base_return$base
    df <- base_return$df
    y_axis_choice <- base_return$y_axis_choice


    # Function to handle transparency with or without yarrr
    add_transparency <- function(color, trans.val = 0.5) {
      if (requireNamespace("yarrr", quietly = TRUE)) {
        return(yarrr::transparent(color, trans.val = trans.val))
      } else {
        rgb_col <- col2rgb(color)
        return(rgb(rgb_col[1], rgb_col[2], rgb_col[3],
                   alpha = (1 - trans.val) * 255,
                   maxColorValue = 255))
      }
    }


    ##### Apply confidence intervals

    # Apply before main line-plot so that lines appear in front of ribbons.

    if(!is.null(ci)) {

      # Stop if ci_upper and/or ci_lower limit isn't provided
      if(is.null(ci_lower) | is.null(ci_upper)) {
        stop("Please provide arguements for 'ci_upper' and 'ci_lower' when ci is specified.")
      }


      # Plot for no group_var
      if (is.null(group_var)) {

        # Add error bars without grouping variable
        if(ci == 'errorbar') {

          # Plotly error bars require upper and lower error divergence
          df <- df |>
            mutate(diff_ci_lower = get(y) - get(ci_lower),
                   diff_ci_upper = get(ci_upper) - get(y))

          # Error bars will be added directly to the main line trace.
          # Add a legend-only entry for CI if ci_legend is TRUE.
          if (ci_legend) {
            base <- base |>
              add_trace(
                x = c(NA), y = c(NA),
                type = 'scatter',
                mode = 'markers',
                yaxis = y_axis_choice,
                name = ci_legend_title,
                showlegend = TRUE,
                legendgroup = 'ci',
                marker = list(
                  color = ci_colours,
                  symbol = 'line-ew-open',
                  size = 10
                ),
                hoverinfo = 'none'
              )
          }

          # Add ribbon without grouping variable
        } else if (ci == 'ribbon') {

          base <- base |>
            add_ribbons(
              x = ~ df[[x]],
              ymin = ~ df[[ci_lower]],
              ymax = ~ df[[ci_upper]],
              yaxis = y_axis_choice,
              hoverinfo='none',
              showlegend = ci_legend,
              legendgroup = 'ci',
              name = ci_legend_title,
              fillcolor = add_transparency(ci_colours, trans.val = .5),
              line = list(color = 'transparent')
            )

        }


        # Plot for group_var provided
      } else if (!is.null(group_var)) {

        # Add error bars or ribbon depending upon ci arguement

        if(ci == 'errorbar') {

          # Error bars will be added directly to each group's main line trace.
          # No separate traces needed here.

          # Add ribbon with grouping variable
        } else if (ci == 'ribbon') {

          unique_groups <- unique(df[[group_var]])
          has_named_ci_colours <- !is.null(names(ci_colours))

          for (i in 1:length(unique_groups)) {

            # Get ci colour for this group - by name if available, otherwise by index
            ci_group_name <- as.character(unique_groups[i])
            ci_group_colour <- if (has_named_ci_colours && ci_group_name %in% names(ci_colours)) {
              ci_colours[[ci_group_name]]
            } else {
              ci_colours[[i]]
            }

            df_group_low <- df |>
              filter(get(group_var) == unique_groups[i]) |>
              select(any_of(c(x, ci_lower))) |>
              dplyr::rename("y_val" = 2)

            df_group_up <- df |>
              filter(get(group_var) == unique_groups[i]) |>
              select(any_of(c(x, ci_upper))) |>
              dplyr::rename("y_val" = 2)

            df_group_ribb <- rbind(df_group_low, (df_group_up |> arrange(desc(row_number()))))

            base <- base |>
              add_trace(
                data = df_group_ribb,
                x = ~ df_group_ribb[[x]],
                y = ~ y_val,
                type = 'scatter',
                mode = 'lines',
                yaxis = y_axis_choice,
                name = unique_groups[[i]],
                line = list(color = 'transparent'),
                fill = 'toself',
                fillcolor = add_transparency(ci_group_colour, trans.val = .5),
                showlegend = ci_legend,
                legendgroup = 'ci',
                legendgrouptitle = list(text = ci_legend_title)
              )

          }

        }

      }

    }



    ##### Define default hover labels

    if (is.null(hover_labels)) {

      if (is.null(ci)) {
        hoverlabels <- paste0('<b>%{x}</b>',
                              '<br>%{y}')
      } else {
        hoverlabels <- paste0('<b>%{x}</b>',
                              '<br>%{meta}',
                              '<br><i>Upper: %{text}</i>',
                              '<br><i>Lower: %{customdata}</i>')
      }

      # Remove tooltip for ungrouped data
      if (is.null(group_var)) {
        hoverlabels <- paste0(hoverlabels, '<extra></extra>')
      }

    } else {
      hoverlabels <- hover_labels
    }


    ##### Define plotly line mode (with or without markers)
    plt_mode <- if (add_points == TRUE) {'lines+markers'} else {'lines'}



    ##### Create line chart

    # Build according to whether plotting variables are grouped or not
    if (is.null(group_var)) {

      # Leverage 'text' and 'customdata' fields to include ci limits in default hover labels
      if (is.null(ci)) {
        text_upper <- df[[x]]
        text_lower <- df[[x]]
      } else {
        text_upper <- if(y_percent==TRUE) {scales::percent(df[[ci_upper]])} else {df[[ci_upper]]}
        text_lower <- if(y_percent==TRUE) {scales::percent(df[[ci_lower]])} else {df[[ci_lower]]}
      }

      # Add plotly trace without groups
      # Build trace arguments - only include markers if add_points is TRUE
      trace_args <- list(
        data = df,
        x = ~ df[[x]],
        y = ~ df[[y]],
        type = 'scatter',
        mode = plt_mode,
        yaxis = y_axis_choice,
        line = list(
          color = line_colours[[1]],
          dash = plotly_line_style(line_types[[1]]),
          width = line_width * 2  # scale ggplot to plotly
        ),
        legendgroup = 'data',
        name = if(legend_title != "") {legend_title} else {y},
        meta = if(y_percent==TRUE) {scales::percent(df[[y]])} else {df[[y]]},
        text = text_upper,
        customdata = text_lower,
        hovertemplate = hoverlabels
      )

      # Add marker list only if add_points is TRUE
      if (add_points) {
        trace_args$marker <- list(
          color = line_colours[[1]],
          size = add_points_size * 3,  # scale ggplot to plotly
          line = list(color = 'transparent', width = 0)
        )
      }

      # Add error bars directly to the line trace if ci = 'errorbar'
      if (!is.null(ci) && ci == 'errorbar') {
        trace_args$error_y <- list(
          type = "data",
          symmetric = FALSE,
          color = ci_colours,
          thickness = 1,
          arrayminus = df$diff_ci_lower,
          array = df$diff_ci_upper
        )
      }

      base <- do.call(add_trace, c(list(p = base), trace_args))

    } else {

      # Add plotly trace with groups

      unique_groups <- unique(df[[group_var]])

      # Determine colour order - if line_colours is a named vector, match by name
      # Otherwise use index-based matching
      has_named_colours <- !is.null(names(line_colours))

      for (i in 1:length(unique_groups)) {

        df_group <- df |>
          filter(get(group_var) == unique_groups[i])

        # Get colour for this group - by name if available, otherwise by index
        # Convert to character to handle factors properly
        group_name <- as.character(unique_groups[i])
        group_colour <- if (has_named_colours && group_name %in% names(line_colours)) {
          line_colours[[group_name]]
        } else {
          line_colours[[i]]
        }

        # Leverage 'text' and 'customdata' fields
        if (is.null(ci)) {
          text_upper <- df_group[[x]]
          text_lower <- df_group[[x]]
        } else {
          text_upper <- if(y_percent==TRUE) {scales::percent(df_group[[ci_upper]])} else {df_group[[ci_upper]]}
          text_lower <- if(y_percent==TRUE) {scales::percent(df_group[[ci_lower]])} else {df_group[[ci_lower]]}
        }

        # Build trace arguments - only include markers if add_points is TRUE
        trace_args <- list(
          data = df_group,
          x = df_group[[x]],
          y = df_group[[y]],
          type = 'scatter',
          mode = plt_mode,
          yaxis = y_axis_choice,
          name = unique_groups[[i]],
          line = list(
            color = group_colour,
            dash = plotly_line_style(line_types[[i]]),
            width = line_width * 2  # scale ggplot to plotly
          ),
          legendgroup = 'data',
          legendgrouptitle = list(text = legend_title),
          meta = if(y_percent==TRUE) {scales::percent(df_group[[y]])} else {df_group[[y]]},
          text = text_upper,
          customdata = text_lower,
          hovertemplate = hoverlabels
        )

        # Add marker list only if add_points is TRUE
        if (add_points) {
          trace_args$marker <- list(
            color = group_colour,
            size = add_points_size * 3,  # scale ggplot to plotly
            line = list(color = 'transparent', width = 0)
          )
        }

        # Add error bars directly to the line trace if ci = 'errorbar'
        if (!is.null(ci) && ci == 'errorbar') {
          has_named_ci_colours <- !is.null(names(ci_colours))
          ci_group_name <- as.character(unique_groups[i])
          ci_group_colour <- if (has_named_ci_colours && ci_group_name %in% names(ci_colours)) {
            ci_colours[[ci_group_name]]
          } else {
            ci_colours[[i]]
          }

          df_group <- df_group |>
            dplyr::mutate(diff_ci_lower = get(y) - get(ci_lower),
                          diff_ci_upper = get(ci_upper) - get(y))

          trace_args$error_y <- list(
            type = "data",
            symmetric = FALSE,
            color = ci_group_colour,
            thickness = 1,
            arrayminus = df_group$diff_ci_lower,
            array = df_group$diff_ci_upper
          )
        }

        base <- do.call(add_trace, c(list(p = base), trace_args))

      }

    }


    ##### Apply legend parameters

    # Legend position
    if (!is.null(legend_pos)) {

      environment(plotly_legend_pos) <- environment()

      if (legend_pos != "none") {
        base <- base |> layout(legend = plotly_legend_pos(legend_pos))
      } else {
        base <- base |> layout(showlegend = F)
      }

    }

    # Legend title + font
    base <- base |>
      layout(
        legend = list(
          #title=list(text = legend_title, font = list(size = legend_title_font_size)),
          font=list(size = legend_font_size)
        )
      )


    # return base plot
    return(base)

  } ### DYNAMIC CHART END


}
