#' Column chart
#'
#' @description A function for producing either a static (ggplot) or dynamic (plotly)
#' column chart.
#'
#' @param dynamic Logical indicating whether to produce a dynamic (plotly) output.
#' Default is \code{FALSE}, which will return a static ggplot output.
#' @param params A named list containing arguements used to create the plot.
#' \describe{
#'    \item{df}{A data frame containing data used to create the column chart.}
#'    \item{x}{character, Name of the variable in \code{df} containing the values used
#'    to populate the x-axis.}
#'    \item{y}{character, Name of the variable in \code{df} containing the values used
#'    to populate the y-axis.)}
#'    \item{x_time_series}{If \code{x_time_series = TRUE} then x-axis data will be aggregated into
#'    a time series. \code{x} must be a date variable when \code{x_time_series = TRUE}. The
#'    granularity of the time series can be set using the \code{time_period}
#'    parameter. If \code{y} is not provided, then x_time_series will assume that each row
#'    corresponds to a single observation and aggregate accordingly.}
#'    \item{x_time_series_bar_labels}{If \code{x_time_series_bar_labels = TRUE} then labels
#'    equalling y-axis values will be added to each bar.}
#'    \item{time_period}{The time period to be used along the x-axis. Options include
#'    \code{c("day","year","month","quarter","year_month","year_quarter",
#'      "iso_year","iso_week","start_iso_year_week","iso_year_week")}. Default = \code{"day"}}
#'    \item{group_var}{Name of the variable in df used to define separate groups within each bar,
#'    e.g. 'species' or 'region'.}
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
#'    \item{bar_border_colour}{character, Colour of the border around each bar. Default = \code{"transparent"},
#'    meaning that no border colour is drawn as default.}
#'    \item{bar_labels}{character, Name of the variable in \code{df} containing the labels to be used
#'    for each bar. To set bar lables when \code{x_time_series = TRUE}, set \code{x_time_series_bar_labels = TRUE}
#'    and labels will be added to each bar equal to y-axis values.}
#'    \item{bar_labels_pos}{character, The position on the bars that labels will be plotted, permitted
#'    values are \code{c('bar_above','bar_base','bar_centre','above_errorbar')}. Default = \code{'bar_above'}.}
#'    \item{bar_labels_font_size}{numeric, Font size for the bar labels. Default = \code{8}.}
#'    \item{bar_labels_font_colour}{character, Font colour for the bar labels. Default = \code{'black'}.}
#'    \item{bar_labels_angle}{numeric, Font angle for the bar labels.}
#'    \item{bar_labels_percent}{boolean, If \code{bar_labels_percent = TRUE} then the values in \code{bar_labels}
#'    will be converted into a percentage before plotting.}
#'    \item{case_boxes}{boolean, If \code{case_boxes = TRUE} then a boundary box will be drawn around
#'    each case within each bar. Defaults to \code{case_boxes = FALSE}.}
#'    \item{case_boxes_colour}{The colour of the border around each case box if \code{case_boxes =
#'    TRUE}. Default = \code{"white"}.}
#'    \item{axis_flip}{boolean, If set to \code{TRUE} then x and y axes will be flipped and bars will
#'    be drawn hozizontally rather than vertically.}
#'    \item{ci}{Confidence interval. If \code{ci = "errorbar"} then confidence intervals be
#'    be plotted with each bar as errorbars. If \code{ci} is provided, then \code{ci_upper}
#'    and \code{ci_lower} must also be provided.}
#'    \item{ci_upper}{character, Name of the variable in df used as the upper confidence limit for
#'    each bar. Mandatory when \code{ci} is provided.}
#'    \item{ci_lower}{character, Name of the variable in df used as the lower confidence limit for
#'    each bar. Mandatory when \code{ci} is provided.}
#'    \item{ci_legend}{Logical indicating whether a separate legend should be included
#'    in the chart for confidence interval parameters. Only applies when \code{group_var}
#'    is provided. Defaults to \code{FALSE}.}
#'    \item{ci_legend_title}{Text to use as title for separate legend when \code{ci_legend = TRUE}.
#'    Default = \code{"Confidence interval"}.}
#'    \item{ci_colours}{Colour(s) used for plotting errorbars when \code{ci = "errorbar"}.
#'    If \code{group_var} has been provided, then \code{fill_colours} must be a character
#'    vector of colours with a number of elements equal to the number of unique groups
#'    in \code{group_var}. If a named character vector is provided where the names are
#'    values within \code{group_var}, then each colour will be mapped to it's corresponding
#'    value in \code{group_var} on the output chart and legend (e.g. \code{c("KLEBSIELLA PNEUMONIAE"
#'    = "#007C91", "STAPHYLOCOCCUS AUREUS" = "#8A1B61","PSEUDOMONAS AERUGINOSA" = "#FF7F32")} or
#'    \code{setNames(c("#007C91","#8A1B61","#FF7F32"), c("KLEBSIELLA PNEUMONIAE","STAPHYLOCOCCUS
#'    AUREUS","PSEUDOMONAS AERUGINOSA"))})}
#'    \item{errorbar_width}{Horizontal width of the plotted error bars when \code{ci =
#'    "errorbar"}.}
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
#'    \item{x_limit_min}{Lower limit for the x-axis. Default used if not provided.}
#'    \item{x_limit_max}{Upper limit for the x-axis. Default used if not provided.}
#'    \item{y_limit_min}{Lower limit for the y-axis. Default used if not provided.}
#'    \item{y_limit_max}{Upper limit for the y-axis. Default used if not provided.}
#'    \item{x_axis_break_labels}{Vector of values to use for x-axis breaks. Defaults
#'    used if not provided. If \code{x_time_series = TRUE} then Values provided must
#'    match the formatting of \code{time_period}.}
#'    \item{y_axis_break_labels}{Vector of values to use for y-axis breaks. Defaults
#'    used if not provided.}
#'    \item{x_axis_n_breaks}{Scales x-axis with approximately n breaks. Cannot be provided
#'    if \code{x_axis_break_labels} has also been provided.}
#'    \item{y_axis_n_breaks}{Scales y-axis with approximately n breaks. Cannot be used
#'    if \code{y_axis_break_labels} has also been provided.}
#'    \item{x_axis_reverse}{Reverses x-axis scale if \code{x_axis_reverse = TRUE}.}
#'    \item{y_percent}{Converts y-axis to percentage scale if \code{y_percent = TRUE}.}
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
#'    \item{hline_label_colour}{Colour of the horizontal line labels if \code{hline_labels} is provided.
#'    A vector of colours can be provided to colour individual hline_labels if multiple hline_labels have been
#'    provided. Default = \code{"black"}.}
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
#' # Example 1: Basic column chart
#'
#' # Create a basic column chart using the epiviz::lab_data dataset
#' library(epiviz)
#'
#' # Summarise the overall number of detections by region in 2023
#' detections_by_region_2023 <- lab_data |>
#'   filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
#'   group_by(region) |>
#'   summarise(detections = n()) |>
#'   ungroup() |>
#'   # Place 'Yorkshire and Humber' on multiple lines
#'   mutate(region = ifelse(region == "Yorkshire and Humber", "Yorkshire and \nHumber", region))
#'
#' # Create column chart
#' basic_col_chart <- col_chart(
#'   params = list(
#'     df = detections_by_region_2023,
#'     x = "region",
#'     y = "detections",
#'     fill_colours = "#007C91",
#'     chart_title = "Laboratory Detections by Region 2023",
#'     x_axis_title = "Region",
#'     y_axis_title = "Number of detections",
#'     x_axis_label_angle = -45
#'   )
#' )
#'
#' basic_col_chart
#'
#'
#'
#'
#' # Example 2: Column chart with bar labels, errorbars, and a horizontal threshold line
#'
#' # Create a basic column chart using the epiviz::lab_data dataset
#' library(epiviz)
#'
#'
#' # Summarise the overall number of detections by region in 2023
#' detections_by_region_2023 <- lab_data |>
#'   filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
#'   group_by(region) |>
#'   summarise(detections = n()) |>
#'   ungroup() |>
#'   # Place 'Yorkshire and Humber' on multiple lines
#'   mutate(region = ifelse(region == "Yorkshire and Humber", "Yorkshire and \nHumber", region)) |>
#'   # Add random error for errorbars
#'   rowwise() |>
#'   mutate(lower_limit = detections - sample(50:200,1),
#'          upper_limit = detections + sample(50:200,1)) |>
#'   ungroup()
#'
#'
#' # Create column chart
#' col_chart <- col_chart(
#'   params = list(
#'     df = detections_by_region_2023,
#'     x = "region",
#'     y = "detections",
#'     fill_colours = "#007C91",
#'     ci = 'errorbar',
#'     ci_lower = "lower_limit",
#'     ci_upper = "upper_limit",
#'     errorbar_width = 0.2,
#'     bar_labels = 'detections',
#'     bar_labels_pos = 'bar_base',
#'     bar_labels_font_size = 12,
#'     bar_labels_font_colour = 'white',
#'     chart_title = "Laboratory Detections by Region 2023",
#'     x_axis_title = "Region",
#'     y_axis_title = "Number of detections",
#'     show_gridlines = FALSE,
#'     hline = 1200,
#'     hline_colour = 'orange',
#'     hline_label = 'Threshold',
#'     hline_label_colour = 'orange'
#'   )
#' )
#'
#' col_chart
#'
#'
#'
#'
#' # Example 3: Create both static and dynamic column charts using grouped data
#'
#' library(epiviz)
#'
#' # Summarise the overall number of detections by species and region in 2023
#' detections_by_species_region_2023 <- lab_data |>
#'   filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
#'   group_by(region, organism_species_name) |>
#'   summarise(detections = n()) |>
#'   ungroup() |>
#'   # Place the name for 'Yorkshire and Humber' on multiple lines
#'   mutate(region = ifelse(region == "Yorkshire and Humber", "Yorkshire and \nHumber", region))
#'
#' # Create parameter list
#' params_list <- list(
#'   df = detections_by_species_region_2023,
#'   x = "region",
#'   y = "detections",
#'   group_var = "organism_species_name",
#'   group_var_barmode = "group",
#'   fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
#'                    "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
#'                    "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
#'   chart_title = "Laboratory Detections by Region \nand Species 2023",
#'   chart_footer = "This chart has been created using simulated data.",
#'   x_axis_title = "Region",
#'   y_axis_title = "Number of detections",
#'   chart_title_colour = "#007C91",
#'   chart_footer_colour = "#007C91",
#'   show_gridlines = FALSE
#' )
#'
#' # Create static column chart
#' static_chart <- col_chart(params = params_list, dynamic = FALSE)
#'
#' # Create dynamic epi curve
#' dynamic_chart <- col_chart(params = params_list, dynamic = TRUE)
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
#'
#'
#'
#' # Example 4: Create both static and dynamic column charts using grouped data; apply
#' #              bar labels and present as horizontal column chart.
#'
#' library(epiviz)
#'
#' # Summarise the overall number of detections by species and region in 2023
#' detections_by_species_region_2023 <- lab_data |>
#'   filter(specimen_date >= as.Date("2023-01-01") & specimen_date <= as.Date("2023-12-31")) |>
#'   group_by(region, organism_species_name) |>
#'   summarise(detections = n()) |>
#'   ungroup() |>
#'   # Place the name for 'Yorkshire and Humber' on multiple lines
#'   mutate(region = ifelse(region == "Yorkshire and Humber", "Yorkshire and \nHumber", region))
#'
#'
#' # Reorder dataframe so that regions with the largest total number of detections will
#' #    appear at the top.
#' detections_by_species_region_2023 <- detections_by_species_region_2023 |>
#'   group_by(region) |>
#'   mutate(group_detections = sum(detections)) |>
#'   ungroup() |>
#'   arrange(group_detections) |>
#'   mutate(region = factor(region, levels = unique(region)))
#'
#'
#' # Create parameter list
#' params_list <- list(
#'   df = detections_by_species_region_2023,
#'   x = "region",
#'   y = "detections",
#'   group_var = "organism_species_name",
#'   group_var_barmode = "stack",
#'   fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
#'                    "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
#'                    "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
#'   bar_labels = 'detections',
#'   bar_labels_pos = 'bar_centre',
#'   bar_labels_font_size = 8,
#'   bar_labels_font_colour = 'white',
#'   chart_title = "Laboratory Detections by Region \nand Species 2023",
#'   chart_footer = "This chart has been created using simulated data.",
#'   x_axis_title = "Region",
#'   y_axis_title = "Number of detections",
#'   chart_title_colour = "#007C91",
#'   chart_footer_colour = "#007C91",
#'   show_gridlines = FALSE,
#'   axis_flip = TRUE  # Create horizontal chart using the axis_flip parameter
#' )
#'
#'
#' # Create static and dynamic column charts
#' static_chart <- col_chart(params = params_list, dynamic = FALSE)
#' dynamic_chart <- col_chart(params = params_list, dynamic = TRUE)
#'
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
#'
#'
#' # Example 5: Create both static and dynamic column charts using a time-series x-axis
#' #              and a percentage y-axis.
#'
#' library(epiviz)
#'
#' # Create parameter list
#' params_list <- list(
#'   df = lab_data,
#'   x = "specimen_date",
#'   x_limit_min = "2022-12-01",
#'   x_limit_max = "2023-03-31",
#'   # Set x_time_series = TRUE and time_period = "iso_year_week" to aggregate data
#'   #   into a time series by ISO week (assumes each row in df corresponds to a
#'   #   single observation).
#'   x_time_series = TRUE,
#'   time_period = "iso_year_week",
#'   x_time_series_bar_labels = TRUE,
#'   # Set y_percent = TRUE to convert y-axis to percentage scale (when x_time_series
#'   #   = TRUE this will also convert y values and bar labels to percentages by group).
#'   y_percent = TRUE,
#'   group_var = "organism_species_name",
#'   group_var_barmode = "stack",
#'   fill_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C91",
#'                    "STAPHYLOCOCCUS AUREUS" = "#8A1B61",
#'                    "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
#'   bar_labels_pos = 'bar_centre',
#'   bar_labels_font_size = 8,
#'   bar_labels_font_colour = 'white',
#'   chart_title = "Laboratory Detections by ISO week \nand Species, winter 2022-23",
#'   chart_footer = "This chart has been created using simulated data.",
#'   x_axis_title = "ISO Week",
#'   y_axis_title = "Number of detections",
#'   x_axis_label_angle = -45,
#'   chart_title_colour = "#007C91",
#'   chart_footer_colour = "#007C91",
#'   show_gridlines = FALSE
#' )
#'
#'
#' # Create static and dynamic column charts
#' static_chart <- col_chart(params = params_list, dynamic = FALSE)
#' dynamic_chart <- col_chart(params = params_list, dynamic = TRUE)
#'
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
#'
#' }
#'
col_chart <- function(
    dynamic = FALSE,
    params = list(
      df = NULL,
      x = NULL,
      y = NULL,
      x_time_series = FALSE,
      x_time_series_bar_labels = FALSE,
      time_period = "day",
      group_var = NULL,
      group_var_barmode = 'stack',
      fill_colours = "lightblue",
      bar_border_colour = "transparent",
      bar_labels = NULL,
      bar_labels_pos = 'bar_above',
      bar_labels_font_size = 8,
      bar_labels_font_colour = 'black',
      bar_labels_angle = 0,
      bar_labels_percent = FALSE,
      case_boxes = FALSE,
      case_boxes_colour = "white",
      axis_flip = FALSE,
      ci = NULL,
      ci_upper = NULL,
      ci_lower = NULL,
      ci_legend = TRUE,
      ci_legend_title = "Confidence interval",
      ci_colours = "red",
      errorbar_width = NULL,
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
      x_limit_min = NULL,
      x_limit_max = NULL,
      y_limit_min = NULL,
      y_limit_max = NULL,
      x_axis_break_labels = NULL,
      y_axis_break_labels = NULL,
      x_axis_n_breaks = NULL,
      y_axis_n_breaks = NULL,
      x_axis_reverse = FALSE,
      y_percent = FALSE,
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
  if(!exists('group_var_barmode',where=params)) params$group_var_barmode <- "stack"
  if(!exists('x_time_series',where=params)) params$x_time_series <- FALSE
  if(!exists('x_time_series_bar_labels',where=params)) params$x_time_series_bar_labels <- FALSE
  if(!exists('time_period',where=params)) params$time_period <- "day"
  if(!exists('fill_colours',where=params)) params$fill_colours <- "lightblue"
  if(!exists('bar_border_colour',where=params)) params$bar_border_colour <- "transparent"
  if(!exists('bar_labels_pos',where=params)) params$bar_labels_pos <- "bar_above"
  if(!exists('bar_labels_font_size',where=params)) params$bar_labels_font_size <- 8
  if(!exists('bar_labels_font_colour',where=params)) params$bar_labels_font_colour <- 'black'
  if(!exists('bar_labels_angle',where=params)) params$bar_labels_angle <- 0
  if(!exists('bar_labels_percent',where=params)) params$bar_labels_percent <- FALSE
  if(!exists('case_boxes',where=params)) params$case_boxes <- FALSE
  if(!exists('case_boxes_colour',where=params)) params$case_boxes_colour <- "white"
  if(!exists('axis_flip',where=params)) params$axis_flip <- FALSE
  if(!exists('ci_legend',where=params)) params$ci_legend <- TRUE
  if(!exists('ci_legend_title',where=params)) params$ci_legend_title <- "Confidence interval"
  if(!exists('ci_colours',where=params)) params$ci_colours <- "red"
  if(!exists('errorbar_width',where=params)) params$errorbar_width <- NULL
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
  if(!exists('y_percent',where=params)) params$y_percent <- FALSE
  # The following are not input parameters for col_chart, but are needed
  #   for base_gg and base_plotly so set defaults
  params$y_sec_axis <- FALSE
  params$y_sec_axis_no_shift <- TRUE
  params$y_sec_axis_percent_full <- FALSE


### DEV: IMPLEMENT
  # 'base' not a user define arguement for col_chart, so set to NULL
  #   for base_gg() and base_plotly()
  base <- NULL

  # Check that the data frame provided is not empty, else stop
  assertthat::assert_that(not_empty(params$df))

  # Assign variables from params list for easier referencing
  df <- params$df
  x <- params$x
  y <- params$y
  group_var <- params$group_var
  fill <- params$fill
  y_axis <- params$y_axis
  position <- params$position
  ci <- params$ci
  lower <- params$lower
  upper <- params$upper
  error_colour <- params$error_colour
  h_line <- params$h_line
  y_label <- params$y_label
  x_label <- params$x_label
  x_label_angle <- params$x_label_angle
  y_label_angle <- params$y_label_angle
  x_labels_reverse <- params$x_labels_reverse
  y_min_limit <- params$y_min_limit
  y_max_limit <- params$y_max_limit
  x_axis_breaks <- params$x_axis_breaks
  legend_pos <- params$legend_pos
  remove_gridlines <- params$remove_gridlines
  percent <- params$percent
  cap_text <- params$cap_text
  no_shift <- params$no_shift



  ##### Checks and warnings

  # Check if df is is.null
  if (!exists('df',where=params)) stop("df = NULL; a data frame argument is required")

  # Check df is a df class
  if(!is.data.frame(params$df)) stop("df is not a data frame object")

  # Check df is empty
  if(!assertthat::not_empty(params$df)) stop("df is empty")

  # Check if x argument is null
  if ((is.null(params$x)) | !exists('x',where=params))
    stop("Please include a variable from df for x, i.e. x = \"variable_name\"")

  # Check if x is in df
  if (!params$x %in% colnames(params$df))
    stop("x not found within df. Please include a variable from df for x, i.e. x = \"variable_name\"")

  # Check if y argument is null (allowed if x_time_series = TRUE)
  if (params$x_time_series == FALSE) {
    if ((is.null(params$y)) | !exists('y',where=params))
      stop("Please include a variable from df for y, i.e. y = \"variable_name\"")
  } #else if (params$x_time_series == TRUE) {
  #   if ((is.null(params$y)) | !exists('y',where=params))
  #     warning("x_time_series: No field stated for y, aggregating df assuming each line equals 1 observation.")
  # }

  # Check if y is in df (may be omitted if x_time_series = TRUE)
  if (params$x_time_series == FALSE) {
    if (!params$y %in% colnames(params$df))
      stop("y not found within df. Please include a variable from df for y, i.e. y = \"variable_name\"")
  }

  # Check time_period valid
  if (!(params$time_period %in% c("day","year","month","quarter","year_month","year_quarter",
                                  "iso_year","iso_week","start_iso_year_week","iso_year_week",
                                  "use_date_var"))) {
    stop("time_period must equal 'day', 'year', 'month', 'quarter', 'year_month', 'year_quarter',
              'iso_year', 'iso_week', 'start_iso_year_week', 'iso_year_week', or 'use_date_var'")
  }

  # Check group_var_barmode valid
  if (!is.null(params$group_var) & !(params$group_var_barmode %in% c('stack', 'group'))) {
    stop("group_var_barmode must equal 'stack' or 'group'")
  }

  # Check bar_labels_pos valid
  if (!(params$bar_labels_pos %in% c('bar_above','bar_base','bar_centre','above_errorbar'))) {
    stop("bar_labels_pos must equal 'bar_above','bar_base','bar_centre', or 'above_errorbar'")
  }

  # Check time_period valid
  if (!(params$time_period %in% c("day","year","month","quarter","year_month","year_quarter",
                                  "iso_year","iso_week","start_iso_year_week","iso_year_week"))) {
    stop("time_period must equal 'day', 'year', 'month', 'quarter', 'year_month', 'year_quarter',
              'iso_year', 'iso_week', 'start_iso_year_week', or 'iso_year_week'")
  }

  # If provided, check if bar_labels is in df
  if ("bar_labels" %in% names(params)) {
    if (!params$bar_labels %in% colnames(params$df))
      stop("bar_labels not found within df. Please include a variable from df for bar_labels, i.e. bar_labels = \"variable_name\"")
  }

  # If provided, check if group_var is in df
  if ("group_var" %in% names(params)) {
    if (!params$group_var %in% colnames(params$df))
      stop("group_var not found within df. Please include a variable from df for group_var, i.e. group_var = \"variable_name\"")
  }

  # Check if number of groups and number of fill colours are the same
  if (exists('group_var', where=params) & length(params$fill_colours) > 1) {
    if (length(params$fill_colours) != length(unique(params$df[[params$group_var]])))
      stop("The number of fill_colours provided must equal the number of unique groups in group_var")
  }

  # If ci = 'errorbar' and ci_colours has multiple values, check if number of groups and number of ci_colours are the same
  if (exists('group_var', where=params) & length(params$ci_colours) > 1) {
    if (length(params$ci_colours) != length(unique(params$df[[params$group_var]])))
      stop("If providing multiple values for ci_colours, the number of ci_colours must equal
           the number of unique groups in group_var")
  }

  # Check bar_labels_pos valid
  if ((params$bar_labels_pos == 'above_errorbar') & (!exists('ci', where=params))) {
    stop("If bar_labels_pos = 'above_errorbar' then ci must equal 'errorbar'")
  }

  # Warn that multiple colours have been provided but group_var absent
  if (length(params$fill_colours) >1 & !exists('group_var',where=params))
    warning("Multiple fill_colours have been provided but group_var is absent")

  # Warn that ci = 'ribbon' is not accepted for col_chart()
  if (exists('ci',where=params)) {
    if (params$ci == 'ribbon')
      warning("ci = \"ribbon\" is not accepted for col_chart()")
  }

  # Warn that ci_upper or ci_lower has been provided but ci has not.
  if (!exists('ci',where=params)) {
    if (exists('ci_lower',where=params) | exists('ci_upper',where=params))
      warning("ci_lower / ci_upper have been provided but ci = NULL")
  }





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
                 "x_time_series",
                 "x_time_series_bar_labels",
                 "time_period",
                 "group_var",
                 "group_var_barmode",
                 "fill_colours",
                 "bar_border_colour",
                 "bar_labels",
                 "bar_labels_pos",
                 "bar_labels_font_size",
                 "bar_labels_font_colour",
                 "bar_labels_angle",
                 "bar_labels_percent",
                 "case_boxes",
                 "case_boxes_colour",
                 "axis_flip",
                 "ci",
                 "ci_legend",
                 "ci_legend_title",
                 "ci_lower",
                 "ci_upper",
                 "ci_colours",
                 "errorbar_width",
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
                 "y_percent",
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

  ### Define full date range for time series if x_time_series = TRUE

  if(x_time_series == TRUE) {

    # Check that x is a date
    if (!is.Date(df[[x]])) {
      stop("If x_time_series = TRUE then x must be a date.")
    }

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

    # Use utils/adorn_dates() function to add additional date variables to df
    df <- adorn_dates(df, x)

    # Redefine x so that it points towards the relevant date column
    x <- time_period



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
        # Calculate raw totals by default, but percentages by group if y_percent = TRUE
        if(y_percent == FALSE) {
          df <- df |>
            group_by(date_factor, .data[[group_var]]) |>
            summarise(n = n()) |>
            ungroup()
        } else {
          df <- df |>
            group_by(date_factor, .data[[group_var]]) |>
            summarise(n = n()) |>
            ungroup() |>
            mutate(date_sum = sum(n), .by=date_factor,
                   n = n / date_sum)
        }

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
        # Calculate raw totals by default, but percentages by group if y_percent = TRUE
        if(y_percent == FALSE) {
          df <- df |>
            group_by(date_factor, .data[[group_var]]) |>
            summarise(n = sum(.data[[y]])) |>
            ungroup()
        } else {
          df <- df |>
            group_by(date_factor, .data[[group_var]]) |>
            summarise(n = sum(.data[[y]])) |>
            ungroup() |>
            mutate(date_sum = sum(n), .by=date_factor,
                   n = n / date_sum)
        }

      }

    }

    # Re-define y so that it points towards n
    y <- "n"

    # Define bar labels for time series
    if (x_time_series_bar_labels == TRUE) {bar_labels <- "n"} else {bar_labels <- NULL}


  }



  ##### Handle categorical x-axis not stored as factor
  if(is.character(df[[x]])) {
    df[[x]] <- as.factor(df[[x]])
    # Reverse level order for dynamic charts if x_axis_reverse == TRUE so that axis labels are printed in reversed order
    if (x_axis_reverse == TRUE & dynamic == TRUE) {
      df[[x]] <- forcats::fct_rev(df[[x]])
    }
  }
 ### DEV: Apply this to base_gg to generalise to all functions?



  ##### Expand ci_colours if group_var is provided but only 1 colour is provided
  if(!is.null(ci) & !is.null(group_var)) {
    if(length(ci_colours) == 1) {
      ci_colours <- rep(ci_colours, each = length(unique(df[[group_var]])))
    }
  }


  ##### Apply bar_label_percent = TRUE
  if(bar_labels_percent == TRUE | (y_percent == TRUE & x_time_series_bar_labels == TRUE)) {
    df <- df |> mutate(bar_labels_perc = scales::percent(.data[[bar_labels]], accuracy=1))
    bar_labels <- "bar_labels_perc"
  }



  #################### COL CHART #################################

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
    xlim <- base_return$xlim
    ylim <- base_return$ylim



    # Supress spurious 'Scale for X is already present' messages
    #   -Will suppress other messages
    suppressMessages({



      ##### Build the column chart


      # Alter bar border colour to match case boxes border colour if case_boxes = TRUE
      if(case_boxes == TRUE) {bar_border_colour <- case_boxes_colour}


      # Build according to whether plotting variables are grouped or not
      if(is.null(group_var)) {

        # Create column chart without groups

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

        # Create column chart with groups

        # Rename group_var_barmode for ggplot and generate group_var_barpos variable
        if(group_var_barmode == "group") {
          group_var_barmode <- "dodge"
          group_var_barpos <- position_dodge(preserve = 'single')  # ensures that bar widths are not stretched to fill when certain groups are empty
          df <- df |> complete(.data[[x]], .data[[group_var]])     # Fill in blank groups to make sure they're plotted as empty bars
        }

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
            position = if(group_var_barmode == 'dodge') {group_var_barpos} else {'stack'},
            na.rm = if(group_var_barmode == 'dodge') {TRUE} else {FALSE}
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

        # Redefine df with one row per case to draw stacked bar chart where each row
        #    is one of the stacking elements 1-unit high.Uncount df to get 1 row per
        #    case, add box = 1 column for y-vals to create 1 box per case.
        df_box <- df |>
                 mutate(across(all_of(y), .fns = ~replace_na(.,0))) |>
                 uncount(get(y)) |>
                 mutate(box = 1)

        # When bars are dodged rather than stacked, slice by max value of y in
        #    each group else y-axis assumes the stacked value and over-scales accordingly
        if(group_var_barmode == 'dodge') {
          df_box <- df_box |>
                     slice_max(order_by = get(y), by = any_of(x)) |>
                     slice_max(order_by = get(group_var), by = any_of(x))   # second slice resolves draws if 2 groups have equal values
        }

        # Add transparent stacked bar plot with external borders to create
        #   boxes around each case.
        base <-
          base + geom_bar(
            data = df_box,
            mapping = aes(x = .data[[x]],
                          y = box
            ),
            fill = "transparent",
            stat = 'identity',
            #position = if(group_var_barmode == 'dodge') {group_var_barpos} else {'stack'},
            color = case_boxes_colour,
            linewidth = 0.5
          )

      }


      ##### Apply confidence intervals

      # Apply after col chart so that errorbars appear in front of columns.

      # Add conf intervals if arguments for ci and ci_upper+ci_lower bounds are provided.
      if(!is.null(ci)) {

        if(!is.null(ci_lower) && !is.null(ci_upper)) {

          # Add error bars

          # If errorbar_width not provided; define default based on x-axis limits
          if (is.null(errorbar_width)) {
            if (is.null(base$coordinates$limits$x)) {
              errorbar_width <- as.numeric((max(df[[x]]) - min(df[[x]])) / 100)
            } else {
              errorbar_width <- as.numeric((base$coordinates$limits$x[[2]] - base$coordinates$limits$x[[1]]) / 100)
            }
          }

          # Account for geom show.legend parameter accepting values of 'NA' or 'FALSE'
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

            }

          # Plot for group_var provided
          } else if (!is.null(group_var)) {

            # Add error bars with grouping variable
            if(ci == 'errorbar') {

              # Generate offset position for errorbars when group_var_barmode = "dodge"
              errorbar_offset <- position_dodge(resolution(as.numeric(df[[x]]))*0.9)

              # geom_errorbar does not support stacked bar charts (https://github.com/tidyverse/ggplot2/issues/1079)
              #   Positions of errorbars for stacked plots must be calculated manually, create new
              #   dataframe to manage this.
              df_errbar <- df |>
                group_by(.data[[x]]) |>
                arrange(.data[[x]], desc(.data[[group_var]])) |>
                mutate(cumul = cumsum(.data[[y]]),
                       lower_lim_stacked = cumul - (.data[[y]] - .data[[ci_lower]]),
                       upper_lim_stacked = cumul + (.data[[ci_upper]] - .data[[y]]))


              base <-
                base + ggplot2::geom_errorbar(
                  data = df_errbar,
                  mapping = aes(
                    x = .data[[x]],
                    ymin = if(group_var_barmode != "stack") {.data[[ci_lower]]} else {lower_lim_stacked},
                    ymax = if(group_var_barmode != "stack") {.data[[ci_upper]]} else {upper_lim_stacked},
                    group = .data[[group_var]],
                    colour =  .data[[group_var]]
                  ),
                  width = errorbar_width,
                  linewidth = .5,
                  position = if(group_var_barmode == "dodge") {errorbar_offset} else {"identity"},
                  na.rm = if(group_var_barmode == "dodge") {TRUE} else {FALSE}
                  #position = position_dodge(resolution(as.numeric(df[[x]]))*0.9)
                )

              # Add ci_colours if provided
              if (length(ci_colours) > 1) {
                base <- base +
                  scale_colour_manual(values = ci_colours)
              }

            }

          # Stop if ci_upper and/or ci_lower limit isn't provided
        } else {
          stop("Please provide arguements for 'ci_upper' and 'ci_lower' when ci is specified.")
        }

      }
    }



      ##### Add bar labels
      if(!is.null(bar_labels)) {


        # Reverse label angle direction to match plotly (i.e. make parameter tilt labels clockwise)
        bar_labels_angle <- -bar_labels_angle

        # Define ynudge for non-right-angles to match with plotly
        ylength <- if (!is.na(ylim[2])) {ylim[2]} else {ggplot_build(base)$layout$panel_params[[1]]$y.range[2]}
        ynudge <- if(abs(bar_labels_angle) %in% c(0,90,180,270,360)) {0} else {ylength * 0.02} # 2% of y-axis length, matches vjust distance
        #ynudge <- ylength * 0.02

        # Define vjust and hjust
        #   - Keep as 0.5/0.5 (i.e. centre/centre) except when using a right angle, then other
        #     alignments are optimal.
        if(-bar_labels_angle %in% c(0,360)) {
          v <- -0.5
          h <- 0.5
        } else if (-bar_labels_angle == 90) {
          v <- 0.5
          h <- 1.3
        } else if (-bar_labels_angle == 180) {
          v <- 1.5
          h <- 0.5
        } else if (-bar_labels_angle == 270) {
          v <- 0.5
          #h <- -0.3
          h <- 0
        } else {
          v <- 0.5
          h <- 0.5
        }

        # Redefine for flipped axes
        if (axis_flip == TRUE) {
          v <- 0.5
          h <- 0.5
        }

        # Plot labels for no group_var
        if (is.null(group_var)) {

          # Define plot label positions depending on bar_labels_pos choice
          if(bar_labels_pos == 'bar_above') {
            x_labpos <- df[[x]]
            y_labpos <- df[[y]]
          } else if (bar_labels_pos == 'bar_base') {
            x_labpos <- df[[x]]
            y_labpos <- if (is.na(ylim[1])) {0} else {ylim[1]}  # position at bottom of y-range
            ynudge <- if(bar_labels_angle %in% c(90,270)) {ynudge} else {ynudge + (0.02 * ylength)} # adjust ynudge for 90/270 rotations to keep congruent with plotly output
          } else if (bar_labels_pos == 'bar_centre') {
            x_labpos <- df[[x]]
            y_labpos <- df[[y]] / 2
              # Redfine ynudge, vjust, and hjust to centre text for bar_centre
              ynudge <- 0
              v <- 0.5
              h <- 0.5
          } else if (bar_labels_pos == 'above_errorbar') {
            x_labpos <- df[[x]]
            y_labpos <- df[[ci_upper]]
          }


          base <- base + geom_text(
                          data = df,
                          aes(x = x_labpos,
                              y = y_labpos,
                              vjust = v,
                              hjust = h,
                              label = .data[[bar_labels]]),
                          colour = bar_labels_font_colour,
                          size = bar_labels_font_size * (5/14), # apply scaling ratio to font size
                          angle = bar_labels_angle,
                          nudge_y = ynudge
                          )


        # Plot for group_var provided
        } else if (!is.null(group_var)) {

          # Reset vjust and hjust so that labels pivot about centre point when label angle is adjusted
          v <- 0.5
          h <- 0.5

          # Calculate positions for stacked labels manually, create new dataframe to manage.
          df_labels <- df |>
            group_by(.data[[x]]) |>
            arrange(.data[[x]], desc(.data[[group_var]])) |>
            mutate(cumul = cumsum(.data[[y]]),                        # cumulative position of top of each bar
                   cumul_bar_base = cumul - .data[[y]],               # cumulative position of bottom of each bar
                   cumul_bar_centre = cumul - (.data[[y]]/2))         # cumulative position of centre of each bar

          # Calculate positions for above_errorbar; needs ci_upper defined
          if(!is.null(ci_upper)) {
            df_labels <- df_labels |>
              mutate(ci_upper_diff = .data[[ci_upper]] - .data[[y]],    # define upper-ci-limit to y-value difference to use in above_errorbar label pos
                     cumul_above_errorbar = cumul + ci_upper_diff)      # cumulative position above each errorbar
          }

          # Define plot label positions depending on bar_labels_pos choice
          if(bar_labels_pos == 'bar_above') {
            x_labpos <- df_labels[[x]]
            y_labpos <- if(group_var_barmode == "stack") {df_labels$cumul} else {df_labels[[y]]}
          } else if (bar_labels_pos == 'bar_base') {
            x_labpos <- df_labels[[x]]
            y_labpos <- if(group_var_barmode == "stack") {df_labels$cumul_bar_base} else {df_labels[[y]]} # position at bottom of each stacked bar
              # Redefine y_labpos to bottom of bar for grouped bars
              y_base <- if (is.na(ylim[1])) {0} else {ylim[1]}
              y_labpos <- if(group_var_barmode == "dodge") {y_base} else {y_labpos}
          } else if (bar_labels_pos == 'bar_centre') {
            x_labpos <- df_labels[[x]]
            y_labpos <- if(group_var_barmode != "stack") {df_labels[[y]]} else {df_labels$cumul_bar_centre}  # position in centre of each stacked bar
              # Redefine y_labpos to halfway up bar for grouped bars
              y_labpos <- if(group_var_barmode == "dodge") {y_labpos / 2} else {y_labpos}
            ynudge <- 0 # reset ynudge to zero so labels pivot about centre point
          } else if (bar_labels_pos == 'above_errorbar') {
            x_labpos <- df_labels[[x]]
            y_labpos <- df_labels$cumul_above_errorbar
              # Redefine y_labpos to halfway up bar for grouped bars
              y_labpos <- if(group_var_barmode == "dodge") {df_labels[[ci_upper]]} else {y_labpos}
          }

          # Generate offset position for labels when group_var_barmode = "dodge"
          label_offset <- position_dodge(resolution(as.numeric(df[[x]]))*0.9)

          base <-
            base + geom_text(
              data = df_labels,
              aes(
                x = x_labpos,
                y = y_labpos + ynudge,
                vjust = v,
                hjust = h,
                label = .data[[bar_labels]],
                group = .data[[group_var]]
              ),
              colour = bar_labels_font_colour,
              size = bar_labels_font_size * (5/14), # apply scaling ratio to font size
              angle = bar_labels_angle,
              #nudge_y = ynudge,
              position = if(group_var_barmode == "dodge") {label_offset} else {"identity"},
              na.rm = if(group_var_barmode == "dodge") {TRUE} else {FALSE}
            )

        }

    }



      ##### Flip axes if axis_flip = true
      if(axis_flip == TRUE) {
        base <- base + coord_flip(xlim = xlim, ylim = ylim, expand = FALSE)
      }


      ##### Redefine elements of base_gg specific to col_chart()

      # Redefine x-axis label so that default = x
      if (!is.null(x_axis_title)) {
        base <- base + labs(x = x_axis_title)
      } else {
        base <- base + labs(x = x) # default to x if label not provided
      }


      # Redefine y-axis scale to remove gap between bars and x-axis
      if (!is.null(y_axis_break_labels)) {
        base <- base + scale_y_continuous(breaks = y_axis_break_labels,
                                          labels = if (y_percent == TRUE) {scales::label_percent()} else {waiver()},
                                          expand = c(0,0))
      } else {
        base <- base + scale_y_continuous(labels = if (y_percent == TRUE) {scales::label_percent()} else {waiver()},
                                          expand = c(0,0))
      }

      # if (y_percent == TRUE) {
      #   base <-
      #     base + scale_y_continuous(labels = scales::label_percent(),
      #                               expand = c(0,0))
      # }



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

        # Plot hline label(s)
        base <- base +
          geom_text(
            aes(
              x = hline_xpos,
              y = hline,
              label = hline_label,
              vjust = -0.5,
              hjust = 0,
              angle = if (axis_flip == FALSE) {0} else {270}
            ),
            #position = position_nudge(y = if (axis_flip == FALSE) {0} else {0.005 * ggplot_build(base)$layout$panel_params[[1]]$x.range[2]}), # nudge label by 0.5% of axis so that it's not sitting in contact with hline when aex are flipped
            colour = hline_label_colour)

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
    #    and use it's autoranges as a basis. This also keeps the formatting the
    #    same as the static chart.
    #   -Must be done outside of base_plotly(), as the ggplot geoms will be
    #      different depending upon the nature of the chart

    # Define ggplot object to harvest axis ranges from
    # if(is.null(group_var)) {
    #   ggobj <- ggplot() +
    #     geom_bar(data = df, aes(x = .data[[x]],y = .data[[y]]), stat = 'identity') +
    #     geom_hline(yintercept = hline)
    # } else {
    #   if(group_var_barmode == "group") {group_var_barmode <- "dodge"}
    #   ggobj <- ggplot() +
    #     geom_bar(data = df, aes(x=.data[[x]],y=.data[[y]],group=.data[[group_var]]),
    #              stat = 'identity',position = group_var_barmode) +
    #     geom_hline(yintercept = hline)
    # }
    # if(axis_flip == TRUE) {ggobj <- ggobj + coord_flip()} else {ggobj}

    # Harvest ggplot object from static version of chart using col_chart() itself
    ggobj <- col_chart(params = params, dynamic = FALSE)
    # Harvest axis limits from ggobj so that plotly output matches ggplot output
    x_min <- ggplot_build(ggobj)$layout$panel_params[[1]]$x.range[1]
    x_max <- ggplot_build(ggobj)$layout$panel_params[[1]]$x.range[2]
    if(axis_flip == FALSE) {y_min <- 0} else {y_min <- ggplot_build(ggobj)$layout$panel_params[[1]]$y.range[1]}
    y_max <- ggplot_build(ggobj)$layout$panel_params[[1]]$y.range[2]
    # Harvest ggplot x-axis resolution to use as offset for dodged errorbars, and bar labels
    errorbar_offset <- resolution(as.numeric(df[[x]]))*0.9


    # Handle numeric-class dates being converted to -ve numbers in ggobj when x_axis_reverse = TRUE
    if(x_axis_reverse == TRUE & is.Date(df[[x]]) & is.numeric(x_max)) {
      x_min <- abs(x_min)
      x_max <- abs(x_max)
    }
    # Handle same scenario when axes are flipped, as then y-axis from ggobj will contain x values / limits
    if (axis_flip == TRUE & x_axis_reverse == TRUE & is.Date(df[[x]]) & is.numeric(y_max)) {
      y_min <- abs(y_min)
      y_max <- abs(y_max)
    }


    # # Handle dates converting to numeric when extracted from ggplot axis range
    # x_min <- if(lubridate::is.Date(df[[x]])) {as.Date.numeric(x_min)} else {x_min}
    # x_max <- if(lubridate::is.Date(df[[x]])) {as.Date.numeric(x_max)} else {x_max}
    # y_min <- if(lubridate::is.Date(df[[y]])) {as.Date.numeric(y_min)} else {y_min}
    # y_max <- if(lubridate::is.Date(df[[y]])) {as.Date.numeric(y_max)} else {y_max}


    ##### To flip axis in plotly, all x and y variables must be exchanged
    if(axis_flip == TRUE) {
      # Use utils/swap_object_names() function to swap all the x/y variable names
      swap_object_names('x', 'y')
      swap_object_names('x_axis_title', 'y_axis_title')
      swap_object_names('x_axis_title_font_size', 'y_axis_title_font_size')
      #swap_object_names('x_axis_label_angle', 'y_axis_label_angle')
      swap_object_names('x_axis_label_font_size', 'y_axis_label_font_size')
      swap_object_names('x_limit_min', 'y_limit_min')
      swap_object_names('x_limit_max', 'y_limit_max')
      swap_object_names('x_axis_break_labels', 'y_axis_break_labels')
    }


    # # If axes are flipped and x_axis_reverse = TRUE, then y-axis values need to be reversed
    # if(axis_flip == TRUE & x_axis_reverse == TRUE) {
    #   swap_object_names('y_limit_min', 'y_limit_max')
    # }



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

      # Reverse the colour levels if axis is reversed and bars are grouped / dodged (axis reversal changes the colour order if not)
      if (x_axis_reverse == TRUE & group_var_barmode == 'group') {df <- df |> mutate(fill_colour = forcats::fct_rev(fill_colour))}

    }

    # Define colour_field parameter for bar plot
    #    Note:- Stacked bar colours need reversing to match with ggplot output, as
    #           do grouped bars when x-axis is reversed
    if (is.factor(df$fill_colour) & group_var_barmode == "stack") {
      colour_field <- forcats::fct_rev(df$fill_colour)
    } else if (x_axis_reverse == TRUE & group_var_barmode == "group") {
      colour_field <- forcats::fct_rev(df$fill_colour)
    } else {
      colour_field <- df$fill_colour
    }




    ##### Define hover labels

    # Define defaults if user does not define hover_labels
    if (is.null(hover_labels)) {

      # Define title / total to display depending upon whether case boxes are enabled and axes are flipped
      if (axis_flip == FALSE) {
        hover_title <- '{x}'
        hover_n <- if (case_boxes == FALSE) {'{y}'} else {'{customdata}'}
      } else {
        hover_title <- '{y}'
        hover_n <- if (case_boxes == FALSE) {'{x}'} else {'{customdata}'}
      }

      # Define hover label text
      hoverlabels <- paste0('<b>%',hover_title,'</b>',
                            '<br>%',hover_n)

      # Add upper and lower limits to label if ci is defined
      if (!is.null(ci)) {
        hoverlabels <- paste0(hoverlabels,'%{text}')  # leverage 'text' parameter in add_trace to pass additional info to hoverlabels
      }

      # Remove tooltip for ungrouped data
      if (is.null(group_var)) {
        hoverlabels <- paste0(hoverlabels, '<extra></extra>')
      }

    } else {

      # Hover labels specified by user input parameter hover_labels
      if (case_boxes == FALSE) {
        hoverlabels <- hover_labels
      } else {
        hoverlabels <- gsub('\\{y\\}','{customdata}',hover_labels)
      }

    }


  # Remove old theme settings that are now handled above

    ##### Build col_chart

    # Convert x-axis to numeric factor if using grouped bars and x is a categorical variable
    #    -Necessary as we need to add a separate trace for the errorbars which need their
    #     positions manually calculated for grouped bar charts. These positions are numeric
    #     and thus can't be plotted over a categorical axis. Thus convert x to a numeric factor
    #     so that the x-axis is numeric, and preserve the levels to use as axis labels.
    if (axis_flip == TRUE) {swap_object_names('x', 'y')} # temp swap names back for calc

      # Reverse x levels if x_time_series == TRUE & x_axis_reverse == TRUE else they get double reversed
      if (x_time_series == TRUE & x_axis_reverse == TRUE) {df[[x]] <- forcats::fct_rev(df[[x]])}

      # Adjust grouping parameters
      if (group_var_barmode == 'group' & is.factor(df[[x]]) & !is.null(group_var)) {
        x_levels <- levels(df[[x]]) # preserve levels to use as axis ticks
          if(x_axis_reverse == TRUE) {x_levels <- rev(x_levels)} # reverse levels if x-axis reversed
        df <- df |> mutate(x_orig = get(x)) # preserve original x column in df
        df[[x]] <- as.numeric(factor(df[[x]]))-1  # -1 to shunt bars down 1 position to start at 0
      }
    if (axis_flip == TRUE) {swap_object_names('x', 'y')} # swap back



    # Add bar plot without boxes around each case
    if (case_boxes == FALSE) {

      base <- base |>
        add_trace(
          df,
          x = ~ df[[x]],
          y = ~ df[[y]],
          # x = ~ if(axis_flip == FALSE) {df[[x]]} else {df[[y]]},
          # y = ~ if(axis_flip == FALSE) {df[[y]]} else {df[[x]]},
          type = 'bar',
          color = ~ colour_field, #df$fill_colour,
          colors = colormap,
          marker = list(
            #color = fill_colours[1]
            line = list(color = bar_border_colour,
                        width = 0.5)
          ),
          text = if(is.null(ci)) {''} else {paste0('<br><i>Upper: ',df[[ci_upper]],'</i>',   # leverage 'text' parameter in add_trace to pass additional info to hoverlabels
                                                   '<br><i>Lower: ',df[[ci_lower]],'</i>')},
          textposition = "none", # prevents text from being printed on plot
          hovertemplate = hoverlabels,
          #legendgroup = 'bars',
          orientation = if(axis_flip == TRUE) {'h'} else {'v'}, #set orientation to horizontal if axis_flip = TRUE
          showlegend = if (is.null(group_var)) {F} else {T}
        ) |>
        layout(barmode = group_var_barmode)


      # Add bar plot with boxes around each case
    } else if (case_boxes == TRUE) {

      # Temporarily swap back axis names for uncount()
      if(axis_flip == TRUE) {swap_object_names('x', 'y')}

      # Uncount data to get one row per case for one box per case.
      df_case_boxes <- df |> uncount(get(y)) |> mutate(box = 1)

        # Swap them back
        if(axis_flip == TRUE) {swap_object_names('x', 'y')}

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
          x = ~ if(axis_flip == FALSE) {df_case_boxes[[x]]} else {df_case_boxes$box},
          y = ~ if(axis_flip == FALSE) {df_case_boxes$box} else {df_case_boxes[[y]]},
          type = 'bar',
          color = ~ colour_field, #df$fill_colour,
          colors = colormap,
          marker = list(
            line = list(color = case_boxes_colour,
                        width = 0.5)
          ),
          text = if(is.null(ci)) {''} else {paste0('<br><i>Upper: ',df_case_boxes[[ci_upper]],'</i>',   # leverage 'text' parameter in add_trace to pass additional info to hoverlabels
                                                   '<br><i>Lower: ',df_case_boxes[[ci_lower]],'</i>')},
          textposition = "none", # prevents text from being printed on plot
          customdata = if(axis_flip == FALSE) {df_case_boxes[[y]]} else {df_case_boxes[[x]]}, # for hoverlabels
          hovertemplate = hoverlabels,
          #legendgroup = 'bars',
          orientation = if(axis_flip == TRUE) {'h'} else {'v'}, #set orientation to horizontal if axis_flip = TRUE
          showlegend = if (is.null(group_var)) {F} else {T}
        ) |>
        layout(barmode = group_var_barmode)

    }


    # Manually re-add labels for categorical x-values converted to numeric factors
    if(exists('x_levels')) {
      if(axis_flip == FALSE) {
        base <- base |>
          layout(xaxis = list(
            tickvals = unique(df[[x]]),
            ticktext = x_levels))
      } else {
        base <- base |>
          layout(yaxis = list(
            tickvals = unique(df[[y]]),
            ticktext = x_levels))
      }

    }



    ##### Apply confidence intervals

    # Apply after main bar-plot so that errorbars appear in front of columns.

    # Add conf intervals if arguments for ci and ci_upper+ci_lower bounds are provided.
    if(!is.null(ci)) {

      # Stop if ci_upper and/or ci_lower limit isn't provided
      if(is.null(ci_lower) | is.null(ci_upper)) {
        stop("Please provide arguements for 'ci_upper' and 'ci_lower' when ci is specified.")
      }


      # Plot for no group_var
      if (is.null(group_var)) {

        # Add error bars without grouping variable
        if(ci == 'errorbar') {

          # Ensure that there's only a single colour in the input vector
          ci_single_colour <- ci_colours[1]

          # Plotly error bars require upper and lower error divergence rather
          #   than values, so calculate
          if(axis_flip == TRUE) {swap_object_names('x', 'y')} # temporarily swap back axis names for calculation if axes are flipped

          df <- df |>
            mutate(diff_ci_lower = get(y) - get(ci_lower),
                   diff_ci_upper = get(ci_upper) - get(y))

          if(axis_flip == TRUE) {swap_object_names('x', 'y')}


          # Add error bars as trace
          base <- base |>
            add_trace(
              data = df,
              x = ~ df[[x]],
              y = ~ df[[y]],
              type = 'scatter',
              mode = 'markers',
              yaxis = y_axis_choice,
              name = ci_legend_title,
              showlegend = ci_legend,
              legendgroup = 'ci',
              marker = list(
                color = ci_single_colour,
                line = list(colour = '#ffffff00', width = 0),
                opacity = 0,
                symbol = 'line-ew-open' # use hozizontal line markers so that horizontal line symbol will be used in legend to match ggplot legend formatting
              ),
              hoverinfo='none',
              error_y = list(   # normal vertical error bars, not visible if axes are flipped
                type = "data",
                symmetric = FALSE,
                color = ci_single_colour,
                thickness = 1,
                arrayminus = ~ diff_ci_lower,
                array = ~ diff_ci_upper,
                visible = if (axis_flip == TRUE) {F} else {T}
              ),
              error_x = list(   # horizontal error bars, only visible if axes are flipped
                type = "data",
                symmetric = FALSE,
                color = ci_single_colour,
                thickness = 1,
                arrayminus = ~ diff_ci_lower,
                array = ~ diff_ci_upper,
                visible = if (axis_flip == TRUE) {T} else {F}
              )
            )
        }



        # Plot for group_var provided
      } else if (!is.null(group_var)) {

        # Add error bars with grouping variable
        if(ci == 'errorbar') {

          # Errorbar traces must be defined individually for each group

          # Define unique groups
          unique_groups <- unique(df[[group_var]])
              # # Reverse order when x-axis is reversed
              # if(x_axis_reverse == TRUE & group_var_barmode == 'group') {unique_groups <- fct_rev(unique_groups)}

          if(axis_flip == TRUE) {swap_object_names('x', 'y')} # temporarily swap back axis names for calculation if axes are flipped

          # y-axis positions of errorbars for stacked plots must be calculated
          #   manually, create new dataframe to manage this.
          df_errbar <- df |>
            group_by(.data[[x]]) |>
            arrange(.data[[x]], desc(.data[[group_var]])) |>
            mutate(cumul = cumsum(.data[[y]]),
                   lower_lim_stacked = cumul - (.data[[y]] - .data[[ci_lower]]),
                   upper_lim_stacked = cumul + (.data[[ci_upper]] - .data[[y]])) |>
            ungroup()


          # x-axis position of errorbars for grouped/dodged plots must be manually offset;
          #    offset is dependent on number of unique groups and x-axis resolution (calculated
          #    from ggobj above = errorbar_offset)
          offset_multiplier <- seq(length(unique_groups)) - ceiling(length(unique_groups)/2) # generate vector of integars to multiply the base offset by
          if(length(unique_groups) %%2 != 0) {offset_multiplier - 0.5}                       # if the number of groups is odd, offset the multiplier by 0.5 as one of the bars will be in the middle (for even numbers, the middle will be between the 2 central bars)
          # Character x-axis offset needs to be treated differently to numeric offset
          if(is.factor(df_errbar[[x]]) | is.character(df_errbar[[x]])) {
            offset_multiplier <- offset_multiplier * 0.8                                     # multiply this offset by 0.8 to match the ggplot defaults
            x_offset <- offset_multiplier * (1/length(unique_groups))                        # divide multiplier by the number of groups and multiply this by the multiplier to get the actual x-values of each bar (recycle errorbar_offset variable as it's doing the same job here but for label positions)
          } else {
            offset_multiplier <- offset_multiplier * 0.9                                     # multiply this offset by 0.9 to match the ggplot defaults
            x_offset <- offset_multiplier * (errorbar_offset/length(unique_groups))          # divide the x-axis resolution by the number of groups and multiply this by the multiplier to get the actual x-values of each bar (recycle errorbar_offset variable as it's doing the same job here but for label positions)
          }
          if(is.Date(df_errbar[[x]]) | is.POSIXct(df_errbar[[x]])) {                         # if x is a date then plotly will shunt the errorbars to the beginning of the nearest whole day (i.e. not in the middle of the barchart bars) so convert axis to time axis if this is the case
            df_errbar[[x]] <- as.POSIXct(df_errbar[[x]])
            x_offset <- x_offset * 24*60*60   # unit value for time is seconds rather than days as for date, so convert
          }

          if(axis_flip == TRUE) {swap_object_names('x', 'y')} # flip axis variables back


          # Iterate over each group
          for (i in 1:length(unique_groups)) {

            if(axis_flip == TRUE) {swap_object_names('x', 'y')} # flip axis variables within each iteration for calculation if needed

            # Plotly error bars require upper and lower error divergence rather
            #   than values, so create df for each group and calculate
            if(group_var_barmode == "group") {

              # For character axis bar position is an integer along the x-axis, calculate these in order to apply offset to
              if(exists('x_levels')) {
                if(x_axis_reverse == FALSE) {x_level_lkp <- x_levels} else {x_level_lkp <- rev(x_levels)} # reverse order for reversed x-axis
                x_bar_order_lookup <- data.frame(x = x_level_lkp) |> mutate(bar_order = row_number()-1)
              }

              df_group <- df_errbar |>
                filter(get(group_var) == unique_groups[i]) |>
                mutate(diff_ci_lower = get(y) - get(ci_lower),
                       diff_ci_upper = get(ci_upper) - get(y))


              # Offset x values for grouped bars
              # Character x-axis offset needs to be treated differently to numeric offset
              if(exists('x_levels')) {
                df_group <- df_group |> left_join(x_bar_order_lookup, join_by('x_orig' == 'x')) |>
                  mutate(x_grouped = as.numeric(bar_order) + x_offset[i])
              } else {
                df_group <- df_group |> mutate(x_grouped = get(x) + x_offset[i])
              }

            } else if(group_var_barmode == "stack") {
              # Use stack values when group_var_barmode == "stack"
              df_group <- df_errbar |>
                filter(get(group_var) == unique_groups[i]) |>
                mutate(diff_ci_lower = cumul - lower_lim_stacked,
                       diff_ci_upper = upper_lim_stacked - cumul)
            }

            if(axis_flip == TRUE) {swap_object_names('x', 'y')}  # flip axis variables back after calculation

            # Add error bars as trace with invisible markers
            base <- base |>
              add_trace(
                data = df_group,
                # Select x and y based on grouped barmode and whether axes are flipped
                #    Note:- There is probably a neater way of coding this.
                # else if(group_var_barmode == "group" & axis_flip == FALSE & x_axis_reverse == TRUE) {rev(df_group$x_grouped)}
                x = if(group_var_barmode == "group" & axis_flip == FALSE) {df_group$x_grouped} else if(group_var_barmode == "stack" & axis_flip == TRUE) {df_group$cumul} else {df_group[[x]]},
                y = if(group_var_barmode == "stack" & axis_flip == FALSE) {df_group$cumul} else if(group_var_barmode == "group" & axis_flip == TRUE) {df_group$x_grouped} else {df_group[[y]]},
                type = 'scatter',
                mode = 'markers',
                yaxis = y_axis_choice,
                name = unique_groups[[i]],
                showlegend = F,
                marker = list(
                  color = '#ffffff00',
                  opacity = 0,
                  line = list(colour = '#ffffff00', width = 0)
                ),
                hoverinfo='none',
                error_y = list(   # normal vertical error bars, not visible if axes are flipped
                  type = "data",
                  symmetric = FALSE,
                  color = ci_colours[[i]],
                  thickness = 1,
                  arrayminus = ~ diff_ci_lower,
                  array = ~ diff_ci_upper,
                  visible = if (axis_flip == TRUE) {F} else {T}
                ),
                error_x = list(   # horizontal error bars, only visible if axes are flipped
                  type = "data",
                  symmetric = FALSE,
                  color = ci_colours[[i]],
                  thickness = 1,
                  arrayminus = ~ diff_ci_lower,
                  array = ~ diff_ci_upper,
                  visible = if (axis_flip == TRUE) {T} else {F}
                )
              )

            # # Manually re-add labels for categorical x-values converted to numeric factors
            # if(exists('x_levels')) {
            #   base <- base |>
            #     layout(xaxis = list(
            #       tickvals = unique(df_errbar[[x]]),
            #       ticktext = x_levels
            #     ))
            # }


          }

        }

      }

    }



    ##### Apply bar labels

    # Apply after main bar-plot and errorbars so that labels appear in front.

    if(!is.null(bar_labels)) {

      # Define ynudge value to make labels appear in same position as in ggplot output
      ynudge <- if (bar_labels_angle %in% c(90,270) | bar_labels_pos == 'bar_centre') {0} else {0.02 * y_max}
      # take into account flipped axes
      if(axis_flip == TRUE) {
        ynudge <- if (bar_labels_pos == 'bar_centre') {0} else {0.02 * x_max}
        }


      # Label according to whether or not bars are grouped

      # Plot for no group_var
      if (is.null(group_var)) {

        # Flip axis variables for calculation if needed
        if(axis_flip == TRUE) {
          swap_object_names('x', 'y')
          swap_object_names('x_min', 'y_min')
          swap_object_names('x_max', 'y_max')
        }

        # Define plot label parameters for bar_labels_pos choice
        if(bar_labels_pos == 'bar_above') {
          x_labpos <- df[[x]]
          y_labpos <- df[[y]]
        } else if (bar_labels_pos == 'bar_base') {
          x_labpos <- df[[x]]
          y_labpos <- if(bar_labels_angle %in% c(90,270)) {y_min + (0.015*y_max)} else {y_min + (1.5*ynudge)}
            # Redefine for flipped axes to eliminate gap
            y_labpos <- if(axis_flip == TRUE) {y_min + (1.5*ynudge)} else {y_labpos}
        } else if (bar_labels_pos == 'bar_centre') {
          x_labpos <- df[[x]]
          y_labpos <- df[[y]] / 2
        } else if (bar_labels_pos == 'above_errorbar') {
          x_labpos <- df[[x]]
          y_labpos <- if(bar_labels_angle %in% c(90,270)) {df[[ci_upper]] + (0.01*y_max)} else {df[[ci_upper]]}
            # Redefine for flipped axes to eliminate gap
            y_labpos <- if(axis_flip == TRUE) {df[[ci_upper]]} else {y_labpos}
        }

        # flip axis variables back after calculation
        if(axis_flip == TRUE) {
          swap_object_names('x', 'y')
          swap_object_names('x_min', 'y_min')
          swap_object_names('x_max', 'y_max')
        }

        # Define y anchor point for text (label will pivot about this)
        y_anchor <- if (bar_labels_angle %in% c(90,270)) {"bottom"} else {"centre"}
        y_anchor <- if (bar_labels_pos == 'bar_centre') {"centre"} else (y_anchor) # redefine for bar_centre position
        y_anchor <- if (axis_flip == TRUE) {"centre"} else (y_anchor)

        # Add labels to plot
        base <- base |>
          add_annotations(text = ~ df[[bar_labels]],
                          x = ~ if(axis_flip == FALSE) {x_labpos} else {y_labpos + ynudge},
                          y = ~ if(axis_flip == FALSE) {y_labpos + ynudge} else {x_labpos},
                          textangle = bar_labels_angle,
                          font = list(
                            family = chart_font,
                            size = bar_labels_font_size,
                            color = bar_labels_font_colour),
                          yanchor = y_anchor,
                          showarrow = FALSE)


      } else {

        # Plot for grouped bar chart

        # Flip axis variables for calculation if needed
        if(axis_flip == TRUE) {
          swap_object_names('x', 'y')
          swap_object_names('x_min', 'y_min')
          }

        # Calculate positions for stacked labels manually, create new dataframe to manage.
        df_labels <- df |>
          group_by(.data[[x]]) |>
          arrange(.data[[x]], desc(.data[[group_var]])) |>
          mutate(cumul = cumsum(.data[[y]]),                        # cumulative position of top of each bar
                 cumul_bar_base = cumul - .data[[y]],               # cumulative position of bottom of each bar
                 cumul_bar_centre = cumul - (.data[[y]]/2)) |>         # cumulative position above each errorbar
          ungroup()

        # Calculate positions for above_errorbar; needs ci_upper defined
        if(!is.null(ci_upper)) {
          df_labels <- df_labels |>
            mutate(ci_upper_diff = .data[[ci_upper]] - .data[[y]],    # define upper-ci-limit to y-value difference to use in above_errorbar label pos
                   cumul_above_errorbar = cumul + ci_upper_diff)      # cumulative position above each errorbar
        }

        # x-axis position of labels for grouped/dodged plots must be manually offset;
        #    offset is dependent on number of unique groups and x-axis resolution (calculated
        #    from ggobj above = errorbar_offset)
        unique_groups <- unique(df_labels[[group_var]])                                    # get number of unique groups
        offset_multiplier <- seq(length(unique_groups)) - ceiling(length(unique_groups)/2) # generate vector of integars to multiply the base offset by
        if(length(unique_groups) %%2 != 0) {offset_multiplier - 0.5}                       # if the number of groups is odd, offset the multiplier by 0.5 as one of the bars will be in the middle (for even numbers, the middle will be between the 2 central bars)
        # Character x-axis offset needs to be treated differently to numeric offset
        if(is.factor(df_labels[[x]]) | is.character(df_labels[[x]])) {
          offset_multiplier <- offset_multiplier * 0.8                                     # multiply this offset by 0.8 to match the ggplot defaults
          x_offset <- offset_multiplier * (1/length(unique_groups))                        # divide multiplier by the number of groups and multiply this by the multiplier to get the actual x-values of each bar (recycle errorbar_offset variable as it's doing the same job here but for label positions)
        } else {
          offset_multiplier <- offset_multiplier * 0.9                                       # multiply this offset by 0.9 to match the ggplot defaults
          x_offset <- offset_multiplier * (errorbar_offset/length(unique_groups))            # divide the x-axis resolution by the number of groups and multiply this by the multiplier to get the actual x-values of each bar (recycle errorbar_offset variable as it's doing the same job here but for label positions)
        }
        if(is.Date(df_labels[[x]]) | is.POSIXct(df_labels[[x]])) {                           # if x is a date then plotly will shunt the errorbars to the beginning of the nearest whole day (i.e. not in the middle of the barchart bars) so convert axis to time axis if this is the case
          df_labels[[x]] <- as.POSIXct(df_labels[[x]])
          x_offset <- x_offset * 24*60*60   # unit value for time is seconds rather than days as for date, so convert
        }

        # Create lookup table to link the offset values back to the main dataframe
        x_offset_lookup <- data.frame(x_offset = x_offset, group = levels(unique_groups))

        # For character axis bar position is an integer along the x-axis, calculate these in order to apply offset to
        if(exists('x_levels')) {
          if(x_axis_reverse == FALSE) {x_level_lkp <- x_levels} else {x_level_lkp <- rev(x_levels)} # reverse order for reversed x-axis
          x_bar_order_lookup <- data.frame(x = x_level_lkp) |> mutate(bar_order = row_number()-1)
        }


        # Join df and lookup table to create offset x values for plotting
        if(group_var_barmode == "group") {
          df_labels <- left_join(df_labels, x_offset_lookup, join_by(!!rlang::sym(group_var) == 'group')) #|>
            # Character x-axis offset needs to be treated differently to numeric offset
            if(exists('x_levels')) { # If x_levels exists then x is a factor/character
              df_labels <- left_join(df_labels, x_bar_order_lookup, join_by('x_orig' == 'x')) |>  # join with preserved x-values
                mutate(x_grouped = as.numeric(bar_order) + x_offset)
            } else {
              df_labels <- df_labels |> mutate(x_grouped = get(x) + x_offset)
            }
        }
        #if(axis_flip == TRUE) {swap_object_names('x', 'y')}  # flip axis variables back after calculation


        # Define plot label positions depending on bar_labels_pos choice

        # x position
        x_labpos <- if(group_var_barmode == "group") {df_labels$x_grouped} else {df_labels[[x]]}

        # y position
        if(bar_labels_pos == 'bar_above') {
          y_labpos <- if(group_var_barmode == "stack") {df_labels$cumul} else {df_labels[[y]]}
        } else if (bar_labels_pos == 'bar_base') {
          y_labpos <- if(group_var_barmode == "stack") {df_labels$cumul_bar_base} else {df_labels[[y]]} # position at bottom of each stacked bar
            # Redefine y_labpos to bottom of bar for grouped bars
            y_base <- if (is.na(y_min)) {0} else {y_min}
            y_labpos <- if(group_var_barmode == "group") {y_base} else {y_labpos}
        } else if (bar_labels_pos == 'bar_centre') {
          y_labpos <- if(group_var_barmode != "stack") {df_labels[[y]]} else {df_labels$cumul_bar_centre}  # position in centre of each stacked bar
            # Redefine y_labpos to halfway up bar for grouped bars
            y_labpos <- if(group_var_barmode == "group") {y_labpos / 2} else {y_labpos}
        } else if (bar_labels_pos == 'above_errorbar') {
          y_labpos <- df_labels$cumul_above_errorbar
            # Redefine y_labpos to halfway up bar for grouped bars
            y_labpos <- if(group_var_barmode == "group") {df_labels[[ci_upper]]} else {y_labpos}
        }


        # flip axis variables back after calculation
        if(axis_flip == TRUE) {
          swap_object_names('x', 'y')
          swap_object_names('x_min', 'y_min')
        }

        # Define y anchor point for text (label will pivot about this)
        y_anchor <- if (bar_labels_angle %in% c(90,270)) {"bottom"} else {"centre"}
        y_anchor <- if (bar_labels_pos == 'bar_centre') {"centre"} else (y_anchor) # redefine for bar_centre position
        y_anchor <- if (axis_flip == TRUE) {"centre"} else (y_anchor)

        # Plot labels
        base <- base |>
          add_annotations(text = ~ df_labels[[bar_labels]],
                          x = ~ if(axis_flip == FALSE) {x_labpos} else {y_labpos + ynudge},
                          y = ~ if(axis_flip == FALSE) {y_labpos + ynudge} else {x_labpos},
                          textangle = bar_labels_angle,
                          font = list(
                                    family = chart_font,
                                    size = bar_labels_font_size,
                                    color = bar_labels_font_colour),
                          yanchor = y_anchor,
                          showarrow = FALSE)

        # # Manually re-add labels for categorical x-values converted to numeric factors
        # if(exists('x_levels')) {
        #   base <- base |>
        #     layout(xaxis = list(
        #       tickvals = unique(df_labels[[x]]),
        #       ticktext = x_levels
        #     ))
        # }


      }

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




    # Manually re-add labels for categorical x-values converted to numeric factors
    if(exists('x_levels')) {
      if(axis_flip == FALSE) {
        base <- base |>
          layout(xaxis = list(
            tickvals = unique(df[[x]]),
            ticktext = x_levels))
      } else {
        base <- base |>
          layout(yaxis = list(
            tickvals = unique(df[[y]]),
            ticktext = x_levels))
      }

    }



    # return base plot
    return(base)

  } ### DYNAMIC CHART END


}






