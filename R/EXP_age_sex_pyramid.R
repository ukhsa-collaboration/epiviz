#' EXP Age-Sex Pyramid
#'
#' @description A function for producing either a static (ggplot) or dynamic (plotly)
#' age-sex pyramid chart. This function can take either a line list (ungrouped data)
#' or pre-grouped data as input.
#'
#' @param dynamic Logical indicating whether to produce a dynamic (plotly) output.
#' Default is \code{FALSE}, which will return a static ggplot output.
#' @param params A named list containing arguments used to create the plot.
#' \describe{
#'    \item{df}{A data frame containing data used to create the age-sex pyramid.}
#'    \item{y}{character, Name of the variable in \code{df} containing the values
#'    (counts) when \code{grouped = TRUE}.}
#'    \item{age_var}{character, Name of the variable in \code{df} containing age values.
#'    Used when \code{grouped = FALSE}. Default is \code{"age"}.}
#'    \item{dob_var}{character, Name of the variable in \code{df} containing date of
#'    birth values. Used when \code{grouped = FALSE} and \code{age_var} is not available.}
#'    \item{sex_var}{character, Name of the variable in \code{df} containing sex values.
#'    Default is \code{"sex"}. Permitted values for sex include M, F, Male, Female
#'    (not case sensitive).}
#'    \item{age_group_var}{character, Name of the variable in \code{df} containing
#'    pre-grouped age groups when \code{grouped = TRUE}.}
#'    \item{grouped}{Logical. If \code{TRUE}, assumes the data is pre-grouped by age
#'    and sex. If \code{FALSE} (default), the function processes line list data.}
#'    \item{age_breakpoints}{A numeric vector specifying the breakpoints for age groups
#'    when \code{grouped = FALSE}. Default is \code{c(0, 5, 19, 65, Inf)}.}
#'    \item{age_calc_refdate}{Reference date for calculating age from date of birth.
#'    Default is \code{Sys.Date()}.}
#'    \item{fill_colours}{A character vector of 2 colours used to fill the male and
#'    female bars. The first colour is used for males, and the second for females.
#'    Default is \code{c("#440154", "#2196F3")}.}
#'    \item{bar_border_colour}{character, Colour of the border around each bar.
#'    Default is \code{"black"}.}
#'    \item{ci}{Confidence interval. If \code{ci = "errorbar"} then confidence intervals
#'    will be plotted with each bar. When \code{grouped = FALSE}, default Poisson
#'    confidence intervals are applied automatically.}
#'    \item{ci_lower}{character, Name of the variable in \code{df} containing lower
#'    confidence limits when \code{grouped = TRUE} and \code{ci = "errorbar"}.}
#'    \item{ci_upper}{character, Name of the variable in \code{df} containing upper
#'    confidence limits when \code{grouped = TRUE} and \code{ci = "errorbar"}.}
#'    \item{ci_colours}{character, Colour of the error bars. Default is \code{"red"}.}
#'    \item{errorbar_width}{numeric, Width of the error bar caps. Default is \code{0.5}.}
#'    \item{shift_origin}{Logical. If \code{TRUE}, allows the value-axis origin to shift
#'    to accommodate asymmetric data where one sex has larger values than the other.
#'    This preserves plot space by independently scaling each side of the pyramid.
#'    Default is \code{FALSE}.}
#'    \item{chart_title}{Text to use as the chart title.}
#'    \item{chart_title_size}{Font size of chart title. Default = \code{13}.}
#'    \item{chart_title_colour}{Font colour of chart title. Default = \code{"black"}.}
#'    \item{chart_footer}{Text to use as chart footer.}
#'    \item{chart_footer_size}{Font size of chart footer. Default = \code{12}.}
#'    \item{chart_footer_colour}{Font colour of chart footer. Default = \code{"black"}.}
#'    \item{x_axis_title}{Text used for the value axis title (horizontal).
#'    Default is \code{"Number of cases"}.}
#'    \item{y_axis_title}{Text used for the age group axis title (vertical).
#'    Default is \code{"Age group (years)"}.}
#'    \item{x_axis_title_font_size}{Font size of the value axis title. Default = \code{11}.}
#'    \item{y_axis_title_font_size}{Font size of the age group axis title. Default = \code{11}.}
#'    \item{x_axis_label_font_size}{Font size for the value axis tick labels. Default = \code{9}.}
#'    \item{y_axis_label_font_size}{Font size for the age group axis tick labels. Default = \code{9}.}
#'    \item{x_axis_n_breaks}{Approximate number of breaks on the value axis.
#'    Default = \code{10}.}
#'    \item{x_limit_max}{Upper limit for the value axis. If not provided, the limit
#'    is determined automatically from the data.}
#'    \item{show_gridlines}{Logical to show chart gridlines. Default = \code{FALSE}.}
#'    \item{show_axislines}{Logical to show chart axis lines. Default = \code{TRUE}.}
#'    \item{legend_title}{Text used for legend title.}
#'    \item{legend_pos}{Position of the legend. Permitted values =
#'    c("top","bottom","right","left"). Default = \code{"top"}.}
#'    \item{legend_font_size}{Font size used in the legend. Default = \code{8}.}
#'    \item{legend_title_font_size}{Font size used for the legend title. Default = \code{8}.}
#'    \item{hover_labels}{string, Text to be used in the hover-over labels in a dynamic
#'    chart. Accepts html.}
#'  }
#'
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
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
#' # Example 1: Basic pyramid from line list data
#'
#' library(epiviz)
#'
#' basic_pyramid <- EXP_age_sex_pyramid(
#'   params = list(
#'     df = lab_data,
#'     dob_var = "date_of_birth",
#'     sex_var = "sex",
#'     age_breakpoints = c(0, 5, 19, 65, Inf),
#'     chart_title = "Age-Sex Pyramid",
#'     x_axis_title = "Number of cases",
#'     y_axis_title = "Age group (years)"
#'   )
#' )
#'
#' basic_pyramid
#'
#'
#'
#' # Example 2: Pyramid from pre-grouped data with confidence intervals
#'
#' library(epiviz)
#'
#' grouped_df <- lab_data |> 
#'   mutate(age = lubridate::time_length(interval(date_of_birth, Sys.Date()), "years")) |> 
#'   mutate(age_group = case_when(
#'     age < 5 ~ "0-4",
#'     age < 19 ~ "5-18",
#'     age < 65 ~ "19-64",
#'     TRUE ~ "65+"
#'   )) |> 
#'   group_by(age_group, sex) |> 
#'   summarise(
#'     count = n(),
#'     lower = count - sample(100:500, 1),
#'     upper = count + sample(100:500, 1)
#'   )
#'
#' # Create parameter list
#' pyramid_params <- list(
#'   df = grouped_df,
#'   y = "count",
#'   age_group_var = "age_group",
#'   sex_var = "sex",
#'   grouped = TRUE,
#'   ci = "errorbar",
#'   ci_lower = "lower",
#'   ci_upper = "upper",
#'   fill_colours = c("#440154", "#2196F3"),
#'   chart_title = "Age-Sex Pyramid with CI",
#'   show_gridlines = FALSE
#' )
#'
#' # Create static pyramid
#' static_pyramid <- EXP_age_sex_pyramid(params = pyramid_params, dynamic = FALSE)
#'
#' # Create dynamic pyramid
#' dynamic_pyramid <- EXP_age_sex_pyramid(params = pyramid_params, dynamic = TRUE)
#'
#' # View both simultaneously using shiny app
#' library(shiny)
#' library(plotly)
#' ui <- fluidPage(
#'   plotOutput('static_pyramid'),
#'   plotlyOutput('dynamic_pyramid')
#' )
#' server <- function(input, output, session) {
#'   output$static_pyramid <- renderPlot(static_pyramid)
#'   output$dynamic_pyramid <- renderPlotly(dynamic_pyramid)
#' }
#' shinyApp(ui, server)
#'
#'
#'
#' # Example 3: Pyramid with shifted origin for asymmetric data
#'
#' library(epiviz)
#'
#' asymmetric_pyramid <- EXP_age_sex_pyramid(
#'   params = list(
#'     df = lab_data,
#'     age_var = "age",
#'     sex_var = "sex",
#'     shift_origin = TRUE,
#'     chart_title = "Asymmetric Age-Sex Pyramid",
#'     fill_colours = c("#007C91", "#8A1B61")
#'   )
#' )
#'
#' asymmetric_pyramid
#'
#' }
#'
EXP_age_sex_pyramid <- function(
    dynamic = FALSE,
    params = list(
      df = NULL,
      y = NULL,
      age_var = NULL,
      dob_var = NULL,
      sex_var = "sex",
      age_group_var = NULL,
      grouped = FALSE,
      age_breakpoints = c(0, 5, 19, 65, Inf),
      age_calc_refdate = Sys.Date(),
      fill_colours = c("#440154", "#2196F3"),
      bar_border_colour = "black",
      ci = NULL,
      ci_upper = NULL,
      ci_lower = NULL,
      ci_colours = "red",
      errorbar_width = 0.5,
      shift_origin = FALSE,
      chart_title = NULL,
      chart_title_size = 13,
      chart_title_colour = "black",
      chart_footer = NULL,
      chart_footer_size = 12,
      chart_footer_colour = "black",
      x_axis_title = "Number of cases",
      y_axis_title = "Age group (years)",
      x_axis_title_font_size = 11,
      y_axis_title_font_size = 11,
      x_axis_label_font_size = 9,
      y_axis_label_font_size = 9,
      x_axis_n_breaks = 10,
      x_limit_max = NULL,
      show_gridlines = FALSE,
      show_axislines = TRUE,
      legend_title = "",
      legend_pos = "top",
      legend_font_size = 8,
      legend_title_font_size = 8,
      hover_labels = NULL
    )
) {


  # Solve warnings regarding font family not found using utils/set_Arial() function
  set_Arial()


  # Where relevant, assign defaults to any parameters not specified by the user
  if(!exists('sex_var',where=params)) params$sex_var <- "sex"
  if(!exists('grouped',where=params)) params$grouped <- FALSE
  if(!exists('age_breakpoints',where=params)) params$age_breakpoints <- c(0, 5, 19, 65, Inf)
  if(!exists('age_calc_refdate',where=params)) params$age_calc_refdate <- Sys.Date()
  if(!exists('fill_colours',where=params)) params$fill_colours <- c("#440154", "#2196F3")
  if(!exists('bar_border_colour',where=params)) params$bar_border_colour <- "black"
  if(!exists('ci_colours',where=params)) params$ci_colours <- "red"
  if(!exists('errorbar_width',where=params)) params$errorbar_width <- 0.5
  if(!exists('shift_origin',where=params)) params$shift_origin <- FALSE
  if(!exists('chart_title_size',where=params)) params$chart_title_size <- 13
  if(!exists('chart_title_colour',where=params)) params$chart_title_colour <- "black"
  if(!exists('chart_footer_size',where=params)) params$chart_footer_size <- 12
  if(!exists('chart_footer_colour',where=params)) params$chart_footer_colour <- "black"
  if(!exists('x_axis_title',where=params)) params$x_axis_title <- "Number of cases"
  if(!exists('y_axis_title',where=params)) params$y_axis_title <- "Age group (years)"
  if(!exists('x_axis_title_font_size',where=params)) params$x_axis_title_font_size <- 11
  if(!exists('y_axis_title_font_size',where=params)) params$y_axis_title_font_size <- 11
  if(!exists('x_axis_label_font_size',where=params)) params$x_axis_label_font_size <- 9
  if(!exists('y_axis_label_font_size',where=params)) params$y_axis_label_font_size <- 9
  if(!exists('x_axis_n_breaks',where=params)) params$x_axis_n_breaks <- 10
  if(!exists('show_gridlines',where=params)) params$show_gridlines <- FALSE
  if(!exists('show_axislines',where=params)) params$show_axislines <- TRUE
  if(!exists('legend_title',where=params)) params$legend_title <- ""
  if(!exists('legend_pos',where=params)) params$legend_pos <- "top"
  if(!exists('legend_font_size',where=params)) params$legend_font_size <- 8
  if(!exists('legend_title_font_size',where=params)) params$legend_title_font_size <- 8



  ##### Checks and warnings

  # Check if df is null
  if (!exists('df',where=params)) stop("df = NULL; a data frame argument is required")

  # Check df is a df class
  if(!is.data.frame(params$df)) stop("df is not a data frame object")

  # Check df is empty
  if(!assertthat::not_empty(params$df)) stop("df is empty")

  # Check fill_colours has exactly 2 elements
  if (length(params$fill_colours) != 2)
    stop("fill_colours must be a vector of exactly 2 colours (Male, Female)")

  # For pre-grouped data, check required variables
  if (params$grouped == TRUE) {
    if (is.null(params$age_group_var))
      stop("Please provide 'age_group_var' when grouped = TRUE.")
    if (is.null(params$y))
      stop("Please provide 'y' (value variable name) when grouped = TRUE.")
    if (!params$age_group_var %in% colnames(params$df))
      stop("age_group_var not found within df.")
    if (!params$y %in% colnames(params$df))
      stop("y not found within df.")
    if (!params$sex_var %in% colnames(params$df))
      stop("sex_var not found within df.")
  }

  # For ungrouped data, check required variables
  if (params$grouped == FALSE) {
    if (is.null(params$age_var) & is.null(params$dob_var))
      stop("Please provide either 'age_var' or 'dob_var' when grouped = FALSE.")
    if (!is.null(params$age_var) & !is.null(params$dob_var))
      warning("If both 'age_var' and 'dob_var' are provided then only age_var will be used.")
  }

  # CI checks for pre-grouped data
  if (!is.null(params$ci) & params$grouped == TRUE) {
    if (is.null(params$ci_lower))
      stop("Please provide 'ci_lower' when ci is specified and grouped = TRUE.")
    if (is.null(params$ci_upper))
      stop("Please provide 'ci_upper' when ci is specified and grouped = TRUE.")
    if (!params$ci_lower %in% colnames(params$df))
      stop("ci_lower variable not found within df.")
    if (!params$ci_upper %in% colnames(params$df))
      stop("ci_upper variable not found within df.")
  }



  ##### Parameter assignment

  # Define parameters as variables using utils/param_assign() function
  #   -Takes input list, compares it to a reference vector of expected
  #     list elements, assigns each element to a variable within the
  #     parent environment, and allocates a value of 'NULL' to anything
  #     it can't find within the reference list.
  param_assign(params,
               c("df",
                 "y",
                 "age_var",
                 "dob_var",
                 "sex_var",
                 "age_group_var",
                 "grouped",
                 "age_breakpoints",
                 "age_calc_refdate",
                 "fill_colours",
                 "bar_border_colour",
                 "ci",
                 "ci_upper",
                 "ci_lower",
                 "ci_colours",
                 "shift_origin",
                 "chart_title",
                 "chart_title_size",
                 "chart_title_colour",
                 "chart_footer",
                 "chart_footer_size",
                 "chart_footer_colour",
                 "x_axis_title",
                 "y_axis_title",
                 "x_axis_title_font_size",
                 "y_axis_title_font_size",
                 "x_axis_label_font_size",
                 "y_axis_label_font_size",
                 "x_axis_n_breaks",
                 "x_limit_max",
                 "show_gridlines",
                 "show_axislines",
                 "legend_title",
                 "legend_pos",
                 "legend_font_size",
                 "legend_title_font_size",
                 "hover_labels"
               ))



  #################### DATA PROCESSING #########################

  .grp_df <- NULL

  if (grouped == FALSE) {

    # Process line list data using existing helper function
    var_map <- list(
      age_var = age_var,
      dob_var = dob_var,
      sex_var = sex_var
    )

    .grp_df <- process_line_list_for_age_sex_pyramid(
      df = df,
      var_map = var_map,
      age_breakpoints = age_breakpoints,
      age_calc_refdate = age_calc_refdate
    )

  } else {

    # Use pre-grouped data: select and rename columns to standard internal names
    .grp_df <- df |>
      select(
        age_group = all_of(age_group_var),
        sex = all_of(sex_var),
        value = all_of(y)
      )

    # Add CI columns if applicable
    if (!is.null(ci) && !is.null(ci_lower) && !is.null(ci_upper)) {
      .grp_df$ci_lower <- df[[ci_lower]]
      .grp_df$ci_upper <- df[[ci_upper]]
    }

  }


  # Order age groups: youngest at bottom, oldest at top
  # Sort ascending by the leading numeric value in each age group label
  .grp_df <- .grp_df |>
    arrange(as.integer(sub("^(\\d+).*", "\\1", sub("[<+]", "", age_group)))) |>
    mutate(age_group = factor(age_group, levels = unique(age_group))) |>
    na.omit()

  # Negate male values for left-side display
  .grp_df <- .grp_df |>
    mutate(value = ifelse(sex == "Male", -value, value))

  # Negate male confidence limits if applicable
  if (!is.null(ci) && ci == "errorbar") {
    .grp_df <- .grp_df |>
      mutate(
        ci_lower = ifelse(sex == "Male", -ci_lower, ci_lower),
        ci_upper = ifelse(sex == "Male", -ci_upper, ci_upper)
      )
  }


  # Calculate value axis limits
  if (!is.null(ci) && ci == "errorbar") {
    val_min <- min(c(.grp_df$ci_lower, .grp_df$ci_upper, .grp_df$value), na.rm = TRUE)
    val_max <- max(c(.grp_df$ci_lower, .grp_df$ci_upper, .grp_df$value), na.rm = TRUE)
  } else {
    val_min <- min(.grp_df$value, na.rm = TRUE)
    val_max <- max(.grp_df$value, na.rm = TRUE)
  }

  # Apply shift_origin or symmetric limits
  if (!shift_origin) {
    # Symmetric: both sides scaled to the max absolute value
    max_abs <- max(abs(val_min), abs(val_max))
    val_min <- -max_abs
    val_max <- max_abs
  }

  # Override with user-specified x_limit_max if provided
  if (!is.null(x_limit_max)) {
    val_max <- x_limit_max
    if (!shift_origin) val_min <- -x_limit_max
  }

  # Calculate tick positions for the value axis
  positive_ticks <- pretty(c(0, val_max), n = ceiling(x_axis_n_breaks / 2))
  negative_ticks <- -pretty(c(0, abs(val_min)), n = ceiling(x_axis_n_breaks / 2))
  tickvals <- sort(unique(c(negative_ticks, positive_ticks)))
  ticklabels_num <- abs(tickvals)



  #################### PYRAMID CHART #########################

  ##### CREATE STATIC CHART

  if (!dynamic) {
    # Produce ggplot object if 'dynamic' is set to FALSE


    # Suppress spurious 'Scale for X is already present' messages
    suppressMessages({


      ##### Build the pyramid chart

      p <- ggplot(.grp_df) +
        geom_col(
          aes(x = age_group, y = value, fill = sex),
          colour = bar_border_colour,
          linewidth = 0.25
        ) +
        coord_flip()


      ##### Add error bars if requested

      if (!is.null(ci) && ci == "errorbar") {
        p <- p +
          geom_errorbar(
            aes(x = age_group, ymin = ci_lower, ymax = ci_upper),
            colour = ci_colours,
            width = errorbar_width,
            linewidth = 0.75
          )
      }


      ##### Set fill colours

      p <- p + scale_fill_manual(values = fill_colours, drop = FALSE)


      ##### Set value axis breaks and labels (absolute values)

      p <- p + scale_y_continuous(
        breaks = tickvals,
        labels = scales::comma(ticklabels_num),
        limits = c(min(tickvals), max(tickvals)),
        expand = expansion(mult = 0.05)
      )


      ##### Apply theme

      p <- p + ggplot2::theme_classic()


      ##### Chart titles and footer

      # Add title
      if (!is.null(chart_title)) {
        p <- p + ggplot2::labs(title = chart_title) +
          theme(plot.title = element_text(
            hjust = 0.5,
            size = chart_title_size,
            colour = chart_title_colour,
            family = chart_font,
            face = "bold"
          ))
      }

      # Add footer
      if (!is.null(chart_footer)) {
        p <- p + ggplot2::labs(caption = chart_footer) +
          theme(plot.caption = element_text(
            size = chart_footer_size,
            colour = chart_footer_colour,
            family = chart_font
          ))
      }


      ##### Axis titles
      # Note: coords are flipped, so ggplot x = visual y (age groups)
      #   and ggplot y = visual x (values)

      p <- p + labs(
        x = y_axis_title,     # visual y-axis = age groups
        y = x_axis_title      # visual x-axis = values
      )


      ##### Axis formatting

      p <- p + theme(
        axis.title.x = element_text(size = x_axis_title_font_size, face = "bold", family = chart_font),
        axis.title.y = element_text(size = y_axis_title_font_size, face = "bold", family = chart_font),
        axis.text.x = element_text(size = x_axis_label_font_size, family = chart_font),
        axis.text.y = element_text(size = y_axis_label_font_size, family = chart_font)
      )


      ##### Grid lines

      if (show_gridlines) {
        p <- p + ggplot2::theme(
          panel.grid.major = element_line(colour = "grey83", linewidth = 0.2),
          panel.grid.minor = element_line(colour = "grey93", linewidth = 0.1)
        )
      } else {
        p <- p + ggplot2::theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
      }


      ##### Axis lines

      if (show_axislines) {
        p <- p + theme(
          axis.line.x = element_line(colour = "black", linewidth = 0.5),
          axis.line.y = element_line(colour = "black", linewidth = 0.5)
        )
      } else {
        p <- p + theme(
          axis.line.x = element_blank(),
          axis.line.y = element_blank()
        )
      }


      ##### Legend

      if (legend_title != "") {
        p <- p + labs(fill = legend_title)
      } else {
        p <- p + theme(legend.title = element_blank())
      }

      p <- p + theme(
        legend.position = legend_pos,
        legend.text = element_text(size = legend_font_size, family = chart_font),
        legend.title = if (legend_title != "") {
          element_text(size = legend_title_font_size, family = chart_font)
        } else {
          element_blank()
        }
      )


    }) # suppressMessages end


    ##### Return final output
    return(clean_gg_labels(p))


    ### STATIC CHART END



  } else {

    ##### CREATE DYNAMIC CHART

    # Produce plotly object if 'dynamic' is set to TRUE


    # Separate male and female data
    male_data <- .grp_df[.grp_df$sex == "Male", ]
    female_data <- .grp_df[.grp_df$sex == "Female", ]

    # Preserve positive values for hover labels
    male_data$value_pos <- abs(male_data$value)


    ##### Define default hover labels

    if (is.null(hover_labels)) {
      male_hover <- "Age group: %{y}<br>Value: %{customdata}<extra></extra>"
      female_hover <- "Age group: %{y}<br>Value: %{x}<extra></extra>"
    } else {
      male_hover <- hover_labels
      female_hover <- hover_labels
    }


    ##### Create the plot

    p <- plot_ly(showlegend = TRUE)

    # Add male bars
    p <- add_trace(p,
                   x = male_data$value,
                   y = male_data$age_group,
                   type = "bar",
                   name = "Male",
                   marker = list(
                     color = fill_colours[1],
                     line = list(color = bar_border_colour, width = 0.5)
                   ),
                   orientation = 'h',
                   customdata = male_data$value_pos,
                   hovertemplate = male_hover)

    # Add female bars
    p <- add_trace(p,
                   x = female_data$value,
                   y = female_data$age_group,
                   type = "bar",
                   name = "Female",
                   marker = list(
                     color = fill_colours[2],
                     line = list(color = bar_border_colour, width = 0.5)
                   ),
                   orientation = 'h',
                   hovertemplate = female_hover)


    ##### Add confidence intervals if requested

    if (!is.null(ci) && ci == "errorbar") {

      # Number of age groups (used for cap width calculation)
      n_groups <- length(unique(.grp_df$age_group))

      # Initial cap width estimate in pixels (will be dynamically corrected
      # by onRender to match the actual rendered plot dimensions).
      # Plotly width is a half-width (center to cap end), so divide by 2.
      errorbar_width_px <- errorbar_width * (300 / n_groups) / 2

      # Preserve positive CI values for male hover labels
      male_data$ci_lower_pos <- abs(male_data$ci_lower)
      male_data$ci_upper_pos <- abs(male_data$ci_upper)

      # Male CI
      p <- add_trace(p,
                     x = male_data$value,
                     y = male_data$age_group,
                     type = "scatter",
                     mode = "markers",
                     name = "Male CI",
                     marker = list(
                       color = 'rgba(255,255,255,0)',
                       line = list(color = 'rgba(255,255,255,0)', width = 0),
                       opacity = 0),
                     showlegend = FALSE,
                     text = paste0(male_data$age_group,
                                   '<br><i>Upper: ', male_data$ci_upper_pos, '</i>',
                                   '<br><i>Lower: ', male_data$ci_lower_pos, '</i>',
                                   '<extra></extra>'),
                     textposition = "none",
                     hovertemplate = '%{text}',
                     error_x = list(
                       type = "data",
                       symmetric = FALSE,
                       color = ci_colours,
                       thickness = 1,
                       width = errorbar_width_px,
                       array = male_data$ci_lower - male_data$value,
                       arrayminus = male_data$value - male_data$ci_upper
                     ))

      # Female CI
      p <- add_trace(p,
                     x = female_data$value,
                     y = female_data$age_group,
                     type = "scatter",
                     mode = "markers",
                     name = "Female CI",
                     marker = list(
                       color = 'rgba(255,255,255,0)',
                       line = list(color = 'rgba(255,255,255,0)', width = 0),
                       opacity = 0),
                     showlegend = FALSE,
                     text = paste0(female_data$age_group,
                                   '<br><i>Upper: ', female_data$ci_upper, '</i>',
                                   '<br><i>Lower: ', female_data$ci_lower, '</i>',
                                   '<extra></extra>'),
                     textposition = "none",
                     hovertemplate = '%{text}',
                     error_x = list(
                       type = "data",
                       symmetric = FALSE,
                       color = ci_colours,
                       thickness = 1,
                       width = errorbar_width_px,
                       array = female_data$ci_upper - female_data$value,
                       arrayminus = female_data$value - female_data$ci_lower
                     ))
    }


    ##### Define fonts

    title_font <- list(
      family = chart_font,
      size = chart_title_size,
      color = chart_title_colour)

    footer_font <- list(
      family = chart_font,
      size = chart_footer_size,
      color = chart_footer_colour)

    x_title_font <- list(
      family = chart_font,
      size = x_axis_title_font_size,
      color = "black")

    y_title_font <- list(
      family = chart_font,
      size = y_axis_title_font_size,
      color = "black")

    x_label_font <- list(
      family = chart_font,
      size = x_axis_label_font_size,
      color = "black")

    y_label_font <- list(
      family = chart_font,
      size = y_axis_label_font_size,
      color = "black")


    ##### Configure legend position

    if (legend_pos %in% c("top", "bottom")) {
      legend_orient <- "h"
    } else {
      legend_orient <- "v"
    }

    legend_config <- switch(
      legend_pos,
      "top" = list(x = 0.5, y = 0.97, xanchor = "center", yanchor = "bottom",
                   orientation = legend_orient),
      "bottom" = list(x = 0.5, y = -0.2, xanchor = "center", yanchor = "top",
                      orientation = legend_orient),
      "left" = list(x = -0.1, y = 0.5, xanchor = "right", yanchor = "middle",
                    orientation = legend_orient),
      "right" = list(x = 1.02, y = 0.5, xanchor = "left", yanchor = "middle",
                     orientation = legend_orient)
    )


    ##### Calculate axis range with padding

    axis_pad <- (max(tickvals) - min(tickvals)) * 0.05
    x_range <- c(min(tickvals) - axis_pad, max(tickvals) + axis_pad)


    ##### Replace R linebreaks with html linebreaks for plotly

    if (!is.null(chart_title)) {chart_title <- gsub("\\n","<br>",chart_title)}
    if (!is.null(chart_footer)) {chart_footer <- gsub("\\n","<br>",chart_footer)}


    ##### Update layout

    p <- layout(p,
                title = if (!is.null(chart_title)) {
                  list(text = html_bold(chart_title),
                       font = title_font,
                       x = 0.5,
                       xanchor = "center",
                       y = 0.95)
                } else {
                  NULL
                },
                xaxis = list(
                  title = list(text = html_bold(x_axis_title), font = x_title_font),
                  tickfont = x_label_font,
                  zeroline = FALSE,
                  showgrid = show_gridlines,
                  showline = show_axislines,
                  linecolor = "black",
                  tickmode = "array",
                  tickvals = tickvals,
                  ticktext = scales::comma(ticklabels_num),
                  range = x_range,
                  ticks = if (show_axislines) "outside" else "",
                  ticklen = if (show_axislines) 3 else 0
                ),
                yaxis = list(
                  title = list(text = html_bold(y_axis_title), font = y_title_font),
                  tickfont = y_label_font,
                  zeroline = FALSE,
                  showgrid = FALSE,
                  showline = show_axislines,
                  linecolor = "black",
                  ticks = if (show_axislines) "outside" else "",
                  ticklen = if (show_axislines) 3 else 0
                ),
                barmode = 'overlay',
                font = list(family = chart_font),
                hoverlabel = list(bgcolor = "white", font = list(size = 12)),
                showlegend = TRUE,
                legend = c(legend_config,
                           list(font = list(size = legend_font_size,
                                            family = chart_font),
                                bgcolor = "rgba(0,0,0,0)",
                                borderwidth = 0)),
                margin = list(t = 50, b = 60, l = 3, r = 10)
    )


    ##### Add footer as annotation

    if (!is.null(chart_footer)) {
      p <- layout(p,
                  annotations = list(
                    x = 1, y = -0.15,
                    text = chart_footer,
                    xanchor = 'right', yanchor = 'middle',
                    xref = 'paper', yref = 'paper',
                    showarrow = FALSE,
                    font = footer_font,
                    align = "right"
                  ))
    }


    ##### Dynamically size error bar caps to match ggplot output
    # ggplot errorbar width is in data units (fraction of category spacing)
    # but plotly width is in pixels, so a fixed constant only matches at one
    # chart size. onRender calculates the correct pixel width from the actual
    # rendered plot area height, keeping caps consistent at any display size.

    if (!is.null(ci) && ci == "errorbar") {
      p <- htmlwidgets::onRender(p, sprintf("
        function(el) {
          var plotHeight = el._fullLayout._size.h;
          var capWidth   = %f * plotHeight / (%d * 2);
          var indices    = [];
          el.data.forEach(function(trace, i) {
            if (trace.error_x) indices.push(i);
          });
          if (indices.length > 0) {
            Plotly.restyle(el, {'error_x.width': capWidth}, indices);
          }
        }
      ", errorbar_width, n_groups))
    }


    return(p)


    ### DYNAMIC CHART END

  }

}
