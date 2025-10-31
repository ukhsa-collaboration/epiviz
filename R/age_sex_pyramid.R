#' Age-Sex Pyramid
#'
#' This function creates an age-sex pyramid visualization, either as a static ggplot or an interactive plotly chart.
#' The function can take either a line list (ungrouped data) or already grouped data as input.
#' When using a line list, the function processes the data, groups it by age and sex, and then generates the pyramid.
#' If grouped data is provided, it directly creates the pyramid.
#'
#' @param dynamic Logical. If `TRUE`, the function returns an interactive plotly chart. If `FALSE`, a static ggplot chart is returned.
#' @param base An optional base plot to add the pyramid to. Default is `NULL`.
#' @param params A list of parameters including:
#' \describe{
#'   \item{df}{Data frame containing the data to be used.}
#'   \item{var_map}{A list mapping variable names in the data frame to the expected names used in the function.
#'     \describe{
#'       \item{age_var}{Name of the variable in `df` containing age values. Default is `'age'`.}
#'       \item{dob_var}{Name of the variable in `df` containing date of birth values. Default is `'date_of_birth'`.}
#'       \item{sex_var}{Name of the variable in `df` containing sex values. Default is `'sex'`.}
#'       \item{age_group_var}{Name of the variable in `df` containing pre-grouped age groups (if `grouped = TRUE`).}
#'       \item{value_var}{Name of the column containing the value counts (if `grouped = TRUE`).}
#'       \item{ci_lower}{Name of the column containing lower confidence limits (if `ci = TRUE`).}
#'       \item{ci_upper}{Name of the column containing upper confidence limits (if `ci = TRUE`).}
#'     }
#'   }
#'   \item{mf_colours}{A vector of 2 colours used to fill the male and female bars in the
#'   plot. The first colour will be used for males, and the second for females. Default
#'   is `c("#440154", "#2196F3")`.}
#'   \item{x_breaks}{Number of breaks on the x-axis. Default is `10`.}
#'   \item{y_axis_title}{Title of the y-axis. Default is `"Individual count"`.}
#'   \item{x_axis_title}{Title of the x-axis. Default is `"Number of cases"`.}
#'   \item{text_size}{Size of the text in the plot. Default is `12`.}
#'   \item{ci}{Confidence interval. If `ci = "errorbar"` then confidence intervals will be
#'   be plotted with each bar as errorbars. If `ci = "errorbar"` and `grouped = FALSE`, then
#'   default confidence intervals are applied using the normal approximation to the Poisson
#'   distribution, with bounds set at \eqn{\pm 1.96 \times \sqrt{n}}.}
#'   \item{ci_colour}{Colour of the plotted errorbars if `ci = TRUE`. Default is `"red"`.}
#'   \item{age_breakpoints}{A numeric vector specifying the breakpoints for age groups. Default is `c(0, 5, 19, 65, Inf)`.}
#'   \item{age_calc_refdate}{Reference date for calculating age from date of birth. Default is `Sys.Date()`.}
#'   \item{grouped}{Logical. If `TRUE`, assumes the data is pre-grouped by age and sex. If `FALSE`,
#'   the function processes the line list data. Default is `FALSE`.}
#'   \item{legend_pos}{Position of the legend. Default is `"top"`.}
#'   \item{chart_title}{Text to use as the chart title.}
#' }
#'
#' @return A ggplot or plotly object representing the age-sex pyramid, depending on the value of `dynamic`.
#'
#' @details
#' When `grouped = FALSE`, the function processes a line list by grouping the data by age and sex,
#' calculating age based on either the provided age column or date of birth, and then generating the age-sex pyramid.
#' When `grouped = TRUE`, it assumes the data is already grouped and uses the provided values directly to generate the pyramid.
#'
#' @examples
#' \dontrun{
#' # Example using a line list
#' df <- epiviz::lab_data
#' age_sex_pyramid(
#'   dynamic = FALSE,
#'   params = list(
#'     df = df,
#'     var_map = list(age_var = 'age', dob_var = 'date_of_birth', sex_var = 'sex'),
#'     grouped = FALSE
#'   )
#' )
#'
#' # Example using pre-grouped data
#' grouped_df <- data.frame(
#'   age_group = c("0-4", "5-18", "19-64", "65+"),
#'   sex = c("Male", "Female"),
#'   value = c(100, 120, 150, 80),
#'   ci_lower = c(90, 110, 140, 70),
#'   ci_upper = c(110, 130, 160, 90)
#' )
#' age_sex_pyramid(
#'   dynamic = FALSE,
#'   params = list(
#'     df = grouped_df,
#'     var_map = list(age_group_var = 'age_group', sex_var = 'sex', value = 'value'),
#'     grouped = TRUE
#'   )
#' )
#' }
#'
#' @import ggplot2
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @importFrom assertthat not_empty
#' @export

age_sex_pyramid <- function(
    dynamic = FALSE,
    base = NULL,
    params = list(
      df,
      var_map = list(age_var = 'age',
                     dob_var = 'date_of_birth',
                     sex_var = 'sex',
                     age_group_var = 'age_group',
                     value_var = 'value',
                     ci_lower = 'ci_lower',
                     ci_upper = 'ci_upper'),
      mf_colours = c("#440154", "#2196F3"),
      x_breaks = 10,
      x_axis_title = "Number of cases",
      y_axis_title = "Age group (years)",
      text_size = 12,
      ci = NULL,
      ci_colour = "red",
      age_breakpoints = c(0, 5, 19, 65, Inf),
      age_calc_refdate = Sys.Date(),
      grouped = FALSE,
      legend_pos = "top",
      chart_title = ""
    )
) {

  # Solve warnings regarding font family not found using utils/set_Arial() function
  set_Arial()

  # Where relevant, assign defaults to any parameters not specified by the user
  if(!exists('mf_colours',where=params)) params$mf_colours <- c("#440154", "#2196F3")
  if(!exists('x_breaks',where=params)) params$x_breaks <- 10
  if(!exists('y_axis_title',where=params)) params$y_axis_title <- "Age group (years)"
  if(!exists('x_axis_title',where=params)) params$x_axis_title <- "Number of cases"
  if(!exists('text_size',where=params)) params$text_size <- 12
  if(!exists('ci',where=params)) params$ci <- NULL
  if(!exists('ci_colour',where=params)) params$ci_colour <- "red"
  if(!exists('age_breakpoints',where=params)) params$age_breakpoints <- c(0, 5, 19, 65, Inf)
  if(!exists('age_calc_refdate',where=params)) params$age_calc_refdate <- Sys.Date()
  if(!exists('grouped',where=params)) params$grouped <- FALSE
  if(!exists('legend_pos',where=params)) params$legend_pos <- "top"
  if(!exists('chart_title',where=params)) params$chart_title <- ""



  ##### Checks and warnings

  # Check if df is is.null
  if (!exists('df',where=params)) stop("A data frame argument is required")

  # Check df is a df class
  if(!is.data.frame(params$df)) stop("df is not a data frame object")

  # Check df is empty
  if(!assertthat::not_empty(params$df)) stop("df is empty")

  # If ci = TRUE and grouped = TRUE, check if ci_upper and ci_lower have been included and are present in df
  if(!is.null(params$ci) & params$grouped == TRUE) {

    # Check whether ci_lower has been provided
    if (is.null(params$var_map$ci_lower))
      stop("Please include a variable from df for ci_lower in var_map, e.g. var_map = list(ci_lower = \"variable_name\")")

    # Check whether ci_upper has been provided
    if (is.null(params$var_map$ci_upper))
      stop("Please include a variable from df for ci_upper in var_map, e.g. var_map = list(ci_upper = \"variable_name\")")

    # Check if ci_lower is in df
    if (!params$var_map$ci_lower %in% colnames(params$df))
      stop("ci_lower not found within df. Please include a variable from df for ci_lower, e.g. var_map = list(ci_lower = \"variable_name\")")

    # Check if ci_upper is in df
    if (!params$var_map$ci_upper %in% colnames(params$df))
      stop("upper not found within df. Please include a variable from df for ci_upper, e.g. var_map = list(ci_upper = \"variable_name\")")

  }

  # Check that 'age_group_var' has been provided for pre-grouped data
  if (params$grouped == TRUE & is.null(params$var_map$age_group_var)) {
    stop("Please provide 'age_group_var' when grouped = TRUE.")
  }

  # Check that at least either 'age' and 'dob_var' are provided for ungrouped data
  if (params$grouped == FALSE & is.null(params$var_map$age_var) & is.null(params$var_map$dob_var)) {
    stop("Please provide either 'age_var' or 'dob_var' when grouped = FALSE.")
  }

  # Warn that if both 'age' and 'dob_var' are provided, then only age will be used
  if (!is.null(params$var_map$age_var) & !is.null(params$var_map$dob_var)) {
      warning("If both 'age_var' and 'dob_var' are provided then only age_var will be used.")
  }



  .grp_df <- NULL

  if(dynamic == FALSE){
    # ggplot implementation of static age-sex-pyramid

    # Correctly reference var_map from params
    var_map <- params$var_map

    if(params$grouped == FALSE){
      # Check if age_var or dob_varis provided and exists in the data frame
      if (!is.null(var_map$age_var) && var_map$age_var %in% names(params$df)) {
        .grp_df <- process_line_list_for_age_sex_pyramid(
          df = params$df,
          var_map = var_map,
          age_breakpoints = params$age_breakpoints,
          age_calc_refdate = params$age_calc_refdate
        )
      } else if (!is.null(var_map$dob_var) && var_map$dob_var %in% names(params$df)) {
        .grp_df <- process_line_list_for_age_sex_pyramid(
          df = params$df,
          var_map = var_map,
          age_breakpoints = params$age_breakpoints,
          age_calc_refdate = params$age_calc_refdate
        )
      }
    }else{
      # have been passed grouped data to function

      .grp_df <- params$df |>
        select(age_group = any_of(var_map$age_group_var),
               sex = any_of(var_map$sex_var),
               value = any_of(var_map$value_var),
               ci_lower = any_of(var_map$ci_lower),
               ci_upper = any_of(var_map$ci_upper))

    }


    result <- agesex_pyramid_grouped(.grp_df,
                           mf_colours = params$mf_colours,
                           ci_colour = params$ci_colour,
                           x_breaks = params$x_breaks,
                           y_axis_title = params$y_axis_title,
                           text_size = params$text_size,
                           ci = params$ci)

    # Customise the plot to match the example
    result <- result +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = params$text_size, face = "bold"),
        axis.title.y = element_text(size = params$text_size, face = "bold"),
        axis.text = element_text(size = params$text_size),
        legend.position = params$legend_pos,
        legend.title = element_blank(),
        legend.text = element_text(size = params$text_size),
        plot.title = element_text(size = params$text_size + 2, face = "bold", hjust = 0.5),
        panel.grid = element_blank(),  # Remove grid lines
        axis.line = element_line(color = "black")  # Add axis lines in black
      ) +
      labs(
        y = params$x_axis_title,  # Swap x_axis_title to y-axis
        x = params$y_axis_title,  # Swap y_axis_title to x-axis
        title = params$chart_title  # Updated plot title
      )

    return(result)

  }else{
    # plotly implementation of dynamic age-sex-pyramid

    # Process data similarly to static version
# plotly implementation of dynamic age-sex-pyramid
  var_map <- params$var_map

  # Factorise provided age groups to ensure that they're always in the correct order on y-axis
  if(!is.null(var_map$age_group_var)) {
    params$df[var_map$age_group_var] <- factor(params$df[[var_map$age_group_var]],
                                           levels = unique(params$df[[var_map$age_group_var]]))
  }


  if (params$grouped == FALSE) {
    .grp_df <- process_line_list_for_age_sex_pyramid(
      df = params$df,
      var_map = var_map,
      age_breakpoints = params$age_breakpoints,
      age_calc_refdate = params$age_calc_refdate
    )
  } else {
    .grp_df <- params$df |>
      select(age_group = any_of(var_map$age_group_var),
             sex = any_of(var_map$sex_var),
             value = any_of(var_map$value_var),
             ci_lower = any_of(var_map$ci_lower),
             ci_upper = any_of(var_map$ci_upper))
  }

  # Create the plotly visualization
  male_data <- .grp_df[.grp_df$sex == "Male", ]
  female_data <- .grp_df[.grp_df$sex == "Female", ]

  # Convert values for males to negative for visualization
  male_data$value_pos <- male_data$value # preserve positive values for hover label
  male_data$value <- -male_data$value
  if (params$ci == 'errorbar') {
    male_data$ci_lower_pos <- male_data$ci_lower
    male_data$ci_lower <- -male_data$ci_lower
    male_data$ci_upper_pos <- male_data$ci_upper
    male_data$ci_upper <- -male_data$ci_upper
  }

  # Calculate maximum range for symmetric axis
  if (params$ci == 'errorbar') {
    all_x <- c(male_data$value, female_data$value, male_data$ci_lower, male_data$ci_upper,
               female_data$ci_lower, female_data$ci_upper)
  } else {
    all_x <- c(male_data$value, female_data$value)
  }
  min_x <- min(all_x)
  max_x <- max(all_x)
  max_range <- max(abs(min_x), max_x)

  # Generate symmetric tick values and positive labels
  positive_ticks <- pretty(c(0, max_range), n = ceiling(params$x_breaks / 2))
  tickvals <- sort(unique(c(-positive_ticks, positive_ticks)))
  ticktext <- as.character(abs(tickvals))

  # Create the plot
  p <- plot_ly(showlegend = TRUE)

  # Add male bars
  p <- add_trace(p,
                 x = male_data$value,
                 y = male_data$age_group,
                 type = "bar",
                 name = "Male",
                 marker = list(color = params$mf_colours[1]),
                 orientation = 'h',
                 customdata = male_data$value_pos,
                 hovertemplate = paste(
                   "Age group: %{y}<br>",
                   "Value: %{customdata}<extra></extra>"
                 ))

  # Add female bars
  p <- add_trace(p,
                 x = female_data$value,
                 y = female_data$age_group,
                 type = "bar",
                 name = "Female",
                 marker = list(color = params$mf_colours[2]),
                 orientation = 'h',
                 hovertemplate = paste(
                   "Age group: %{y}<br>",
                   "Value: %{x}<extra></extra>"
                 ))

  # Add confidence limits if requested
  if (params$ci == 'errorbar') {
    p <- add_trace(p,
                   x = male_data$value,
                   y = male_data$age_group,
                   type = "scatter",
                   mode = "markers",
                   name = "Male CI",
                   marker = list(
                     color = '#ffffff00',
                     line = list(colour = '#ffffff00', width = 0),
                     opacity = 0),
                   showlegend = FALSE,
                   #customdata = male_data$ci_lower_pos,
                   #hovertemplate = "%{y}, Lower: %{customdata}<extra></extra>",
                   text = paste0(male_data$age_group,
                                 '<br><i>Upper: ',male_data$ci_upper_pos,'</i>',   # leverage 'text' parameter in add_trace to pass additional info to hoverlabels
                                 '<br><i>Lower: ',male_data$ci_lower_pos,'</i>',
                                 '<extra></extra>'),
                   textposition = "none", # prevents text from being printed on plot
                   hovertemplate = '%{text}',
                   error_x = list(
                     type = "data",
                     symmetric = FALSE,
                     color = params$ci_colour,
                     thickness = 1,
                     arrayminus = male_data$value - male_data$ci_lower,
                     array = male_data$ci_upper - male_data$value
                   )
    )
    p <- add_trace(p,
                   x = female_data$value,
                   y = female_data$age_group,
                   type = "scatter",
                   mode = "markers",
                   name = "Female CI",
                   marker = list(
                     color = '#ffffff00',
                     line = list(colour = '#ffffff00', width = 0),
                     opacity = 0),
                   showlegend = FALSE,
                   #customdata = female_data$ci_lower,
                   #hovertemplate = "%{y}, Lower: %{customdata}<extra></extra>",
                   text = paste0(female_data$age_group,
                                 '<br><i>Upper: ',female_data$ci_upper,'</i>',   # leverage 'text' parameter in add_trace to pass additional info to hoverlabels
                                 '<br><i>Lower: ',female_data$ci_lower,'</i>',
                                 '<extra></extra>'),
                   textposition = "none", # prevents text from being printed on plot
                   hovertemplate = '%{text}',
                   error_x = list(
                     type = "data",
                     symmetric = FALSE,
                     color = params$ci_colour,
                     thickness = 1,
                     arrayminus = female_data$value - female_data$ci_lower,
                     array = female_data$ci_upper - female_data$value
                   )
    )
  }

  # Update layout with corrected titles and custom ticks
  p <- layout(p,
              title = params$chart_title,
              xaxis = list(
                title = list(text = params$x_axis_title, font = list(size = params$text_size)),
                tickfont = list(size = params$text_size),
                zeroline = TRUE,
                showgrid = FALSE,
                showline = TRUE,
                linecolor = "black",
                tickmode = "array",
                tickvals = tickvals,
                ticktext = ticktext,
                range = c(-max_range * 1.05, max_range * 1.05)
              ),
              yaxis = list(
                title = list(text = params$y_axis_title, font = list(size = params$text_size)),
                tickfont = list(size = params$text_size),
                zeroline = TRUE,
                showgrid = FALSE,
                showline = TRUE,
                linecolor = "black"
              ),
              barmode = 'overlay',
              font = list(family = "Arial"),
              hoverlabel = list(bgcolor = "white", font = list(size = 12)),
              showlegend = TRUE,
              legend = list(orientation = "h",
                            xanchor = "center",
                            x = 0.5,
                            y = -0.2)
  )

  return(p)
  }
}


