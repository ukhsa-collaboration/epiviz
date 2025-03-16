#' A function for producing either static (ggplot) or dynamic (plotly)
#' line charts.
#'
#' This function generates a line chart from a data frame using specified x and y variables.
#' Optionally, the plot can be rendered as an interactive Plotly object. The function also allows
#' grouping of data based on a specified grouping variable.
#'
#' @param dynamic A logical value. If `TRUE`, the line chart will be rendered as a Plotly object for interactivity.
#'        If `FALSE`, a static ggplot2 object will be returned.
#' @param base A base plotly or ggplot2 object to add the line chart to. Default is `NULL`.
#' @param params A list containing the following elements:
#' \itemize{
#'   \item \code{df} A data frame containing the data to be plotted.
#'   \item \code{x} A character string specifying the name of the column in `df` to be used for the x-axis.
#'   \item \code{y} A character string specifying the name of the column in `df` to be used for the y-axis.
#'   \item \code{group_var} A character string specifying the name of the column in `df` to be used for grouping the data.
#'   \item \code{ci} Optional. A character string specifying the column in `df` for confidence intervals.
#'   \item \code{lower} Optional. A character string specifying the column in `df` for lower bounds of confidence intervals.
#'   \item \code{upper} Optional. A character string specifying the column in `df` for upper bounds of confidence intervals.
#'   \item \code{error_colour} The color for error bars. Default is `#f2c75c`.
#'   \item \code{line_colour} List of colours for lines. Default is `blue`.
#'   \item \code{line_type} Line type for single graph, or list of line types
#'   Permissable values: "solid", "dotted", "dashed", "longdash", "dotdash"
#'   \item \code{width} A numeric value specifying the width of the lines.
#'   \item \code{title} Optional. A character string specifying the title of the plot.
#'   \item \code{x_label} Optional. A character string specifying the label for the x-axis.
#'   \item \code{x_label_angle} Optional. A numeric value specifying the rotation angle for the x-axis labels.
#'   \item \code{y_label} Optional. A character string specifying the label for the y-axis.
#'   \item \code{y_label_angle} Optional. A numeric value specifying the rotation angle for the y-axis labels.
#'   \item \code{y_percent} Optional. A logical value. If `TRUE`, the y-axis will be scaled to percentages.
#'   \item \code{st_theme} Optional. A ggplot2 theme object to customize the style of the plot.
#'   \item \code{add_points} Optional. A logical value. If `TRUE`, points will be added to the line chart.
#' }
#' @param ... Additional arguments passed to \code{geom_line} for static (ggplot2) plots
#'   or to \code{plot_ly}/\code{add_trace} for dynamic (Plotly) plots, allowing custom
#'   styling of the lines (e.g., \code{alpha}, \code{size}, \code{marker}, etc.).
#'
#' @import ggplot2
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @return A plotly or ggplot2 object representing the line chart.
#' @examples
#' library(dplyr)
#' library(epiviz)
#'
#' # Import df lab_data from epiviz and do some manipulation before passing for the test
#' test_df <- epiviz::lab_data
#'
#' # Manipulating date within df
#' test_df$specimen_date <- as.Date(test_df$specimen_date)
#'
#' # Setting start date and end date for aggregation
#' start_date <- as.Date("2023-01-01")
#' end_date <- as.Date("2023-12-31")
#'
#' # Summarization
#' summarised_df <- test_df |>
#'   group_by(organism_species_name, specimen_date) |>
#'   summarize(count = n(), .groups = 'drop') |>
#'   ungroup() |>
#'   filter(specimen_date >= start_date & specimen_date <= end_date)
#'
#' # Ensure that summarised_df is a data frame
#' summarised_df <- as.data.frame(summarised_df)
#'
#'  # Create params list
#'  params <- list(
#'    df = summarised_df,  # Ensure this is correctly referencing the data frame
#'    x = "specimen_date", # Ensure this matches the column name exactly
#'    y = "count",         # Ensure this matches the column name exactly
#'    group_var = "organism_species_name",  # Ensure this matches the column name exactly
#'    line_colour = c("blue","green","orange"),
#'    line_type = c("solid", "dotted", "dashed")
#'  )
#'  # Generate the line chart
#' line_chart(params = params, dynamic = FALSE)
#'
#' # Generate the line chart
#' result <- epiviz::line_chart(params = params, dynamic = FALSE)
#'
#'  # Import df lab_data from epiviz and do some manipulation before passing for the test
#'  test_df <- epiviz::lab_data
#'
#'  # Manipulating date within df
#'  test_df$specimen_date <- as.Date(test_df$specimen_date)
#'
#'  # Setting start date and end date for aggregation
#'  start_date <- as.Date("2023-01-01")
#'  end_date <- as.Date("2023-12-31")
#'
#'  # Summarization
#'  summarised_df <- test_df |>
#'    group_by(organism_species_name, specimen_date) |>
#'    summarize(count = n(), .groups = 'drop') |>
#'    ungroup() |>
#'    filter(specimen_date >= start_date & specimen_date <= end_date)
#'
#'  # Ensure that summarised_df is a data frame
#'  summarised_df <- as.data.frame(summarised_df)
#'
#'  # Create params list
#'  params <- list(
#'    df = summarised_df,  # Ensure this is correctly referencing the data frame
#'    x = "specimen_date", # Ensure this matches the column name exactly
#'    y = "count",         # Ensure this matches the column name exactly
#'    group_var = "organism_species_name",  # Ensure this matches the column name exactly
#'    line_colour = c("blue","green","orange"),
#'    line_type = c("solid", "dotted", "dashed")
#'  )
#'
#'  # Generate the line chart
#'  epiviz::line_chart(params = params, dynamic = TRUE)
#'
#' @export
line_chart <-  function(dynamic = FALSE,
                        base = NULL,
                        params = list(
                          df,
                          x,
                          y,
                          ci = NULL,
                          lower = NULL,
                          upper = NULL,
                          error_colour = c("#f2c75c"),
                          group_var,
                          line_colour = c("blue"),
                          line_type = "solid",
                          width = 1,
                          title = NULL,
                          x_label = NULL,
                          x_label_angle = NULL,
                          #rotation counterclockwise
                          y_label = NULL,
                          y_label_angle = NULL,
                          #rotation counterclockwise
                          y_percent = FALSE,
                          st_theme = NULL,
                          add_points = FALSE,
                          show_gridlines = FALSE,
                          show_axislines = TRUE,
                          legend_title = NULL,
                          legend_position = NULL,
                          #  "left","top","right","bottom","none".
                          hline = NULL,
                          hline_colour = "red",
                          hline_label = NULL
                        ),
                        ...) {
  # Solve warnings regarding font family not found using utils/set_Arial() function
  set_Arial()

  # Where relevant, assign defaults to any parameters not specified by the user
  if (!exists('ci', where = params))
    params$ci <- NULL
  if (!exists('lower', where = params))
    params$lower <- NULL
  if (!exists('upper', where = params))
    params$upper <- NULL
  if (!exists('error_colour', where = params))
    params$error_colour <- c("#f2c75c")
  if (!exists('group_var', where = params))
    params$upper <- NULL
  if (!exists('line_colour', where = params))
    params$line_colour <- c("blue")
  if (!exists('line_type', where = params))
    params$line_type <- "solid"
  if (!exists('width', where = params))
    params$width <- 1
  if (!exists('title', where = params))
    params$title <- NULL
  if (!exists('x_label', where = params))
    params$x_label <- NULL
  if (!exists('x_label_angle', where = params))
    params$x_label_angle <- NULL # rotation counterclockwise
  if (!exists('y_label', where = params))
    params$y_label <- NULL
  if (!exists('y_label_angle', where = params))
    params$y_label_angle <- NULL # rotation counterclockwise
  if (!exists('y_percent', where = params))
    params$y_percent <- FALSE
  if (!exists('st_theme', where = params))
    params$st_theme <- NULL
  if (!exists('add_points', where = params))
    params$add_points <- FALSE
  if (!exists('show_gridlines', where = params))
    params$show_gridlines <- FALSE
  if (!exists('show_axislines', where = params))
    params$show_axislines <- TRUE
  if (!exists('legend_title', where = params))
    params$legend_title <- NULL
  if (!exists('legend_position', where = params))
    params$legend_position <- NULL # "left","top","right","bottom","none"
  if (!exists('hline', where = params))
    params$hline <- NULL
  if (!exists('hline_colour', where = params))
    params$hline_colour <- "red"
  if (!exists('hline_label', where = params))
    params$hline_label <- NULL

  valid_line_types <-
    c("solid", "dotted", "dashed", "longdash", "dotdash")

  # default RColorBrewer color pallette



  ##### Checks and warnings

  # Check if df is is.null
  if (!exists('df', where = params))
    stop("A data frame argument is required")

  # Check df is a df class
  if (!is.data.frame(params$df))
    stop("df is not a data frame object")

  # Check df is empty
  if (!not_empty(params$df))
    stop("df is empty")

  # Check if x argument is is.null
  if ((is.null(params$x)) | !exists('x', where = params))
    stop("Please include a variable from df for x, i.e. x = \"variable_name\"")

  # Check if y argument is is.null
  if ((is.null(params$y)) | !exists('y', where = params))
    stop("Please include a variable from df for y, i.e. y = \"variable_name\"")

  # Check if x is in df
  if (!params$x %in% colnames(params$df))
    stop(
      "x not found within df. Please include a variable from df for x, i.e. x = \"variable_name\""
    )

  # Check if y is in df
  if (!params$y %in% colnames(params$df))
    stop(
      "y not found within df. Please include a variable from df for y, i.e. y = \"variable_name\""
    )

  # Check if number of groups and number of point colours are the same
  if (exists('group_var', where = params)) {
    if (length(params$line_colour) != length(unique(params$df[[params$group_var]])))
      stop(
        paste0(
          "The number of point_colours provided must equal the number of
                  unique groups in group_var. There are ",
          length(unique(params$df[[params$group_var]])),
          " groups in the data provided."
        )
      )
  }

  # Define parameters as variables using utils/param_assign() function
  #   -Takes input list, compares it ro a reference vector of expected
  #     list elements, assigns each element to a variable within the
  #     parent environment, and allocates a value of 'NULL' to anything
  #     it can't find within the reference list.
  param_assign(
    params,
    c(
      "df",
      "x",
      "y",
      "ci",
      "lower",
      "upper",
      "error_colour",
      "group_var",
      "line_colour",
      "line_type",
      "width",
      "title",
      "x_label",
      "x_label_angle",
      "y_label",
      "y_label_angle",
      "y_percent",
      "st_theme",
      "add_points",
      "show_gridlines",
      "show_axislines",
      "legend_title",
      "legend_position",
      "hline",
      "hline_colour",
      "hline_label"
    )
  )

  if (!dynamic) {
    # produce ggplot graph if 'dynamic' is set to FALSE

    # create base plot
    if (is.null(base)) {
      base <- ggplot2::ggplot() + theme_minimal()  # Change to minimal theme
    }

    # add default theme if theme argument not provided
    if (!is.null(st_theme)) {
      base <- base + st_theme
    } else {
      # If not provided, use the consistent theme
      base <- base + 
        theme_minimal() +
        theme(
          axis.title.x = element_text(size = params$text_size, face = "bold"),
          axis.title.y = element_text(size = params$text_size, face = "bold"),
          axis.text = element_text(size = params$text_size),
          legend.position = params$legend_position,
          legend.title = element_blank(),
          legend.text = element_text(size = params$text_size),
          panel.grid = element_blank(),  # Remove grid lines
          axis.line = element_line(color = "black"),  # Add axis lines in black
          plot.title = element_text(size = params$text_size + 2, face = "bold", hjust = 0.5)
        )
    }

    

    if (is.null(group_var)) {
      # creating base graph without groups
      base <-
        base + ggplot2::geom_line(
          data = df,
          aes(x = .data[[x]], y = .data[[y]]),
          linetype = line_type,
          colour = line_colour,
          linewidth = width,
          ...
        )
    } else{
      # creating base graph with groups
      base <-
        base + ggplot2::geom_line(
          data = df,
          aes(
            x = .data[[x]],
            y = .data[[y]],
            group = .data[[group_var]],
            colour = .data[[group_var]],
            linetype = .data[[group_var]]
          ),
          linewidth = width,
          ...
        ) +
        scale_colour_manual(values = line_colour) +
        scale_linetype_manual(values = line_type)


      # Legend parameters

      if (!is.null(legend_title)) {
        base <-  base + labs(color = legend_title)
      }

      if (!is.null(legend_position)) {
        base <-  base + theme(legend.position = legend_position)
      }

    }


    ##### Titles and labels

    # Add title
    if (!is.null(title)) {
      base <- base + ggplot2::labs(title = title) +
        # centre title
        theme(plot.title = element_text(hjust = 0.5))
    }

    # Apply x label using arguments provided
    if (!is.null(x_label)) {
      base <- base + ggplot2::labs(x = x_label)
    }

    # Apply y label using arguments provided
    if (!is.null(y_label)) {
      base <- base + ggplot2::labs(y = y_label)
    }

    # Rotate axis text if angle is given
    if (!is.null(x_label_angle)) {
      base <- base + ggplot2::theme(axis.text.x = element_text(angle  = x_label_angle, vjust = 0.5))
    }

    if (!is.null(y_label_angle)) {
      base <- base + ggplot2::theme(axis.text.y = element_text(angle  = y_label_angle, vjust = 0.5))
    }


    ####### Grid lines and axis

    #remove major and minor grid lines
    if (!show_gridlines) {
      base <- base + ggplot2::theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()
      )
    }


    # shows axis lines
    if (show_axislines) {
      base <- base + theme(
        axis.line.x = element_line(colour = "black", linewidth = 1),
        axis.line.y = element_line(colour = "black", linewidth = 1)
      )
    } else{
      base <- base + theme(axis.line.x = element_blank(), axis.line.y = element_blank())
    }

    # adds horizontal line at the y value specified for hline
    if (!is.null(hline)) {
      base <-
        base + geom_hline(yintercept = hline,
                          colour = hline_colour,
                          linewidth = 1.5)
    }

    # adds a label specified at the start of the horizontal line
    if (!is.null(hline) && !(!is.null(hline_label))) {
      base <-
        base + geom_text(aes(
          x = min(df[[x]]),
          y = hline,
          label = hline_label,
          vjust = -1,
          hjust = -0.1
        ))

    }

    # adds percent sign to the y axis values - doesnt change the value
    if (y_percent) {
      base <-
        base +  scale_y_continuous(labels = scales::percent_format(scale = 1))
    }


    ### Points in lines

    # if add points included then add geom
    if (add_points) {
      if (!is.null(group_var)) {
        base <-
          base + ggplot2::geom_point(data = df, aes(
            x = .data[[x]],
            y = .data[[y]],
            colour = .data[[group_var]],
            size = 1
          )) +
          guides(size = FALSE) # hides size of point from legend

      } else{
        base <-
          base + ggplot2::geom_point(data = df, aes(
            x = .data[[x]],
            y = .data[[y]],
            size = 1
          )) +
          guides(size = FALSE) # hides size of point from legend
      }
    }


    #### confidence interval; ribbon \ error bar
    if ((!is.null(ci)) && (!is.null(group_var))) {
      # continue if  arguments for ci and bounds are provided
      ifelse(
        !is.null(lower) && !is.null(upper),

        # continue if type geom required is error else ribbon
        ifelse(
          ci == 'e',

          # Apply error bar with same legend and colour for line and ci
          base <-
            base + ggplot2::geom_errorbar(
              data = df,
              aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]]
              ),
              colour = error_colour,
              linewidth = 1
            )
          ,

          # Apply ribbon with same legend and colour for line and ci
          base <-
            base + ggplot2::geom_ribbon(
              data = df,
              aes(
                x = .data[[x]],
                ymin = .data[[lower]],
                ymax = .data[[upper]],
                group = 1
              ),
              fill = error_colour,
              alpha = .5
            )
        )

      )


    }




  } else{
    # produce plotly graph if 'dynamic' is set to TRUE

    # Mapping ggplot2 line types to plotly dash styles
    plotly_line_types <- lapply(line_type, function(lt) {
      switch(
        lt,
        "solid" = "solid",
        "dotted" = "dot",
        "dashed" = "dash",
        "longdash" = "longdash",
        "dotdash" = "dashdot",
        stop(
          "Invalid line type. Choose from: solid, dotted, dashed, longdash, dotdash"
        )
      )
    })

    # Valid linetypes in plotly (after mapping)
    valid_line_types <- c("solid", "dot", "dash", "longdash", "dashdot")

    # Validation for line types
    if (length(plotly_line_types) == 1) {
      if (!(plotly_line_types[[1]] %in% valid_line_types)) {
        stop("Invalid line type. Choose from: solid, dotted, dashed, longdash, dotdash")
      }
    } else {
      if (!all(unlist(plotly_line_types) %in% valid_line_types)) {
        invalid_types <- line_type[!(unlist(plotly_line_types) %in% valid_line_types)]
        stop(
          paste(
            "Invalid line types:",
            paste(invalid_types, collapse = ", "),
            "Choose from: solid, dotted, dashed, longdash, dotdash"
          )
        )
      }
    }


    # build list of line attributes

    # Create a list to store line attributes
    line_attributes <- list()

    # Add line modifiers
    if (!is.null(width)) {
      line_attributes$width <- width * 2
    }
    if (!is.null(line_type)) {
      line_attributes$dash <- plotly_line_style
    }
    if (!is.null(line_colour)) {
      line_attributes$color <- line_colour
    }

    if (add_points == TRUE) {
      plt_mode = 'lines+markers'
    } else{
      plt_mode = 'lines'
    }


    if (is.null(group_var)) {
      # create plotly base plot without groups
      base <- df |>
        plot_ly(
          x = ~ .data[[x]],
          y = ~ .data[[y]],
          type = 'scatter',
          mode = 'lines',
          line = list(
            color = line_colour,
            dash = plotly_line_types[[1]],
            width = width
          ),
          ...
        ) |>
        layout(xaxis = list(title = x), yaxis = list(title = y))

    } else{
      # create plotly base plot groups

      unique_groups <- unique(df[[group_var]])
      base <- plot_ly()

      for (i in 1:length(unique_groups)) {
        group_name <- unique_groups[i]
        group_data <- params$df[params$df[[group_var]] == group_name, ]

        base <- base |>
          add_trace(
            data = group_data,
            x = ~ .data[[params$x]],
            y = ~ .data[[params$y]],
            type = 'scatter',
            mode = 'lines',
            name = group_name,
            line = list(
              color = params$line_colour[i],
              dash = plotly_line_types[[i]],
              width = params$width
            ),
            ...
          )
      }


      base <- base |>
        layout(xaxis = list(title = x), yaxis = list(title = y))

    }


    ##### Titles and labels

    # add title
    if (!is.null(title)) {
      base <- base |> layout(title = list(text = title))
    }

    # add x axis label
    if (!is.null(x_label)) {
      base <- base |> layout(xaxis = list(title = x_label))
    }

    # add y axis label
    if (!is.null(y_label)) {
      base <- base |> layout(yaxis = list(title = y_label))
    }

    # change x axis angle
    if (!is.null(x_label_angle)) {
      # angle negated as this function following ggplot rotation direction
      base <- base |>  layout(xaxis = list(tickangle = -x_label_angle))
    }

    # change y axis angle
    if (!is.null(y_label_angle)) {
      # angle negated as this function following ggplot rotation direction
      base <- base |>  layout(yaxis = list(tickangle = -y_label_angle))
    }


    ####### Grid lines and axis

    if (!is.null(show_gridlines)) {
      base <- base |> layout(xaxis = list(showgrid = F),
                             yaxis = list(showgrid = F))
    }

    if (show_axislines) {
      base <- base |> layout(
        xaxis = list(showline = TRUE),
        yaxis = list(showline = TRUE, zeroline = FALSE)
      )
    }

    if (!is.null(hline)) {
      base <- base |>
        layout(shapes = list(
          type = "line",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = hline,
          y1 = hline,
          line = list(color = hline_colour)
        ))
    }

    if (!is.null(hline_label)) {
      base <- base |> add_annotations(
        text = hline_label,
        x = min(df[[x]]),
        y = hline,
        showarrow = FALSE,
        bgcolor = "white",
        font = list(weight = "bold")
      )
    }

    if (y_percent) {
      # Apply percentage formatting to y-axis labels (inline)
      base <- base |>  layout(yaxis = list(ticksuffix  = "%"))

    }

    ## Legend settings

    if (!is.null(legend_position)) {
      legend_orientation <-
        if (legend_position %in% c("top", "bottom"))
          "h"
      else
        "v"

      # configure legend settings
      legend_settings <- switch(
        legend_position,
        "left" = list(
          x = -0.2,
          y = 0.5,
          xanchor = "right",
          yanchor = "middle",
          orientation = legend_orientation
        ),
        "top" = list(
          x = 0.5,
          y = 0.97,
          xanchor = "center",
          yanchor = "bottom",
          orientation = legend_orientation
        ),
        "right" = list(
          x = 1.1,
          y = 0.5,
          xanchor = "left",
          yanchor = "middle",
          orientation = legend_orientation
        ),
        "bottom" = list(
          x = 0.5,
          y = -0.2,
          xanchor = "center",
          yanchor = "top",
          orientation = legend_orientation
        )

      )  # Move legend outside of the plot area)

      if (legend_position != "none") {
        base <- base |> layout(legend = legend_settings)
      } else{
        base <- base |> layout(showlegend = F)
      }
    }

  }

  # return either ggplot or base plot
  return(base)
}
