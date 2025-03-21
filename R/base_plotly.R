#' Creates base plotly object for use across other functions.
#' Parameters are not passed explicitly to the function, so
#' function call needs to be proceeded by environment(base_plotly) <- environment()
#'
#' @return plotly object
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' environment(base_plotly) <- environment()
#' base <- base_plotly()
#' }
base_plotly <- function() {


    ##### Create base plotly object
    if (!is.null(base)) {
      base <- base
      base_chart <- base # store unmodified base chart
    } else {
      base <- plot_ly()
    }


    ##### Apply title, footer, and axis labels

    # Define fonts
    title_font <- list(
      family = chart_font,
      size = chart_title_size,
      color = chart_title_colour)

    footer_font <- list(
      family = chart_font,
      size = chart_footer_size,
      color = chart_footer_colour)

    x_axis_title_font <- list(
      family = chart_font,
      size = if (exists("x_axis_title_font_size")) {x_axis_title_font_size} else {11},
      color = "black")

    y_axis_title_font <- list(
      family = chart_font,
      size = if (exists("y_axis_title_font_size")) {y_axis_title_font_size} else {11},
      color = "black")

    x_axis_label_font <- list(
      family = chart_font,
      size = if (exists("x_axis_label_font_size")) {x_axis_label_font_size} else {9},
      color = "black")

    y_axis_label_font <- list(
      family = chart_font,
      size = if (exists("y_axis_label_font_size")) {y_axis_label_font_size} else {9},
      color = "black")

    axis_break_font <- list(
      family = chart_font,
      size = 9,
      color = "black")


    # Replace R linebreaks with html linebreaks for plotly
    if (!is.null(chart_title)) {chart_title <- gsub("\\n","<br>",chart_title)}
    if (!is.null(chart_footer)) {chart_footer <- gsub("\\n","<br>",chart_footer)}


    # Add title
    if (!is.null(chart_title)) {
      base <- base |>
        layout(title = list(text = html_bold(chart_title), # utils/html_bold function used to apply <b> </b> tags to title text
                            font = title_font,
                            x = 0.5,
                            xanchor = "centre",
                            xref = 'paper', yref = 'paper'))
    }

    # Add footer
    if (!is.null(chart_footer)) {
      base <- base |>
        layout(annotations =
                 list(x = 1,
                      y = -0.19-abs((sin(-x_axis_label_angle))*0.12),  # scale y position according to x-label text angle
                      text = chart_footer,
                      xanchor='right',
                      yanchor='middle',
                      #yanchor='auto',
                      xref='paper',
                      yref='paper',
                      showarrow = F,
                      font = footer_font,
                      align = "right"
                 )
        )
    }


    # Note:- utils/html_bold function used to apply <b> </b> tags to axis titles for bold font

    # Add x axis title
    if (!is.null(x_axis_title)) {
      base <- base |>
        layout(xaxis = list(title =
                              list(text = html_bold(x_axis_title),
                                   font = x_axis_title_font)))
    } else {
      base <- base |>
        layout(xaxis = list(title =
                              list(text = html_bold(x),
                                   font = x_axis_title_font)))
    }


    # Add y axis title
    #    (but only if base_chart isn't provided and sec y-axis isn't specified, otherwise leave y-axis title as is)
    if (!exists("base_chart") & y_sec_axis == FALSE) {
      if (!is.null(y_axis_title)) {
        base <- base |>
          layout(yaxis = list(title =
                                list(text = html_bold(y_axis_title),
                                     font = y_axis_title_font)))
      } else {
        base <- base |>
          layout(yaxis = list(title =
                                list(text = html_bold(y),
                                     font = y_axis_title_font)))
      }
    }


    # Set font for axis break text
    base <- base |> layout(font = axis_break_font)


    # Set axis tick font
    base <- base |>
      layout(xaxis = list(tickfont = x_axis_label_font))

    base <- base |>
      layout(yaxis = list(tickfont = y_axis_label_font))



    # Change x axis text angle
    if (!is.null(x_axis_label_angle)){
      # angle negated as this function following ggplot rotation direction
      base <- base |> layout(xaxis = list(tickangle = -x_axis_label_angle))
    }

    # Change y axis text angle
    if (!is.null(y_axis_label_angle)){
      # angle negated as this function following ggplot rotation direction
      base <- base |> layout(yaxis = list(tickangle = -y_axis_label_angle))
    }




    ####### Set grid lines, axes, and graph margin

    # Set margin to match ggplot
    base <- base |>
      layout(margin = list(r = if (y_sec_axis == TRUE) {40} else {10}, # increase right margin when secondary y-axis used
                           t=30,
                           b = 60+abs((sin(-x_axis_label_angle))*50), # scale bottom margin according to x-label text angle
                           l=3))

    # Grid lines
    if (!show_gridlines) {
      base <- base |> layout(xaxis = list(showgrid = F),
                             yaxis = list(showgrid = F))
    }

    # Axis lines
    if (show_axislines) {
      base <- base |> layout(
        xaxis = list(showline = TRUE,
                     linewidth = 1,
                     ticks="outside",
                     ticklen=3),
        yaxis = list(showline = TRUE,
                     zeroline = FALSE,
                     linewidth = 1,
                     ticks="outside",
                     ticklen=3)
      )
    }



    ##### Apply y percentage axis

    if ((y_percent == TRUE) & (y_sec_axis == FALSE)) {
      #base <- base |> layout(yaxis = list(ticksuffix  = "%"))
      base <- base |> layout(yaxis = list(tickformat  = ".0%"))
    }



    ##### Apply axis limits

    # Uses x_min, x_max, y_min, y_max variables derived from ggobj outside of base_plotly()

    # Replace existing limits with user defined x / y limits if provided
    if(!is.null(x_limit_min)) {x_min <- x_limit_min}
    if(!is.null(x_limit_max)) {x_max <- x_limit_max}
    if(!is.null(y_limit_min)) {y_min <- y_limit_min}
    if(!is.null(y_limit_max)) {y_max <- y_limit_max}

    # Pad x-axis range in-line with ggplot formatting (5% on each side)
    if (is.numeric(df[[x]])) {
      xpad <- (x_max-x_min)*0.05
      x_max <- x_max+xpad
      x_min <- x_min-xpad
    } else if (lubridate::is.Date(df[[x]])) {
      xpad <- round(as.numeric(difftime(x_max, x_min, units="days"))*0.05, digits = 0)
      x_max <- as.Date(x_max)+xpad
      x_min <- as.Date(x_min)-xpad
    }

    # Convert dates to character so they aren't coerced to numeric when added to range vector
    x_min <- if(lubridate::is.Date(df[[x]])) {as.character(x_min)} else {x_min}
    x_max <- if(lubridate::is.Date(df[[x]])) {as.character(x_max)} else {x_max}
    y_min <- if(lubridate::is.Date(df[[y]])) {as.character(y_min)} else {y_min}
    y_max <- if(lubridate::is.Date(df[[y]])) {as.character(y_max)} else {y_max}

    # Define x and y range vectors
    x_range <- c(x_min, x_max)
    y_range <- c(y_min, y_max)

    #Reverse x-axis range if specified
    if (x_axis_reverse == TRUE) {x_range <- rev(x_range)}

    # Apply axis ranges to chart
    base <- base |>
      layout(
        xaxis = list(range = x_range),
        yaxis = list(range = y_range)
      )




    ##### Apply axis breaks

    # Apply user specified axis breaks
    # x
    if(!is.null(x_axis_break_labels)) {
      base <- base |>
        layout(
          xaxis = list(
            tickvals = as.list(x_axis_break_labels)
          )
        )
      # use x_axis_date_breaks if provided
    } else if (!is.null(x_axis_date_breaks)) {
      base <- base |>
        layout(
          xaxis = list(
            #dtick = "D7"
            dtick = datebreak_to_d3(x_axis_date_breaks)  # use utils/datebreak_to_d3() function
          )
        )
    }

    # y
    if(!is.null(y_axis_break_labels)) {
      base <- base |>
        layout(
          yaxis = list(
            tickvals = as.list(y_axis_break_labels)
          )
        )
    }


    # Apply axis_n_breaks if provided
    #    Note:- axis_break_labels and x_axis_date_breaks take priority

    # x
    if(is.null(x_axis_break_labels) & !is.null(x_axis_n_breaks)) {
      base <- base |>
        layout(
          xaxis = list(
            nticks = x_axis_n_breaks
          )
        )
    }

    # y
    if(is.null(y_axis_break_labels) & !is.null(y_axis_n_breaks)) {
      base <- base |>
        layout(
          yaxis = list(
            nticks = y_axis_n_breaks
          )
        )
    }



    ##### Apply axis tick formatting

    # Set axis tick label angle
    # (negative angle as ggplot and plotly use opposite rotations)

    # x
    if(!is.null(x_axis_label_angle)) {x_tickangle <- -x_axis_label_angle} else {x_tickangle <- 0}

    base <- base |>
      layout(
        xaxis = list(
          tickangle = x_tickangle
        )
      )

    # y
    if(!is.null(y_axis_label_angle)) {y_tickangle <- -y_axis_label_angle} else {y_tickangle <- 0}

    base <- base |>
      layout(
        yaxis = list(
          tickangle = y_tickangle
        )
      )


    # Force x-axis date format to YYYY-MM-DD

    if(lubridate::is.Date(df[[x]])) {
      base <- base |>
        layout(
          xaxis = list(
            tickformat="%Y-%m-%d"
          )
        )
    }



    ##### Apply secondary y-axis

    # Define variable for y primary/secondary axis choice
    y_axis_choice <- if (y_sec_axis == TRUE) {"y2"} else {"y"}
    # Apply to: Main trace, ci trace, hline trace

    # Add sec y-axis to base
    if (y_sec_axis == TRUE) {

      base <- base |>
        layout(
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            showline = TRUE,
            zeroline = FALSE,
            linewidth = 1,
            ticks="outside",
            ticklen=3
          )
        )

      # Add sec y-axis label
      if (!is.null(y_axis_title)) {
        base <- base |>
          layout(yaxis2 = list(title =
                                 list(text = html_bold(y_axis_title),
                                      font = y_axis_title_font)))
      } else {
        base <- base |>
          layout(yaxis2 = list(title =
                                 list(text = html_bold(y),
                                      font = y_axis_title_font)))
      }

      # Change sec y axis text angle
      if (!is.null(y_axis_label_angle)){
        # angle negated as this function following ggplot rotation direction
        base <- base |> layout(yaxis2 = list(tickangle = -y_axis_label_angle))
      }

      # Apply sec y percentage axis
      if (y_percent == TRUE) {
        base <- base |> layout(yaxis2 = list(tickformat  = ".0%"))
      }

      # Force sec y percentage scale to 0-100% if specified
      if(y_sec_axis_percent_full == TRUE) {
        y_range <- c(0,1)
      }

      # Apply sec y-axis range
      base <- base |>
        layout(yaxis2 = list(range = y_range))

      # Apply sec y_axis_break_labels if provided
      if(!is.null(y_axis_break_labels)) {
        base <- base |>
          layout(
            yaxis2 = list(
              tickvals = as.list(y_axis_break_labels)
            )
          )
      }

      # Apply sec y_axis_n_breaks if provided
      if(is.null(y_axis_break_labels) & !is.null(y_axis_n_breaks)) {
        base <- base |>
          layout(
            yaxis2 = list(
              nticks = y_axis_n_breaks
            )
          )
      }

      # Apply sec y axis tick angle
      base <- base |>
        layout(
          yaxis2 = list(
            tickangle = y_tickangle
          )
        )

      # y1-axis range of supplied 'base' plot becomes mis-aligned when applying y2 axis
      #   -Not possible to harvest y-axis ranges from an existant plotly object and reapply manually
      #   -Set autorange = TRUE instead to rescale / realign primary y-axis range
      base <- base |>
        layout(yaxis = list(autorange = TRUE))

    }




    ##### Apply hline(s)

    if (!is.null(hline)) {

      # Each hline requires a separate list of values
      # Plotly then requires a list of lists to apply multiple hlines
      #    https://www.geeksforgeeks.org/add-horizontal-or-vertical-line-in-plotly-using-r/


      # Define base hline list
      hlines_list <- list()

      # create each sub-list iteratively
      for (i in 1:length(hline)) {

        hline_sublist <- list(
              type = "line",
              x0 = 0,
              x1 = 1,
              xref = "paper",
              yref = y_axis_choice,
              y0 = hline[i],
              y1 = hline[i],
              line = list(color = if (length(hline_colour)>1) {hline_colour[i]} else {hline_colour},
                          dash = if (length(hline_type)>1) {plotly_line_style(hline_type[i])} else {plotly_line_style(hline_type)},   # uses utils/plotly_line_style() function
                          width = if (length(hline_width)>1) {hline_width[i]} else {hline_width})
            )

        # Append sublist to main list
        hlines_list[[length(hlines_list)+1]] <- hline_sublist


        # Add hline labels with each iteration as this doesn't have to be done via lists-of-lists
        if (!is.null(hline_label)) {

          # Define positiion of label depending on whether x axis is reversed
          #   - use min value of x, or max x if axis reversed

          # handle factor x-axes
          xpos_high <- if (!is.factor(df[[x]])) {max(df[[x]])} else {last(df[[x]])}
          xpos_low <- if (!is.factor(df[[x]])) {min(df[[x]])} else {first(df[[x]])}

          if (x_axis_reverse == FALSE) {
            hline_xpos <- if(!is.null(x_limit_min)) {x_limit_min} else {xpos_low}
          } else {
            hline_xpos <- if(!is.null(x_limit_max)) {x_limit_max} else {xpos_high}
          }


          base <- base |>
            add_annotations(
              text = hline_label[i],
              x = hline_xpos,
              y = hline[i],
              #yaxis = y_axis_choice,
              yref = y_axis_choice,
              xanchor = "left",
              yanchor = "bottom",
              showarrow = FALSE,
              bgcolor = "#ffffff00",
              font = list(color = if (length(hline_colour)>1) {hline_colour[i]} else {hline_colour},
                          size = 12)
            )
        }
      }

      # Draw hlines outside of iterative loop
        base <- base |>
          layout(shapes = hlines_list)

    }



    ##### Return both df, base, and other variables required in rest of plotly

    return_list <- list("base" = base, "df" = df,
                        "y_axis_choice" = y_axis_choice,
                        "x_axis_title_font" = x_axis_title_font,
                        "y_axis_title_font" = y_axis_title_font)

    return(return_list)


}


