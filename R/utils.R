#' Function to scale/shift secondary y axis labels
#'
#' @param x the value to be scaled/shifted
#' @param scale the value to scale the axis by
#' @param shift the value to shift the axis by
#'
#' @return scaled/shifted y axis values
#'
#' @examples
#' \dontrun{
#' scale_function(2,4,5)
#' }
scale_function <- function(x, scale, shift){
  return ((x)*scale - shift)
}




#' Function to scale/shift secondary y axis data
#'
#' @param x the value to be scaled/shifted
#' @param scale the value to scale the data by
#' @param shift the value to shift the data by
#'
#' @return scaled/shifted data to plot on secondary y axis
#'
#' @examples
#' \dontrun{
#' inv_scale_function(2,4,5)
#' }
inv_scale_function <- function(x, scale, shift){
  return ((x + shift)/scale)
}



#' Function for determining user's operating system
#' credit: https://www.r-bloggers.com/2015/06/identifying-the-os-from-r/
#'
#' @return Name of user's operating system
#'
#' @examples
#' \dontrun{
#' get_os()
#' }
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}




#' Function to set chart_font variable to Arial in order to resolve warnings
#'
#' @return chart_font string
#'
#' @examples
#' \dontrun{
#' set_Arial()
#' }
set_Arial <- function() {
  if(get_os()[[1]] == "windows") {
    windowsFonts("Arial" = windowsFont("Arial"))
    chart_font <- "Arial"
  } else if(get_os()[[1]] == "osx") {
    chart_font <- "Arial"
  } else {
    # Arial not included with linux as standard, so default to sans
    chart_font <- "sans"
  }
  assign("chart_font", chart_font, envir = parent.frame())
}





#' Function to assign list elements to variables within the parent
#' environment. Compares input list against a reference vector of expected
#' list elements, and assigns any that it cannot find a value of 'NULL'.
#'
#' @param params List of input parameters.
#' @param reference Character vector of expected parameters.
#'
#' @return Assigns parameter values in the parent environment
#'
#' @examples
#' \dontrun{
#' param_assign(params, c("df","x","y","legend","title"))
#' }
param_assign <- function(params, reference) {

  # Define parameters from params list
  for(i in 1:length(params)) {
    assign(names(params)[i], params[[i]], envir = parent.frame())
  }

  # Set any unused parameter values to NULL
  unused <- setdiff(reference, names(params))

  if (length(unused) > 0) {
    for(i in 1:length(unused)) {
      assign(unused[i], NULL, envir = parent.frame())
    }
  }

}






#' Function to apply html bold tags to input text strings for use in
#' plotly font definitions.
#'
#' @param x A text string
#'
#' @return A text string wrapped in <b> </b> for html bold font
#'
#' @examples
#' \dontrun{
#' html_bold("Chart Title")
#' }
html_bold <- function(x) {
  x <- paste0("<b>",x,"</b>")
}




#' Function to translate ggplot linetypes into plotly linetypes
#'
#' @param x A text string ggplot line type ("solid", "dotted", "dashed",
#' "longdash", "dotdash")
#'
#' @return A text string plotly line type
#'
#' @examples
#' \dontrun{
#' plotly_line_style("dotdash")
#' }
plotly_line_style <- function(x) {

  # Define valid line types
  valid_line_types <-
      c("solid", "dotted", "dashed", "longdash", "dotdash")

  # resolve line style
  if (!(x %in% valid_line_types)) {

    stop(
      "Invalid line type. Please provide one of the following line types:
             solid, dotted, dashed, longdash, dotdash"
    )

  } else {

    plotly_line_style <- switch(
      x,
      "solid" = "solid",
      "dotted" = "dot",
      "dashed" = "dash",
      "longdash" = "longdash",
      "dotdash" = "dashdot",
      "solid"
    )
  }

  return(plotly_line_style)

}





#' Function to convert ggplot scale_date(date_breaks) values to d3 date
#' format for use in plotly dtick variable for date axis break setting.
#'
#' @param x A string giving the distance between breaks like "2 weeks",
#' or "10 years". Valid specifications are 'sec', 'min', 'hour', 'day',
#' 'week', 'month' or 'year', optionally followed by 's'.
#' See https://ggplot2.tidyverse.org/reference/scale_date.html
#'
#' @return A string in a format appropriate for plotly dtick date break formatting.
#'
#'
#' @examples
#' \dontrun{
#' datebreak_to_d3("2 months")
#'
#' datebreak_to_d3("1 week")
#' }
datebreak_to_d3 <- function(x) {

  # Gather string elements
  number <- stringr::word(x , 1)
  interval <- stringr::word(x , 2)

  # If 'day'/'days' selected; convert to milliseconds
  if(interval %in% c('day', 'days')) {

    return(as.numeric(number)*86400000.0)

  # If 'week'/'weeks' selected; convert to multiples of 7-days in milliseconds
  } else if (interval %in% c('week', 'weeks')) {

    return(as.numeric(number)*7*86400000.0)

  # If 'month'/'months' selected; convert to 'Mxxx' format
  } else if (interval %in% c('month', 'months')) {

    return(paste0("M",number))

  # If 'year'/'years' selected; convert to 'Mxxx' format times 12
  } else if (interval %in% c('year', 'years')) {

    return(paste0("M",as.numeric(number)*12))

  # If 'hour'/'hours' selected; convert to milliseconds
  } else if (interval %in% c('hour', 'hours')) {

    return(as.numeric(number)*60*60*1000)

  # If 'minute'/'minutes' selected; convert to milliseconds
  } else if (interval %in% c('minute', 'minutes')) {

    return(as.numeric(number)*60*1000)

  # If 'second'/'seconds' selected; convert to milliseconds
  } else if (interval %in% c('second', 'seconds')) {

    return(as.numeric(number)*1000)

  }
}





#' Function to convert ggplot legend positions into list of
#' parameters for plotly layout
#'
#' @param x A ggplot legend position ("top","bootom","right","left")
#'
#' @return A list of parameters for plotly layout.legend
#'
#' @examples
#' \dontrun{
#' plotly_legend_pos("right")
#' }
plotly_legend_pos <- function(x) {

    # Define cumulative_sum_line for functions that don't use it
    if (!exists("cumulative_sum_line")) {cumulative_sum_line <- FALSE}

    # Define plotly legend orientation
    if (x %in% c("top", "bottom")) {
      legend_orientation <- "h"
    } else {
      legend_orientation <- "v"
    }

    # Configure plotly legend settings
    legend_settings <- switch(
      x,
      "left" = list(
        x = -0.1,
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
        x = if(y_sec_axis == TRUE | cumulative_sum_line == TRUE) {1.1} else {1.02}, # account for secondary y-axis causing legend overlap
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
    )

    return(legend_settings)

}





#' A function to generate a colour palette of n_pal hexcodes based upon an input
#' colour (or colours) \code{x}
#'
#' @param x Either the character name of an RColorBrewer palette (e.g.
#' \code{fill_palette = "YlOrRd"}), a character containing a single rgb
#' colour code, hexcode, or colour name that will be used to generate
#' a colour range (e.g. \code{fill_palette = "#007C91"}), or a character
#' vector containing multiple rgb codes, hexcodes, or colour names that
#' will be used to generate a colour range (e.g. \code{c("#007C91","purple",
#' "red")}). Defaults to the RColorBrewer "Blues" palette.
#' @param n_pal numeric The number of desired colours in the returned palette.
#'
#' @return A character vector of hex codes for use in a colour palette.
#'
#' @examples
#' \dontrun{
#' palette_gen(c("#007C91","purple"),7)
#'
#' palette_gen("#007C91",5)
#'
#' palette_gen("YlOrRd",5)
#' }
palette_gen <- function(x, n_pal) {

  # x is single element; i.e. single hexcode or RColorBrewer palette name
  if (length(x) == 1) {

    # x specified as RColorBrewer palette
    if (x %in% rownames(RColorBrewer::brewer.pal.info)) {

      # Create RColorBrewer palette
      #   RColourBrewer::brewer.pal has a min palette size of 3 and max of 9
      #   -If n_pal <= 2, manually create 2 element palette
      #   -If n_pal >= 9, extend palette with colorRampPalette()
      if (n_pal <= 2) {
        suppressWarnings({
          pal <- RColorBrewer::brewer.pal(n = n_pal, name = x)
        })
        pal <- c(dplyr::first(pal),dplyr::last(pal))
      } else if (n_pal >= 9) {
        pal <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = x))(n_pal)
      } else {
        pal <- RColorBrewer::brewer.pal(n = n_pal, name = x)
      }

      # x specified as single colour
    } else {

      # Combine the single colour with a baseline of white
      x <- c("white", x)
      pal <- colorRampPalette(x)(n_pal)
    }

    # x is multiple element; i.e. x specified as vector of multiple colours
  } else if (length(x) > 1) {
    pal <- colorRampPalette(x)(n_pal)
  }

  return(pal)

}
#' A function to rename an item in a named list
#'
#' @param params_list A named list
#' @param current_name The name of the list item to be modified
#' @param new_name The new name of the list item
#'
#' @return A list with the item renamed
#'
#' @examples
#' \dontrun{
#' params <- param_rename(params,"chart_footer_colour","new_name")
#' }
#'
param_rename <- function(params_list, current_name, new_name) {

  names(params_list)[match(current_name, names(params_list))] <- new_name

  return(params_list)

}





#' A function to add assorted date lookups to an existing
#' dataframe with a date variable.
#'
#' @param df A dataframe
#' @param date_var character, the name of a date column within df
#'
#' @return A dataframe
#'
#' @examples
#' \dontrun{
#' dataframe_out <- adorn_date(df = dataframe, date_var = "date_column")
#' }
adorn_dates <- function(df, date_var) {

  df <- df |>
    mutate(day = as.Date(get(date_var)),
           month = format(day, "%m"),
           year = format(day, "%Y"),
           quarter = sprintf("%02d", lubridate::quarter(get(date_var))),  ###query: correct quarter for UK/epi?
           year_month = format(day, "%Y-%m"),
           #year_quarter = format(day, "%Y/0%q"),
           year_quarter = paste0(year,'-Q',quarter),
           iso_week_full = ISOweek::ISOweek(get(date_var)),
           iso_week = lubridate::isoweek(get(date_var)),
           iso_year = lubridate::isoyear(get(date_var)),
           iso_year_week = paste0(iso_year,'-W',iso_week),
           start_iso_year_week = as.character(floor_date(get(date_var), "weeks", week_start = 1)) ### from GAS dashboard, check
           )

  return(df)

}
