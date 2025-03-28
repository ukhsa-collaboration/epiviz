% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/col_chart.R
\name{col_chart}
\alias{col_chart}
\title{Column Chart}
\usage{
col_chart(
  dynamic = FALSE,
  base = NULL,
  params = list(df = NULL, x = NULL, y = NULL, group_var = NULL, fill = "blue", y_axis =
    "y1", position = "dodge", ci = NULL, lower = NULL, upper = NULL, error_colour =
    "red", h_line = NULL, y_label = NULL, x_label = NULL, x_label_angle = NULL,
    y_label_angle = NULL, x_labels_reverse = NULL, y_min_limit = NULL, y_max_limit =
    NULL, x_axis_breaks = NULL, legend_pos = "bottom", remove_gridlines = NULL, percent =
    NULL, cap_text = NULL, no_shift = FALSE),
  ...
)
}
\arguments{
\item{dynamic}{Logical indicating whether to produce a dynamic (plotly) output.
Default is FALSE, which will return a static ggplot output.}

\item{base}{A base ggplot object to add the column chart to. Default is NULL.}

\item{params}{A named list containing arguments used to create the plot:
\itemize{
\item df: The dataframe containing the data to be plotted
\item x: The x value column name to be plotted
\item y: The y value column name to be plotted
\item group_var: The variable used to group the bars i.e. region if plotting by region
\item fill: The colour with which to fill the columns
\item y_axis: Either "y1" for primary y-axis or "y2" for secondary y-axis
\item position: The positions of the bars to be plotted i.e."dodge", "stack" etc
\item ci: Indicator for using ribbon or error bar geom (if required), enter 'e' for error bar, enter any other value for ribbon
\item lower: Lower value for error/ribbon geom (mandatory if ci argument passed)
\item upper: Upper value for error/ribbon geom (mandatory if ci argument passed)
\item error_colour: If not plotting by group this is the colour of the error bars or ribbon
\item h_line: Will display a horizontal line if valid integer passed
\item y_label: For provision of an y axis label
\item x_label: For provision of an x axis label
\item x_label_angle: To adjust the x axis label by the degrees of the integer provided
\item y_label_angle: To adjust the y axis label by the degrees of the integer provided
\item x_labels_reverse: Enter an argument of any value i.e. 'y' to reverse the x labeling order
\item y_min_limit: Set the limit on the y axis scaling by proving an integer
\item y_max_limit: Set the limit on the x axis scaling by proving an integer
\item x_axis_breaks: Modify the x axis breaks by providing an integer
\item legend_pos: Modify the position of the legend (where applicable)
\item remove_gridlines: Enter an argument of any value i.e. 'y' to remove grid lines
\item percent: Enter an argument of any value i.e. 'y' to include the \% symbol
\item cap_text: Enter text for a caption to appear below plot
\item no_shift: If no shift should be applied to the secondary y-axis
}}

\item{...}{Additional arguments passed to geom_col for static (ggplot2) plots
or to plot_ly/add_trace for dynamic (Plotly) plots, allowing custom
styling of the columns (e.g., alpha, width, marker, etc.).}
}
\value{
A ggplot or plotly object depending on the value of dynamic parameter
}
\description{
Creates a column chart using either ggplot2 (static) or plotly (dynamic).
}
\examples{
\dontrun{
library(dplyr)
data <- epiviz::lab_data |>
 group_by(organism_species_name) |>
 summarise(Count=n())

col_chart(params = list(
  df = data,
  x = "organism_species_name",
  y = "Count"
))
}
}
