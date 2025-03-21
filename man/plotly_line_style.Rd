% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{plotly_line_style}
\alias{plotly_line_style}
\title{Function to translate ggplot linetypes into plotly linetypes}
\usage{
plotly_line_style(x)
}
\arguments{
\item{x}{A text string ggplot line type ("solid", "dotted", "dashed",
"longdash", "dotdash")}
}
\value{
A text string plotly line type
}
\description{
Function to translate ggplot linetypes into plotly linetypes
}
\examples{
\dontrun{
plotly_line_style("dotdash")
}
}
\keyword{internal}
