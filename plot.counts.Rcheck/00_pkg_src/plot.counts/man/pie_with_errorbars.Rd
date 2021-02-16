\name{pie_with_errorbars}
\alias{pie_with_errorbars}

\title{
pie_with_errorbars
}
\description{
Plots a pie chart with the option of placing errorbars
}
\usage{
	             
pie_with_errorbars(x,sd_x=NULL, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE, 
init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
col = NULL, border = NULL, lty = NULL, main = NULL, initiate_plot=TRUE,col_errorbar=NULL,...) 
}

\arguments{
  \item{x}{As in \link{pie}: A vector of non-negative numerical quantities. The values in x are displayed as the areas of pie slices
}
  \item{sd_x}{Errorbars to be displayed for each slice.
}
\item{labels}{As in \link{pie}: one or more expressions or character strings giving names for the slices.}
\item{edges}{As in \link{pie}: the circular outline of the pie is approximated by a polygon with this many edges.}
\item{radius}{As in \link{pie}: the pie is drawn centered in a square box whose sides range from -1 to 1. If the character strings labeling the slices are long it may be necessary to use a smaller radius.}
\item{clockwise}{As in \link{pie}: logical indicating if slices are drawn clockwise or counter clockwise (i.e., mathematically positive direction), the latter is default.}
\item{init.angle}{As in \link{pie}: number specifying the starting angle (in degrees) for the slices. Defaults to 0 (i.e.,3 o clock) unless clockwise is true where init.angle defaults to 90 (degrees), (i.e., 12 o clock).}
\item{density}{As in \link{pie}: the density of shading lines, in lines per inch. The default value of NULL means that no shading lines are drawn. Non-positive values of density also inhibit the drawing of shading lines.}
\item{angle}{As in \link{pie}: the slope of shading lines, given as an angle in degrees (counter-clockwise).}
\item{col}{As in \link{pie}: a vector of colors to be used in filling or shading the slices. If missing a set of 6 pastel colours is used, unless density is specified when par("fg") is used.}
\item{border}{As in \link{pie}: (possibly vector) argument passed to \link{polygon} which draws each slice.}
\item{lty}{As in \link{pie}: (possibly vector) argument passed to \link{polygon} which draws each slice.}
\item{main}{As in \link{pie}: an overall title for the plot.}
\item{initiate_plot}{If true, a new plot is drawn, otherwise the function draws onto the currently active plot}
\item{col_errorbar}{As in \link{pie}: Colors for the errorbars}

  \item{...}{
 All additional arguments have the same meaning as for the underlying \link{pie} function.
}

}
\author{
Thomas Braschler
}

\keyword{ misc }

\examples{
pie_with_errorbars(x=c(1,2,3),sd_x=c(0.1,0.2,0))

}
