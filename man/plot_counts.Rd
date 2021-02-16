\name{plot_counts}
\alias{plot_counts}

\title{
plot_counts
}
\description{
Plots count data with errorbars, significance levels and regression model
}
\usage{
	             
plot_counts(x,y,sd_y=NULL,sig_codes=NULL,showlinreg="NONE", plot.new = TRUE, groups=NULL, group_order = NULL, weights=NULL, category_bounds=NULL,sig.cex=1,sd_x=NULL,...)
}

\arguments{
  \item{x}{
  x values
}
  \item{y}{
 associated y values
}
  \item{sd_y}{
 standard deviation of the y values
}

 \item{sig_codes}{
 Codes to indicate significances levels above each data point
} 
 
\item{showlinreg}{
Indicates what type of regression line should be shown. Options are "LINEAR", "SQUARE", and "NONE"
}
 
 \item{plot.new}{
If true, a new plot is drawn, if false, the line is added to the existing active plot
}

 \item{groups}{
 Vector of the same length as x, y and sd_y, indicates to which group the elements belong. Leave out if there the values are not grouped
}

 \item{group_order}{
 Indicating the order in which the groups should be plotted. This is important for the coloring of the lines. If no \code{col} argument is provided in the optional argument, the standard colors
 available via \code{palette()} will be used, otherwise the colors indicated with the "col" argument are used. The \code{col} argument should then be a vector of the same length as the \code{group_order} argument
}

\item{weights}{
Explicit specification of the weights for the linear regression (useful only if a linear regression model is chosen). By default, the weights are taken to be inversely proportional to the standard deviation of the data points.

}
\item{category_bounds}{If supplied, the points are grouped into categories, using the \code{x} values and the \code{category_bounds} to assign the \code{y} values. For each category, the mean \code{y} value is then displayed at the category midpoint x value, along with errorbars as calculated by \link{sd_mean}
}

\item{sig.cex}{Character expansion (cex) for the significance labels. Relevant only if \code{sig_codes} is supplied}

\item{sd_x}{Standard deviation of x values if available}

\item{...}{
Additional graphical parameters, such as xlab, ylab, col
}
}
\details{
The function is intended to handle an x vector with ascending, unique values, and associated y data
}
\author{
Thomas Braschler
}

\keyword{ misc }

\examples{
plot_counts(x=c(1,2,3),y=c(2,2,3),sd_y=c(1,1,0.5))

}
