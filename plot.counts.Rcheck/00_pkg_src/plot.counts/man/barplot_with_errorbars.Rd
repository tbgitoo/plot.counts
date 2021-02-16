\name{barplot_with_errorbars}
\alias{barplot_with_errorbars}

\title{
barplot_with_errorbars
}
\description{
Bar plots for count data with errorbars and significance levels
}
\usage{
barplot_with_errorbars(height, sd_height=NULL, beside = FALSE, horiz = FALSE,group_order=NULL,sig_codes=NULL,sig.cex=1, ...)
}

\arguments{
  \item{height}{
 As in barplot: Either a vector or matrix of values describing the bars which make up the plot. If height is a vector, the plot consists of a sequence of rectangular bars with heights given by the values in the vector. If height is a matrix and beside is FALSE then each bar of the plot corresponds to a column of height, with the values in the column giving the heights of stacked sub-bars making up the bar. If height is a matrix and beside is TRUE, then the values in each column are juxtaposed rather than stacked.
}
  \item{sd_height}{
 Errors associated with the values, should have the same dimensions as \code{height}
}
  \item{beside}{
 As in barplot: a logical value. If FALSE, the columns of height are portrayed as stacked bars, and if TRUE the columns are portrayed as juxtaposed bars.
}

 \item{horiz}{
 As in barplot: a logical value. If FALSE, the bars are drawn vertically with the first bar to the left. If TRUE, the bars are drawn horizontally with the first at the bottom.
} 
 \item{group_order}{
 If provided, allows to reorder the columns of \code{height} and hence to change the order of the bars} 
\item{sig_codes}{
 Significance codes (or any other text) to be shown above the bars
} 
\item{sig.cex}{Character expansion (cex) for the significance labels. Relevant only if \code{sig_codes} is supplied} 
 
 
\item{...}{
Additional graphical parameters, such as xlab, ylab, will be passed to barplot, or, like code or angle, to the \link{arrows} function used to produce the errorbars
}
}

\author{
Thomas Braschler
}

\keyword{ misc }
