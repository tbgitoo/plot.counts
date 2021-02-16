\name{multipleHistograms}
\alias{multipleHistograms}

\title{
multipleHistograms
}
\description{
Draws several histograms on the plot area. They are arranged vertically, and share a common x-axis. This a convenience function, it calls several \code{\link{hist}} for each group as defined by the argument \code{levels}
}
\usage{
multipleHistograms(x,levels, breaks ="Sturges",overhead=0.2,xlab="",category_labels=NULL,add.legend=FALSE,legend.text=NULL,
	legend.args=vector(mode="list",length=0),barcolors=NULL,cex.axis=1,FUN=NULL,...)
}

\arguments{
  \item{x}{
Observations for the counts, as for \code{\link{hist}}. Vector of numerical values.
}
  \item{levels}{
Assignment of the observations to the different histograms. Must be a vector of the same length as \code{x}; for each unique value in \code{levels}, a histogram will be drawn via a call to \code{\link{hist}} for the counts and \code{\link{barplot}} for the actual drawing
}
  \item{breaks}{
Algorithm or direct indication of the breaks for the histogram counts as for \link{hist}
}
  \item{overhead}{
Graphical parameter for the spacing between the figures. The larger, the more stuffed the page will appear
}
  \item{xlab}{
Label for the x-axis as for general plotting functions (cf. \link{plot})
}
  \item{category_labels}{
Labels to be displayed along the bottom-most histogram
}
\item{add.legend}{
Whether or not a legend should be added}
\item{legend.text}{
Text for legend. Corresponds to the \code{legend} argument of the function \link{legend} which is called for adding the legend}
\item{legend.args}{
Additional arguments to be passed down to \link{legend}. Must be a named list}
\item{barcolors}{
Colors for the histogram. Should be a vector with as many elements as there are unique values in \code{levels}. The colors are hexadecimal values of the type "#808AC0", with RGB coding}
\item{cex.axis}{ Character expansion for the axis; character expansion in R bascially means the size of some text, here it is the size of the category labels appearing under the axis)
 }
 \item{FUN}{Call back function. If provided, will be called for each histogram. The idea is that in this fashion, additional things can be drawn on a histogram.\cr This function must take three arguments: \code{FUN(level,histogram_info,x_vals)}. The first argument, \code{level}, indicates the level associated with the current histogram; \code{histogram_info} gives the information concerning the current counts (corresponds to the return of \code{hist}); \code{x_vals} gives the local x-coordinates associated with the center of the bars for the current histogram (e.g., this is the result of the local calls of \link{barplot})} 
  \item{\dots}{
Additional graphical parameters to pass down to the underlying \code{\link{barplot}} function, which does the actual drawing
}
}

\author{
Thomas Braschler
}

\keyword{ misc }

