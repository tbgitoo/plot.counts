\name{barplot_xpos}
\alias{barplot_xpos}

\title{
barplot_xpos
}
\description{
Draws a barplot with the bars at specific x-locations rather than regularly spread
}
\usage{
barplot_xpos(height, xpos=NULL,width = 0.5,col = NULL, border = par("fg"), main=NULL, sub=NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,  axes = TRUE, cex.axis = par("cex.axis"), plot.new=TRUE,   density=NULL, angle=45, at_x=NULL, at_y=NULL, labels_x = NULL, labels_y=NULL, ...)
                            
}

\arguments{
  \item{height}{
The heights of the bars, should be a vector
}
  \item{xpos}{
The central positions of the bars, should be a vector of length identical to the \code{heights} vector
}
  \item{width}{
Width of the bars, can be either a single number or a vector of length identical to the \code{heights} vector  
}
  \item{col}{
Color of the bars, either single character string or vector of length identical to the \code{heights} vector
}
  \item{border}{
Border color of the bars
}
  \item{main}{
Main title of the plot
}
  \item{sub}{
Subtitle of the plot
}
  \item{xlab}{
Label for the x-axis
}
  \item{ylab}{
Lable for the y-axis
}
  \item{xlim}{
Limits for the x-axis, numeric vector of length 2
}
  \item{ylim}{
Limits for the y-axis, numeric vector of length 2
}
  \item{axes}{
Logical indicating whether axes should be drawn
}

  \item{cex.axis}{
expansion factor for numeric axis labels (whatever that's supposed to mean, see \link{barplot}
}
  \item{plot.new}{
Whether the bars should be added on an existing plot or whether a new plot should be drawn
}
 \item{density}{
If provided, density of the hashing lines
}
  \item{angle}{
If density is provided, angle of the hashing lines
}
\item{at_x}{If provided, tick locations for the x-axis (see \link{axis})}
\item{at_y}{If provided, tick locations for the y-axis (see \link{axis})}
\item{labels_x}{If provided, tick labels for the x-axis (see \link{axis})}
\item{labels_y}{If provided, tick locations for the y-axis (see \link{axis})}
  \item{\dots}{
Additional graphical parameters passed on to \code{\link{plot.window}}, \code{\link{title}} or \code{\link{axis}}
}
}

\author{
Thomas Braschler
}


\keyword{ misc }

