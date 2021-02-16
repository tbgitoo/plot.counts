\name{get_associated_values}
\alias{get_associated_values}
\title{get_associated_values
}
\description{
Looks up corresponding lines in a reference table
}
\usage{
get_associated_values(descriptive_data,lookup_data,lookup_name_correspondence=NULL,FUN=NULL,lookup_value_col=NULL,...)
}

\arguments{
 \item{descriptive_data}{Base table indicating the measurements for which a reference value should be found. Is typically a dataframe or matrix }
 \item{lookup_data}{Reference table in which the lookup should be carried out . Is typically a dataframe or matrix; usually, it will have different dimensions than the \code{descriptive_data} table}
 \item{lookup_name_correspondence}{Translation table for column correspondence in \code{descriptive_data} and \code{lookup_data}. Should be a matrix of two columns, the first column containing the
 column names or indices that should be taken into account for \code{descriptive_data}, the second column being the corresponding column names or indices for \code{lookup_name_correspondence}}
 \item{FUN}{Aggregation function. Based on the translation table \code{lookup_name_correspondence}, \code{get_associated_values} will run row by row through \code{descriptive_data}, trying to find matching rows
 in \code{lookup_name_correspondence} based on the translation indicated in \code{lookup_name_correspondence}. For each of these lines, the element designated by \code{lookup_value_col} is selected, and 
 \code{FUN} is applied to a vector containing all these elements. Hence, \code{FUN} should accept a vector as its input, and return a single value. By default, \code{FUN} is chosen to be \code{mean}, such that the average 
 value for the matching rows in \code{lookup_data} is calculated for every row in \code{descriptive_data}
 }
 \item{lookup_value_col}{Column containing the reference values of interest in \code{lookup_data}; by default, this is assumed to be the last column of \code{lookup_data}}
 \item{...}{Additional arguments to be passed on to \code{FUN}}
}

\details{
The function can be thought of as an SQL join between \code{descriptive_data} and \code{lookup_data}, the matching condition being that the values in the columns listed in \code{lookup_name_correspondence} should be identical. The column to be selected in \code{lookup_data} is given by \code{lookup_value_col}. As in general, there may be several rows in \code{lookup_data} matching a given row in \code{descriptive_data}, the values selected 
need to be aggregated into a single numerical value; this is performed by \code{FUN}   
}
\value{
Of vector with length equal to the number of rows in \code{descriptive_data}
}

\author{
Thomas Braschler
}


\examples{
descriptive = data.frame(matching_col1=c(1,1,2,2,3,3), matching_col2=c(16,17,16,17,16,17),irrelevant_col1=c(1,1,1,1,1,1),irrelevant_col2=c(16,18,20,15,2,-1))
lookup=data.frame(match_col_1=c(1,1,1,2,2,2,3,3,3),match_col_2=c(16,17,17,16,17,17,16,17,17),value_col=c(1,2,2.5,4,3.5,3,6,5.5,NA),some_col=c(5,5,5,4,3,2,1,2,3))
lookup_name_correspondence=matrix(data=c("matching_col1","match_col_1","matching_col2","match_col_2"),ncol=2,byrow=TRUE)
lookup_value_col="value_col"
assoc_vals=get_associated_values(descriptive_data=descriptive,lookup_data=lookup,lookup_name_correspondence=lookup_name_correspondence,FUN=sum,lookup_value_col=lookup_value_col,na.rm=TRUE)
descriptive_and_associated=descriptive
descriptive_and_associated$associated = assoc_vals
cat("The base data: ")
descriptive
cat("The lookup table:")
lookup
cat("The associated values found (here, the sum of corresponding lines):")
assoc_vals
cat("The associated values in comparison with the base data:")
descriptive_and_associated
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }

