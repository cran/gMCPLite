% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{placeNodes}
\alias{placeNodes}
\title{Placement of graph nodes}
\usage{
placeNodes(graph, nrow, ncol, byrow = TRUE, topdown = TRUE, force = FALSE)
}
\arguments{
\item{graph}{A graph of class \code{graphMCP} or class
\code{entangledMCP}.}

\item{nrow}{The desired number of rows.}

\item{ncol}{The desired number of columns.}

\item{byrow}{Logical whether the graph is filled by rows (otherwise by
columns).}

\item{topdown}{Logical whether the rows are filled top-down or bottom-up.}

\item{force}{Logical whether a graph that has already a layout should be
given the specified new layout.}
}
\value{
The graph with nodes placed according to the specified layout.
}
\description{
Places the nodes of a graph according to a specified layout.
}
\details{
If one of \code{nrow} or \code{ncol} is not given, an attempt is made to
infer it from the number of nodes of the \code{graph} and the other
parameter.  If neither is given, the graph is placed as a circle.
}
\examples{


g <- matrix2graph(matrix(0, nrow=6, ncol=6))

g <- placeNodes(g, nrow=2, force=TRUE)


}
\seealso{
\code{graphMCP}, \code{entangledMCP}
}
\author{
Kornelius Rohmeyer \email{rohmeyer@small-projects.de}
}
\keyword{graphs}
