% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix2graph.R
\name{matrix2graph}
\alias{matrix2graph}
\alias{graph2matrix}
\title{Matrix2Graph and Graph2Matrix}
\usage{
matrix2graph(m, weights = rep(1/dim(m)[1], dim(m)[1]))

graph2matrix(graph)
}
\arguments{
\item{m}{A transition matrix.}

\item{weights}{A numeric for the initial weights.}

\item{graph}{A graph of class \code{graphMCP}.}
}
\value{
A graph of class \code{graphMCP} with the given transition
matrix for matrix2graph.  The transition matrix of a \code{graphMCP}
graph for graph2matrix.
}
\description{
Creates a graph of class \code{graphMCP} from a given transition
matrix or vice versa.
}
\details{
The hypotheses names are the row names or if these are \code{NULL}, the
column names or if these are also \code{NULL} of type H1, H2, H3, ...

If the diagonal of the matrix is unequal zero, the values are ignored and a
warning is given.
}
\examples{


# Bonferroni-Holm:
m <- matrix(rep(1/3, 16), nrow=4)
diag(m) <- c(0, 0, 0, 0)
graph <- matrix2graph(m)
print(graph)
graph2matrix(graph)


}
\author{
Kornelius Rohmeyer \email{rohmeyer@small-projects.de}
}
\keyword{graphs}
