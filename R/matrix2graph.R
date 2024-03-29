#    Copyright (c) 2022 Merck & Co., Inc., Rahway, NJ, USA and its affiliates. All rights reserved.
#
#    This file is part of the gMCPLite program.
#
#    gMCPLite is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 
#' Matrix2Graph and Graph2Matrix
#'
#' Creates a graph of class \code{graphMCP} from a given transition
#' matrix or vice versa.
#'
#' The hypotheses names are the row names or if these are \code{NULL}, the
#' column names or if these are also \code{NULL} of type H1, H2, H3, ...
#'
#' If the diagonal of the matrix is unequal zero, the values are ignored and a
#' warning is given.
#'
#' @aliases matrix2graph graph2matrix
#' @param m A transition matrix.
#' @param weights A numeric for the initial weights.
#' @param graph A graph of class \code{graphMCP}.
#' @return A graph of class \code{graphMCP} with the given transition
#' matrix for matrix2graph.  The transition matrix of a \code{graphMCP}
#' graph for graph2matrix.
#' @author Kornelius Rohmeyer \email{rohmeyer@@small-projects.de}
#' @keywords graphs
#' @examples
#'
#'
#' # Bonferroni-Holm:
#' m <- matrix(rep(1/3, 16), nrow=4)
#' diag(m) <- c(0, 0, 0, 0)
#' graph <- matrix2graph(m)
#' print(graph)
#' graph2matrix(graph)
#'
#'
#' @export matrix2graph

matrix2graph <- function(m, weights=rep(1/dim(m)[1],dim(m)[1])) {
  # Checking for 0 on diagonal:
  if (!(all(TRUE == all.equal(unname(diag(m)), rep(0, length(diag(m)))))
        || all(TRUE == all.equal(unname(diag(m)), rep("0", length(diag(m))))))) {
    warning("Matrix has a diagonal not equal to zero. Loops are not allowed.")
    diag(m) <- rep(0, length(diag(m)))
  }
  # Creating graph without edges:
  if (dim(m)[1]!=dim(m)[2]) stop("Matrix has to be quadratic.")
  hnodes <- rownames(m)
  if (is.null(hnodes)) hnodes <- colnames(m)
  if (is.null(hnodes)) hnodes <- paste("H",1:(dim(m)[1]),sep="")
  rownames(m) <- colnames(m) <- hnodes
  graph <- new("graphMCP", m=m, weights=weights)
  return(graph)
}

#' @rdname matrix2graph
#' @export graph2matrix
graph2matrix <- function(graph) {
  if (class(graph) %in% "graphMCP") {
    return(graph@m)
  } else if (class(graph) %in% "entangledMCP"){
    # TODO What do we want to return in this case?
    warning("graph2matrix only returns the transition matrix of the first subgraph.")
    return(graph@subgraphs[[1]]@m)
  } else {
    stop("This function should only be used for objects of class graphMCP or entangledMCP.")
  }
}
