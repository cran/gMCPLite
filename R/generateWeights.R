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

#' generateWeights
#'
#' compute Weights for each intersection Hypotheses in the closure of a graph
#' based multiple testing procedure
#'
#'
#' @param g Graph either defined as a matrix (each element defines how much of the
#' local alpha reserved for the hypothesis corresponding to its row index is
#' passed on to the hypothesis corresponding to its column index), as \code{graphMCP}
#' object or as \code{entangledMCP} object.
#' @param w Vector of weights, defines how much of the overall alpha is
#' initially reserved for each elementary hypothesis. Can be missing if \code{g}
#' is a \code{graphMCP} object (in which case the weights from the graph object are used).
#' Will be ignored if \code{g} is an \code{entangledMCP} object (since then the matrix
#' of weights from this object is used).
#' @return Returns matrix with each row corresponding to one intersection
#' hypothesis in the closure of the multiple testing problem. The first half of
#' elements indicate whether an elementary hypotheses is in the intersection
#' (1) or not (0). The second half of each row gives the weights allocated to
#' each elementary hypotheses in the intersection.
#' @author Florian Klinglmueller <float@@lefant.net>, Kornelius Rohmeyer \email{rohmeyer@@small-projects.de}
#' @references Bretz F, Maurer W, Brannath W, Posch M; (2008) - A graphical
#' approach to sequentially rejective multiple testing procedures. - Stat Med -
#' 28/4, 586-604 Bretz F, Posch M, Glimm E, Klinglmueller F, Maurer W, Rohmeyer
#' K; (2011) - Graphical approaches for multiple endpoint problems using
#' weighted Bonferroni, Simes or parametric tests - to appear
#' @keywords htest
#' @examples
#'
#'  g <- matrix(c(0,0,1,0,
#'                0,0,0,1,
#'                0,1,0,0,
#'                1,0,0,0), nrow = 4,byrow=TRUE)
#'  ## Choose weights
#'  w <- c(.5,.5,0,0)
#'  ## Weights of conventional gMCP test:
#'  generateWeights(g,w)
#'
#' g <- Entangled2Maurer2012()
#' generateWeights(g)
#'
#' @export generateWeights
#'
generateWeights <- function(g,w){
  if ("entangledMCP" %in% class(g)) {
    mL <- getMatrices(g)
    wL <- getWeights(g)
    split <- g@weights
    result <- 0
    for (i in 1:length(mL)) {
      m <- mL[[i]]
      w <- wL[i,]
      result <- result + split[i]*generateWeights(m, w)
    }
    n <- dim(m)[1]
    # If weights don't sum up to one:
    result[,1:n][result[,1:n]>0] <- 1
    return(result)
  } else if ("graphMCP" %in% class(g)) {
    if (missing(w)) {
      w <- getWeights(g)
    }
    g <- getMatrix(g)
  }
  ## compute all intersection hypotheses and corresponding weights for a given graph
  n <- length(w)
  intersect <- (permutations(n))[-1,]
  g <- apply(intersect,1,function(i) list(int=i,
                                          w=mtp.weights(i,g,w)#, g=mtp.edges(i,g,w)
                                     ))
  m <- as.matrix(as.data.frame(lapply(g,function(i) c(i$int,i$w))))
  colnames(m) <- NULL
  t(m)
}
