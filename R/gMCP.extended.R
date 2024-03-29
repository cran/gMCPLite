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

#' Weighted Test Functions for use with gMCP
#'
#' The package gMCP provides the following weighted test functions:
#' \describe{
#'   \item{bonferroni.test}{Bonferroni test - see \code{?bonferroni.test} for details.}
#'   \item{parametric.test}{Parametric test - see \code{?parametric.test} for details.}
#'   \item{simes.test}{Simes test - see \code{?simes.test} for details.}
#'   \item{bonferroni.trimmed.simes.test}{Trimmed Simes test for intersections of two hypotheses and otherwise Bonferroni - see \code{?bonferroni.trimmed.simes.test} for details.}
#'   \item{simes.on.subsets.test}{Simes test for intersections of hypotheses from certain sets and otherwise Bonferroni - see \code{?simes.on.subsets.test} for details.}
#' }
#'
#' Depending on whether \code{adjPValues==TRUE} these test functions return different values:
#' \itemize{
#'   \item If \code{adjPValues==TRUE} the minimal value for alpha is returned for which the null hypothesis can be rejected. If that's not possible (for example in case of the trimmed Simes test adjusted p-values can not be calculated), the test function may throw an error.
#'   \item If \code{adjPValues==FALSE} a logical value is returned whether the null hypothesis can be rejected.
#' }
#'
#' To provide your own test function write a function that takes at least the following arguments:
#' \describe{
#'   \item{pvalues}{A numeric vector specifying the p-values.}
#'   \item{weights}{A numeric vector of weights.}
#'   \item{alpha}{A numeric specifying the maximal allowed type one error rate. If \code{adjPValues==TRUE} (default) the parameter \code{alpha} should not be used.}
#'   \item{adjPValues}{Logical scalar. If \code{TRUE} an adjusted p-value for the weighted test is returned (if possible - if not the function should call \code{stop}).
#' Otherwise if \code{adjPValues==FALSE} a logical value is returned whether the null hypothesis can be rejected.}
#'   \item{...}{ Further arguments possibly passed by \code{gMCP} which will be used by other test procedures but not this one.}
#' }
#'
#' Further the following parameters have a predefined meaning:
#' \describe{
#'   \item{verbose}{Logical scalar. If \code{TRUE} verbose output should be generated and printed to the standard output}
#'   \item{subset}{}
#'   \item{correlation}{}
#' }
#'
#' @name weighted.test.functions
#' @author Kornelius Rohmeyer \email{rohmeyer@@small-projects.de}
#' @examples
#'
#' # The test function 'bonferroni.test' is used in by gMCP in the following call:
#' graph <- BonferroniHolm(4)
#' pvalues <- c(0.01, 0.05, 0.03, 0.02)
#' alpha <- 0.05
#' r <- gMCP.extended(graph=graph, pvalues=pvalues, test=bonferroni.test, verbose=TRUE)
#'
#' # For the intersection of all four elementary hypotheses this results in a call
#' bonferroni.test(pvalues=pvalues, weights=getWeights(graph))
#' bonferroni.test(pvalues=pvalues, weights=getWeights(graph), adjPValues=FALSE)
#'
#' # bonferroni.test function:
#' bonferroni.test <- function(pvalues, weights, alpha=0.05, adjPValues=TRUE, verbose=FALSE, ...) {
#'   if (adjPValues) {
#'     return(min(pvalues/weights))
#'   } else {
#'     return(any(pvalues<=alpha*weights))
#'   }
#' }
#'
NULL


#' Weighted Bonferroni-test
#'
#' @param pvalues A numeric vector specifying the p-values.
#' @param weights A numeric vector of weights.
#' @param adjPValues Logical scalar. If \code{TRUE} (the default) an adjusted p-value for the weighted Bonferroni-test is returned.
#' Otherwise if \code{adjPValues==FALSE} a logical value is returned whether the null hypothesis can be rejected.
#' @param alpha A numeric specifying the maximal allowed type one error rate. If \code{adjPValues==TRUE} (default) the parameter \code{alpha} is not used.
#' @param verbose Logical scalar. If \code{TRUE} verbose output is generated.
#' @param ... Further arguments possibly passed by \code{gMCP} which will be used by other test procedures but not this one.
#' @return adjusted p-value or decision of rejection
#' @examples
#'
#' bonferroni.test(pvalues=c(0.1,0.2,0.05), weights=c(0.5,0.5,0))
#' bonferroni.test(pvalues=c(0.1,0.2,0.05), weights=c(0.5,0.5,0), adjPValues=FALSE)
#'
#' @export
bonferroni.test <- function(pvalues, weights, alpha=0.05, adjPValues=TRUE, verbose=FALSE, ...) { #TODO Do we really need ... ?
  if (adjPValues) {
    return(min(pvalues/weights))
  } else {
    return(any(pvalues<=alpha*weights))
  }
}

#' Weighted parametric test
#'
#' It is assumed that under the global null hypothesis
#' \eqn{(\Phi^{-1}(1-p_1),...,\Phi^{-1}(1-p_m))} follow a multivariate normal
#' distribution with correlation matrix \code{correlation} where
#' \eqn{\Phi^{-1}} denotes the inverse of the standard normal distribution
#' function.
#'
#' For example, this is the case if \eqn{p_1,..., p_m} are the raw p-values
#' from one-sided z-tests for each of the elementary hypotheses where the
#' correlation between z-test statistics is generated by an overlap in the
#' observations (e.g. comparison with a common control, group-sequential
#' analyses etc.). An application of the transformation \eqn{\Phi^{-1}(1-p_i)}
#' to raw p-values from a two-sided test will not in general lead to a
#' multivariate normal distribution. Partial knowledge of the correlation
#' matrix is supported. The correlation matrix has to be passed as a numeric
#' matrix with elements of the form: \eqn{correlation[i,i] = 1} for diagonal
#' elements, \eqn{correlation[i,j] = \rho_{ij}}, where \eqn{\rho_{ij}} is the
#' known value of the correlation between \eqn{\Phi^{-1}(1-p_i)} and
#' \eqn{\Phi^{-1}(1-p_j)} or \code{NA} if the corresponding correlation is
#' unknown. For example correlation[1,2]=0 indicates that the first and second
#' test statistic are uncorrelated, whereas correlation[2,3] = NA means that
#' the true correlation between statistics two and three is unknown and may
#' take values between -1 and 1. The correlation has to be specified for
#' complete blocks (ie.: if cor(i,j), and cor(i,j') for i!=j!=j' are specified
#' then cor(j,j') has to be specified as well) otherwise the corresponding
#' intersection null hypotheses tests are not uniquely defined and an error is
#' returned.
#'
#' For further details see the given references.
#'
#' @param pvalues A numeric vector specifying the p-values.
#' @param weights A numeric vector of weights.
#' @param correlation Correlation matrix. For parametric tests the p-values
#' must arise from one-sided tests with multivariate normal distributed test
#' statistics for which the correlation is (partially) known. In that case a
#' weighted parametric closed test is performed (also see
#' \code{\link{generatePvals}}). Unknown values can be set to NA. (See details
#' for more information)
#' @param alpha A numeric specifying the maximal allowed type one error rate. If \code{adjPValues==TRUE} (default) the parameter \code{alpha} is not used.
#' @param adjPValues Logical scalar. If \code{TRUE} (the default) an adjusted p-value for the weighted parametric test is returned.
#' Otherwise if \code{adjPValues==FALSE} a logical value is returned whether the null hypothesis can be rejected.
#' @param verbose Logical scalar. If \code{TRUE} verbose output is generated.
#' @param ... Further arguments possibly passed by \code{gMCP} which will be used by other test procedures but not this one.
#' @return adjusted p-value or decision of rejection
#' @examples
#'
#' parametric.test(pvalues=c(0.1,0.2,0.05), weights=c(0.5,0.5,0), correlation = diag(3))
#' parametric.test(pvalues=c(0.1,0.2,0.05), weights=c(0.5,0.5,0), correlation = diag(3),
#' adjPValues = FALSE)
#'
#' @references
#' Bretz F., Posch M., Glimm E., Klinglmueller F., Maurer W., Rohmeyer K.
#' (2011): Graphical approaches for multiple endpoint problems using weighted
#' Bonferroni, Simes or parametric tests. Biometrical Journal 53 (6),
#' pages 894-913, Wiley.
#' \doi{10.1002/bimj.201000239}
#'
#' @export
parametric.test <- function(pvalues, weights, alpha=0.05, adjPValues=TRUE, verbose=FALSE, correlation, ...) {

  # ToDo Document dropping these dimensions with zero weights
  I <- which(weights>0)
  pvalues <- pvalues[I]
  weights <- weights[I]
  correlation <- correlation[I,I]

  if(length(correlation)>1){
    conn <- conn.comp(correlation)
  } else {
    conn <- 1
  }

  conn <- lapply(conn,as.numeric)
  e <- sapply(1:length(pvalues),function(i) {
    sum(sapply(conn, function(edx){
      if(length(edx)>1){
        return(1-mvtnorm::pmvnorm(lower=-Inf,
                         upper=qnorm(1-pmin(1,(weights[edx]*pvalues[i]/(weights[i])))),
                         corr=correlation[edx,edx],abseps=10^-5))
      } else {
        return((weights[edx]*pvalues[i]/(weights[i]*sum(weights))))
      }
    }))})

  e <- min(c(e,1))

  if (adjPValues) {
    return(e)
  } else {
    return(e<=alpha)
  }
}

#' Trimmed Simes test for intersections of two hypotheses and otherwise weighted Bonferroni-test
#'
#'
#'
#' @param pvalues A numeric vector specifying the p-values.
#' @param weights A numeric vector of weights.
#' @param adjPValues Logical scalar. If \code{TRUE} (the default) an adjusted p-value for the weighted test is returned.
#' Otherwise if \code{adjPValues==FALSE} a logical value is returned whether the null hypothesis can be rejected.
#' @param alpha A numeric specifying the maximal allowed type one error rate. If \code{adjPValues==TRUE} (default) the parameter \code{alpha} is not used.
#' @param verbose Logical scalar. If \code{TRUE} verbose output is generated.
#' @param ... Further arguments possibly passed by \code{gMCP} which will be used by other test procedures but not this one.
#' @return adjusted p-value or decision of rejection
#' @references
#' Brannath, W., Bretz, F., Maurer, W., & Sarkar, S. (2009).
#' Trimmed Weighted Simes Test for Two One-Sided Hypotheses With Arbitrarily Correlated Test Statistics.
#' Biometrical Journal, 51(6), 885-898.
#' @examples
#'
#' bonferroni.trimmed.simes.test(pvalues=c(0.1,0.2,0.05), weights=c(0.5,0.5,0))
#' bonferroni.trimmed.simes.test(pvalues=c(0.1,0.2,0.05), weights=c(0.5,0.5,0), adjPValues=FALSE)
#'
#' @export
bonferroni.trimmed.simes.test <- function(pvalues, weights, alpha=0.05, adjPValues=FALSE, verbose=FALSE, ...) {
  if (adjPValues) stop("Alpha level is needed and adjusted p-values can not be calculated for this test.")
  if (length(pvalues)==2) {
    # Truncated Simes:
    rejected <- (pvalues[1]<=alpha*weights[1] && pvalues[2]<=1-alpha*weights[2]) ||
      (pvalues[2]<=alpha*weights[2] && pvalues[1]<=1-alpha*weights[1]) ||
      max(pvalues)<=alpha
    return(rejected)
    #TODO adjusted p-values?
  } else {
    return(bonferroni.test(pvalues, weights, alpha, adjPValues, ...))
  }
}

#' Simes on subsets, otherwise Bonferroni
#'
#' Weighted Simes test introduced by Benjamini and Hochberg (1997)
#'
#' As an additional argument a list of subsets must be provided, that states in which cases a Simes test is applicable (i.e. if all hypotheses to test belong to one of these subsets), e.g.
#' subsets <- list(c("H1", "H2", "H3"), c("H4", "H5", "H6"))
#' Trimmed Simes test for intersections of two hypotheses and otherwise weighted Bonferroni-test
#'
#' @param pvalues A numeric vector specifying the p-values.
#' @param weights A numeric vector of weights.
#' @param adjPValues Logical scalar. If \code{TRUE} (the default) an adjusted p-value for the weighted test is returned.
#' Otherwise if \code{adjPValues==FALSE} a logical value is returned whether the null hypothesis can be rejected.
#' @param alpha A numeric specifying the maximal allowed type one error rate. If \code{adjPValues==TRUE} (default) the parameter \code{alpha} is not used.
#' @param verbose Logical scalar. If \code{TRUE} verbose output is generated.
#' @param subsets A list of subsets given by numeric vectors containing the indices of the elementary hypotheses for which the weighted Simes test is applicable.
#' @param subset A numeric vector containing the numbers of the indices of the currently tested elementary hypotheses.
#' @param ... Further arguments possibly passed by \code{gMCP} which will be used by other test procedures but not this one.
#' @return adjusted p-value or decision of rejection
#' @examples
#'
#' simes.on.subsets.test(pvalues=c(0.1,0.2,0.05), weights=c(0.5,0.5,0))
#' simes.on.subsets.test(pvalues=c(0.1,0.2,0.05), weights=c(0.5,0.5,0), adjPValues=FALSE)
#'
#' graph <- BonferroniHolm(4)
#' pvalues <- c(0.01, 0.05, 0.03, 0.02)
#'
#' gMCP.extended(graph=graph, pvalues=pvalues, test=simes.on.subsets.test, subsets=list(1:2, 3:4))
#'
#' @export
simes.on.subsets.test <- function(pvalues, weights, alpha=0.05, adjPValues=TRUE, verbose=FALSE, subsets, subset, ...) {
  subsets <- list(...)[["subsets"]]
  if (any(sapply(subsets, function(x) {all(subset %in% x)}))) {
    # Simes test:
    if (verbose) cat("Subset: ", subset, " -> Simes\n")
    return(simes.test(pvalues, weights, alpha, adjPValues, ...))
  } else {
    # Bonferroni test:
    if (verbose) cat("Subset: ", subset, " -> Bonferroni\n")
    return(bonferroni.test(pvalues, weights, alpha, adjPValues, ...))
  }
}

#' Weighted Simes test
#'
#' Weighted Simes test introduced by Benjamini and Hochberg (1997)
#'
#' @param pvalues A numeric vector specifying the p-values.
#' @param weights A numeric vector of weights.
#' @param adjPValues Logical scalar. If \code{TRUE} (the default) an adjusted p-value for the weighted Simes test is returned.
#' Otherwise if \code{adjPValues==FALSE} a logical value is returned whether the null hypothesis can be rejected.
#' @param alpha A numeric specifying the maximal allowed type one error rate. If \code{adjPValues==TRUE} (default) the parameter \code{alpha} is not used.
#' @param verbose Logical scalar. If \code{TRUE} verbose output is generated.
#' @param ... Further arguments possibly passed by \code{gMCP} which will be used by other test procedures but not this one.
#' @return adjusted p-value or decision of rejection
#' @examples
#'
#' simes.test(pvalues=c(0.1,0.2,0.05), weights=c(0.5,0.5,0))
#' simes.test(pvalues=c(0.1,0.2,0.05), weights=c(0.5,0.5,0), adjPValues=FALSE)
#'
#' @export
simes.test <- function(pvalues, weights, alpha=0.05, adjPValues=TRUE, verbose=FALSE, ...) {
  mJ <- Inf
  for (j in 1:length(pvalues)) {
    Jj <- pvalues <= pvalues[j] # & (1:n)!=j
    if (adjPValues) {
      mJt <- pvalues[j]/sum(weights[Jj])
      if (is.na(mJt)) { # this happens only if pvalues[j] is 0
        mJt <- 0
      }
      if (mJt<mJ) {
        mJ <- mJt
      }
    }
    # explanation[i] <- paste("Subset {",paste(J,collapse=","),"}: p_",j,"=", pvalues[j],"<=a*(w_",paste(which(Jj),collapse ="+w_"),")\n     =",alpha,"*(",paste(weights[i, Jj],collapse ="+"),")=",alpha*sum(weights[i, Jj]),sep="")
  }
  if (adjPValues) {
    return(mJ)
  } else {
    return(mJ<=alpha)
  }
}

# TODO resampling tests?


#' Graph based Multiple Comparison Procedures
#'
#' Performs a graph based multiple test procedure for a given graph and unadjusted p-values.
#'
#' @param graph A graph of class \code{graphMCP}.
#' @param pvalues A numeric vector specifying the p-values for the graph based
#' MCP. Note the assumptions in the description of the selected test (if there are any -
#' for example \code{test=bonferroni.test} has no further assumptions, but
#' \code{test=parametric.test} assumes p-values from a multivariate normal distribution).
#' @param test A weighted test function.
#'
#' The package gMCP provides the following weighted test functions:
#' \describe{
#'   \item{bonferroni.test}{Bonferroni test - see \code{?bonferroni.test} for details.}
#'   \item{parametric.test}{Parametric test - see \code{?parametric.test} for details.}
#'   \item{simes.test}{Simes test - see \code{?simes.test} for details.}
#'   \item{bonferroni.trimmed.simes.test}{Trimmed Simes test for intersections of two hypotheses and otherwise Bonferroni - see \code{?bonferroni.trimmed.simes.test} for details.}
#'   \item{simes.on.subsets.test}{Simes test for intersections of hypotheses from certain sets and otherwise Bonferroni - see \code{?simes.on.subsets.test} for details.}
#' }
#'
#' To provide your own test function see \code{?weighted.test.function}.
#'
#' @param alpha A numeric specifying the maximal allowed type one error rate.
#' @param eps A numeric scalar specifying a value for epsilon edges.
#' @param upscale Logical. If \code{upscale=FALSE} then for each intersection
#' of hypotheses (i.e. each subgraph) a weighted test is performed at the
#' possibly reduced level alpha of sum(w)*alpha,
#' where sum(w) is the sum of all node weights in this subset.
#' If \code{upscale=TRUE} all weights are upscaled, so that sum(w)=1.
#' @param verbose Logical scalar. If \code{TRUE} verbose output is generated
#' during sequentially rejection steps.
#' @param adjPValues Logical scalar. If \code{FALSE} no adjusted p-values will
#' be calculated. Especially for the weighted Simes test this will result in
#' significantly less calculations in most cases.
#' @param ...  Test specific arguments can be given here.
#' @return An object of class \code{gMCPResult}, more specifically a list with
#' elements
#' \describe{
#' \item{\code{graphs}}{list of graphs}
#' \item{\code{pvalues}}{p-values}
#' \item{\code{rejected}}{logical whether hypotheses could be rejected}
#' \item{\code{adjPValues}}{adjusted p-values}
#' }
#' @author Kornelius Rohmeyer \email{rohmeyer@@small-projects.de}
#' @seealso \code{graphMCP} \code{\link[multcomp:contrMat]{multcomp::contrMat()}}
#' @references Frank Bretz, Willi Maurer, Werner Brannath, Martin Posch: A
#' graphical approach to sequentially rejective multiple test procedures.
#' Statistics in Medicine 2009 vol. 28 issue 4 page 586-604.
#' \url{https://www.meduniwien.ac.at/fwf_adaptive/papers/bretz_2009_22.pdf}
#'
#' Bretz F., Posch M., Glimm E., Klinglmueller F., Maurer W., Rohmeyer K.
#' (2011): Graphical approaches for multiple endpoint problems using weighted
#' Bonferroni, Simes or parametric tests. Biometrical Journal 53 (6),
#' pages 894-913, Wiley.
#' \doi{10.1002/bimj.201000239}
#'
#' Strassburger K., Bretz F.: Compatible simultaneous lower confidence bounds
#' for the Holm procedure and other Bonferroni based closed tests. Statistics
#' in Medicine 2008; 27:4914-4927.
#'
#' Hommel G., Bretz F., Maurer W.: Powerful short-cuts for multiple testing
#' procedures with special reference to gatekeeping strategies. Statistics in
#' Medicine 2007; 26:4063-4073.
#'
#' Guilbaud O.: Simultaneous confidence regions corresponding to Holm's
#' stepdown procedure and other closed-testing procedures. Biometrical Journal
#' 2008; 50:678-692.
#'
#' @keywords htest graphs
#'
#' @examples
#' g <- BonferroniHolm(5)
#' gMCP(g, pvalues=c(0.01, 0.02, 0.04, 0.04, 0.7))
#' # Simple Bonferroni with empty graph:
#' g2 <- matrix2graph(matrix(0, nrow=5, ncol=5))
#' gMCP(g2, pvalues=c(0.01, 0.02, 0.04, 0.04, 0.7))
#' # With 'upscale=TRUE' equal to BonferroniHolm:
#' gMCP(g2, pvalues=c(0.01, 0.02, 0.04, 0.04, 0.7), upscale=TRUE)
#'
#' # Entangled graphs:
#' g3 <- Entangled2Maurer2012()
#' gMCP(g3, pvalues=c(0.01, 0.02, 0.04, 0.04, 0.7), correlation=diag(5))
#'
#' @export
gMCP.extended <- function(graph, pvalues, test, alpha=0.05, eps=10^(-3), upscale=FALSE, verbose=FALSE, adjPValues=TRUE, ...) {
  callFromGUI <- !is.null(list(...)[["callFromGUI"]])

  test.parameters <- names(formals(test))
  further.parameters <-  list(...)
  provide.subset <- ("subset" %in% test.parameters)
  provide.correlation <- ("correlation" %in% test.parameters)
  if (provide.correlation) {
    correlation <- list(...)[["correlation"]]
    further.parameters[["correlation"]] <- NULL
  }

  exclude <- c("callFromGUI")
  for (p in names(list(...))) {
    if (!(p%in%exclude) && !(p%in%test.parameters)) {
      warning(paste("Parameter '",p,"' will not be used by this test.", sep=""))
    }
  }

  exclude <- c("callFromGUI", "pvalues", "weights", "alpha", "adjPValues", "verbose", "subset")
  for (p in test.parameters) {
    if (p=="...") break # Everything after "..." in the test functions is optional.
    if (!(p%in%exclude) && !(p%in%names(list(...)))) stop(paste("Specified test requires parameter '",p,"', which is missing.",sep=""))
  }

  # Replace epsilon
  graph <- substituteEps(graph, eps=eps)

  # Check whether sequential rejective testing is applicable.
  if(FALSE) {
    graph2 <- subgraph(graph, !getRejected(graph))
    pvalues2 <- pvalues[!getRejected(graph)]
  } else {
    graph2 <- graph
    pvalues2 <- pvalues
  }

  allSubsets <- permutations(length(getNodes(graph2)))[-1,]
  result <- cbind(allSubsets, 0, Inf)
  n <- length(graph2@weights)
  # Allow for different generateWeights? generateWeights can handle entangled graphs.
  weights <- generateWeights(graph2@m, getWeights(graph2))[,(n+(1:n))]
  if (upscale) {
    weights <- weights / rowSums(weights)
    # 0/0 should be 0 and not NaN
    weights[is.na(weights)] <- 0
  }

  if (verbose) explanation <- rep("not rejected", dim(allSubsets)[1])
  for (i in 1:dim(allSubsets)[1]) {
    subset <- allSubsets[i,]
    if(!all(subset==0)) {
      J <- which(subset!=0)
      parameters <- list(pvalues=pvalues2[J], weights=weights[i, J], alpha=alpha, adjPValues=adjPValues, verbose=verbose)
      if (provide.correlation) {
        parameters <- c(parameters, list(correlation=correlation[J,J, drop=FALSE]))
      }
      if (provide.subset) {
        parameters <- c(parameters, list(subset=J))
      }
      parameters <- c(parameters, further.parameters)
      if (adjPValues) {
        test.result <- do.call(test, parameters, quote=TRUE)
        result[i, n+2] <- test.result
        result[i, n+1] <- ifelse(test.result<=alpha*sum(weights[i, J]), 1, 0)
        if (verbose) {
          if (result[i, n+1]==1) {
            explanation[i] <- paste("Subset {",paste(J,collapse=","),"}: rejected (adj-p: ",round(result[i, n+2],5),")", sep="")
          } else {
            explanation[i] <- paste("Subset {",paste(J,collapse=","),"}: not rejected (adj-p: ",round(result[i, n+2],5),")", sep="")
          }
        }
        #if (verbose) {
        #  explanation[i] <- paste("Adjusted p-value for subset {",paste(J,collapse=","),"}: ", result[i, n+2], " [pvalues: ", dput2(pvalues2[J]) ,", weights: ", dput2(round(weights[i, J],4)), "]", sep="")
        #}
      } else { # No adjusted p-values, just rejections:
        test.result <- do.call(test, parameters, quote=TRUE)
        result[i, n+1] <- ifelse(test.result, 1, 0)
        result[i, n+2] <- NA
        if (verbose) {
          if (test.result) {
            explanation[i] <- paste("Subset {",paste(J,collapse=","),"}: rejected [pvalues=", dput2(pvalues2[J]) ,", weights=", dput2(round(weights[i, J],4)), "]", sep="")
          } else {
            explanation[i] <- paste("Subset {",paste(J,collapse=","),"}: not rejected [pvalues=", dput2(pvalues2[J]) ,", weights=", dput2(round(weights[i, J],4)), "]", sep="")
          }
        }
      }
      if (verbose) {
        expl <- attr(test.result, "explanation")
        if (!is.null(expl)) explanation[i] <- expl
      }
    }
  }
  adjPValuesV <- rep(NA, n)
  for (i in 1:n) {
    if (all(result[result[,i]==1,n+1]==1)) {
      graph2 <- rejectNode(graph2, getNodes(graph2)[i])
    }
    adjPValuesV[i] <- max(result[result[,i]==1,n+2])
  }
  # Creating result object:
  result <- new("gMCPResult", graphs=list(graph, graph2), alpha=alpha, pvalues=pvalues, rejected=getRejected(graph2), adjPValues=adjPValuesV)
  # Adding explanation for rejections:
  output <- "Info:"
  if (verbose) {
    output <- paste(output, paste(explanation, collapse="\n"), sep="\n")
    if (!callFromGUI) cat(output,"\n")
    attr(result, "output") <- output
  }
  # Adding attribute call:
  attr(result, "call") <- call2char(match.call())
  return(result)
}
