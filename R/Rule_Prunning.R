#' Prunnig redundant rules based on lift
#'
#' This function prunes the rules obtained from the apriori algorithm on the arules package based on lift
#' @param X A set of rules
#' @keywords Prunning Rules Apriori
#' @import arules
#' @export
#' @author http://www.rdatamining.com/examples/association-rules
#' @examples http://www.rdatamining.com/examples/association-rules

Rules_prunning = function(X){
  X <- sort(X, by="lift")
  subset.X <- is.subset(X, X)
  subset.X[lower.tri(subset.X, diag=T)] <- NA
  redundant <- colSums(subset.X, na.rm=T) >= 1
  X.pruned <- X[!redundant]
  return(X.pruned)
}