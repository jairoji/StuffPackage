#' Other rules measures
#'
#' This function calculates Kulczinsky and Imbalance ratio for rules generated with the apriori algorithm of the arules package
#'
#' @param X A set of rules
#' @param X.Transactions The set of transactions where the rules came from
#' @import arules
#' @keywords Kulczynsky, Imbalance ratio, rules, apriori
#' @export
#' @examples Important_measures()
#' @author JJ


Important_measures = function(X, X.Transactions){
  Soporte.compartido = interestMeasure(X, method = c("support"),
                                       transactions = X.Transactions)
  Soporte.izquierdo = Soporte.compartido/interestMeasure(X, method = c("confidence"),
                                                         transactions = X.Transactions)
  Confidence = interestMeasure(X, method = c("confidence"),
                               transactions = X.Transactions)
  lift = interestMeasure(X, method = c("lift"),
                         transactions = X.Transactions)
  soporte.derecho = Soporte.compartido /(Soporte.izquierdo*lift)
  Kulczinsky = Soporte.compartido*((1/Soporte.izquierdo) + (1/soporte.derecho))/2
  IR = abs(Soporte.izquierdo -soporte.derecho)/(Soporte.izquierdo + soporte.derecho - Soporte.compartido)
  Other.Measures = interestMeasure(X, method = c("chiSquared", "cosine"),
                                   transactions = X.Transactions)
  Measures = cbind(Other.Measures, Kulczinsky, IR)
  return(Measures)
}