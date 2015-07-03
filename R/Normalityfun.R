#' Normality tests
#'
#' This function calculates the p values of the following normality tests:
#' Anderson-Darling 
#' Shapiro-Wilks
#' Lilliefors
#' Cramer Von Misses
#' @param x A vector of numerical values
#' @keywords Normality 
#' @import nortest
#' @export
#' @examples
#' x = rnorm(1000)
#' Normality_fun()

Normality_fun = function(x){
  if(!is.numeric(x))
    stop("Please use a numberic vector")
  x = na.omit(x)
  Pruebas.Normalidad = matrix(c(ad.test(x)$p.value, shapiro.test(x)$p.value,
                                lillie.test(x)$p.value, cvm.test(x)$p.value), ncol = 1)
  rownames(Pruebas.Normalidad) = c("Anderson-Darling", "Shapiro-Wilks",
                                   "Lilliefors", "Cramer Von Misses")
  colnames(Pruebas.Normalidad) = "p value"
  return(Pruebas.Normalidad)
}
