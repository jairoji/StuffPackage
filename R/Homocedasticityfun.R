#' Homocedasticity tests
#'
#' This function calculates the p values of the following homocedasticity tests:
#' Bartlett
#' Levene
#' @param x A vector of numerical values
#' @param y A vector of groups
#' @keywords Homocedasticity
#' @import lawstat
#' @export
#' @examples
#' x = rnorm(1000)
#' Homocedasticity_fun()

Homocedasticity_fun = function(x, y){
  Pruebas.Homocedasticidad = matrix(c(bartlett.test(x, y)$p.value,
                                      levene.test(x, y)$p.value), ncol = 1)
  rownames(Pruebas.Homocedasticidad) = c("Bartlett", "Levene")
  colnames(Pruebas.Homocedasticidad) = "p value"
  return(Pruebas.Homocedasticidad)
}
