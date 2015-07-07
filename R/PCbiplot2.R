#' Biplot FactoMineR PCA object
#'
#' Plots biplot of a FactoMineR PCA object
#' 
#' @param X A FactoMineR PCA object 
#' @param x Dimension of the PCA object for the x axis
#' @param y Dimension of the PCA object for the y axis
#' @param size.obs.text Observation's text size
#' @import ggplot2, FactoMineR
#' @keywords ggplot, PCA, FactoMineR
#' @export
#' @examples http://stackoverflow.com/questions/22381560/how-to-create-a-biplot-with-factominer
#' @author http://stackoverflow.com/questions/22381560/how-to-create-a-biplot-with-factominer
#' PCbiplot2()

PCbiplot2 <- function(X, x="Dim.1", y="Dim.2", size.obs.text = 3) {
  if(!require(ggplot2)) install.packages("ggplot2")
  # X being a PCA object
  data <- data.frame(obsnames=row.names(X$ind$coord), X$ind$coord)
  plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=size.obs.text,     aes(label=obsnames))
  plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
  datapc <- data.frame(varnames=rownames(X$var$coord), X$var$coord)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2,     label=varnames), size = 5, vjust=1, color="red")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),     arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
  plot
}