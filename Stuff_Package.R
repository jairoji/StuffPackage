install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("/run/media/ahriman/Stuff/R_Packages")
create("Stuff")

setwd("./Stuff")
document()

setwd("..")
install("Stuff")