

setwd("Z:/Documents/Temno foraging")
library("grid", lib.loc="Z:/Programs/R-3.1.1/library")
library("magrittr", lib.loc="Z:/Programs/R-3.2.2/library")


filenames<-c("e11-1.dat", "e68-1.dat", "e57-1.dat", "e46-1.dat", "e69-1.dat", "e57-2.dat","e11-2.dat","e69-2.dat", "e68-2.dat","e69-3.dat","e11-3.dat","e57-3.dat")
formarter(filenames)
frame();
multiprint(c(filenames),TRUE)

formater(filenames)
