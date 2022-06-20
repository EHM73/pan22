#Hay que meter todo en una funcion y a ejecutar con un for
#Hay que ver como mandar los resultados a fichero de las matrices de correlacion

#https://biocosas.github.io/R/025_guardar_resultados.html

set.seed(1) 

source("D:/OneDrive - UPV/BigData/Asignaturas/15 Text mining/pan22/functions.R")


# Preparing parameters
#vtn <- 20 # Número de palabras para los vocabularios parciales de ironia y no
#vn <- 10   # tamaño final del vocabulario 
k <- 10        # Number of folds in cross-validation. Usually used 10
r <- 3       # Number of repeats in cross-validation. Usually used 3
path_training <- "D:/OneDrive - UPV/BigData/Asignaturas/15 Text mining/pan22/data/training/"	# Your training path
path_test <- "D:/OneDrive - UPV/BigData/Asignaturas/15 Text mining/pan22/data/test/"
lang <- "en"
verbose = FALSE


#vt <- c(10,20,30,40,50,100)
vt <- c(10000)

v <- c(500,600,700,800,900,1000,1100,1200)
#v <- c(10,20)

swl <- c("s","amp", "m")


setwd("D:/OneDrive - UPV/BigData/Asignaturas/15 Text mining/pan22/")
sink("resultados_rf10000_2_velocidad.txt")
for (vtn in vt)
{
  for (vn in v)
  {
    if (vn<=vtn)
     {
      cat(paste("vocabulario I/NI:",vtn,"\n",sep = " "))
        cat(paste("vocabulario:",vn,"\n",sep = " "))
        ejecutarmodelo(vtn,vn,k,r,path_training,path_test,lang,swl,verbose)
        cat("\n")
        cat("--------------------------------------------------------------------------------------------\n")
        cat("\n")
    }
  }
}

sink()
  
