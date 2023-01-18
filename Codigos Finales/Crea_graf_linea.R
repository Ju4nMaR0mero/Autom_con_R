Crea_graf_linea <- function(tabla_frecuencias, titulo_graf){
  y <- tabla_frecuencias$frecuencias
  x <- seq(1,length(y),1)
  
  plot(x, y, type = "b", main=paste("Graf. Lineas-",titulo_graf),xlab=titulo_graf,ylab="Frecuencia",col="lightblue", lwd = 5)
}