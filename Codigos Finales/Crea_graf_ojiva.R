# Recibe como parametros el pivote (1:Intervalo, 2:Valor_fijo)
Crea_graf_ojiva <- function(tabla_frecuencias_acum, pivote, name_var){
  # Adecuacion tabla para que incluya la frecuencia cero y todos los limites sup e inf
  if(pivote==1){
    nuevos_limites <- c(tabla_frecuencias_acum[1,1],as.vector(tabla_frecuencias_acum$limites_sup))
    nuevas_frec_acum <- c(0, as.vector(tabla_frecuencias_acum$frecuencias_acum))
    aux <- (nuevos_limites[2]-nuevos_limites[1])/2
    x <- nuevos_limites-aux
    y <- nuevas_frec_acum
  }else{
    x <- tabla_frecuencias_acum$clases
    y <- tabla_frecuencias_acum$frecuencias_acum
    aux <- 0
  }
  
  plot(x,y,type="p",pch=20,lty=1,xlab="Clases",ylab="frec",main=paste("Ojiva - ",name_var),xaxt="n",yaxt="n")
  axis(side=1,x+aux,labels=TRUE)
  axis(side=2,y,labels=TRUE,las=2) 
  lines(x,y)}
