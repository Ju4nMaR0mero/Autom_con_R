Crea_frec_acum <- function(tabla_frecuencias){
  num_clases <- length(tabla_frecuencias[,1])
  frecuencias_acum <- rep(0,num_clases)
  for (i in 1:num_clases) {
    if(i==1){ frecuencias_acum[i]=tabla_frecuencias[1,4]}
    else { frecuencias_acum[i]=frecuencias_acum[i-1]+tabla_frecuencias[i,4]}
  }
  frecuencias_acum_tabla <- data.frame(tabla_frecuencias,frecuencias_acum)
  return(frecuencias_acum_tabla)
}