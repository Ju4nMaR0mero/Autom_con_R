# Recibe tal cual la columna de la tabla con los datos numericos ($)
Crea_graf_histograma <- function(vector_columna, titulo_graf){
  x <- vector_columna
  
  hist(x, main=paste("Histograma-",titulo_graf),xlab=titulo_graf,ylab="Frecuencia",col=palette())
}
# Crea_graf_histograma(tabla_airtable_prueba[,6],names(tabla_airtable_prueba[6]))