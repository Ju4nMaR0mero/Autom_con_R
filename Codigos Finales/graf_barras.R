#Funcion que crea el Grafico de Barras
#Recibe como parametro el nuevo vector adecuado
graf_barras_f_abs <- function(lista_categorias,var_name) {
  library(plotrix)
  
  datos <- frecuencias_vector_categorias(lista_categorias)
  frecuencia_datos <- table(datos)
  gama_colores <- hcl.colors(length(frecuencia_datos), "Plasma")
  factor_et <- max(frecuencia_datos)*0.1
  grafica <- barplot(frecuencia_datos, ylim=c(0,max(frecuencia_datos)+(factor_et*2)), main = paste("Frec. Abs - ",var_name),col=gama_colores)
  text(grafica, frecuencia_datos + factor_et, labels = frecuencia_datos)
}

graf_barras_f_relat <- function(lista_categorias,var_name) {
  library(plotrix)
  
  datos <- frecuencias_vector_categorias(lista_categorias)
  frecuencia_datos <- table(datos)
  gama_colores <- hcl.colors(length(frecuencia_datos), "Plasma")
  factor_et <- max(frecuencia_datos)*0.1
  grafica <- barplot(prop.table(frecuencia_datos), main = paste("Frec. Relativa - ",var_name),col=gama_colores)
  #text(grafica, frecuencia_datos + factor_et, labels = frecuencia_datos)
}