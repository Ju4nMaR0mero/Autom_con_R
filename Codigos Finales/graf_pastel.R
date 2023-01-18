#Funcion que crea el Grafico de Pastel
#Recibe como parametro del nuevo vector adecuado
graf_pastel <- function(lista_categorias, name_variable) {
  library(plotrix)
  
  datos <- frecuencias_vector_categorias(lista_categorias)
  frecuencia_datos <- data.frame(table(datos))
  gama_colores <- hcl.colors(length(frecuencia_datos$Freq), "Plasma")
  pie3D(frecuencia_datos$Freq, labels = frecuencia_datos$datos, explode=0.2, main = paste("Diagrama de Pastel - ",name_variable),
        col = gama_colores)
  #legend(x = "bottom", legend = frecuencia_datos$datos, fill = gama_colores, title = "Categorias")
  
}