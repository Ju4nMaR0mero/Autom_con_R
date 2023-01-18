corroborar_tipo_dato_numeric <- function(columna_vector){
  tipo_dato_verificado <- class(columna_vector)
  tryCatch({
    columna_vector_piv<-as.numeric(columna_vector)
    ##print("El tipo de Dato es Numeric")
    ##print(class(columna_vector_piv))
    tipo_dato_verificado <- class(columna_vector_piv)
    
  }, warning = function(w) {
    #try(columna_vector_piv<-as.integer(columna_vector_piv))
    #try(columna_vector<-as.logical(columna_vector))
    
    #else if(is.integer(columna_vector)==TRUE){
    #  print("El tipo de Dato es Integer")
    #  print(tipo_dato_verificar)}
    #else if(is.logical(columna_vector)==TRUE){
    #  print("El tipo de Dato es Logico")
    #  print(tipo_dato_verificar)}
    tipo_dato_verificado <- class(columna_vector)
    ##print("Efectivamente es tipo Character")
    ##print(tipo_dato_verificar)
  }, error = function(e){
    tipo_dato_verificado <- class(columna_vector)
  }, message = function(m){
    tipo_dato_verificado <- class(columna_vector)
  })
  print(tipo_dato_verificado)
  return(tipo_dato_verificado)
}