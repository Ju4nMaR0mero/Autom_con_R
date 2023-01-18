#Esta es una funcion que recibe de parametro una lista y la transforma en otra lista
#Igual pero sin registros de Null

list_sin_null <- function(lista_con_null) {
  lista_sin_null <- Filter(Negate(is.null),lista_con_null)
  return(lista_sin_null)
}