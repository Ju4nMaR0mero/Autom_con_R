#Esta funcion cuenta los registros null de una lista
count_null_list <- function(lista_con_null) {
  vect_frec_null <- c()
  for(j in 1:length(lista_con_null)) { 
    vect_frec_null[j]=is.null(lista_con_null[[j]])
  }
  frec_null <- sum(vect_frec_null, na.rm = TRUE)
  return(frec_null)
}