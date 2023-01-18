# Funcion que va recibir como parametro un data.frame para procesar la automatización

#Creacion de Datos Resumen de la tabla
#   Nombre de Variable
#   Tipo de Datos
#   Resumen de datos de acuerdo al tipo de variable
#     -2 Tipos de Resumenes
#   Valores NULL
#   Valores NA
#   Totalidad de observaciones o registros
#   Grafica

Procesar_Automatizacion <- function(tabla_a_automatizar){
  # El nombre de la variable que se recibe como parametro es:
  #  tabla_a_automatizar
  #Se verifica el numero de columnas
  
  num_variables <- length(tabla_a_automatizar) #Numero de Columnas/Variables
  nombres_Variables <- names(tabla_a_automatizar) #Extraemos el nombre de nuestras variables
  
  #Se crean vectores que guardaran info de cada una de las variables
  id_Variable <- c()
  name_Var <- c()
  tipo_Variable <- c()
  num_null_Var <- c()
  num_NA_Var <- c()
  
  for(k in 1:num_variables) { 
    columna_Var <- tabla_a_automatizar[[k]]
    
    id_Variable[k] <- k
    name_Var[k] <- nombres_Variables[k]#: Ya esta declarada al extraer los nombres
    tipo_Variable[k] <- class(columna_Var)
    
    # Vamos a verificar los NULL y NA para limpiarlos y saber la presencia
    num_null_Var[k] <- count_null_list(columna_Var)
    if(num_null_Var[k]>0){ columna_Var_nueva <- list_sin_null(columna_Var)}
    num_NA_Var[k] <- count_na_list(columna_Var)
    if(num_NA_Var[k]>0 & num_null_Var[k]>0){ columna_Var_nueva <- list_sin_na(columna_Var_nueva)}
    if(num_NA_Var[k]>0 & num_null_Var[k]==0){ columna_Var_nueva <- list_sin_na(columna_Var)}
    if(num_NA_Var[k]==0 & num_null_Var[k]==0){ columna_Var_nueva <- columna_Var}
    
    #if(tipo_variable[k]!="list"){ tipo_Variable[k]=corroborar_tipo_dato_character(columna_Var_nueva)}
    
    mensajeK1 <- paste(id_Variable[k],". La variable '", name_Var[k],"' es de tipo:",tipo_Variable[k], ".")
    mensajeK2 <- paste("Contiene ", length(columna_Var), " registros, de los cuales son ", num_null_Var[k], " Null y ", num_NA_Var[k], " NA")
    
    print(mensajeK1)
    print(mensajeK2)
    
    # Verificación de tipo Var
    if(tipo_Variable[k]=="character" | tipo_Variable[k]=="list"){
      
      if(length(unique(columna_Var_nueva))<length(columna_Var_nueva)*0.7){
        print("Se muestra su grafica de pastel y  grafica de Barras de acuerdo a la dist:")
        graf_pastel(columna_Var_nueva, name_Var[k])
        graf_barras_f_abs(columna_Var_nueva, name_Var[k])
        graf_barras_f_relat(columna_Var_nueva, name_Var[k])
        print(unique(columna_Var_nueva))
      } else if(length(unique(columna_Var_nueva))==length(columna_Var_nueva)) { 
        print("La variable tiene todos los registros diferentes (Funciona como ID),")
        print("no podemos obtener estadistícas")
        print(head(columna_Var_nueva))
      } else {print("La variable presenta muchos valores unicos, no se puede obtener estadisticas")
        print(paste("Son ", length(columna_Var_nueva), " observaciones, de las cuales ", length(unique(columna_Var_nueva)), " son diferentes, se muestran las primeras 6:"))
        print(head(columna_Var_nueva))}
    } else { 
      
      resumen_var_numeric(columna_Var_nueva, name_Var[k])
      
      print("Se muestra a continuacion las graficas de Linea, el histograma y la Ojiva de acuerdo a su dist.:")
      clases_y_frecuencias <- creacion_vector_Clases(columna_Var)
      frecuencias_acumuladas <- Crea_frec_acum(clases_y_frecuencias)
      
      Crea_graf_ojiva(frecuencias_acumuladas, 1, name_Var[k])
      Crea_graf_histograma(columna_Var_nueva, name_Var[k])
      Crea_graf_linea(clases_y_frecuencias, name_Var[k])
      
    }
  }
}

