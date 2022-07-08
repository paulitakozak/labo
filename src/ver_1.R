# Zero to Hero
# 1.05 Creando un data.table a partir de las columnas
# Hasta ahora, para crear una data.table estamos leyendo un archivo del disco
# ( o bajándolo de internet)

# Ahora veremos como crearla a partir de dos vectores

rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table

options(repr.plot.width=20, repr.plot.height=10) 
setwd("C:\\Lab_Imp_1\\")  #Aqui se debe poner la ruta de la PC local
# 
# Supongamos que tengo dos vectores, uno con numero_de_clente y otro con la 
# decision de si a ese registro le envio o no estímulo
# Importante : ambos vectores tienen la misma longitud

vector_ids <-  c( 107, 228, 351, 468, 579)
vector_ids
vector_enviar  <-   c( 0, 1, 1, 0, 1 )
vector_ids
# finalmente creo un dataset a partir de las dos columnas
# a la primer columna la voy a llamar "numero_de_cliente"
# 
# a la segunda columna la voy a llamar "Predicted"

En el lenguaje R, un dataframe o data.table es una lista de columnas
es una lsita y no un vector, porque los vectores necesitan que todos los 
valores sean del mismo tipo de datos
pero un dataset puede tener columnas que sean vectores de numeros, vectores de 
cadenas de caracters (string)
en R, exista el tipo list , para meter en una bolsa de gatos objetos de
distinto tipo

tabla_final  <-   as.data.table(  list(  "numero_de_cliente"= vector_ids,
                                         "Predicted"=         vector_enviar))
veo como qyuedó la nueva data.table

tabla_final
Finalmente, grabo ese archivo con la instruccion fwrite que pertenece a la libreria **data.table

Cuando utilizamos la instruccion fread para leer una data.table , ella tiene la inteligencia de darse cuenta cual es el separador de campos
pero para grabar, necesitamos nosotros especificarle cual queremos que sea el separador de campos

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/ZH2015/", showWarnings = FALSE )

fwrite( tabla_final,
        file= "./labo/exp/ZH2015/entrega_de_juguete.csv",
        sep= ",")
Revisar ahora que hay en la carpeta /labo/exp/ZH2015/ , editar el archvo entrega_de juguete.txt
Este archivo de juguete dará error si se sube a Kaggle, fue solo de ejemplo