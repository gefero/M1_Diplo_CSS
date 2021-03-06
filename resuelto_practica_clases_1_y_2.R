library(tidyverse)
library(openxlsx)

#' 
#' ## Ejercicios práctica    
#'  
#'  - Abran un proyecto (o comiencen uno nuevo) e importen la base de datos sobre los casos de covid detallando la ruta desde el punto de partida del proyecto. Recuerden que dicha base es una muestra del 2% de los casos diarios.  
#'   
 base.covid <- readRDS("../Fuentes/base_covid_sample.RDS")

#'  - Supongan que sólo les interesa trabajar con las variables **sexo**, **edad** y **fallecido**. Utilice el método de acceso de los corchetes ``[]`` para crear un nuevo objeto que contenga todas las filas de la base, pero solo esas columnas
 base.recortada<- base.covid[,c("sexo","edad","fallecido")]
## 

#'  - Tome esa misma base y con el método de acceso de corchetes ``[]`` conserve simplemente los primeros 500 casos/filas
base.recortada.2 <- base.recortada[1:500,]

#'  - Cree un vector con todos los números enteros entre 18 y 60. Nómbrelo como **vector_filtro**   
 vector_filtro <- c(18:60)

#'  - Ejecute la siguiente sentencia que servirá para comprobar qué valores del vector en cuestión están dentro del rango contemplado por **vector_filtro** 
 c(1,5,20,55,60,72) %in%  vector_filtro

#'  - Ejecute una sentencia que compruebe para toda la columna edad de la base con 500 filas qué valores están dentro del rango contemplado por **vector_filtro** 
 base.recortada.2$edad %in%  vector_filtro

#'  - Utilice los métodos de acceso ``[]``y tome como guía el ejercicio anterior para diseñar una sentencia que permita crear un nuevo objeto filtrando de la base anterior las edades entre  18 y 60 años 
 base.filtrada <- base.recortada.2[base.recortada.2$edad %in%  vector_filtro, ]

#' 
#'  - Aplique alguna medida de resumen de la información que le permita corroborar que en su base nueva sólo haya casos con edad entre 18 y 60
#' 
summary(base.filtrada$edad)

#'  - Cambie el nombre a la variable **fallecido** por cualquier otro. Recuerde que la función que muestra los nombres de un data.frame es **`names()`**
names(base.filtrada)[3] <- "deceso"

#' - Cree una subcarpeta **"bases_procesadas"** dentro del directorio de su proyecto mediante la función **`dir.create()`**
 dir.create("bases_procesadas/")

#' - Exporte hacia dicha subcarpeta la base final en formato excel, y también en formato .RDS
 write.xlsx(base.filtrada,file = "bases_procesadas/base_covid.xlsx")
 saveRDS(base.filtrada,file = "bases_procesadas/base_covid.RDS")

