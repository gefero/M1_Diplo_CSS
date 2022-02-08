library(tidyverse)
library(lubridate)
#############Lectura y pool de bases#########
chats<- list.files("../Chats/")
 
rutas.df <- data.frame(
   ruta = list.files("../Chats/",recursive = T)) 

listado.completo <- data.frame()
for(caso in chats){
  
text <- read_table(file = paste0("../Chats/",caso),col_names = F) %>% 
  mutate(fecha = str_sub(string = caso,start = 22,end = 31))

listado.completo <- bind_rows(listado.completo,text)
}

mensajes <- listado.completo %>% 
  filter(str_detect(string = str_sub(X1,1,1),
                    pattern = "\\D")) %>% 
  filter(str_detect(string = X1,
                     pattern = ":"))

intervenciones <- mensajes %>% 
  separate(X1,into = c("nombre","texto"),sep = ":") %>% 
  mutate(nombre = str_to_lower(nombre),
         texto = str_to_lower(texto))
  

###Presentes###
presentes <- intervenciones %>% 
  filter(str_detect(texto,pattern = "present")) %>% 
  group_by(fecha,nombre) %>% 
  summarise(intervenciones = n()) %>%  
  arrange(intervenciones,fecha)


presentes.por.fecha <- presentes %>% 
  pivot_wider(names_from = fecha,values_from = intervenciones)%>% 
  arrange(nombre)


###cualquier intervencion####

conte.intervenciones <- intervenciones %>% 
  filter(nombre %in% presentes.por.fecha$nombre) %>% 
  group_by(fecha,nombre) %>% 
  summarise(intervenciones = n()) 

intervenciones.por.fecha <- conte.intervenciones %>% 
  pivot_wider(names_from = fecha,values_from = intervenciones)%>% 
  arrange(nombre)

openxlsx::write.xlsx(intervenciones.por.fecha,"../listado_intervenc.xlsx")
