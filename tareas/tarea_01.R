library(arules)
library(tidyverse)
library(naniar)


#####EJERCICIO EN CLASE

data("Groceries")
Groceries


# Hacer lista los datos
lista_mb <- as(Groceries, 'list')

lista_mb[[2]]

sprintf("Número de canastas: %s", length(lista_mb))

num_items <- sapply(lista_mb, length)

sprintf("Promedio de artículos por canasta: %.3f" , mean(num_items))

# Canastas anidadas

canastas_nest <- tibble(
  canasta_id = 1:length(lista_mb),
  articulos = lista_mb
)

canastas_nest$articulos[[3]]

# Artículos mas frecuentes

num_canastas <- nrow(canastas_nest)

articulos_frec <- canastas_nest %>% 
  unnest(cols = articulos) %>% 
  group_by(articulos) %>% 
  summarise( n = n(), .groups = "drop") %>% 
  mutate (prop = n/ num_canastas) %>% 
  arrange(desc(n))

DT::datatable(articulos_frec %>% 
                mutate_if(is_numeric,~ round(.x, 3)))

# Canastas completas más frecuentes

colapsar_canasta <- function(x, sep = '-'){
  
  #Convierte canasta en una cadena
  
  x %>% as.character %>% sort %>% paste(collapse = '-')
}


canastas_conteo <- canastas_nest %>% 
                  mutate(canasta_str = map_chr(articulos, colapsar_canasta)) %>% 
                  group_by(canasta_str) %>% 
                  summarise(n = n(), .groups = "drop") %>% 
                  mutate(prop = round(n/num_canastas, 5)) %>% 
                  arrange(desc(n))

head(canastas_conteo)
tail(canastas_conteo)



DT::datatable(canastas_conteo %>% head(n = 100) %>%  
                mutate_if(is_numeric,~ round(.x, 3)))


pars <- list(supp = 0.01 , target = "frequent items")

ap <- apriori(lista_mb, parameter = pars)
  
length(ap)
  

# Conjuntos frecuentes de tamaño 1 

ap_1 <- subset(ap, size(ap) == 1)

sort(ap_1, by="support") %>%  head(10) %>% DATAFRAME


# Conjuntos frecuentes de tamaño 2 

ap_2 <- subset(ap, size(ap) == 2)

sort(ap_2, by="support") %>%  head(10) %>% DATAFRAME

# Conjuntos frecuentes de tamaño 3

ap_3 <- subset(ap, size(ap) == 3)

sort(ap_3, by="support") %>%  head(10) %>% DATAFRAME


# Items que incluyen berries

ap_berries <- subset(ap, items %pin% 'berries')

sort(ap_berries, by="support") %>%  head(10) %>% DATAFRAME


#################################################### TAREA 18 ENERO ##################################################

datos<- read.csv("datos/recetas/srep00196-s3.csv")

names(datos) 
head(datos)

# Número de canastas
sprintf("Número de canastas: %s", nrow(datos))

# Filtrar por tipo de cocina
datos_filtrados <- datos %>% 
  group_by(region) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(prop = round(n/nrow(datos), 5)) %>% 
  arrange(desc(n))


# Limpieza
datos <- datos %>% 
        mutate(a1 = na_if(a1, ""),
               a2 = na_if(a2, ""),
               a3 = na_if(a3, ""),
               a4 = na_if(a4, ""),
               a5 = na_if(a5, ""),
               a6 = na_if(a6, ""),
               a7 = na_if(a7, ""),
               a8 = na_if(a8, ""),
               a9 = na_if(a9, ""),
               a10 = na_if(a10, ""),
               a11 = na_if(a11, ""),
               a12 = na_if(a12, ""),
               a13 = na_if(a13, ""),
               a14 = na_if(a14, ""),
               a15 = na_if(a15, ""),
               a16 = na_if(a16, ""),
               a17 = na_if(a17, ""),
               a18 = na_if(a18, ""),
               a19 = na_if(a19, ""),
               a20 = na_if(a20, ""),
               a21 = na_if(a21, ""),
               a22 = na_if(a22, ""),
               a23 = na_if(a23, ""),
               a24 = na_if(a24, ""),
               a25 = na_if(a25, ""),
               a26 = na_if(a26, ""),
               a27 = na_if(a27, ""),
               a28 = na_if(a28, ""),
               a29 = na_if(a29, ""),
               a30 = na_if(a30, ""),
               X = na_if(X, ""), 
               X.1 = na_if(X.1, ""))

# Crear datos$articulos
datos$articulos <- apply(datos[,c(2:33)], 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = " "));

#Hacer lista artículos

datos_lista <- as(datos$articulos, 'list')

datos_lista <- str_split(datos_lista, " ")


#### Función apriori

pars <- list(supp = 0.0001 , target = "frequent items")

ap_4 <- apriori(datos_lista, parameter = pars)
length(ap)

ap_4 <- subset(ap, size(ap) == 1)
  
sort(ap_4, by="support") %>%head(15) %>% DATAFRAME
  

### Otra forma 

# Canastas anidadas

recetas_nest <- tibble(
  receta_id = 1:length(datos_lista),
  cocina = datos$region,
  articulos = datos_lista
)

recetas_nest$articulos[[3]]

# Artículos mas frecuentes

num_recetas <- nrow(datos)

ingredientes_frec <- recetas_nest %>% 
  unnest(cols = articulos) %>% 
  group_by(articulos) %>% 
  summarise( n = n(), .groups = "drop") %>% 
  mutate (prop = n/ num_recetas) %>% 
  arrange(desc(n))

DT::datatable(ingredientes_frec %>% 
                mutate_if(is_numeric,~ round(.x, 3)))

