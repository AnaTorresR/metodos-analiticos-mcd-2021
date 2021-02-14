library(tidyverse)
library(arules)
library(naniar)
library(arulesViz)
library(tidygraph)
library(ggraph)

# Artículos por región Southern European

limpiar <- function(lineas,...){
  str_split(lineas, ',') %>% 
    keep(~.x[1] == 'SouthernEuropean') %>%
    map(~.x[-1]) %>% # quitar tipo de cocina
    map(~.x[nchar(.x) > 0]) # quitar elementos vacios
}

callback_limpiar <- ListCallback$new(limpiar)

filtrado <- read_lines_chunked('datos/recetas/srep00196-s3.csv',
                               skip = 1, 
                               callback = callback_limpiar,
                               chunk_size = 1000)

recetas <-  filtrado %>% flatten

# Soporte mínimo 

S = 1/(2*sqrt(length(recetas)))

# El soporte mínimo para analizar las recetas de esta región es S>= 0.007733603 

########

# Utilizando 0.2 como valor de confianza observamos lo siguiente: 

pars <- list(supp = 0.05, confidence = 0.2, target = 'rules',
             ext = TRUE, minlen =2)


reglas <- apriori(recetas, parameter = pars)

df_1 <- sort(reglas, by='lift') %>% DATAFRAME

df_2 <- reglas %>% select(LHS, RHS,coverage,support , confidence, lift) %>% 
  head(100) %>% 
  mutate_if(is.numeric, ~round(.x,2))

DT::datatable(df_1)

######

# Utilizando 0.4 como valor de confianza observamos lo siguiente: 

pars <- list(supp = S, confidence = 0.4, target = 'rules',
             ext = TRUE, minlen =2)


reglas <- apriori(recetas, parameter = pars)

df_1 <- sort(reglas, by='lift') %>% DATAFRAME

df_2 <- df_1 %>% select(LHS, RHS,coverage,support , confidence, lift) %>% 
  head(100) %>% 
  mutate_if(is.numeric, ~round(.x,2))

DT::datatable(df_2)

######

# Utilizando 0.6 como valor de confianza observamos lo siguiente: 

pars <- list(supp = S, confidence = 0.6, target = 'rules',
             ext = TRUE, minlen =2)


reglas <- apriori(recetas, parameter = pars)

df_1 <- sort(reglas, by='lift') %>% DATAFRAME

df_2 <- df_1 %>% select(LHS, RHS,coverage,support , confidence, lift) %>% 
  head(100) %>% 
  mutate_if(is.numeric, ~round(.x,2))

DT::datatable(df_2)

###### 

# Gráfica con confianza > 0.2, soporte > S 
agregar_hyperlift <- function(reglas, trans){
  quality(reglas) <- cbind(quality(reglas), 
                           hyper_lift = interestMeasure(reglas, measure = "hyperLift", 
                                                        transactions = trans))
  reglas
}
reglas_recetas <- agregar_hyperlift(reglas, recetas)

reglas_1 <- subset(reglas_recetas, hyper_lift > 1 & support > S & confidence > 0.2)

reglas_tam_2 <- subset(reglas_1, size(reglas_1)==2)

df_reglas <- reglas_tam_2 %>% DATAFRAME %>% rename(from=LHS, to=RHS) %>% data.frame

df_reglas$weight <- log(df_reglas$lift)

graph_1 <- as_tbl_graph(df_reglas) %>%
  mutate(centrality = centrality_degree(mode = "all")) 

set.seed(20210124)
ggraph(graph_1, layout = 'fr') +
  geom_edge_link(aes(alpha=lift), 
                 colour = 'red',
                 arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  geom_node_text(aes(label = name), size=4,
                 colour = 'gray20', repel=TRUE) +
  theme_graph(base_family = "sans")


# Gráfica con confianza > 0.4, soporte > 0.05

reglas_1 <- subset(reglas_recetas, hyper_lift > 1 & support > 0.05 & confidence > 0.4)

reglas_tam_2 <- subset(reglas_1, size(reglas_1)==2)

df_reglas <- reglas_tam_2 %>% DATAFRAME %>% rename(from=LHS, to=RHS) %>% data.frame

df_reglas$weight <- log(df_reglas$lift)

graph_1 <- as_tbl_graph(df_reglas) %>%
  mutate(centrality = centrality_degree(mode = "all")) 

set.seed(20210124)

ggraph(graph_1, layout = 'fr') +
  geom_edge_link(aes(alpha=lift), 
                 colour = 'red',
                 arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  geom_node_text(aes(label = name), size=4,
                 colour = 'gray20', repel=TRUE) +
  theme_graph(base_family = "sans")


# Gráfica con confianza > 0.6, soporte > 0.05

reglas_1 <- subset(reglas_recetas, hyper_lift > 1.1 & support > 1.6 & confidence > 2.6)

reglas_tam_2 <- subset(reglas_1, size(reglas_1)==2)

df_reglas <- reglas_tam_2 %>% DATAFRAME %>% rename(from=LHS, to=RHS) %>% data.frame

df_reglas$weight <- log(df_reglas$lift)


graph_1 <- as_tbl_graph(df_reglas) %>%
  mutate(centrality = centrality_degree(mode = "all")) 


ggraph(graph_1, layout = 'fr') +
  geom_edge_link(aes(alpha=lift), 
                 colour = 'red',
                 arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  geom_node_text(aes(label = name), size=4,
                 colour = 'gray20', repel=TRUE) +
  theme_graph(base_family = "sans")
