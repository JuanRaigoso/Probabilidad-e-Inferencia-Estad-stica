### Librerias a utilizar
library(mice)
library(tidyverse)
library(stats)
library(DT)
library(kableExtra)
library(ggmap)
library(modeest)
library(psych)
library(moments)
library(kableExtra)
###Cargamos la base de datos desde nuestro directorio
vivienda_cali <- read_csv("C:/Users/juanr/OneDrive/Escritorio/U Javeriana/Primer semestre/Métodos Y simulación estadística/Unidad N°1/vivienda_faltantes.csv",
                               na = "NA")
### Eliminar filas ID
vivienda_cali <- vivienda_cali[complete.cases(vivienda_cali$id), ] 


###Analisis descriptivo precio

## histograma de los precios de vivienda
ggplot(data=vivienda_cali)+
  geom_histogram(aes(x=preciom),binwidth = 100)+
  theme(axis.text.x = element_text(size = 10))+
  labs(title = "Histograma variable Precio", x = "Precio", y = "Frecuencia")+
  theme(plot.title = element_text(hjust = 0.5))

## Indicadores de Posición 
kable(
  data.frame(Maximo=max(vivienda_cali$preciom),Mínimo= min(vivienda_cali$preciom), Moda= mfv(vivienda_cali$preciom)),
  caption= "Indicadores de posición", aling= "c",col.names = c("Máximo", "Mínimo", "Moda")
)%>%
  kable_classic(full_width= F, html_font = "Cambria")
## Cuartiles variable Precio
ggplot(data=vivienda_cali)+
  geom_boxplot(mapping = aes(x=preciom), col = "green")+
  labs(title = "Cuartiles", x = "Precio")+
  theme(plot.title = element_text(hjust = 0.5))
## Indicadores de Centro variable Precio
kable(table(Media = mean(vivienda_cali$preciom), Mediana = median(vivienda_cali$preciom),
            Media_geometrica= geometric.mean(vivienda_cali$preciom),
            Media_truncada =mean(vivienda_cali$preciom,                                                                                        na.rm = TRUE, trim = 0.10)),caption= "Indicadores de centro",
      aling= "c", col.names = c("Media", "Mediana", "Media geométrica", "Media Truncada", "Freq")) %>%
  kable_classic(full_width= F, html_font = "Cambria")
precio_medio <- mean(vivienda_cali$preciom) 
precio_sd <- sd(vivienda_cali$preciom)
##Inndicadores de Variablidad y forma variable precio
kable(
  table(Rango=max(vivienda_cali$preciom)-min(vivienda_cali$preciom), Varianza= var(vivienda_cali$preciom), Desviación_estandar=sd(vivienda_cali$preciom), Coeficiente_variacion=(precio_sd / precio_medio)*100, kurtosis(vivienda_cali$preciom)), 
  caption= "Indicadores de variabilidad y de forma",
  aling= "c", col.names = c("Rango", "Varianza", "Desviación estandar", "coeficiente de variación", "kurtosis", "Freq")) %>%
  kable_classic(full_width= F, html_font = "Cambria")%>%
  kable_styling(position = "center")

## Agrupamos el dataser vivienda cali sólo por la variable zona, la cual queda guardada en el dataset precio_promedio_vivienda_zona
precio_promedio_vivienda_zona <- group_by(vivienda_cali, zona)
##Calculamos en pormedio del precio de las viviendas por cada zona.
promedio_zona<-summarise(precio_promedio_vivienda_zona, precio_promedio=na.omit(mean(preciom)))
### Gráfico pormedio de precios por zona
ggplot(data=promedio_zona,aes(x=zona, y=precio_promedio, fill=zona)) +
  geom_bar(stat = "identity", ) +
  labs(title = "Precio promedio por zona",
       x = "Zona",
       y = "Precio promedio")+
  theme(plot.title = element_text(hjust = 0.5))
# Zona - Precio 
ggplot(vivienda_cali,mapping=aes(x=zona, y=preciom )) +
  geom_boxplot()+
  labs(title = "Caja de bigotes por precio según zona",
       x = "Zona",
       y = "Precio promedio")+
  theme(plot.title = element_text(hjust = 0.5))
#Cantidad de viviendas construidas según precio y área por zona
l<-vivienda_cali%>%
  group_by(zona)
ggplot(data = l) +
  geom_point(mapping = aes(x =preciom, y =areaconst, color=zona),na.rm = TRUE)+
  labs(title = "Cantidad de viviendas construidas según precio y área por zona",
       x = "Precio",
       y = "Área construida")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=l) +
  geom_point(mapping = aes(x =preciom, y =areaconst))+
  facet_grid(. ~ zona)+
  labs(title = "Cantidad de viviendas construidas según precio y área por zona",
       x = "Precio",
       y = "Área construida")+
  theme(plot.title = element_text(hjust = 0.5))


### En esta parte comenzamos a realizar el análisis descriptivo para la variable precio para en ambas categorias del tipo de vivienda (Casa y Apartamento)
apartamento <- filter(vivienda_cali, tipo=="Apartamento") #Creamos un nuevo dataset que contenga solo la categoría Apartamentos
Casa <- filter(vivienda_cali, tipo=="Casa")# Creamos un nuevo dataser que contenga solo la categoría casa
## Sacamos el precio pormedio y la desviación estandar, los cuales nos van a servir para sacar el coeficiente de variación.
precio_casa_medio <- mean(Casa$preciom) 
precio_casa_sd <- sd(Casa$preciom)
##Al analizar la variable Tipo, observamos que esta presenta varios errores, pues casa y oartamento están categorizadas unas con mayúscula o munúscula
vivienda_cali$tipo <- gsub("apto", "Apartamento", vivienda_cali$tipo)
vivienda_cali$tipo <- gsub("APARTAMENTO", "Apartamento", vivienda_cali$tipo)
vivienda_cali$tipo <- gsub("CASA", "Casa", vivienda_cali$tipo)
vivienda_cali$tipo <- gsub("casa", "Casa", vivienda_cali$tipo)
##Indicadores de posición, forma, centro y variabilidad para la categoría casa variable precio
kable(
  data.frame(Maximo=max(Casa$preciom),Mínimo= min(Casa$preciom), Moda= mfv(Casa$preciom), Media = mean(Casa$preciom), Mediana =  median(Casa$preciom),Media_geometrica=geometric.mean(Casa$preciom),Media_truncada=mean(Casa$preciom),Rango=max
             (Casa$preciom)-min(Casa$preciom), Varianza= var(Casa$preciom), Desviación_estandar=sd(Casa$preciom), Coeficiente_variacion=(precio_casa_sd / precio_casa_medio)*100, kurtosis(Casa$preciom)
  ),
  caption= "Indicadores de posición, forma, centro y variabilidad", aling= "c",col.names = c("Máximo", "Mínimo", "Moda", "Media", "Mediana", "Media Geometrica", "Media truncada","Rango", "Varianza", "Desviación estandar", "coeficiente de variación", "kurtosis"))%>%
  kable_classic(full_width= F, html_font = "Cambria")
##Sacamos el precio pormedio y la desviación estandar, los cuales nos van a servir para sacar el coeficiente de variación.
precio_apartamento_medio <- mean(apartamento$preciom) 
precio_apartamento_sd <- sd(apartamento$preciom)
##Indicadores de posición, forma, centro y variabilidad para la categoría apartamento variable precio
kable(
  data.frame(Maximo=max(apartamento$preciom),Mínimo= min(apartamento$preciom), Moda= mfv(apartamento$preciom), Media = mean(apartamento$preciom), Mediana =  median(apartamento$preciom),  Media_geometrica=geometric.mean(apartamento$preciom), Media_truncada=mean(apartamento$preciom), Rango=max(apartamento$preciom)-min(apartamento$preciom) ,Varianza= var(apartamento$preciom), Desviación_estandar=sd(apartamento$preciom), Coeficiente_variacion=(precio_apartamento_sd / precio_apartamento_medio)*100, kurtosis(apartamento$preciom)
  ),
  caption= "Indicadores de posición, forma, centro y variabilidad", aling= "c",col.names = c("Máximo", "Mínimo", "Moda", "Media", "Mediana", "Media Geometrica", "Media truncada","Rango", "Varianza", "Desviación estandar", "coeficiente de variación", "kurtosis"))%>%
  kable_classic(full_width= F, html_font = "Cambria")

## Análisis entre la variable Estrato y la categoria Casa
#observamos la cantidad de casas que hay por estrato
estrato_casa <- table(Casa$estrato)
#Creamos un dataframe de la categoría casa y variable estrato
estrato_casa_1 <- as.data.frame(estrato_casa)
colnames(estrato_casa_1) <- c("Estratos", "Frecuencia")
#Calulamos los porcentajes de los estratros en el set de datos
estrato_casa_1$Porcentaje <- prop.table(estrato_casa_1$Frecuencia) * 100
#Gráfico Pie - porcentajes por estrato en la categoría casa.
ggplot(data = estrato_casa_1,aes(x = "", y = Porcentaje, fill = Estratos)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Porcentaje de casas por estrato", fill = "Estratos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
# Gráfico de puntos - Relación Estrato - Precio - Área Construida según tipo de vivenda (Casa)
ggplot(data = Casa) +
  geom_point(mapping = aes(x =preciom, y =areaconst,  color= factor(estrato)))+
  labs(title = "Relación Estrato - Precio - Área Construida según tipo de vivenda (Casa)", x = "Precio", y = "Area Construida")+
  scale_color_discrete(name = "Estrato")+
  theme(plot.title = element_text(hjust = 0.5))
# Tabla - Promedio del Área construida y Precio por estrato
kable(Casa %>%
        group_by(estrato) %>%
        summarize(Promedio_Area_Construida = mean(areaconst),
                  Promedio_Precio = mean(preciom)),
      caption= "Promedio del Área construida y Precio por estrato", aling= "c",colnames = c("Promedio área construida", "promedio Precio"))%>%
  kable_classic(full_width= F, html_font = "Cambria")
## Análisis entre la variable Estrato y la categoria Apartamento
estrato_apartamento <- table(apartamento$estrato)
#Creamos un dataframe de la categoría casa y variable estrato
estrato_casa_1 <- as.data.frame(estrato_casa)
#Calulamos los porcentajes de los estratros en el set de datos
estrato_apartamento_1 <- as.data.frame(estrato_apartamento)
colnames(estrato_apartamento_1) <- c("Estratos", "Frecuencia")
estrato_apartamento_1$Porcentaje <- prop.table(estrato_apartamento_1$Frecuencia) * 100
#Gráfico Pie - porcentajes por estrato en la categoría Apartamento.
ggplot(data = estrato_apartamento_1, aes (x = "", y = Porcentaje, fill = Estratos)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Porcentaje de apartamento por estrato - Casas", fill = "Estratos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
# Gráfico de puntos - Relación Estrato - Precio - Área Construida según tipo de vivenda (Apartamento)
ggplot(data = apartamento) +
  geom_point(mapping = aes(x =preciom, y =areaconst, color= factor(estrato)))+
  labs(title = "Relación Estrato - Precio - Área Construida según tipo de vivenda (Apartamento)", x = "Precio", y = "Area Construida")+
  scale_color_discrete(name = "Estrato")+
  theme(plot.title = element_text(hjust = 0.5))
# Tabla - Promedio del Área construida y Precio por estrato - apartamento
kable(
  apartamento %>%
    group_by(estrato) %>%
    summarize(Promedio_Area_Construida = mean(areaconst),
              Promedio_Precio = mean(preciom)),
  caption= "Promedio del Área construida y Precio por estrato - Apartamento", aling= "c",colnames = c("Promedio área construida", "promedio Precio", "Kurtosis"))%>%
  kable_classic(full_width= F, html_font = "Cambria")

##Precio según zona y tipo de vivienda.
# Caja de bigotes - Casa
ggplot(Casa,mapping=aes(x=zona, y=preciom, fill=zona)) +
  geom_boxplot()+
  labs(title = "Caja de bigotes por precio según zona - Vivienda Casa",
       x = "Zona",
       y = "Precio")+
  theme(plot.title = element_text(hjust = 0.5))

# Caja de bigotes - Apartamento
ggplot(apartamento,mapping=aes(x=zona, y=preciom, fill= zona)) +
  geom_boxplot()+
  labs(title = "Caja de bigotes por precio según zona - Vivienda Apartamento",
       x = "Zona",
       y = "Precio")+
  theme(plot.title = element_text(hjust = 0.5))
## agrupamos a partir del dataser vivienda_cali, un dataset llamado vivienda_tipo, el cual contiene los datos agrupados por la variable tipo
Vivienda_tipo <- group_by(vivienda_cali, tipo)
## En esta parte se crea un histograma para ver cuanttas casas y apartaemntos hay por cada cantidad de baños.
ggplot(data=Vivienda_tipo, aes( x= banios, fill=tipo))+
  geom_histogram(position = "dodge", binwidth = .5) +
  labs(title = "Histograma de Cantidad de Baños por Tipo de Vivienda",
       x = "Cantidad de Baños",
       y = "Frecuencia") +
  scale_fill_discrete(name = "Tipo de Vivienda")+
  scale_x_continuous(breaks = seq(min(Vivienda_tipo$banios), max(Vivienda_tipo$banios), 1))+
  theme(plot.title = element_text(hjust = 0.5))
# Precio según cantidad de baños por tipo de vivienda
ggplot(data = vivienda_cali) +
  geom_point(mapping = aes(x =preciom, y =banios, color=tipo))+
  labs(title = "Precio según cantidad de baños por tipo de vivenda", x = "Precio", y = "Cantidad de baños")+
  scale_color_discrete(name = "Estrato")+
  theme(plot.title = element_text(hjust = 0.5))