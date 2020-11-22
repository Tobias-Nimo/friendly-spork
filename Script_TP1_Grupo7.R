##############################################################
#          Computación Científica Actuarial (746)            #
#             Facultad de Ciencias Económicas                #
#               Universidad de Buenos Aires                  #
##############################################################

################### Trabajo Práctico N°1 #####################

#### Docente: Rodrigo Del Rosso
#### Colaboradores: Santiago Silva - Auza - Sanchez Gavier - Ponce
#### Integrantes del grupo 7: Tobías Nimo - Franco Natiello - Lautaro Ferroni - Silvina Solange Martin
#### Deadline: 22/11/2020

# Ejercicio N°1: Análisis Exploratorio del Dataset y Preparación de Datos

# 1.

# 1.1 Se importa el dataset ´flavors_of_cacao´

setwd(choose.dir())
getwd()
dir()

flavors_of_cacao = read.csv2("flavors_of_cacao.csv", header = T)
View(flavors_of_cacao)

dim(flavors_of_cacao)
names(flavors_of_cacao)
summary(flavors_of_cacao)
str(flavors_of_cacao)

head(flavors_of_cacao)
tail(flavors_of_cacao)

attach(flavors_of_cacao)

# Se tiene un dataset con 9 columnas (variables de interés).
# Las columnas ´Cocoa.Percent´ y ´Rating´ son las únicas que representan 
# varaibles numéricas, las demás son de clase character, a exepción de
# las columnas ´REF´ y ´Review.Date´ que no tienen un valor numérico relevante,
# sino que funcionan como un ID y una variable categótica, respectivamente.

flavors_of_cacao[,"REF"] = as.character(flavors_of_cacao[,"REF"])
flavors_of_cacao[,"Review.Date"] = as.character(flavors_of_cacao[,"Review.Date"])
summary(flavors_of_cacao)

# Se observa que el dataset contiene un total de 1795 filas (observaciones),
# sin embargo algunas de ellas presentan valores no disponibles (NA).

# 1.2 Tratamiento de datos faltantes (NA)

# Primero, se debe conoocer la cantidad de NAs en la base de datos.

flavors_of_cacao[flavors_of_cacao == ""] <- NA
attach(flavors_of_cacao)

sapply(flavors_of_cacao, function(x) sum(is.na(x)))

# Se observan unos pocos NAs en las columnas de clase numérica, aunque
# una mayor cantidad en las columnas ´Broad.Bean.Origin´ y ´Bean.Type´.

# A continuación, se decide de que manera tratar los valores NO disponibles,
# segun cada caso particular ...

# Se observa que la columna ´Bean.Type´ contiene la mayor cantidad de NAs,
# por lo tanto se decide reemplzar esos valores faltantes por la categoría 
# ´Other´, en lugar de eliminar las filas comprometidas o la columna 
# en su totalidad con el fin de no perder demasiada información. 

length(is.na(Bean.Type)[is.na(Bean.Type) == T]) #889 NAs

posiciones_NA = NULL
cont = 1
for (i in 1:length(Bean.Type)) {
  if (is.na(Bean.Type)[i] == T){
    posiciones_NA[cont] = i
    cont = cont + 1
  } 
}

flavors_of_cacao[,"Bean.Type"][posiciones_NA] <- "Other"
attach(flavors_of_cacao)

head(flavors_of_cacao)
length(is.na(Bean.Type)[is.na(Bean.Type) == T]) #0 NAs

# Se decide aplicar el mismo tratamiento para la varible ´Broad.Bean.Origin´,
# por las mismas razonas que antes.

length(is.na(Broad.Bean.Origin)[is.na(Broad.Bean.Origin) == T]) #78 NAs

flavors_of_cacao[,"Broad.Bean.Origin"][is.na(Broad.Bean.Origin)] <- "Other"
attach(flavors_of_cacao)

length(is.na(Broad.Bean.Origin)[is.na(Broad.Bean.Origin) == T]) #0 NAs

sapply(flavors_of_cacao, function(x) sum(is.na(x)))
str(flavors_of_cacao)

# Las colmunas numéricas ´Cocoa.Percent´, ´Rating´, ´Review.Date´, 
# ´Company.Location´ y ´REF´ contienen todas 4 valores faltantes.

# Se procede a elminar dichas observaciones (filas), pues se considera que
# la perdida de información es insignificante. 

flavors_of_cacao = na.omit(flavors_of_cacao)
attach(flavors_of_cacao)

sapply(flavors_of_cacao, function(x) sum(is.na(x)))
str(flavors_of_cacao)
dim(flavors_of_cacao)

# El dataset ahora, una vez finalizado el proceso de limpieza de datos 
# y tratamiento de NAs, posee un total de 1791 filas, es decir que
# se perdieron tan solo 4 observaciones.

# 1.3 Estadística descrpitiva y Análisis de la distribución de las variables

# 1.3.1 Variables Cualitativas

# Nota: Se excluyen las variables ´REF´ y ´Date.Review´ para el presente
# análisis, pues la primera funciona como un ID, y la segunda no brinda
# ningún valor explicativo sobre el rating de una barra de chocolate.

sapply(flavors_of_cacao, function(x) length(unique(x)))

# Los siguientes objetos creados representan las categorías posibles dentro de
# cada variable cualitativa. 

Categorias_Company = unique(Company.Maker.if.known.) #416
Categorias_Bar = unique(Specific.Bean.Origin.or.Bar.Name) #1035
Categorias_Company.Location = unique(Company.Location) #60
Categorias_Bean = unique(Bean.Type) #41
Categorias_Origin = unique(Broad.Bean.Origin) #100

# Mediante la función table se observa que cantidad de observaciones se tiene para
# casa categoría con cada tipo de rating.

table(Company.Maker.if.known., Rating)
table(Specific.Bean.Origin.or.Bar.Name, Rating)
table(Company.Location, Rating)
table(Bean.Type, Rating)
table(Broad.Bean.Origin, Rating)

# Observaciones sobre las barras Elite:
flavors_of_cacao[Rating == 5, ]
# La única compañia que produce barras con calif. 5 es Amedei. 
# La única compañia productora de barras Elite (Amedei) , se encuentra en Italia.
# Las barras de calif. 5 fueron elaboradas con granos de tipo Blend y Trinitario.
# Los granos que corresponden a barras Elite provienen de Venezuela y un origen desconocido.

barplot(table(Company.Maker.if.known., Rating), col = rainbow(616))
barplot(table(Specific.Bean.Origin.or.Bar.Name, Rating), col = rainbow(1035))
barplot(table(Company.Location, Rating), col = rainbow(60))
barplot(table(Bean.Type, Rating), col = rainbow(41))
barplot(table(Broad.Bean.Origin, Rating), col = rainbow(100))

# Se observa una cantidad considerable de categórias dentro de cada variable
# cualitativa. De esta forma, se entiende que representar dichas variables
# mediante un diagrama de barras o de torta no es lo ideal, debido a que estos 
# gráficos pierden poder explicativo cuando se trabaja con una gran cantidad 
# de datos.

# Se procede a crear una nueva varaible cualitativa ´Criterio.Evaluación´ 
# a partir de la variable ´Rating´.

Criterio.Evaluación = NULL
for (i in 1:length(Rating)) {
  if (Rating[i] >=0 & Rating[i] <= 1.75) Criterio.Evaluación[i] = "Implacentero"
  if (Rating[i] >= 2 & Rating[i] <= 2.75) Criterio.Evaluación[i] = "Decepcionante"
  if (Rating[i] >= 3 & Rating[i] <= 3.75) Criterio.Evaluación[i] = "Satisfactorio"
  if (Rating[i] >= 4 & Rating[i] <= 4.75) Criterio.Evaluación[i] = "Premium"
  if (Rating[i] == 5) Criterio.Evaluación[i] = "Elite"
}
Criterio.Evaluación

flavors_of_cacao.modif <- cbind(flavors_of_cacao, Criterio.Evaluación)
head(flavors_of_cacao.modif)

Categorias_Criterio = unique(Criterio.Evaluación);length(Categorias_Criterio)
table(Criterio.Evaluación, Rating)
barplot(table(Criterio.Evaluación, Rating), col = rainbow(5))

# Obervaciones:
flavors_of_cacao[Rating == 5,]
# Solo 2 barras de chocolate consiguieron la máxima calificación (5). 
flavors_of_cacao[Rating == 1,]
# solo 4 barras obtuvieron la mínima calificación (1).  
modeest::mfv(Rating)
# La calificación mas veces otorgaga es ´Satisfactorio´. 
# Luego, en gnral se observa que las calificaiones que se encuentran en el
# centro de la distribución poseen una mayor frecuencia que en las colas.

# 1.3.2 Variables Cuantitativas

### Tabla Descriptiva

summary(flavors_of_cacao)

library(pastecs)
options(scipen = 999)
round(stat.desc(flavors_of_cacao[,5]),2) # Cocoa.Percent
round(stat.desc(flavors_of_cacao[,7]),2) # Rating

Tabla.Descrptiva = function(v){
  require(modeest)
  Tabla.Descrptiva <- list(
    mean(v),
    var(v),
    sd(v),
    median(v),
    mfv(v),
    min(v),
    max(v),
    100*sd(v)/mean(v)
  )
  names(Tabla.Descrptiva) = c("Media","Varianza","Desvio","Mediana","Moda","Min","Max","Coeficiente de variación %")
  return(Tabla.Descrptiva)
}

Tabla.Descrptiva(Cocoa.Percent)
Tabla.Descrptiva(Rating)

Lista.Cuantiles = function(v){ 
  Cuantiles <- list( 
    quantile(v, probs = c(0.25,0.50,0.75)),
    quantile(v, probs = c(0.20,0.40,0.60,0.80)),
    quantile(v, probs = seq(0.1,0.9,0.1))
  )
  names(Cuantiles) = c("Cuartiles","Quintiles","Deciles")
  return(Cuantiles)
}

Lista.Cuantiles(Cocoa.Percent)
Lista.Cuantiles(Rating)

### Histograma y Curva de Densidad

library(lessR)

Freq = Histogram(Cocoa.Percent, data = flavors_of_cacao,
                 main = "Histograma",
                 fill = "rainbow", 
                 color = "white", 
                 breaks = 25, 
                 density = F)
Freq$out_summary
Freq$out_outliers

Density = Histogram(Cocoa.Percent, data = flavors_of_cacao,
                    main = "Density Curve",
                    fill_hist = "lightskyblue1",
                    density = T)

Freq = Histogram(Rating, data = flavors_of_cacao,
                 main = "Histograma",
                 fill = "rainbow", 
                 color = "white", 
                 breaks = 25, 
                 density = F)
Freq$out_summary
Freq$out_outliers

Density = Histogram(Rating, data = flavors_of_cacao,
                    main = "Density Curve",
                    fill_hist = "salmon",
                    density = T)

### Boxplot

# Se procede a identificar los valores atípicos de las variable ´Rating´ y
# ´Cocoa.Percent´.

boxplot(Rating,
        main = "Rating",
        xlab= "Yield",
        col = "salmon",
        horizontal = T)
identify(Rating,rep(1, length(Rating)), Rating, rownames(flavors_of_cacao)) #Finish

# Nota: se cosidera a la columna ´Rating´ como la variable objetivo del trabajo,
# y solo contiene valores de 1 a 5 como resultados posibles. Por lo tanto, no
# se considera a ninguna de las observaciones que la componen como outliers.

boxplot(Cocoa.Percent,
        main = "Boxplot",
        xlab= "Cocoa Percent",
        col = "lightblue1",
        horizontal = T)
identify(Cocoa.Percent,rep(1, length(Cocoa.Percent)), Cocoa.Percent, rownames(flavors_of_cacao))

library(ggplot2)
ggplot(data.frame(Cocoa.Percent)) +
  aes(x = "", y = Cocoa.Percent) +
  geom_boxplot(fill = "lightblue1")+
  theme_bw()

quantile(Cocoa.Percent)
median(Cocoa.Percent)
range(Cocoa.Percent)
diff(range(Cocoa.Percent))
IQR(Cocoa.Percent)
  
boxplot.stats(Cocoa.Percent)$out #outliers

# Una vez identificados los valores atípicos, se debería proceder a eliminarlos 
# del dataset para que no distorcionen los análisis posteriores y las
# conclusiones finales. Sin embargo, es este caso se opta por ignorarlos.

### Ajuste a una distribución conocida

normtest_Rating <-round(stat.desc(Rating, basic = F, desc = F, norm = T),4)
normtest_Rating # p-value = 0 -> NO se distrbuye de forma normal

normtest_Cocoa.Percent <-round(stat.desc(Cocoa.Percent, basic = F, desc = F, norm = T),4)
normtest_Cocoa.Percent # p-value = 0 -> NO se distrbuye de forma normal

library(rriskDistributions)
fit.cont(Rating) 
fit.cont(Cocoa.Percent) 

# No se logra ajustar una distribución teórica conocida de buena forma a la
# distrbución de los datos observados.

# 2. Análisis de la relación entre las variables explicativas y el Rating

# Se crea un dataframe que asocia cada compañia productora con el rating medio
# para las barras de chocolate producidas por cada una de ellas.

Categorias_Company = unique(Company.Maker.if.known.);length(Categorias_Company)

Rating_medio_por_Compania = round(as.vector(tapply(Rating, Company.Maker.if.known., mean)),3)

Company <- data.frame(Categorias_Company, Rating_medio_por_Compania)
head(Company)

# Se crea un dataframe que asocia cada país de origen del grano con el rating
# medio de las barras elaboradas a partir de los mismos.

Categorias_Origin = unique(Broad.Bean.Origin);length(Categorias_Origin)

Rating_medio_por_Origen = NULL
cont = 1
for (i in Categorias_Origin) {
  Ratings_por_Origen = flavors_of_cacao[Broad.Bean.Origin == i,]$Rating
  Rating_medio_por_Origen[cont] = round(sum(Ratings_por_Origen)/length(Ratings_por_Origen),3)
  cont = cont + 1
}

Origin <- data.frame(Categorias_Origin, Rating_medio_por_Origen)
head(Origin)

# Se crea un dataframe que indica el rating medio de las barras de chocolate
# según la locacion de la compañia productora. 

Categorias_Company.Location = unique(Company.Location);length(Categorias_Company.Location)

Rating_medio_por_Locacion = NULL
cont = 1
for (i in Categorias_Company.Location) {
  Ratings_por_Loacion = flavors_of_cacao[Company.Location == i,]$Rating
  Rating_medio_por_Locacion[cont] = round(sum(Ratings_por_Loacion)/length(Ratings_por_Loacion),3)
  cont = cont + 1
}

Location <- data.frame(Categorias_Company.Location, Rating_medio_por_Locacion)
head(Location)

# Se crea un dataframe que indica el rating medio de las barras de chocolate
# según el tipo de grano de cocoa utilizado para su elaboración. 

Categorias_Bean.Type = unique(Bean.Type);length(Categorias_Bean.Type)

Rating_medio_por_Grano = NULL
cont = 1
for (i in Categorias_Bean.Type) {
  Ratings_por_Grano = flavors_of_cacao[Bean.Type == i,]$Rating
  Rating_medio_por_Grano[cont] = round(sum(Ratings_por_Grano)/length(Ratings_por_Grano),3)
  cont = cont + 1
}

Bean <- data.frame(Categorias_Bean.Type, Rating_medio_por_Grano)
head(Bean)

# Se crea un dataframe que indica el rating medio de las barras de chocolate
# segun el tipo de grano de cocoa utilizado para su elaboración. 

Categorias_Bar = unique(Specific.Bean.Origin.or.Bar.Name);length(Categorias_Bar)

Rating_medio_por_Bar = NULL
cont = 1
for (i in Categorias_Bar) {
  Ratings_por_Bar = flavors_of_cacao[Specific.Bean.Origin.or.Bar.Name == i,]$Rating
  Rating_medio_por_Bar[cont] = round(sum(Ratings_por_Bar)/length(Ratings_por_Bar),3)
  cont = cont + 1
}

Bar.Name <- data.frame(Categorias_Bar, Rating_medio_por_Bar)
head(Bar.Name)

# 3.

# a. ¿Dónde se producen los mejores granos de cacao?

df <- data.frame(Broad.Bean.Origin, Rating)
df_sorted <- df[with(df, order(-Rating)), ]
head(df_sorted,10)

# Se observa que solo 2 barras de cholate logran la calificación 5 (Elite), y
# estas fueron elaborados a partir de granos de origen Venezolano y otro no 
# especificado. Luego, se pueden apreciar varias paises en los cuales se producen
# granos de cacao con la segunda mejor calificación. 

Origin_sorted <- Origin[with(Origin, order(-Origin$Rating_medio_por_Origen)), ]
head(Origin_sorted, 13)

# Se tienen los orígenes de los granos a partir de los cuales se produjeron 
# las barras de chocolate que obtuvieron el mayor ranking medio. Una vez, 
# Venezuela vuelve a liderar el grupo de mejores productores de granos de cacao.

# Conclusión: Los mejores granos de cacao se producen en Venezuela.

# b. ¿Qué paises producen las barras de cacao con mejor calificación?

df2 <- data.frame(Company.Location, Rating)
df2_sorted <- df2[with(df2, order(-Rating)), ]
head(df2_sorted, 20)

# Las 2 barras de cholate que logran la calificación 5 (Elite) fueron producidas
# por compañias que se encuentran en Italia, luego se observa que en otros países
# como USA, Francia, Brazil, Belgica y Suiza, entre otros, se produjeron barras
# con calificación 4 (Premium).

Location_sorted <- Location[with(Location, order(-Location$Rating_medio_por_Locacion)), ]
head(Location_sorted, 5)

# Se tienen las locaciones de las compañias productoras de las barras de
# chocolate que obtuvieron el mayor ranking en promedio. En este caso 
# Chile lidera el grupo de los mejores países productores.

# Conclusión: Las mejores 2 barras se produjeron en Italia, pero luego si asociamos
# un rating promedio a cada país Chile es el mejor productor.

# c. ¿Qué relación hay entre el porcentaje de cacao en una barra y su calificación?

ggplot(data.frame(Cocoa.Percent, Rating), aes(x = Cocoa.Percent, y = Rating)) +
  geom_point(shape = 19, color = Rating) +
  labs(title = "Gráfico de Dispersión", subtitle = "Rating - Cocoa.Percent") +
  labs(x = "Cocoa  %", y = "Rating") +
  theme_classic()

# No se observa una relación clara entre las variables ´Rating´ y ´Cocoa.Percent´.

cor(Cocoa.Percent, Rating) 

# Se tiene un coeficiente de correlation negativo, pero cercano a cero.
# Se concluye no existe una fuerte relación lineal entre el procentaje de
# cacao que contiene una barra de chocolate y su calidad (Rating).

# Se procede a elaborar un modelo simple de regresión lineal.

linear.model <- lm(Rating ~ Cocoa.Percent, data = flavors_of_cacao) 

print(linear.model) # Rating = 4.06 - 1.22 * Cacao.Percent
options(scipen = 10)
summary(linear.model) # R^2 del 2%

residuos <- linear.model$residuals
shapiro.test(residuos) # Los residuos NO son normales :(

# Se observa que los coeficientes del modelo lineal son significativos,
# sin embargo el R^2 es bajo. 

library(ggplot2)
ggplot(data.frame(Cocoa.Percent, Rating), aes(x = Cocoa.Percent, y = Rating)) +
  geom_point(shape = 19, color = Rating) +
  labs(title = "Regresión Lineal", subtitle = "Rating ~ Cocoa.Percent") +
  labs(x = "Cocoa  %", y = "Rating") +
  theme_classic() + 
  geom_smooth(method = 'lm', formula = y ~ x)

# Conclusión: no existe una relación clara entre el porcentaje de cacao utilizado 
# en la elaboración de una barra de chocolate y el rating que se le otorga.

#_________________________________________________________________________________________________

# Ejercicio N°2: Desarrollo de Funciones y Regresión

# En esta sección se confeccionan 2 modelos de regresión distintos, con el fin de explicar
# la varaible ´Rating´, el primero trata a la variable dependiente como continua, y el segundo
# como binaria.

flavors_of_cacao[Rating < 1, ] # 0
flavors_of_cacao[Rating >= 1 & Rating < 2, ] # 17
dim(flavors_of_cacao[Rating >= 2 & Rating < 3, ]) #430
dim(flavors_of_cacao[Rating >= 3 & Rating < 4, ]) #1244
dim(flavors_of_cacao[Rating >= 4 & Rating < 5, ]) #98
flavors_of_cacao[Rating == 5, ] # 2

# 1. 

# Se procede a guardar en una lista los 2 datasets con los que se va a trabajar; flavors_of_cacao.1
# para el enfoque con variable objetivo continua y flavors_of_cacao.2 para el enfoque con variable
# objetivo discreta.

# Pero primero, se realizan las modificaciones correspondientes sobre la variable objetivo ...

# IMPORTANTE: 
# Se opta por transformar el ´Rating´ de las barras con calficaión Satisfactoria, Premium y Elite
# a 1, y las demás a 0, debido a que existen solo 2 observaciones de barras con calificación 5
# dentro del dataset original.

Var.discreta = NULL
for (i in 1:length(Rating)) {
  if (Rating[i] >= 3 & Rating[i] <= 5) Var.discreta[i] = 1
  else Var.discreta[i] = 0
} 

flavors_of_cacao.2 = flavors_of_cacao
flavors_of_cacao.2[,"Rating"] = Var.discreta
colnames(flavors_of_cacao.2) = c("Company.Maker.if.known..2",
                                 "Specific.Bean.Origin.or.Bar.Name.2",
                                 "REF.2","Review.Date.2","Cocoa.Percent.2",                    
                                 "Company.Location.2","Rating.2","Bean.Type.2",
                                 "Broad.Bean.Origin.2") 
head(flavors_of_cacao.2) # Df con variable objetico binaria
attach(flavors_of_cacao.2)

flavors_of_cacao.1 = flavors_of_cacao
colnames(flavors_of_cacao.1) = c("Company.Maker.if.known..1",
                                 "Specific.Bean.Origin.or.Bar.Name.1",
                                 "REF.1","Review.Date.1","Cocoa.Percent.1",                    
                                 "Company.Location.1","Rating.1","Bean.Type.1",
                                 "Broad.Bean.Origin.1")
head(flavors_of_cacao.1) # Df con variable objetico continua
attach(flavors_of_cacao.1)

Datasets_2 <- list(flavors_of_cacao.1, flavors_of_cacao.2)

# 2 Tratamiento de las variables explicativas para realizar los modelos.

# 2.1 Enfoque para variable objetivo continua

# Se procede a clasificar las categorias propias de cada variable cualitativa como una
# variable numérica (1 a 5), según el rating promedio asociado a cada categoria.

Company.1 = Company  # Rating medio por compania (Company.Maker.if.known.)
attach(Company.1)

Company.1[Rating_medio_por_Compania >= 1 & Rating_medio_por_Compania < 2,]$Categorias_Company = 1
Company.1[Rating_medio_por_Compania >= 2 & Rating_medio_por_Compania < 3,]$Categorias_Company = 2
Company.1[Rating_medio_por_Compania >= 3 & Rating_medio_por_Compania < 4,]$Categorias_Company = 3
Company.1[Rating_medio_por_Compania >= 4 & Rating_medio_por_Compania < 5,]$Categorias_Company = 4
Company.1[Rating_medio_por_Compania == 5,]$Categorias_Company = 5 # Erro 0 data

Company.1 = cbind(Company$Categorias_Company,Company.1)
head(Company.1)

Bar.Name.1 = Bar.Name # Rating medio por nombre del bar (Specific.Bean.Origin.or.Bar.Name)
attach(Bar.Name.1)

Bar.Name.1[Rating_medio_por_Bar >= 1 & Rating_medio_por_Bar < 2,]$Categorias_Bar = 1
Bar.Name.1[Rating_medio_por_Bar >= 2 & Rating_medio_por_Bar < 3,]$Categorias_Bar = 2
Bar.Name.1[Rating_medio_por_Bar >= 3 & Rating_medio_por_Bar < 4,]$Categorias_Bar = 3
Bar.Name.1[Rating_medio_por_Bar >= 4 & Rating_medio_por_Bar < 5,]$Categorias_Bar = 4
Bar.Name.1[Rating_medio_por_Bar == 5,]$Categorias_Bar = 5 # Erro 0 data

Bar.Name.1 = cbind(Bar.Name$Categorias_Bar,Bar.Name.1)
head(Bar.Name.1)

Location.1 = Location # Rating medio por locación de la compania (Company.Location)
attach(Location.1)

Location.1[Rating_medio_por_Locacion >= 1 & Rating_medio_por_Locacion < 2,]$Categorias_Company.Location = 1 # Erro 0 data
Location.1[Rating_medio_por_Locacion >= 2 & Rating_medio_por_Locacion < 3,]$Categorias_Company.Location = 2
Location.1[Rating_medio_por_Locacion >= 3 & Rating_medio_por_Locacion < 4,]$Categorias_Company.Location = 3
Location.1[Rating_medio_por_Locacion >= 4 & Rating_medio_por_Locacion < 5,]$Categorias_Company.Location = 4 # Erro 0 data
Location.1[Rating_medio_por_Locacion == 5,]$Categorias_Company.Location = 5 # Erro 0 data

Location.1 = cbind(Location$Categorias_Company.Location,Location.1)
head(Location.1)

Bean.1 = Bean # Rating medio por tipo de grano (Bean.Type)
attach(Bean.1)

Bean.1[Rating_medio_por_Grano >= 1 & Rating_medio_por_Grano < 2,]$Categorias_Bean.Type = 1 # Erro 0 data
Bean.1[Rating_medio_por_Grano >= 2 & Rating_medio_por_Grano < 3,]$Categorias_Bean.Type = 2
Bean.1[Rating_medio_por_Grano >= 3 & Rating_medio_por_Grano < 4,]$Categorias_Bean.Type = 3
Bean.1[Rating_medio_por_Grano >= 4 & Rating_medio_por_Grano < 5,]$Categorias_Bean.Type = 4
Bean.1[Rating_medio_por_Grano == 5,]$Categorias_Bean.Type = 5 # Erro 0 data

Bean.1 = cbind(Bean$Categorias_Bean.Type,Bean.1)
head(Bean.1)

Origin.1 = Origin # Rating medio por país de origen (Broad.Bean.Origin)
attach(Origin.1)

Origin.1[Rating_medio_por_Origen >= 1 & Rating_medio_por_Origen < 2,]$Categorias_Origin = 1 # Erro 0 data
Origin.1[Rating_medio_por_Origen >= 2 & Rating_medio_por_Origen < 3,]$Categorias_Origin = 2
Origin.1[Rating_medio_por_Origen >= 3 & Rating_medio_por_Origen < 4,]$Categorias_Origin = 3
Origin.1[Rating_medio_por_Origen >= 4 & Rating_medio_por_Origen < 5,]$Categorias_Origin = 4
Origin.1[Rating_medio_por_Origen == 5,]$Categorias_Origin = 5 # Erro 0 data

Origin.1 = cbind(Origin$Categorias_Origin,Origin.1)
head(Origin.1)

# Una vez realizada la transformación de variables cualitativas a cuantitativas 
# se procede a incorporar las nuevas variables al dataset flavors_of_cacao.1.

for (j in Company.1$ `Company$Categorias_Company`) {
  flavors_of_cacao.1[,1][flavors_of_cacao.1[,1]==j] = Company.1[Company.1==j,]$Categorias_Company
}

for (j in Bar.Name.1$ `Bar.Name$Categorias_Bar`) {
  flavors_of_cacao.1[,2][flavors_of_cacao.1[,2]==j] = Bar.Name.1[Bar.Name.1==j,]$Categorias_Bar
}

for (j in Location.1$ `Location$Categorias_Company.Location`) {
  flavors_of_cacao.1[,6][flavors_of_cacao.1[,6]==j] = Location.1[Location.1==j,]$Categorias_Company.Location
}

for (j in Bean.1$ `Bean$Categorias_Bean.Type`) {
  flavors_of_cacao.1[,8][flavors_of_cacao.1[,8]==j] = Bean.1[Bean.1==j,]$Categorias_Bean.Type
}

for (j in Origin.1$ `Origin$Categorias_Origin`) {
  flavors_of_cacao.1[,9][flavors_of_cacao.1[,9]==j] = Origin.1[Origin.1==j,]$Categorias_Origin
}

View(flavors_of_cacao.1)
str(flavors_of_cacao.1)
attach(flavors_of_cacao.1)

# 2.2 Enfoque para variable objetivo discreta 

str(flavors_of_cacao.2)

# En este caso se vuelve a tomar el criterio de segmentación segun promedio de rating asociado
# a cada categoria, para así poder reducir la canitad de valores posibles dentro de cada 
# variable categórica.

flavors_of_cacao.2$Company.Maker.if.known..2 <- flavors_of_cacao.1$Company.Maker.if.known..1
flavors_of_cacao.2$Specific.Bean.Origin.or.Bar.Name.2 <- flavors_of_cacao.1$Specific.Bean.Origin.or.Bar.Name.1
flavors_of_cacao.2$Company.Location.2 <- flavors_of_cacao.1$Company.Location.1
flavors_of_cacao.2$Bean.Type.2 <- flavors_of_cacao.1$Bean.Type.1
flavors_of_cacao.2$Broad.Bean.Origin.2 <- flavors_of_cacao.1$Broad.Bean.Origin.1
head(flavors_of_cacao.2)
attach(flavors_of_cacao.2)

# Luego, se transforma cada variable cualitativa del dataset "flavors_of_cacao.2" en un factor.

flavors_of_cacao.2$Company.Maker.if.known..2 <- factor(Company.Maker.if.known..2)
flavors_of_cacao.2$Specific.Bean.Origin.or.Bar.Name.2 <- factor(Specific.Bean.Origin.or.Bar.Name.2)
flavors_of_cacao.2$Company.Location.2 <- factor(Company.Location.2)
flavors_of_cacao.2$Bean.Type.2 <- factor(Bean.Type.2)
flavors_of_cacao.2$Broad.Bean.Origin.2 <- factor(Broad.Bean.Origin.2)
str(flavors_of_cacao.2)
attach(flavors_of_cacao.2)

contrasts(Company.Maker.if.known..2)
contrasts(Specific.Bean.Origin.or.Bar.Name.2)
contrasts(Company.Location.2)
contrasts(Bean.Type.2)
contrasts(Broad.Bean.Origin.2)

# Nota: la función glm trabaja con variables de clase numérica y factores, por lo tanto no es
# necesario generar k-1 variable dummy para las k categorias que se encuentran dentro de cada
# variable cualitativa para entrenar el modelo logístico. Esto es porqur la funcíon glm realiza 
# mismo tranajo de forma interna. De todas formas, se procede a generar y mostrar las variables
# dummy ...

library(fastDummies)
names(flavors_of_cacao.2)
flavors_of_cacao.dummies = dummy_cols(flavors_of_cacao.2,
                                select_columns = c("Company.Maker.if.known..2",
                                                   "Specific.Bean.Origin.or.Bar.Name.2",
                                                   "Company.Location.2",
                                                   "Bean.Type.2",
                                                   "Broad.Bean.Origin.2"),
                                remove_first_dummy = T)

View(flavors_of_cacao.dummies)

# Se guardan en una lista los 2 datasets con las variables explicativas transformadas,
# junto con el dataset original flavour_of_cacao, al cual solo se le aplicaron 
# transformaciones con el fin de tratar los valores no disponibles (NA).

Datasets_3 <- list(flavors_of_cacao, flavors_of_cacao.1, flavors_of_cacao.2)

# 3 

# Se divide el dataset completo en un Training_set (población de entrenamiento) y en un
# Validation_set (población de validación).

# 3.1 Enfoque para variable objetivo continua

set.seed(1) 

Pos_trainingset.1 <- sample(1:nrow(flavors_of_cacao.1), round(0.7*nrow(flavors_of_cacao.1),0))
Training_set.1 <- flavors_of_cacao.1[Pos_trainingset.1, ]  
head(Training_set.1);dim(Training_set.1)

Validation_set.1  <- flavors_of_cacao.1[-Pos_trainingset.1, ]
head(Validation_set.1);dim(Validation_set.1)

# 3.2 Enfoque para variable objetivo discreta

set.seed(1) 

Pos_trainingset.2 <- sample(1:nrow(flavors_of_cacao.2), round(0.7*nrow(flavors_of_cacao.2),0))
Training_set.2 <- flavors_of_cacao.2[Pos_trainingset.2, ]  
head(Training_set.2);dim(Training_set.2)

Validation_set.2  <- flavors_of_cacao.2[-Pos_trainingset.2, ]
head(Validation_set.2);dim(Validation_set.2)

# Se guardan en otra lista todos los datasets (original, con variable coninua/discreta
# y para testear/validar), para luego entrenar y validar los modelos de regresión 
# múltiple a cofeccionar.

Datasets_7 <- list(flavors_of_cacao,
                   list(flavors_of_cacao.1, Training_set.1, Validation_set.1), 
                   list(flavors_of_cacao.2, Training_set.2, Validation_set.2))

# 4

# Se procede a entrenar los modelos de regresión, y luego validarlos. 

# 4.1 Regresión Lineal Múltiple (var. objetivo continua)

linear.model.1 <- lm(Rating.1 ~ Broad.Bean.Origin.1 +
                       Bean.Type.1 + Company.Location.1 +
                       Cocoa.Percent.1 + Specific.Bean.Origin.or.Bar.Name.1 +
                       Company.Maker.if.known..1,
                     data = Training_set.1)  
# Nota: Se omiten las columnas ´REF´ y ´Review.Date´ porque se entiende que no poseen
# poder explicativo sobre la variable objetivo.

print(linear.model.1) 
summary(linear.model.1) #R^2 > 50%
confint(linear.model.1) #IC

residuos.1 <- linear.model.1$residuals
shapiro.test(residuos.1) # Los residuos NO son normales :(

Rating_predictions <- predict(linear.model.1, Validation_set.1)

Validation <- data.frame(Prediction = Rating_predictions,
                        Validation_set = Validation_set.1$Rating.1,
                        Difference = Rating_predictions - Validation_set.1$Rating.1)
View(Validation)
mape = mean(abs((Rating_predictions - Validation_set.1$Rating.1)) / Validation_set.1$Rating.1) 
mape # mean absolute percentage error 8,9%

# Conclusión: El modelo tiene la mayoria de sus coeficientes significativos, aunque 
# posee un R^2 menor al 70% deseado. Sin embargo, el mape resultada ser 8,9%, lo cual
# es sorprendente y satisfactorio.

# Se intenta replicar el modelo anterior pero quitandole los coeficientes no significativos ...

linear.model.1.2 <- lm(Rating.1 ~ Bean.Type.1  +
                       Cocoa.Percent.1 + 
                       Specific.Bean.Origin.or.Bar.Name.1 +
                       Company.Maker.if.known..1,
                     data = Training_set.1)  
print(linear.model.1.2) 
summary(linear.model.1.2) #R^2 > 50%

AIC(linear.model.1)
AIC(linear.model.1.2) # Desafortunadamente, el nuevo modelo presenta un Akeike mayor al primero.

# 4.2 Regresión Logística Múltiple (var. objetivo binaria)

head(Training_set.2)
str(Training_set.2)

logistic.model.2.1 <- glm(Rating.2 ~ Broad.Bean.Origin.2 +
                          Bean.Type.2 + Company.Location.2 +
                          Cocoa.Percent.2 + Specific.Bean.Origin.or.Bar.Name.2 +
                          Company.Maker.if.known..2,
                        data = Training_set.2)

print(logistic.model.2.1) 
summary(logistic.model.2.1) # AIC: 582.11
confint(logistic.model.2.1)

# Se intenta un nuevo modelo con menor cantidad de regresores ...

logistic.model.2.2 <- glm(Rating.2 ~ Cocoa.Percent.2 +
                          Specific.Bean.Origin.or.Bar.Name.2 +
                          Company.Maker.if.known..2,
                        data = Training_set.2)

print(logistic.model.2.2) 
summary(logistic.model.2.2) # AIC: 572.75
confint(logistic.model.2.2)

# Se prefiere el segundo modelo de regresión logística, ya que posee un menor AIK y
# contiene una menor cantidad de parámetros NO significativos que el primero.

Rating_predictions <- predict(logistic.model.2.2, type = "response", newdata = Validation_set.2)

Table_predictions <- table(Validation_set.2$Rating.2 ,Rating_predictions >= 0.5)
Table_predictions # matriz de confusión

sum(diag(Table_predictions)) / sum(Table_predictions) # Prediction accuracy 84,5%

# Conlsuión: Se tiene un modelo la mitad de sus coeficientes NO significativos, sin embargo
# la medida de rendimiento elegida muestra que el modelo puede ser útil, ya que presenta
# una eficienncia del casi 85% para identificar las barras que seran aceptables (Elite, Premium, Satisfactorias) 
# y no aceptables (Insactifacotrias, Implacenteras).

# 5. Enfoque alternativo

# En esta sección se trata de entender que atributos contribuyen a la elaboración de las mejores 
# y peores barras de chocolate.

flavors_of_cacao[flavors_of_cacao$Rating == 5,]

# Como ya se ha mencionado antes, el dataset original contiene 2 observaciones de barras
# ´Elite´ 

# Ambas barras ´Elite´ fueron producidas en Italia por la compañia Amedei y además comparten
# el contenido de porcentaje de cocoa (70%). 

# Sin embargo, en su elaboración se utilizan granos cocoa de origen distinto, pues uno proviene 
# de Venezuela y el otro de un origen desconocido. Además, se utilizaron distintos tipos de granos
# en la elaboración de cada barra.

length(flavors_of_cacao[flavors_of_cacao$Bean.Type == "Blend",]$ Rating);mean(flavors_of_cacao[flavors_of_cacao$Bean.Type == "Blend",]$Rating)
length(flavors_of_cacao[flavors_of_cacao$Bean.Type == "Trinitario",]$ Rating);mean(flavors_of_cacao[flavors_of_cacao$Bean.Type == "Trinitario",]$Rating)

mean(flavors_of_cacao$Rating)
# Tanto Blend como Trinitario parecen ser un tipo de grano bastante utilizado en barras de chocolate, y 
# ambos tienen un promedio de rating asociado que se encuentra por encima de la media de la colmuna rating.

# A continuación, se analizan las contribuciones de los atributos para las barras de calificación 1 ...

flavors_of_cacao[flavors_of_cacao$Rating == 1,]

# Las 4 únicas barras con calificaión ´Implacentero´, poseen todas un porcentaje de contenido de cacao
# del 70% o mayor, y 3 de ellas fueron producidas en Bélgica, aunque por distintas companias.

# En cuento al tipo de grano utilizado para su elaboración, no se encuentran similitudes entre estas
# barras de baja calificación.

flavors_of_cacao[flavors_of_cacao$Specific.Bean.Origin.or.Bar.Name == "Dark",];mean(flavors_of_cacao[flavors_of_cacao$Specific.Bean.Origin.or.Bar.Name == "Dark",]$Rating)
flavors_of_cacao[flavors_of_cacao$Specific.Bean.Origin.or.Bar.Name == "Sensations Intense",]
flavors_of_cacao[flavors_of_cacao$Specific.Bean.Origin.or.Bar.Name == "Principe",];mean(flavors_of_cacao[flavors_of_cacao$Specific.Bean.Origin.or.Bar.Name == "Principe",]$Rating)
flavors_of_cacao[flavors_of_cacao$Specific.Bean.Origin.or.Bar.Name == "Baking",]

# De todas formas, se observa que los tipos de granos específicos "Sensations Intense" y "Baking"
# aparecen en el dataset únicamente en 2 de estas barras implacenteras. De forma adicional, las
# categórias "Dark" y "Principe" tienen asociado un rating promedio menor a la media global.

# Se recuerda que no es fácil encontrar una relación clara entre el rating de una barra y el porcentaje
# de cacao utilizado en su elaboración, sin la utilización de un modelo predictor.

ggplot(data.frame(Cocoa.Percent, Rating), aes(x = Cocoa.Percent, y = Rating)) +
  geom_point(shape = 19, color = Rating) +
  labs(title = "Gráfico de Dispersión", subtitle = "Rating - Cocoa.Percent") +
  labs(x = "Cocoa  %", y = "Rating") +
  theme_classic()
# Se tiene una gran concentración de observaciones entre los valores centrales de la variable ´Cacao.Percent´,
# donde algunas barras presentan buenas calificaiones y otras peores. 

# 6. Comparación de resultados entre los 3 enfoques utlizados

# Se encuentran ventajas y desventajas para cada enfoque utilizado durante el trabajo.
# Las conclusiones sobre cada enfoque se desarrollan en la conclusión final del informe.
