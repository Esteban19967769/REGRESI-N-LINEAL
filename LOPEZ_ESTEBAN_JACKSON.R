
# ENTREGA 1 _ ESTEBAN-LOPEZ-ALAMOS ----------------------------------------

library(skimr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(PerformanceAnalytics) # Analisis de Riesgo y Regresion
library(MASS)  # Para poder usar la funcion stepAIC
library(GGally)
library(nortest) #test estadisticos de regresion
library(lmtest) #para homocedasticidad
library(car) #para ejecutar influenceplot


# PREGUNTA 1 --------------------------------------------------------------
#Cargue los datos lluvia.csV

library(readr)
Lluvia <- read.csv("C:/Users/Luis/Downloads/DATA SCIENCE/ENTREGA TAREA 1/Lluvia.csv")
#Read.csv para que aparezcan las Ñ y el script en versiÓn UTF-8

#Elimino la variable x
Lluvia <- Lluvia[,c(-1)]

#Resumenes aleatorios
glimpse(Lluvia)
table(Lluvia$Koppen)
length(unique(Lluvia$Koppen))

#Búqueda de valores NA

dim(na.omit(Lluvia))
colMeans(is.na(Lluvia))
sum(is.na(Lluvia))

##Limpieza de tildes y otros elementos no deseados del dataset

Lluvia %>% janitor::clean_names()
names(Lluvia)
View(Lluvia)

names(Lluvia) <- c("koppen", "estacion", "lluvia", "velrafaga", "vel9am", 
                   "vel3pm", "hum9am", "hum3pm", "pres9am", "pre3pm", "temp9am", 
                   "nub9am", "nub3pm", "evaporacion", "sol", "temp3pm", 
                   "lluviahoy", "target")
str(Lluvia)

# PREGUNTA 1: VerifiCAR SINO codificar como factor las variables que corresponda

str(Lluvia)
Lluvia$LluviaHoy <- factor(ifelse(Lluvia$LluviaHoy ==1, "Excede 1 mm", "No excede 1 mm"))
Lluvia$Koppen <- as.factor(Lluvia$Koppen)
Lluvia$Estacion <- as.factor(Lluvia$Estacion)

str(Lluvia)

#Resumen exploratorio de los datos

skim(Lluvia)
#En Nub3pm se elimina el 9 porque los valores van de 0 a 8
Lluvia <- Lluvia %>%  filter(Nub3pm<=8) 


# RESUMEN PREGUNTA 1 ------------------------------------------------------

#Al cargar la base, se decidió eliminar el ID de la variable "X" porque no representa un dato
#importante para el problema, además, se codificó "LluviaHoy" como factores, el resumen estadístico
#mostró los principales estadísticos de posición, dispersión y centralidad de las variables
#y finalmente se eliminó el 9 de la variable "Nub3m"porque esta variable va de 0 a 8










# Pregunta 2: -------------------------------------------------------------

#Eliminar las variables factores, categÃ³ricas
dimnames(Lluvia)
lluvia_continuas <- Lluvia[,-c(1,2,17)]
str(lluvia_continuas)

#ggcorrplot(cor(lluvia_continuas))

corrplot(cor(lluvia_continuas),method = "color",
         addCoef.col = "black", # para agregar coeficientes
         number.cex = 0.5, # tamaÃ±o de los coeficientes
         tl.col = "black", # color nombres de columnas y filas
         tl.srt = 45)

#Subdivido las bases en 3 grupos para que me cargen los graficos de las variables contra target

lluvia_continuas2 <- lluvia_continuas[,c(1,2,3,4,5,15)]
lluvia_continuas3 <- lluvia_continuas[,c(6,7,8,9,10,15)]
lluvia_continuas4 <- lluvia_continuas[,c(11,12,13,14,15)]

chart.Correlation(lluvia_continuas2, histogram = TRUE)
chart.Correlation(lluvia_continuas3, histogram = TRUE)
chart.Correlation(lluvia_continuas4, histogram = TRUE)

#Para mas detalle
plot(lluvia_continuas$target~., data = lluvia_continuas)

#Resumen
hist(Lluvia$pres9am, col = "gray") #distribuciÃ³n normal
qqnorm(Lluvia$pres9am, pch = 19, col = "steelblue")
qqline(Lluvia$pres9am, col = "red", lwd = 2)

#Mientras menos los datos se ajusten a la recta (sobretodo los extremos)
# La variable no será normal

# RESUMEN PREGUNTA 2 ------------------------------------------------------

#La matriz de correlación demuestra que target esta altamente correlacionado
# con Hum3pm con 75% negativo, y positivamente con 57% el sol esta
# medianamente correlacionado con target. Se eliminaron las variables cualitativas
# de este análisis. Además se filtro en 3 gráficos el histograma existente
# en cada variable con la función chart.correlation











# Pregunta 3 --------------------------------------------------------------

Modelo_1 = lm(target ~ Temp3pm , data = Lluvia)
resumen <- summary(Modelo_1)
resumen
resumen$coefficients #Significancia del coeficiente
resumen$adj.r.squared #R2 coeficiente de determinaciÃ³n

ggplot(Lluvia, aes(Temp3pm, target)) +
  geom_point() + labs(title = "Regresion lineal Target ~ Temp3pm",
                      y = "Target") + geom_smooth()

#Grafico cuantil-cuantil
res_mod = Modelo_1$residuals
ggplot(data=Lluvia, aes(sample = res_mod))+
  stat_qq() + stat_qq_line()

nortest::lillie.test(res_mod)
#No es normal, Ho se rechaza para el test de Kolmogorov-Smirnov

#Notamos que en las colas de la distribuciÃ³n estas
#se alejan de la recta de los cuantiles normales,
#entonces con esta evidencia no se estarÃ­a
#cumpliendo el supuesto de normalidad.

#La variable es significativa con p-valor 0 y r2= 20,36%

#Revisamos cuales obseraciones perjudican el modelo
outlierTest(Modelo_1)
influencePlot(Modelo_1)

#Las filas 2091, 11142,13488,18293,29530 y 33513
Modelo_2 = lm(target ~ Temp3pm , 
              data = Lluvia[-c(229,459,8923,26081,27405,32682,10790,15039,16047,20139,20299,39975,2091,2452,11511,18282,21680,23787,30133,11142,13488,18293,29530,33513),])
summary(Modelo_2)

outlierTest(Modelo_2)
influencePlot(Modelo_2)


# RESUMEN PREGUNTA 3 ------------------------------------------------------

#La variable Temp3pm si es significativa a cualquier nivel de significancia
# porque su p-valor es cero. El coeficiente de determinación del modelo da como
# resultado un 20,36%, osea, un 20,36% de la variabilidad de la Diferencia de 
# temperatura el día siguiente de la medición es explicado por la variabilidad de la 
# temperatura en grados Celsius a las 3pm










# Pregunta 4 --------------------------------------------------------------

# BACKWARD ----------------------------------------------------------------
# Se considera el modelo con (y) y todas sus covariables luego
# con la función stepAIC se utiliza la dirección backward

full.model <- lm(target ~ ., data=Lluvia)
summary(full.model)

modback <- stepAIC(full.model, trace=TRUE, direction="backward")

#Resumen del proceso, el proceso no me eliminó ninguna variable, el modelo
# inicial y final es el mismo
modback$anova
#tabla de resultado
uno <- summary(modback)

# FORWARD -----------------------------------------------------------------
# se crea un modelo vacío que inicia el proceso y un horizonte o punto final de 
# búsqueda el cual considera (y) con todas sus covariables

empty.model <- lm(target ~ 1, data=Lluvia)
summary(empty.model)
modelo_completo <- lm(target ~., data = Lluvia)

# el modelo inicia vacío y termina completo
modforward<-step(empty.model, scope = list(lower=empty.model, upper=modelo_completo),direction = "forward")

modforward$anova

dos <- summary(modforward)

#Comparación de AIC para elegir el mejor
AIC(modback, modforward) #para considerar modelo con menor AIC

uno$adj.r.squared
dos$adj.r.squared

# RESUMEN PREGUNTA 4 ------------------------------------------------------

# Los modelos obtenidos son iguales, con el mismo valor en el criterio AIC
# El p-valor de ambos modelos es 0 con un estadístico F = 6582, son significativos
# y ambos modelosposeen un R2 de 78,35%











# PREGUNTA 5 --------------------------------------------------------------

par(mfrow=c(1, 2))
plot(modback, main="Backward", pch=19, cex=1, which=1)
plot(modforward, main="Forward", pch=19, cex=1, which=1)

#1 Normalidad de los residuos con Kolmogorov por ser muestra grande
lillie.test(modback$residuals)
# P-valor=0 se rechaza Ho y los residuos del modelo no son normales

#2 Media de los residuos, debe ser 0
mean(modback$residuals)
# La media de los errores es 0

#3 Multicolinealidad de los beta
vif(modback)
# Hay problemas de multicolinealidad al haber VIF > 10

#5 homocedasticidad
lmtest::bptest(modback)
#Se rechaza HO, residuos no tienen varianza constante

#6
dwtest(modback)
#No se rechaza Ho, los errores son independientes por ser p-valor=0,6594

#Grafico de residuos y valores ajustados
ggplot() +
  geom_point(aes(x = fitted(modback), y = resid(modback))) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red") +
  labs(title = "Residuos vs Valores ajustados",
       x = "Valores ajustados",
       y = "Esperanza de Residuos")


# RESUMEN PREGUNTA 5 ------------------------------------------------------

# El gráfico denota la heterocedasticidad existente puesto que hay puntos
# fuera de la concentración de los demás puntos de los gráficos. Además,
# el único supuesto que se cumple es que los errores son independientes, 
# mientras que los supuestos que no se cumplen son:
# residuos normales, residuos homocedasticos y no multicolinealidad











# Pregunta 6 --------------------------------------------------------------

Datos_extras <- read.csv("Datos extras.csv") #Read.csv para que lea las tildes, etc
#Elimino x
Datos_extras <- Datos_extras[,c(-1)]
#Cambio algunas variables a factores
Datos_extras$LluviaHoy <- factor(ifelse(Datos_extras$LluviaHoy ==1, "Excede 1 mm", "No excede 1 mm"))
Datos_extras$Koppen <- as.factor(Datos_extras$Koppen)
Datos_extras$Estacion <- as.factor(Datos_extras$Estacion)

predict(modback, Datos_extras, interval = "prediction")

# COMENTARIOS PREGUNTA 6 --------------------------------------------------

# El comando predict me ayuda a predecir las diferencias de temperaturas para cada fila
# de la base de datos "datos_extras" según los datos que aparecen y con un 95% de confianza
