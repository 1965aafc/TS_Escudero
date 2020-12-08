#Magdalena Cornejo
#Clase PrÃ¡ctica 2
#Model Selection

#install.packages("forecast")
#install.packages("glmnet") # Este paquete lo necesitamos para LASSO
#install.packages("corrplot") # Este paquete lo necesitamos para graficar la matriz de correlaciones
#install.packages("caret") #Classification And REgression Training

rm(list=ls()) 
library(glmnet)
library(corrplot)
library(forecast)
library(caret)

#setwd("C:/Users/Magdalena Cornejo/Dropbox/Cursos/Forecasting - BCU 2020/Modulo 2/Clase 2 (SincrÃ³nica)")
data <- read.csv("sample.csv")
#como predice yt+1 toma y desde el segundo dato
y <- ts(data$y[2:268], start = c(1947,2), frequency = 4)
ts.plot(y, main = "Equity Premium")
#consecuentemente, no toma el último dato de X
x <- as.matrix(data[1:267,3:13]) #matriz de variables independientes (laguedas un perÃ�odo)
#salva x como matriz para que corra corrplot
#dentro del conjunto de predictores voy a agregar el primer rezago de y:
lagy <- ts(data$y[1:267], start = c(1947,2), frequency = 4)
#podría haber trabajado con lag(y)
#x y lagy tienen la misma longitud, las conforma
x <- cbind(x, lagy)

#Mapa de calor (correlaciones)
corrplot(cor(x), method="color")
x <- ts(x, start = c(1947,2), frequency = 4)

#PRONOSTICOS RECURSIVOS
#Ventana inicial: 1947Q1-1969Q4
#Out-of-sample: 1970Q1-2013Q4 (T=176)
#PronÃ³sticos un paso adelante y reevalúa los parámetros
time(y) #me muestra cómo R almacena los datos de ahí el 1969.75

#Stepwise Selection
#selecciona los modelos con la funcion step que minimiza un criterio de info
#podría utilizarse otra librería, olsrr y la función ols_step_both_p() 
#que maximiza la significatividad del regresor al momento de evaluar su inclusión
#Backward: pronÃ³sticos recursivos
fcst.backward<-matrix(0, nrow = 176, ncol = 1)  #armo un vector donde voy a ir almacenando los pronÃ³sticos
fcst.backward <- ts(fcst.backward, start=c(1970,1), frequency = 4)
for(i in 1:176){
  y.train <- window(y, end = 1969.75 + (i-1)/4) #window es como un slice
  x.train <- window(x, end = 1969.75 + (i-1)/4) #va cambiando la ventana de entrenamiento y predicc
  x.test <- window(x, start = 1970 + (i-1)/4)
  #parte del full (.)
  #trace = FALSE evita el reporte de lo smodelos
  step.model <- step(lm(y.train~., data=x.train), direction = "backward", trace = FALSE)
  fcst.backward[i,] <- predict(step.model, newdata = x.test)[1]  #sólo retiene el pronóstico a un paso
}


#Forward: pronÃ³sticos recursivos
#parto del modelo lm = f(cte) y voy agregando parámetros
fcst.forward <- matrix(0, nrow = 176, ncol = 1)  #armo un vector donde voy a ir almacenando los pronÃ³sticos
fcst.forward <- ts(fcst.forward, start=c(1970,4), frequency = 4)
for(i in 1:176){
  y.train <- window(y, end = 1969.75 + (i-1)/4)
  x.train <- window(x, end = 1969.75 + (i-1)/4)
  x.test <- window(x, start = 1970 + (i-1)/4)
  full.model <- formula(lm(y.train~.,data = x.train))
  step.model <- step(lm(y.train~1, data=x.train), direction = "forward", trace = FALSE, scope = full.model)
  fcst.forward[i,] <- predict(step.model, newdata = x.test)[1]
}

#GRAFICOS
y.test <- window(y, start = c(1970,1))
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(y.test, main="Realized", ylab = "", xlab = "")
plot(fcst.forward, col = "grey", lwd = 5, main="Iterative Selection", ylab = "", xlab = "")
lines(fcst.backward)
legend(1980, -0.025, legend=c("Frw", "Bck"), col=c("grey", "black"), lwd = c(5,1), box.lty=0, ncol = 1, cex = 0.8, y.intersp=1)

#LASSO: Least Absolute Shrinkage and Selection Operator
#funciÃ³n glmnet() con alpha=1
#Probamos con 2 variantes de lambda. En los Ãºltimos dos seguimos la sugerencia de Belloni y Chernozhukov (2011) para c=0.002 y c=0.006
# c cercano a 0 para que no anulo los predictores

#PronÃ³sticos recursivos de la regresiÃ³n LASSO con lambda1
#glmnet corre lasso, se guarda el primer predictor solamente
fcst.lasso1<-matrix(0, nrow = 176, ncol = 1)  #armo un vector donde voy a ir almacenando los pronÃ³sticos
for(i in 1:176){
  y.train <- window(y, end = 1969.75 + (i-1)/4)
  x.train <- window(x, end = 1969.75 + (i-1)/4)
  x.test <- window(x, start = 1970 + (i-1)/4)
  reg <- lm(y.train~as.matrix(x.train))
  sigma <- sqrt(sum(reg$residuals^2)/(length(y.train)-12-1)) 
  lambda1 <- 2*0.002*sigma*sqrt(length(y.train))*qnorm(1-0.05/2*12)^(-1)
  refit.lasso1 <- glmnet(x.train, y.train, alpha = 1, lambda = lambda1)
  fcst.lasso1[i,] <- predict(refit.lasso1, s = lambda1, newx = x.test)[1]
}

fcst.lasso1 <- ts(fcst.lasso1, start=c(1970,1), frequency = 4)

#PronÃ³sticos recursivos de la regresiÃ³n LASSO con lambda2
fcst.lasso2<-matrix(0, nrow = 176, ncol = 1)  #armo un vector donde voy a ir almacenando los pronÃ³sticos
for(i in 1:176){
  y.train <- window(y, end = 1969.75 + (i-1)/4)
  x.train <- window(x, end = 1969.75 + (i-1)/4)
  x.test <- window(x, start = 1970 + (i-1)/4)
  reg <- lm(y.train~x.train)
  sigma <- sqrt(sum(reg$residuals^2)/(length(y.train)-12-1)) 
  lambda2 <- 2*0.006*sigma*sqrt(length(y.train))*qnorm(1-0.05/2*12)^(-1)
  refit.lasso2 <- glmnet(x.train, y.train, alpha = 1, lambda = lambda2)
  fcst.lasso2[i,] <- predict(refit.lasso2, s = lambda2, newx = x.test)[1]
}

fcst.lasso2 <- ts(fcst.lasso2, start=c(1970,1), frequency = 4)

#GRAFICOS
y.test <- window(y, start = c(1970,1))
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(y.test, main="Realized", ylab = "", xlab = "")
plot(fcst.lasso1, col = "grey", lwd = 5, main="Lasso", ylab = "", xlab = "")
lines(fcst.lasso2)
legend(1981, -0.008, legend=c("c=0.002", "c=0.006"), col=c("grey", "black"), lwd = c(5,1), box.lty=0, ncol = 1, cex = 0.8, y.intersp=1)

#PM (the prevailing mean model) -> BENCHMARK
fcst.pm<-matrix(0, nrow = 176, ncol = 1)  #armo un vector donde voy a ir almacenando los pronÃ³sticos
for(i in 1:176){
  y.train <- window(y, end = 1969.75 + (i-1)/4)
  fcst.pm[i,] <- mean(y.train)
}

fcst.pm <- ts(fcst.pm, start = c(1970,1), frequency = 4)

#GRAFICOS
y.test <- window(y, start = c(1970,1))
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(y.test, main="Realized", ylab = "", xlab = "")
plot(fcst.pm, col = "grey", lwd = 5, main="Prevailing Mean", ylab = "", xlab = "", type = "l")


#ACCURACY MEASURES
accuracy(fcst.forward, y.test)
accuracy(fcst.backward, y.test)
accuracy(fcst.lasso1, y.test)
accuracy(fcst.lasso2, y.test)
accuracy(fcst.pm, y.test)

#TEST DE DIEBOLD-MARIANO
#tengo que construirme la serie de errores de pronÃ³stico de cada modelo
e.fcst.forward <- as.numeric(y.test)-as.numeric(fcst.forward)
e.fcst.backward <- as.numeric(y.test)-as.numeric(fcst.backward)
e.fcst.lasso1 <- as.numeric(y.test)-as.numeric(fcst.lasso1)
e.fcst.lasso2 <- as.numeric(y.test)-as.numeric(fcst.lasso2)
e.fcst.pm <- as.numeric(y.test)-as.numeric(fcst.pm)

#contrasto a cada uno de los modelos respecto del benchmark (media histórica)
#H0 es comparar errores de pronóstico de j son en media equivalentes al benchmark
#es un test de diferencias de medias(?)
#no da diferencias significativas
#power define la funcion cuadrática o lineal de los errores, z defecto cuadrática
#se recom
dm.test(e.fcst.forward, e.fcst.pm, alternative = "two.sided", power = 2)
dm.test(e.fcst.backward, e.fcst.pm, alternative = "two.sided", power = 2)
dm.test(e.fcst.lasso1, e.fcst.pm, alternative = "two.sided", power = 2)
dm.test(e.fcst.lasso2, e.fcst.pm, alternative = "two.sided", power = 2)