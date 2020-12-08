#Magdalena Cornejo
#Clase PrÃ¡ctica 4
#ARIMA vs VAR vs FAVAR

install.packages("dplyr")
install.packages("vars")
install.packages("forecast")

options(scipen = 999) 
library(forecast)
library(vars)
library(dplyr)

#setwd("C:/Users/Magdalena Cornejo/Dropbox/Cursos/Forecasting - BCU 2020/Modulo 4")
data <- read.csv("macro.csv")
attach(data)

lista <- c("C1.1.GDP251", "C1.2.GDP252", "C1.6.GDP256", "C1.7.GDP257", "C1.8.GDP258", "C1.14.GDP265", "C2.1.IPS10", "C2.2.IPS11", "C2.3.IPS299", "C2.4.IPS12", "C2.8.IPS32", "C3.1.CES002", "C3.2.CES003", "C3.5.CES015", "C3.16.LHEM", "C5.1.HSFR", "C5.2.HSBR", "C7.1.GDP272A", "C7.2.GDP273A", "C7.3.CPIAUCSL", "C7.4.PCEPILFE", "C7.5.CPILFESL", "C7.6.GDP274A", "C7.10.GDP275A", "C7.15.GDP276A", "C7.17.GDP276_2", "C7.24.GDP277A",	"C7.25.GDP278A",	"C7.26.GDP279A", "C7.32.GDP286A", "C7.36.PW561", "C8.1.CES275R", "C9.3.FYGM6", "C9.5.FYGT5", "C9.7.FYAAAC", "C9.8.FYBAAC")
macro.agg <- data %>% dplyr::select(one_of(lista))
macro.disagg <- data %>% dplyr::select(-one_of(lista))
macro.disagg <- macro.disagg[,-1] #eliminÃ© la primera variable ("quarter") que corresponde al Ã­ndice temporal
length(macro.agg)
length(macro.disagg)

#elimino (por missings) los datos de 1959
macro.agg <- tail(macro.agg, -4)
macro.disagg <- tail(macro.disagg, -4)

#doy formato de series temporales:
macro.agg <- ts(macro.agg, start = c(1960,1), frequency = 4)
macro.disagg <- ts(macro.disagg, start = c(1960,1), frequency = 4)

y <- cbind(macro.agg[,"C1.1.GDP251"], macro.agg[,"C3.16.LHEM"], macro.agg[,"C1.6.GDP256"])
colnames(y) <- c("dlog.GDP", "dlog.E", "dlog.I")
plot(y, main="")


#PronÃ³sticos rolling del FAVAR para h=1
fcst.favar <- matrix(0, nrow = 96, ncol = 4) #matriz para almacenar prostico, li, ls etc
fcst.favar <- ts(fcst.favar, start=c(1985,1), frequency = 4)
for(i in 1:96){
  y.train <- window(y, start = 1960 + (i-1)/4, end = 1984.75 + (i-1)/4) #ventana fija que se corre
  macro.disagg.train <- window(macro.disagg, start = 1960 + (i-1)/4, end = 1984.75 + (i-1)/4)
  pr.out <- prcomp(macro.disagg.train, scale =TRUE) 
  PC <- scale(macro.disagg.train)%*%pr.out$rotation #scale normaliza, %*% multiplica matrices
  factores <- PC[,1:5]
  y.train <- cbind(y.train, factores)
  var2 <- VAR(y.train, p=2, type = "const")
  forecasts <- predict(var2, n.ahead = 1)
  fcst.favar[i,] <- forecasts$fcst$y.train.dlog.GDP
}

#PronÃ³sticos rolling del VAR para h=1
fcst.var <- matrix(0, nrow = 96, ncol = 4)  
fcst.var <- ts(fcst.var, start=c(1985,1), frequency = 4)
for(i in 1:96){
  y.train <- window(y, start = 1960 + (i-1)/4, end = 1984.75 + (i-1)/4)
  var2 <- VAR(y.train, p=2, type = "const")
  forecasts <- predict(var2, n.ahead = 1)
  fcst.var[i,] <- forecasts$fcst$dlog.GDP
}

#PronÃ³sticos rolling del AR(1) para h=1 (va a tardar unos minutos)
fcst.ar1 <- matrix(0, nrow = 96, ncol = 1)  
fcst.ar1 <- ts(fcst.ar1, start=c(1985,1), frequency = 4)
for(i in 1:96){
  y.train <- window(macro.agg[, "C1.1.GDP251"], start = 1960 + (i-1)/4, end = 1984.75 + (i-1)/4)
  fit <- arima(y.train, order = c(1,0,0))
  forecasts <- predict(fit, n.ahead = 1)
  fcst.ar1[i,] <- forecasts$pred
}

#Grafico los pronÃ³sticos de los 3 modelos:
y.test <- macro.agg[101:196,"C1.1.GDP251"]
y.test <- ts(y.test, start=c(1985,1), frequency = 4)

par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(y.test, main="Actual", ylab = "", xlab = "")
plot(fcst.favar[,"Series 1"], col = "grey", lwd = 5, main="Forecasts", ylab = "", xlab = "")
lines(fcst.var[,"Series 1"])
lines(fcst.ar1, col = "red")
legend(1985, 0, legend=c("FAVAR", "VAR", "AR(1)"), col=c("grey", "black", "red"), lwd = c(5,1,1), box.lty=0, ncol = 1, cex = 0.8, y.intersp=0.8)

#Obtengo las medidas de accuracy: (evaluación media del modelo en todo el período)
accuracy(fcst.favar[,"Series 1"],y.test)
accuracy(fcst.var[,"Series 1"],y.test)
accuracy(fcst.ar1,y.test)

#Obtengo los errores de pronÃ³stico:
e.fcst.favar <- y[101:196,1]-fcst.favar[,"Series 1"]
e.fcst.var <- y[101:196,1]-fcst.var[,"Series 1"]
e.fcst.ar1 <- y[101:196,1]-fcst.ar1

#Test de Diebold-Mariano:
dm.test(e.fcst.favar, e.fcst.ar1, alternative = "two.sided", power = 2)
dm.test(e.fcst.var, e.fcst.ar1, alternative = "two.sided", power = 2)

#Test de Giacomini-Rossi:
install.packages("murphydiagram")
library(murphydiagram)
#obs out of sample > 30
loss.favar <-  e.fcst.favar^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)
loss.var <-  e.fcst.var^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)
loss.ar1 <-  e.fcst.ar1^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)

fluctuation_test(loss.favar,loss.ar1, mu = 0.5) #mu es la inversa de m, ver paper
fluctuation_test(loss.var,loss.ar1, mu = 0.5)
#hacerlo para los otros, se observa que no hay cambio estructural en el periodo

#PronÃ³sticos rolling del FAVAR para h=2
fcst.favar <- matrix(0, nrow = 95, ncol = 4) 
fcst.favar <- ts(fcst.favar, start=c(1985,2), frequency = 4)
#ojo que cambia el largo del loop, pensarlo
for(i in 1:95){
  y.train <- window(y, start = 1960 + (i-1)/4, end = 1984.75 + (i-1)/4)
  macro.disagg.train <- window(macro.disagg, start = 1960 + (i-1)/4, end = 1984.75 + (i-1)/4)
  pr.out <- prcomp(macro.disagg.train, scale =TRUE) 
  PC <- scale(macro.disagg.train)%*%pr.out$rotation
  factores <- PC[,1:5]
  y.train <- cbind(y.train, factores)
  var2 <- VAR(y.train, p=2, type = "const")
  forecasts <- predict(var2, n.ahead = 2)
  fcst.favar[i,] <- t(forecasts$fcst$y.train.dlog.GDP[2,])
}

#PronÃ³sticos rolling del VAR para h=2
fcst.var <- matrix(0, nrow = 95, ncol = 4)  
fcst.var <- ts(fcst.var, start=c(1985,2), frequency = 4)
for(i in 1:95){
  y.train <- window(y, start = 1960 + (i-1)/4, end = 1984.75 + (i-1)/4)
  var2 <- VAR(y.train, p=2, type = "const")
  forecasts <- predict(var2, n.ahead = 2)
  fcst.var[i,] <- forecasts$fcst$dlog.GDP[2,]
}

#PronÃ³sticos rolling del ARMA para h=2 (va a tardar unos minutos)
fcst.ar1 <- matrix(0, nrow = 95, ncol = 1)  
fcst.ar1 <- ts(fcst.ar1, start=c(1985,2), frequency = 4)
for(i in 1:95){
  y.train <- window(macro.agg[, "C1.1.GDP251"], start = 1960 + (i-1)/4, end = 1984.75 + (i-1)/4)
  fit <- arima(y.train, order = c(1,0,0))
  forecasts <- predict(fit, n.ahead = 2)
  fcst.ar1[i,] <- forecasts$pred[2]
}

#Grafico los pronÃ³sticos de los 3 modelos:
y.test <- macro.agg[102:196,"C1.1.GDP251"]
y.test <- ts(y.test, start=c(1985,2), frequency = 4)

par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(y.test, main="Actual", ylab = "", xlab = "")
plot(fcst.favar[,"Series 1"], col = "grey", lwd = 5, main="Forecasts", ylab = "", xlab = "")
lines(fcst.var[,"Series 1"])
lines(fcst.ar1, col = "red")
legend(1995, 0.02, legend=c("FAVAR", "VAR", "AR(1)"), col=c("grey", "black", "red"), lwd = c(5,1,1), box.lty=0, ncol = 1, cex = 0.8, y.intersp=0.8)

#Obtengo las medidas de accuracy:
accuracy(fcst.favar[,"Series 1"],y.test)
accuracy(fcst.var[,"Series 1"],y.test)
accuracy(fcst.ar1,y.test)

#Obtengo los errores de pronÃ³stico:
e.fcst.favar <- y[102:196,1]-fcst.favar[,"Series 1"]
e.fcst.var <- y[102:196,1]-fcst.var[,"Series 1"]
e.fcst.ar1 <- y[102:196,1]-fcst.ar1

#Test de Diebold Mariano (con HACSE) (ojo que hay dependencia temporal pues estimo t+2 con el pronostico t+1)
loss.favar <-  e.fcst.favar^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)
loss.var <-  e.fcst.var^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)
loss.ar1 <-  e.fcst.ar1^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)
dl.favar <- loss.favar-loss.ar1 #armo la diferencia de las funciones de pÃ©rdida (con respecto al benchmark: AR(1))
dl.var <- loss.var-loss.ar1 #armo la diferencia de las funciones de pÃ©rdida (con respecto al benchmark: AR(1))

NW.COV <- NeweyWest(lm(dl.var~1))#obtengo la matriz de var-cov de Newey-West (errores consistentes a autocorr y heteroesc)
coeftest(lm(dl.var~1), vcov. = NW.COV)#testeo la significatividad de la constante

NW.COV <- NeweyWest(lm(dl.var~1))#obtengo la matriz de var-cov de Newey-West
coeftest(lm(dl.favar~1), vcov. = NW.COV)#testeo la significatividad de la constante

#Test de Giacomini-Rossi:
fluctuation_test(loss.favar,loss.ar1, mu = 0.5) #ojo con el orden
fluctuation_test(loss.var,loss.ar1, mu = 0.5)
#cuanto mas positivo peor le va al primero resp al segundo (más pérdida)
#es distinto el resultado si trabajo con evaluacion media en el periodo vs local
#outliers pueden influenciar desempeño medio

#PronÃ³sticos rolling del FAVAR para h=4
#razonar el largo del loop sobre la linea del tiempo, pierdo las primeras cuatro obs

fcst.favar <- matrix(0, nrow = 93, ncol = 4) 
fcst.favar <- ts(fcst.favar, start=c(1985,4), frequency = 4)
for(i in 1:93){
  y.train <- window(y, start = 1960 + (i-1)/4, end = 1984.99 + (i-1)/4)
  macro.disagg.train <- window(macro.disagg, start = 1960 + (i-1)/4, end = 1984.75 + (i-1)/4)
  pr.out <- prcomp(macro.disagg.train, scale =TRUE) 
  PC <- scale(macro.disagg.train)%*%pr.out$rotation
  factores <- PC[,1:5]
  y.train <- cbind(y.train, factores)
  var2 <- VAR(y.train, p=2, type = "const")
  forecasts <- predict(var2, n.ahead = 4)
  fcst.favar[i,] <- forecasts$fcst$y.train.dlog.GDP[4,]
}

#PronÃ³sticos rolling del VAR para h=4
fcst.var <- matrix(0, nrow = 93, ncol = 4)  
fcst.var <- ts(fcst.var, start=c(1984,1), frequency = 4)
for(i in 1:93){
  y.train <- window(y, start = 1960 + (i-1)/4, end = 1984.75 + (i-1)/4)
  var2 <- VAR(y.train, p=2, type = "const")
  forecasts <- predict(var2, n.ahead = 4)
  fcst.var[i,] <- forecasts$fcst$dlog.GDP[4,]
}

#PronÃ³sticos rolling del ARMA para h=4 (va a tardar unos minutos)
fcst.ar1 <- matrix(0, nrow = 93, ncol = 1)  
fcst.ar1 <- ts(fcst.ar1, start=c(1985,4), frequency = 4)
for(i in 1:93){
  y.train <- window(macro.agg[, "C1.1.GDP251"], start = 1960 + (i-1)/4, end = 1984.75 + (i-1)/4)
  fit <- arima(y.train, order = c(1,0,0))
  forecasts <- predict(fit, n.ahead = 4)
  fcst.ar1[i,] <- forecasts$pred[4]
}
#OJO CON LA CANTIDAD DE OBSERVACIONES DEL TEST, debe ser el correcto

#Grafico los pronÃ³sticos de los 3 modelos:
y.test <- macro.agg[104:196,"C1.1.GDP251"] #empieza en 104 no en 101
#los primeros 3 no los pronostica
y.test <- ts(y.test, start=c(1985,4), frequency = 4)

par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(y.test, main="Actual", ylab = "", xlab = "")
plot(fcst.favar[,"Series 1"], col = "grey", lwd = 5, main="Forecasts", ylab = "", xlab = "")
lines(fcst.var[,"Series 1"])
lines(fcst.ar1, col = "red")
legend(1985, 0.014, legend=c("FAVAR", "VAR", "AR(1)"), col=c("grey", "black", "red"), lwd = c(5,1,1), box.lty=0, ncol = 1, cex = 0.8, y.intersp=0.8)

#Obtengo las medidas de accuracy:
accuracy(fcst.favar[,"Series 1"],y.test)
accuracy(fcst.var[,"Series 1"],y.test)
accuracy(fcst.ar1,y.test)


#Obtengo los errores de pronÃ³stico:
e.fcst.favar <- y[104:196,1]-fcst.favar[,"Series 1"]
e.fcst.var <- y[104:196,1]-fcst.var[,"Series 1"]
e.fcst.ar1 <- y[104:196,1]-fcst.ar1

#Test de Diebold Mariano (con HACSE)
loss.favar <-  e.fcst.favar^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)
loss.var <-  e.fcst.var^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)
loss.ar1 <-  e.fcst.ar1^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)
dl.favar <- loss.favar-loss.ar1 #armo la diferencia de las funciones de pÃ©rdida (con respecto al benchmark: AR(1))
dl.var <- loss.var-loss.ar1 #armo la diferencia de las funciones de pÃ©rdida (con respecto al benchmark: AR(1))

NW.COV <- NeweyWest(lm(dl.var~1))#obtengo la matriz de var-cov de Newey-West
coeftest(lm(dl.var~1), vcov. = NW.COV)#testeo la significatividad de la constante

NW.COV <- NeweyWest(lm(dl.var~1))#obtengo la matriz de var-cov de Newey-West
coeftest(lm(dl.favar~1), vcov. = NW.COV)#testeo la significatividad de la constante

#Test de Giacomini-Rossi:
fluctuation_test(loss.favar,loss.ar1, mu = 0.5)
fluctuation_test(loss.var,loss.ar1, mu = 0.5)