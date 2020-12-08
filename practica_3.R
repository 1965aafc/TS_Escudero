#Magdalena Cornejo
#Clase PrÃ¡ctica 3
#ARIMA vs VAR vs VEC

library(urca)
library(forecast)
library(vars)

#setwd("C:/Users/Magdalena Cornejo/Dropbox/Cursos/Forecasting - BCU 2020/Modulo 3/Clase 3")
df <- read.csv("crude_oil.csv")
s<-ts(df$spot, frequency=12, start=c(1986,1))
f<-ts(df$future, frequency=12, start=c(1986,1))
full.sample <- cbind(s,f) #muestra total

plot.ts(s, type="l", lwd=2, xlab="",ylab="",bty="n")
lines(f, type = "l",lwd=2, col="red", bty = "n", xlab = "", ylab = "",bty="n")
legend(1985, 100, legend=c("Precio spot", "Precio futuro"),
       col=c("black", "red"), lty=1, cex=1.2,lwd=2,bty = "n")

#In-sample: Enero 1986 - Diciembre 2014 (T=348)
#Out-of-sample: Enero 2015 - Diciembre 2019 (H=60)
in.sample <- cbind(log(s[1:348]), log(f[1:348]))
colnames(in.sample) <- c("log.s", "log.f")
in.sample <- ts(in.sample, start = c(1986,1), frequency = 12)
length(in.sample[,1])

out.of.sample <- cbind(log(s[349:408]), log(f[349:408]))
colnames(out.of.sample) <- c("log.s", "log.f")
out.of.sample <- ts(out.of.sample, start = c(2015,1), frequency = 12)
length(out.of.sample[,1])

## MODELO UNIVARIADO (ARIMA) SUELE ser un buen benchmark
#Estimo recursivamente un ARIMA para el precio spot y pronostico para h=1:
#Va a tardar (BASTANTE) porque va a aplicar 60 veces el algoritmo auto.arima()
fcst.arima <- matrix(0, nrow = 60, ncol = 1)  #armo un vector donde voy a ir almacenando los pronÃ³sticos
fcst.arima <- ts(fcst.arima, start=c(2015,1), frequency = 12)
#time(full.sample) me dice cómo almacena los meses o trimestes en ts
for(i in 1:60){
  train <- window(log(full.sample), end = 2014.917 + (i-1)/12) #voy moviendo el paso mes a mes
  arima <- auto.arima(train[,1]) #solo spot, AIC por defecto
  fcst.arima[i,] <- forecast(arima, h=1)$mean #pronóstico a 1 paso, pronóstico en nivel del log(spot); en VAR será en diff OJO
}

logs.test <- out.of.sample[,1]
#grÃ¡fico:
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(logs.test, main="log(s)", ylab = "", xlab = "")
plot(fcst.arima, col = "grey", lwd = 5, main="Forecast ARIMA", ylab = "", xlab = "")


## MODELO VAR (en diferencias)
#Estimo un VAR en diferencias para el periodo in-sample
y <- diff(in.sample) #necesito que sea estacionaria
colnames(y) <- c("dlog.s", "dlog.f")
VARselect(y, lag.max = 13, type = "const")
var7 <- VAR(y, p = 7, type = "const") #fija el modelo en var 7, pero podría reseleccionar como el AR
summary(var7) #ojo que in sample hay outliers que en situaciones normales debiera intervenir
#pues llega a confundirse los test de correlación de los residuos 
serial.test(var7, lags.pt = 13) #trabajemos con un 1% de significaciÃ³n
#incorporar un outlier me mejora la especificación pero no el pronóstico
#podría ser un step (efecto permanente) o una intervención

#Actualizo recursivamente los parÃ¡metros del VAR(7) #NOTAR que al no actualizar en modelo se favorece al Arima
#Pronostico para h=1 la difencia logarÃ�tmica del precio spot
#criterio de schwarz da un modelo parsimonioso
#si voy a modificar modelo paso a paso elijo un criterio
#AIC come más gl pero especifica mas rezagos
fcst.var <- matrix(0, nrow = 60, ncol = 1)  #armo un vector donde voy a ir almacenando los pronÃ³sticos
fcst.var <- ts(fcst.var, start=c(2015,1), frequency = 12)
for(i in 1:60){
  train <- window(diff(log(full.sample)), end = 2014.917 + (i-1)/12)
#aca puedo hacer el varselect y parametrizar el p
  var <- VAR(train, p = 7, type = "const")
  fcst.var[i,] <- forecast(var, h=1)$forecast$s$mean
}

dlogs.test <- diff(out.of.sample)[,1]
#grÃ¡fico:
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(dlogs.test, main="Diff(log(s))", ylab = "", xlab = "")
plot(fcst.var, col = "grey", lwd = 5, main="Forecast VAR(7)", ylab = "", xlab = "")
#recordar que estoy en diflog y debo recuperar niveles
#forecast da 4 valores: mean(est. puntual), lím inf y sup, ee

## MODELO VEC
#Estimo un VAR en niveles y evalÃºo cointegraciÃ³n (en el perÃ�odo in-sample)
VARselect(in.sample, lag.max = 13, type = "const")
var5 <- VAR(in.sample, p = 5, type = "const")
summary(var5)
serial.test(var5, lags.pt = 13) #(H0 ausencia de autocorr)
normality.test(var5, multivariate.only = TRUE)
#normalidad es fundamental en Johansen 
#usamos Jarque Bera
#se descompone en asimetria y curtosis
#outliers genera asimetría (podrian compensarse)
#curtosis se da en modelos de alta frecuencia (exceso concentracion en la media)
#Juselius dice que si el problema es curtosis entonces se puede aplicar igual johansen
#si hay asimetria hay que tratar outliers
#Johansen es como un ADF entonces es sensible a tendencia, se prueba y si no da luego se quita
#testeo cointegraciÃ³n via Johansen:
summary(ca.jo(in.sample, type = "trace", ecdet = "trend", K=5)) #incluir siempre tendencia
#r=0 rechazo, entonces paso a ver full rank, donde no debo rechazar
#si rechazo r<=1 entonces es full rank (r = 2 en este caso) lo que quiere decir que no hice RU
summary(ca.jo(in.sample, type = "eigen", ecdet = "trend", K=5))
#por ambos test tengo evidencia de cointegracion
#me fijo ahora si la trend que incorporé es significativa

#testeo la tendencia lineal: (debo darle el rango)
#no rechazo que no es significativa, debiera sacar la tendencia
lttest(ca.jo(in.sample, type = "trace", ecdet = "trend", K=5), r=1)
lttest(ca.jo(in.sample, type = "eigen", ecdet = "trend", K=5), r=1)


#vuelvo a testear cointegraciÃ³n eliminando la tendencia:
summary(ca.jo(in.sample, type = "trace", ecdet = "const", K=5))
summary(ca.jo(in.sample, type = "eigen", ecdet = "const", K=5))
#debo replicar porque al quitar la tendencia podria pasar que cambien las conclusiones 
#en muestra chica puede que difieran y el test de autovalor es más robusto para muestra chica
#se podrían calcular valores de test de muestra chica con boostrap pero no sabe si esta en R 

#estimo el VEC que usarÃ¡ para pronosticar:
cointest <- ca.jo(in.sample, type = "eigen", ecdet = "const", K=5) #guardo el test
vec <- cajorls(cointest, r=1) #estimo el test dado los resultados q encontre y r= 1 indico que hay una Ãºnica relaciÃ³n de cointegraciÃ³n
summary(vec$rlm)
#da muy significativo el ect1, 35% de ajuste frente a shocks (tutorial)
#con esto desarmo las diferencias y tomo en cuenta la cointegracion
vec2var <- vec2var(cointest, r=1) #esta funciÃ³n transforma el VEC en un VAR en niveles
predict(vec2var, n.ahead=1)$fcst$log.s #pronÃ³stico para h=1
#el resultado es en niveles log

#asumo que la evidencia de cointegraciÃ³n se mantiene en el tiempo
#re-estimo recursivamente el VEC a lo largo del perÃ�odo out-of-sample
#y voy haciendo pronÃ³sticos para h=1 del logaritmo del precio spot
fcst.vec <- matrix(0, nrow = 60, ncol = 1)  #armo un vector donde voy a ir almacenando los pronÃ³sticos
fcst.vec <- ts(fcst.vec, start=c(2015,1), frequency = 12)
for(i in 1:60){
  train <- window(log(full.sample), end = 2014.917 + (i-1)/12)
  cointest <- ca.jo(train, type = "eigen", ecdet = "const", K=5)
  vec2var <- vec2var(cointest, r=1)
  fcst.vec[i,] <- predict(vec2var, n.ahead=1)$fcst$s[1]
}

logs.test <- out.of.sample[,1]
#grafico
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(logs.test, main="log(s)", ylab = "", xlab = "")
plot(fcst.vec, col = "grey", lwd = 5, main="Forecast VEC", ylab = "", xlab = "")


#EVALUACION DE PRONOSTICOS
#OJO que los pronÃ³sticos del ARIMA y del VEC estÃ¡n en niveles logarÃ�tmicos y los del VAR en diferencias logarÃ�tmicas
#Re-expreso todos los pronÃ³sticos en USD/barril

fcst.arima_level <- exp(fcst.arima)
fcst.var_level <- exp(fcst.var+log(full.sample[348:407,1]))
fcst.vec_level <- exp(fcst.vec)
#para recuperar el nivel log le sumo el valor anterior a la diff
#luego aplico antilog para recuperar el nivel
#notar que va desde 348 y no desde 349

#grÃ¡fico
s.out <- full.sample[349:408,1]
s.out <- ts(s.out, start=c(2015,1), frequency = 12)

par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot.ts(s.out, main="Precio Spot (USD/barril)", ylab = "", xlab = "")
plot(fcst.arima_level, col = "grey", lwd = 5, main="PronÃ³sticos (h=1)", ylab = "", xlab = "")
lines(fcst.var_level)
lines(fcst.vec_level, col = "red")
legend(2017, 40, legend=c("ARIMA", "VAR", "VEC"), col=c("grey", "black", "red"), lwd = c(5,1,1), box.lty=0, ncol = 1, cex = 0.8, y.intersp=1)

#medidas de accuracy
library(forecast)
accuracy(fcst.arima_level,s.out)
accuracy(fcst.var_level,s.out)
accuracy(fcst.vec_level,s.out)

#diebold mariano y contrastar contra ARIMA
#las diferencias no son significativas (ver ppt)
#qué pasa si quiero pronosticar horizonte > 1
#readaptar los loop para h>1
#aplicar diebold mariano
#es de esperar que los errores de pronostico esten autocorrelacionados
#habria que trabajar con erroes estandares  a la autocorrelacion y heteroescedasticidad
#pag 6 ppt
#ver la practica siguiente
# si reseleccionara, podría lograr mejoras
e.fcst.arima <- s[349:408]-fcst.arima_level
e.fcst.var <- s[349:408]-fcst.var_level
e.fcst.vec <- s[349:408]-fcst.vec_level

dm.test(e.fcst.var, e.fcst.arima, alternative = "two.sided", power = 2)
dm.test(e.fcst.vec, e.fcst.arima, alternative = "two.sided", power = 2)