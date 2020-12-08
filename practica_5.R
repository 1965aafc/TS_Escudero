#Magdalena Cornejo
#Clase PrÃ¡ctica 5
#Modelos de volatilidad condicional

options(scipen = 999) 
library(forecast)
library(rugarch)
library(quantmod) #Quantitative Financial Modelling Framework
library(xts) #se usa para darle la forma ts

getSymbols('GOOGL', src = 'yahoo', from = '2010-01-01',
           to = "2020-09-30", warnings = FALSE,
           auto.assign = TRUE)

plot(GOOGL)
R <- diff(log(GOOGL$GOOGL.Close))
plot(R, main = "Retorno diario de Google")

#PerÃ­odo in-sample: inicio 2010-fines 2018 (T=2264)
#PerÃ­odo out-of-sample: inicio 2019-septiembre 2020 (H=440)
#One-step ahead rolling window forecasting
R.in <- window(R, end = "2018-12-31")
R.out <- window(R, start = "2019-01-02")
#no se programa con loops por ser muy lento
#en sustitución se usa ugarch***
#GARCH(1,1): 
#especifica el modelo, con media (por mas que suponga que es 0), le pide sin componente arma
#eso le deja los residuos heterocedásticos
#inicia en 2 por la diferencia, ojo que si no da error
spec.garch <- ugarchspec(mean.model=list(include.mean=T,armaOrder=c(0,0)),variance.model=list(garchOrder=c(1,1)))
fit.garch <- ugarchfit(data = R[2:2704], spec = spec.garch, out.sample = 439)
show(fit.garch) #da la salida
fcst.garch <- ugarchforecast(fit.garch, n.ahead = 1, n.roll = 440-1, out.sample = 440)
#ojo que los guarda apaisados por eso lo traspone:
fcst.garch.sigma <- t(fcst.garch@forecast$sigmaFor)
plot.ts(fcst.garch.sigma) #pronÃ³sticos de la volatilidad
#hace los retornos cuadráticos para estimar el error de predicción
R2.out <- R.out^2
index(R2.out) #los salva como "texto"
#la serie no tiene cotización todos los días (fin de semana,feriados)
index(fcst.garch.sigma) #no coincide el formato, está correlativo
#le cambia el formato para unificar
fcst.garch.sigma <- as.xts(fcst.garch.sigma,dateFormat='Date')
index(fcst.garch.sigma) #ahora sí está como fecha!

par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(R2.out, main="Actual", ylab = "", xlab = "")
#ojo le pido sigma**2
plot(fcst.garch.sigma^2, col = "red", main="GARCH(1,1)", ylab = "", xlab = "")

#EGARCH(1,1)
spec.egarch <- ugarchspec(mean.model=list(include.mean=T,armaOrder=c(0,0)),variance.model=list(model="eGARCH", garchOrder=c(1,1)))
fit.egarch <- ugarchfit(data = R[2:2704], spec = spec.egarch, out.sample = 439)
fcst.egarch <- ugarchforecast(fit.egarch, n.ahead = 1, n.roll = 440-1, out.sample = 440)
fcst.egarch.sigma <- t(fcst.egarch@forecast$sigmaFor)
par(mfrow=c(1,1))
plot.ts(fcst.egarch.sigma) #pronÃ³sticos de la volatilidad

fcst.egarch.sigma <- as.xts(fcst.egarch.sigma,dateFormat='Date')

par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(R2.out, main="Actual", ylab = "", xlab = "")
plot(fcst.egarch.sigma^2, col = "red", main="EGARCH(1,1)", ylab = "", xlab = "")


#TGARCH(1,1)
spec.tgarch <- ugarchspec(mean.model=list(include.mean=T,armaOrder=c(0,0)),variance.model=list(model="fGARCH", garchOrder=c(1,1), submodel="TGARCH"))
fit.tgarch <- ugarchfit(data = R[2:2704], spec = spec.tgarch, out.sample = 439)
fcst.tgarch <- ugarchforecast(fit.tgarch, n.ahead = 1, n.roll = 440-1, out.sample = 440)
fcst.tgarch.sigma <- t(fcst.tgarch@forecast$sigmaFor)
plot.ts(fcst.tgarch.sigma) #pronÃ³sticos de la volatilidad

fcst.tgarch.sigma <- as.xts(fcst.tgarch.sigma,dateFormat='Date')

par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(R2.out, main="Actual", ylab = "", xlab = "")
plot(fcst.tgarch.sigma^2, col = "red", main="TGARCH(1,1)", ylab = "", xlab = "")

#tengo los pronósticos de los 3 modelos y quiero saber si hay alguna ganancia 
#en la especificación de alguno de ellos
#Medidas de accuracy (simÃ©tricas)
#ojo que el DM en el slide está para fn de pérdida cuadrática
R2.out <- ts(R2.out, frequency = ) #le cambio formato a ts() para usar la funciÃ³n accuracy
fcst.garch.sigma <- ts(fcst.garch.sigma, frequency = ) #le cambio formato a ts() para usar la funciÃ³n accuracy
fcst.egarch.sigma <- ts(fcst.egarch.sigma, frequency = ) #le cambio formato a ts() para usar la funciÃ³n accuracy
fcst.tgarch.sigma <- ts(fcst.tgarch.sigma, frequency = ) #le cambio formato a ts() para usar la funciÃ³n accuracy
accuracy(fcst.garch.sigma^2, R2.out)
accuracy(fcst.egarch.sigma^2, R2.out)
accuracy(fcst.tgarch.sigma^2, R2.out)
#Medidas de accuracy (asimÃ©tricas)
LL.garch <- mean((log(R2.out)-log(fcst.garch.sigma^2))^2); LL.garch
LL.egarch <- mean((log(R2.out)-log(fcst.egarch.sigma^2))^2); LL.egarch
LL.tgarch <- mean((log(R2.out)-log(fcst.tgarch.sigma^2))^2); LL.tgarch

#Test de Diebold-Mariano
e.garch <- R2.out-fcst.garch.sigma^2
e.egarch <- R2.out-fcst.egarch.sigma^2 
e.tgarch <- R2.out-fcst.tgarch.sigma^2

dm.test(e.garch, e.egarch, alternative = "two.sided", power = 2)
dm.test(e.garch, e.tgarch, alternative = "two.sided", power = 2)

dm.test(e.garch, e.egarch, alternative = "two.sided", power = 1)
dm.test(e.garch, e.tgarch, alternative = "two.sided", power = 1)

#Test de Giacomini-Rossi
library(murphydiagram)

loss.garch <-  e.garch^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)
loss.egarch <- e.egarch^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)
loss.tgarch <- e.tgarch^2 #armo las funciones de pÃ©rdidas (cuadrÃ¡ticas)

fluctuation_test(loss.egarch,loss.garch, mu = 0.5)
fluctuation_test(loss.tgarch,loss.garch, mu = 0.5)


loss.garch <-  abs(e.garch) #armo las funciones de pÃ©rdidas (absolutas)
loss.egarch <- abs(e.egarch) #armo las funciones de pÃ©rdidas (absolutas)
loss.tgarch <- abs(e.tgarch) #armo las funciones de pÃ©rdidas (absolutas)
#ojo que los gráficos muestran la comparacion con la base, el GARCH(1,1)
fluctuation_test(loss.egarch,loss.garch, mu = 0.5)
fluctuation_test(loss.tgarch,loss.garch, mu = 0.5)