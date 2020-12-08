#Magdalena Cornejo
#Clase Práctica 1

#install.packages("zoo")
#install.packages("urca")

#raíces unitarias
library(zoo)
library(urca)

spread <- read.csv("spread.csv")
spread <- zoo(spread[,-1], order.by = as.Date(spread[,1], "%m/%d/%Y"))  #zoo brinda infraestructura para ts irregulares

soy <- read.csv("soy.csv")
soy <- ts(soy[,2], frequency = 12, start = c(1980,1))

ipc <- read.csv("ipc.csv")
ipc <- ts(ipc[,2], frequency = 12, start = c(2016,12))

plot.zoo(spread, main = "Spread bancario 90-180d")
plot.ts(soy, main = "Precio spot de la soja")
plot.ts(ipc, main = "Índice de Precios al Consumidor de Argentina")

#tests de RU

summary(ur.df(spread,type="drift",selectlags="BIC")) #ADF
summary(ur.pp(spread, type="Z-tau", model="constant",lags="long")) #Philips-Perron
summary(ur.kpss(spread,type="mu",lags="long")) #KPSS

summary(ur.df(log(soy),type="trend",selectlags="BIC"))
summary(ur.df(log(soy),type="drift",selectlags="BIC"))
summary(ur.df(diff(log(soy)),type="drift",selectlags="BIC"))
summary(ur.pp(log(soy), type="Z-tau", model="trend",lags="long"))
summary(ur.pp(diff(log(soy)), type="Z-tau", model="constant",lags="long"))
summary(ur.kpss(log(soy),type="tau",lags="long"))
summary(ur.kpss(diff(log(soy)),type="mu",lags="long"))

summary(ur.df(log(ipc),type="trend",selectlags="BIC")) 
summary(ur.df(diff(log(ipc)),type="trend",selectlags="BIC"))
summary(ur.df(diff(diff(log(ipc))),type="drift",selectlags="BIC"))
summary(ur.pp(log(ipc), type="Z-tau", model="trend",lags="short"))
summary(ur.pp(diff(log(ipc)), type="Z-tau", model="trend",lags="short"))
summary(ur.pp(diff(diff(log(ipc))), type="Z-tau", model="constant",lags="short"))
summary(ur.kpss(log(ipc),type="tau",lags="short"))
summary(ur.kpss(diff(log(ipc)),type="tau",lags="short"))
summary(ur.kpss(diff(diff(log(ipc))),type="mu",lags="short"))


#ARIMA (soja) 

fit <- auto.arima(diff(log(soy)), ic=c("aicc"))
summary(fit)
checkresiduals(fit)
autoplot(forecast(fit))
fit <- auto.arima(diff(log(soy)), ic=c("bic")) #selecciona mismo modelo con BIC
summary(fit)
checkresiduals(fit)
fit <- auto.arima(log(soy), ic=c("aicc")) #sin diferenciar
summary(fit) #agrega un componente I = 1
checkresiduals(fit)
autoplot(forecast(fit))


#ARIMA vs ETS

rm(list = ls()) #para remover todos los objetos que estÃ©n en el "Global Environment"

install.packages("Rtools")
install.packages("forecast")
library(forecast)

setwd("C:/Users/alvar/OneDrive/Documentos/Datacamp/TS_Escudero") #establezco el directorio
#shortcut ctrl+shift+h
df <- read.csv("ipc.csv", header = TRUE, sep = "," ) #importo los datos

ipc <- ts(df$ipc, start = c(2016,12), frequency = 12)
plot.ts(ipc, main = "Índice de Precios al Consumidor de Argentina")
abline(v=c(2020), col = 'red')
text(x=c(2020.3), y=250, labels = "Test data", col = 'blue')
text(x=c(2018.5), y=250, labels = "Training data", col = 'blue')

#Dividimos la muestra:
train <- window(ipc, end = c(2019,12))
test <- window(ipc, start = c(2020,1))

length(train)
length(test)

#Estimamos los modelos:
fit.arima <- auto.arima(train)
summary(fit.arima)
checkresiduals(fit.arima)

fit.ets <- ets(train)
summary(fit.ets)
checkresiduals(fit.ets)

#Generamos los pronÃ³sticos:
fcst.arima <- forecast(fit.arima, h = length(test))
fcst.ets <- forecast(fit.ets, h = length(test))
autoplot(fcst.arima)
autoplot(fcst.ets)

#Evaluamos el desempeño:
accuracy(fcst.arima$mean, test) #es el elemento mean del pronostico (lista)
accuracy(fcst.ets$mean, test)

#################################################################################
### con IPC mensual URY
install.packages("readxl")
library(readxl)
library(lubridate)

#http://www.ine.gub.uy/c/document_library/get_file?uuid=361cf1e4-7d00-416a-960b-d618a0b7d7e8&groupId=10181

setwd("C:/Users/alvar/OneDrive/Documentos/Datacamp/TS_Escudero") #establezco el directorio
df_ury <- read_excel("IPC gral var M_B10.xls", col_names = TRUE, skip =7) #importo los datos
#convierto tipos
df_ury$year <- year(df_ury$`Mes y año`)
df_ury$month <- month(df_ury$`Mes y año`)
df_ury$fecha <- paste(as.character(df_ury$year), "-", as.character(df_ury$month))
remove <- c(1000:1002)
df_ury <- df_ury[ - remove, ]
ipc_ury <- ts(df_ury$Índice, start = c(1937,7), frequency = 12)


plot.ts(ipc_ury, main = "Índice de Precios al Consumidor de Uruguay")
abline(v=c(1996), col = 'red')
text(x=c(2008.3), y=100, labels = "Test data", col = 'blue')
text(x=c(1967.5), y=100, labels = "Training data", col = 'blue')

#Dividimos la muestra:
train <- window(ipc_ury, end = c(1996,12))
test <- window(ipc_ury, start = c(1997,1))

length(train)
length(test)

#Estimamos los modelos:
fit.arima_ury <- auto.arima(train)
summary(fit.arima_ury)
checkresiduals(fit.arima_ury)

fit.ets_ury <- ets(train)
summary(fit.ets_ury)
checkresiduals(fit.ets_ury)

#Generamos los pronÃ³sticos:
fcst.arima_ury <- forecast(fit.arima_ury, h = length(test))
fcst.ets_ury <- forecast(fit.ets_ury, h = length(test))
autoplot(fcst.arima_ury)
autoplot(fcst.ets_ury)

#Evaluamos el desempeño:
accuracy(fcst.arima_ury$mean, test)
accuracy(fcst.ets_ury$mean, test)

##### período más corto
n_ury_corto <- which(df_ury$year<2004)
df_ury_corto <- as.data.frame(df_ury[- n_ury_corto, ])
head(df_ury_corto)
ipc_ury_corto <- ts(df_ury_corto$Índice, start = c(2004,1), frequency = 12)

plot.ts(ipc_ury_corto, main = "Índice de Precios al Consumidor de Uruguay (2004-2020)")
abline(v=c(2017), col = 'red')
text(x=c(2019.3), y=100, labels = "Test data", col = 'blue')
text(x=c(2010.5), y=100, labels = "Training data", col = 'blue')

#hago un autoarima para que me estime inicialmente las RU
fit.arima_ury_c <- auto.arima(ipc_ury_corto)
summary(fit.arima_ury_c)
checkresiduals(fit.arima_ury_c)
#hay una RU y una RU estacional


#RU
#dudas, por qué no se prueba con trend, drift y none
#se debe seleccionar el nro de lags?


summary(ur.df(ipc_ury_corto,type="trend",lags= 4, selectlags=c("BIC"))) 
summary(ur.pp(ipc_ury_corto, type="Z-tau", model="trend",lags="short"))
summary(ur.kpss(ipc_ury_corto,type="tau",lags="short"))
summary(ur.df(log(ipc_ury_corto),type="trend",selectlags="BIC")) 
summary(ur.pp(log(ipc_ury_corto), type="Z-tau", model="trend",lags="short"))
summary(ur.kpss(diff(ipc_ury_corto),type="tau",lags="short"))

plot.ts(diff(ipc_ury_corto), main = "Índice de Precios al Consumidor de Uruguay 1ra dif")
plot.ts(log(ipc_ury_corto), main = "Índice de Precios al Consumidor de Uruguay log")
plot.ts(diff(diff(ipc_ury_corto)), main = "Índice de Precios al Consumidor de Uruguay 2a dif")
plot.ts(diff(log(ipc_ury_corto)), main = "Índice de Precios al Consumidor de Uruguay diff log")


#Dividimos la muestra:
train <- window(ipc_ury_corto, end = c(2016,12))
test <- window(ipc_ury_corto, start = c(2017,1))

length(train)
length(test)

#Estimamos los modelos:
fit.arima_ury_corto <- auto.arima(train)
summary(fit.arima_ury_corto)
checkresiduals(fit.arima_ury_corto)

fit.ets_ury_corto <- ets(train)
summary(fit.ets_ury_corto)
checkresiduals(fit.ets_ury_corto)

fit.tbats_ury_corto <- tbats(train)
summary(fit.tbats_ury_corto)
checkresiduals(fit.tbats_ury_corto)

#Generamos los pronÃ³sticos:
fcst.arima_ury_corto <- forecast(fit.arima_ury_corto, h = length(test))
fcst.ets_ury_corto <- forecast(fit.ets_ury_corto, h = length(test))
fcst.tbats_ury_corto <- forecast(fit.tbats_ury_corto, h = length(test))
autoplot(fcst.arima_ury_corto)
autoplot(fcst.ets_ury_corto)
autoplot(fcst.tbats_ury_corto)

#Evaluamos el desempeño:
accuracy(fcst.arima_ury_corto$mean, test)
accuracy(fcst.ets_ury_corto$mean, test)
accuracy(fcst.tbats_ury_corto$mean, test)  #muy superior a los otros


#Dividimos la muestra con serie diff log:
train <- window(diff(log(ipc_ury_corto)), end = c(2016,12))
test <- window(diff(log(ipc_ury_corto)), start = c(2017,1))

length(train)
length(test)

#Estimamos los modelos:
fit.arima_ury_corto <- auto.arima(train)
summary(fit.arima_ury_corto)
checkresiduals(fit.arima_ury_corto)

fit.ets_ury_corto <- ets(train)
summary(fit.ets_ury_corto)
checkresiduals(fit.ets_ury_corto)

fit.tbats_ury_corto <- tbats(train)
summary(fit.tbats_ury_corto)
checkresiduals(fit.tbats_ury_corto)

#Generamos los pronÃ³sticos:
fcst.arima_ury_corto <- forecast(fit.arima_ury_corto, h = length(test))
fcst.ets_ury_corto <- forecast(fit.ets_ury_corto, h = length(test))
fcst.tbats_ury_corto <- forecast(fit.tbats_ury_corto, h = length(test))
autoplot(fcst.arima_ury_corto)
autoplot(fcst.ets_ury_corto)
autoplot(fcst.tbats_ury_corto)

#Evaluamos el desempeño:
accuracy(fcst.arima_ury_corto$mean, test)
accuracy(fcst.ets_ury_corto$mean, test)
accuracy(fcst.tbats_ury_corto$mean, test)  #se vuelven muy parecidos los tres trabajando la serie antes

#obtengo predicciones
forecast(fit.arima_ury_corto, h = 48)
220.64000*exp(0.0048788333)*exp(0.0045841958)*exp(0.0025136505) 
ipc_oct <- 220.64000*exp(0.0048788333)
ipc_nov <- ipc_oct*exp(0.0045841958)
ipc_dic <- ipc_nov*exp(0.0025136505)
ipc_oct
ipc_nov
ipc_dic
(ipc_dic/203.02000-1)*100
#chequear que esta sea la forma de deshacer el diff log