library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
install.packages("fUnitRoots")
library("fUnitRoots")
install.packages("forecast")
library(forecast)

# Analisis Importacion 

summary(Importacion)

glimpse(Importacion)

table(Importacion$`Diesel bajo azufre`)

table(Importacion$`Diesel ultra bajo azufre`)

table(Importacion$`Gas licuado de petróleo`)

table(Importacion$`Diesel alto azufre`)

table(Importacion$`Gasolina regular`)

table(Importacion$`Gasolina superior`)

# Creacion de Series de tiempo y Graficos
Diesel_BA.ts <- ts(Importacion$`Diesel bajo azufre`, start = c(2001,1), frequency = 12)
plot(Diesel_BA.ts)

Diesel_UBA.ts <- ts(Importacion$`Diesel ultra bajo azufre`, start = c(2001,1), frequency = 12)
plot(Diesel_UBA.ts)

Diesel_AA.ts <- ts(Importacion$`Diesel alto azufre`, start = c(2001,1), frequency = 12)
plot(Diesel_AA.ts)

Gas_Licuado.ts <- ts(Importacion$`Gas licuado de petróleo`, start = c(2001,1), frequency = 12)
plot(Gas_Licuado.ts)

Gasolina_Regular.ts <- ts(Importacion$`Gasolina regular`, start = c(2001,1), frequency = 12)
plot(Gasolina_Regular.ts)

Gasolina_Superior.ts <- ts(Importacion$`Gasolina superior`, start = c(2001,1), frequency = 12)
plot(Gasolina_Superior.ts)

#Inicio y Fin de las Series
start(Diesel_AA.ts)
end(Diesel_AA.ts)

start(Diesel_BA.ts)
end(Diesel_BA.ts)

start(Diesel_UBA.ts)
end(Diesel_UBA.ts)

start(Gas_Licuado.ts)
end(Gas_Licuado.ts)

start(Gasolina_Regular.ts)
end(Gasolina_Regular.ts)

start(Gasolina_Superior.ts)
end(Gasolina_Superior.ts)

#Frecuencia de las Series
frequency(Diesel_BA.ts)

frequency(Diesel_UBA.ts)

frequency(Diesel_AA.ts)

frequency(Gas_Licuado.ts)

frequency(Gasolina_Regular.ts)

frequency(Gasolina_Superior.ts)

#Descomposicion de las Series 
Diesel_BA.ts = decompose(Diesel_BA.ts)
plot(Diesel_BA.ts)

Diesel_AA.ts = decompose(Diesel_AA.ts)
plot(Diesel_AA.ts)

Diesel_UBA.ts = decompose(Diesel_UBA.ts)
plot(Diesel_UBA.ts)

Gas_Licuado.ts = decompose(Gas_Licuado.ts)
plot(Gas_Licuado.ts)

Gasolina_Regular.ts = decompose(Gasolina_Regular.ts)
plot(Gasolina_Regular.ts)

Gasolina_Superior.ts = decompose(Gasolina_Superior.ts)
plot(Gasolina_Superior.ts)

#Test de Raices Unitarias y Graficos de Autocorrelacion 
adf.test(Diesel_BA.ts)

acf(Diesel_BA.ts)

adf.test(Diesel_AA.ts)

acf(Diesel_AA.ts)

adf.test(Diesel_UBA.ts)

acf(Diesel_UBA.ts)

adf.test(Gas_Licuado.ts)

acf(Gas_Licuado.ts)

adf.test(Gasolina_Regular.ts)

acf(Gasolina_Regular.ts)

adf.test(Gasolina_Superior.ts)

acf(Gasolina_Superior.ts)

# Modelos ARIMA
auto.arima(Diesel_BA.ts)

auto.arima(Diesel_AA.ts)

auto.arima(Diesel_UBA.ts)

auto.arima(Gas_Licuado.ts)

auto.arima(Gasolina_Regular.ts)

auto.arima(Gasolina_Superior.ts)

# Predicciones
forecast_Diesel_BA <- forecast(Diesel_BA.ts)
autoplot(forecast_Diesel_BA)

forecast_Diesel_AA <- forecast(Diesel_AA.ts)
autoplot(forecast_Diesel_AA)

forecast_Diesel_UBA <- forecast(Diesel_UBA.ts)
autoplot(forecast_Diesel_UBA)

forecast_Gas_Licuado <- forecast(Gas_Licuado.ts)
autoplot(forecast_Gas_Licuado)

forecast_Regular <- forecast(Gasolina_Regular.ts)
autoplot(forecast_Regular)

forecast_Superior <- forecast(Gasolina_Superior.ts)
autoplot(forecast_Superior)

# Analisis Consumo 

summary(Consumo)

table(Consumo$`Diesel bajo azufre`)

table(Consumo$`Diesel ultra bajo azufre`)

table(Consumo$`Gas licuado de petróleo`)

table(Consumo$`Diesel alto azufre`)

table(Consumo$`Gasolina regular`)

table(Consumo$`Gasolina superior`)

# Creacion de las Series de Tiempo y Graficos 

diesel_ba.ts <- ts(Consumo$`Diesel bajo azufre`, start = c(2000,1), frequency = 12)
plot(diesel_ba.ts)

diesel_uba.ts <- ts(Consumo$`Diesel ultra bajo azufre`, start = c(2000,1), frequency = 12)
plot(diesel_uba.ts)

diesel_aa.ts <- ts(Consumo$`Diesel alto azufre`, start = c(2000,1), frequency = 12)
plot(diesel_aa.ts)

gas_licuado.ts <- ts(Consumo$`Gas licuado de petróleo`, start = c(2000,1), frequency = 12)
plot(gas_licuado.ts)

gasolina_regular.ts <- ts(Consumo$`Gasolina regular`, start = c(2000,1), frequency = 12)
plot(gasolina_regular.ts)

gasolina_superior.ts <- ts(Consumo$`Gasolina superior`, start = c(2000,1), frequency = 12)
plot(gasolina_superior.ts)

# Inicio y Fin de las Series 

start(diesel_aa.ts)
end(diesel_aa.ts)

start(diesel_ba.ts)
end(diesel_ba.ts)

start(diesel_uba.ts)
end(diesel_uba.ts)

start(gas_licuado.ts)
end(gas_licuado.ts)

start(gasolina_regular.ts)
end(gasolina_regular.ts)

start(gasolina_superior.ts)
end(gasolina_superior.ts)

# Frecuencia de las Series 

frequency(diesel_ba.ts)

frequency(diesel_uba.ts)

frequency(diesel_aa.ts)

frequency(gas_licuado.ts)

frequency(gasolina_regular.ts)

frequency(gasolina_superior.ts)

# Descomposicion de las Series 

diesel_ba.ts = decompose(diesel_ba.ts)
plot(diesel_ba.ts)

diesel_aa.ts = decompose(diesel_aa.ts)
plot(diesel_aa.ts)

diesel_uba.ts = decompose(diesel_uba.ts)
plot(diesel_uba.ts)

gas_licuado.ts = decompose(gas_licuado.ts)
plot(gas_licuado.ts)

gasolina_regular.ts = decompose(gasolina_regular.ts)
plot(gasolina_regular.ts)

gasolina_superior.ts = decompose(gasolina_superior.ts)
plot(gasolina_superior.ts)

# Test de Raices Unitarias y Graficos de Correlacion 

adf.test(diesel_ba.ts)

acf(diesel_ba.ts)

adf.test(diesel_aa.ts)

acf(diesel_aa.ts)

adf.test(diesel_uba.ts)

acf(diesel_uba.ts)

adf.test(gas_licuado.ts)

acf(gas_licuado.ts)

adf.test(gasolina_regular.ts)

acf(gasolina_regular.ts)

adf.test(gasolina_superior.ts)

acf(gasolina_superior.ts)


# Modelos ARIMA 

auto.arima(diesel_ba.ts)

auto.arima(diesel_aa.ts)

auto.arima(diesel_uba.ts)

auto.arima(gas_licuado.ts)

auto.arima(gasolina_regular.ts)

auto.arima(gasolina_superior.ts)

# Predicciones 

forecast_diesel_ba <- forecast(diesel_ba.ts)
autoplot(forecast_diesel_ba)

forecast_diesel_aa <- forecast(diesel_aa.ts)
autoplot(forecast_diesel_aa)

forecast_diesel_uba <- forecast(diesel_uba.ts)
autoplot(forecast_diesel_uba)

forecast_gas_licuado <- forecast(gas_licuado.ts)
autoplot(forecast_gas_licuado)

forecast_regular <- forecast(gasolina_regular.ts)
autoplot(forecast_regular)

forecast_superior <- forecast(gasolina_superior.ts)
autoplot(forecast_superior)







