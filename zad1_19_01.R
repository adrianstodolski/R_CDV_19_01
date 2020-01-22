library(ggplot2) # Bibliotek z której będziemy korzystać przy tworzeniu wykresu
data_ap <- AirPassengers #
plot(data_ap) # widzimy, ze dane maja trend i sezonowosc

class(data_ap)
str(data_ap) # struktura danych
# sprawdzenie danych -> tylko time series
start(data_ap)
end(data_ap)
frequency(data_ap)

# naniesienie trendu na wykres
plot(data_ap, ylab = "Liczba pasażerów")
abline(lm(data_ap ~ time(data_ap)))

# analiza danych
decompose(data_ap, type = "multiplicative") # rozkladanie danych na skladowe
# w rezultacie otrzymamy kilka komponentow

# Przedstawiamy dekompozycje na wykresie
ap_deco <- decompose(data_ap, type = "multiplicative")
plot(ap_deco)

#######################
Prognozowanie przyszlosci
cycle(data_ap)

# dane po miesiacach
boxplot(data_ap ~ cycle(data_ap))

# do tej pory dane maja trend
plot(data_ap, ylab = "Liczba pasażerów")
# dalsz analiza - wyrownujemy dane
plot(log(data_ap))
# usunmy trend (funkcja diff) - stosujemy tyle razy az uzyskamy stala srednia
plot(diff(log(data_ap)))

# Przechodzimy do tworzenia modelu
# model ARIMA
# Jest to zintegrowania(I) regresji(AR) i sredniej kraczacej(MA)
# aby uzyc model mamy 3 parametry
# AR -> p
# I -> d
# MA -> q

acf(diff(log(data_ap)))
pacf(diff(log(data_ap)))
plot(diff(log(data_ap)))

# Tworzymy model
mod <- arima(log(data_ap), c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))

# Prognozujemy dane na 5 lat
mod_pred <- predict(mod, n.ahead = 5*12)
summary(mod)

pred1 <- 2.718^mod_pred$pred
ts.plot(data_ap,pred1,lty=c(1,3)) #lty 1,3 typ lini : 1 oraz 3

ap_szkoleniowe <- ts(data_ap, frequency = 12, start = c(1949,1), end =c(1959,12))
model_1 <- arima(log(ap_szkoleniowe), c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
ap_pred <- predict(model_1, n.ahead = 1*12)
ap_pred <- 2.718^ap_pred$pred
ap_pred

ap_pred=round(ap_pred,digit=0)
#Porównajmy
data_ap
ap_pred
