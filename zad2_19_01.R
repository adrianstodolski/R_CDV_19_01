# Potrzebne biblioteki
library(ggplot2)
library(forecast)
library(astsa)
library(forecast)

# Zapisanie TS nottem w zmiennej data
data <- nottem
# Wykres
plot(nottem)

# Struktura danych
str(data)
# Sprawdzenie danych
start(data)
end(data)
frequency(data)
class(nottem)

# Dekompozycja
data_decomposed <- decompose(data, type = "multiplicative")
autoplot(data_decomposed)
# Data – nasze dane wejsciowe "data"
# Trend – Czy występuje trend
# Seasonal –Sezonowość
# Random – "niewytłumaczalna" część danych

# Wyrównanie danych
plot(log(data))
# Usunięcie trendu
data_diff <- diff(log(data))
plot(data_diff)

# Autokorelacja i Autoregresja
acf(diff(log(data)))
pacf(diff(log(data)))

# Model
# Funkcja "auto.arima" znajdzie nam odpowiedni model, funkcja ta pomimo wczesniejszych krokow jest 
# najodpowiedniejsza na tym etapie nauki
# Zastosujemy ja jako nasz model do predykcji
model <- auto.arima(data, trace = T, stepwise = F, approximation = F)
summary(model)
autoplot(model)

# Predykcja
model_predykcja <- predict(model, h= 240)
plot(forecast(model, h = 240), ylab = "Średnia miesięczna temperatura w Nottingham (F)", 
     bty = "l", xlab = "Time")

# Predykcja przy użyciu funkcji forecast z biblioteki "forecast"
model_forecast <- forecast(model, h=240)
autoplot(model_forecast)
# Predykcja -> podanie temperatur w kolejnych latach
predykcja_new <- model_forecast$mean
#Porównajmy
data
predykcja_new
