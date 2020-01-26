zad1 # korzystajac z danych mpg
# Biblioteka z której będziemy korzystać przy tworzeniu wykresu
library(ggplot2) 
# Wczytanie pliku csv i zapisanie go w zmiennej -> "mpg"
mpg <- read.csv("mpg.csv") 
# Opis danych "mpg"
?mpg 
# Dane wejściowe
input_data <- mpg[,c("displ", "cyl", "cty", "hwy")] 
row(mpg) # Zwraca macierz liczb całkowitych wskazującą ich numer wiersza
library(caTools) # Biblioteka, która podzieli nam dane
split_cty <- sample.split(input_data[,"cty"], SplitRatio = 0.75) # Tworzymy wektor podzialu dla kolumny "cty"
split_hwy <- sample.split(input_data[,"hwy"], SplitRatio = 0.75) # Tworzymy wektor podziału dla kolumny "hwy"
# SplitRatio = 0.75 -> ile % ma byc danymi szkoleniowymi
split_cty
split_hwy

training_data <- subset(input_data, split == TRUE) # dane szkoleniowe
test_data <- subset(input_data, split == FALSE) # dane testowe

model_hwy <- lm(hwy ~ displ + cyl, data = training_data) # Tworzymy model -> "hwy" zależy liniowo od displ+cyl
model_cty <- lm(cty ~ displ + cyl, data = training_data) # Tworzymy model -> "cty" zależy liniowo od displ+cyl
summary(model_cty) # Podsumowanie modeli
summary(model_hwy)




cty_pred <- predict(model_cty, test_data)
hwy_pred <- predict(model_hwy, test_data)

cty_pred
hwy_pred

test_data$cty_pred <- cty_pred
test_data$hwy_pred <- hwy_pred

test_data$cty_predykcja <- cty_pred # dodajemy kolumne z predykcja
summary(model_cty)
# CTY
nrow(test_data)
ggplot(test_data) +
  geom_point(mapping = aes(x=c(1:57), y=cty),color="blue") +
  geom_point(mapping = aes(x=c(1:57),y=cty_pred),color="red")

# HWY
nrow(test_data)
ggplot(test_data) +
  geom_point(mapping = aes(x=c(1:57), y=hwy),color="blue") +
  geom_point(mapping = aes(x=c(1:57),y=hwy_pred),color="red")
  
  
  