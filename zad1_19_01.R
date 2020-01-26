# Korzystając z danych mpg.csv stwórz dwa modele które będą na podstawie 
# ilości cylindrów i pojemności silnika przewidywały ile mil przejedzie samochód
# na jednym galonie w mieście i na autostradzie. 
# Przetestuj modele w celu oceny ich wiarygodności.
# Biblioteka z której będziemy korzystać przy tworzeniu wykresu

library(ggplot2) 

# Wczytanie pliku csv i zapisanie go w zmiennej -> "mpg"
mpg <- read.csv("/Users/adrianstodolski/Desktop/R_projects/R_19_01/mpg.csv") 

# Opis danych "mpg"
?mpg 

# Dane wejściowe
input_data <- mpg[,c("displ", "cyl", "cty", "hwy")] 

# Zwraca macierz liczb całkowitych wskazującą ich numer wiersza
row(mpg) 

# Biblioteka, która podzieli nam dane
library(caTools)

# Tworzymy wektor podzialu dla kolumny "cty"
split_cty <- sample.split(input_data[,"cty"], SplitRatio = 0.75) 
# Tworzymy wektor podzialu dla kolumny "hwy"
split_hwy <- sample.split(input_data[,"hwy"], SplitRatio = 0.75) 
# SplitRatio = 0.75 -> ile % ma byc danymi szkoleniowymi

# Obraz macierz
split_cty
split_hwy

# dane szkoleniowe
training_data <- subset(input_data, split == TRUE) 

# dane testowe
test_data <- subset(input_data, split == FALSE) 

# Tworzymy model -> "hwy" zależy liniowo od displ+cyl
model_hwy <- lm(hwy ~ displ + cyl, data = training_data) 
# Tworzymy model -> "ctyy" zależy liniowo od displ+cyl
model_cty <- lm(cty ~ displ + cyl, data = training_data) 

# Podsumowanie modeli
summary(model_cty) 
summary(model_hwy)

# Predykcja
cty_pred <- predict(model_cty, test_data)
hwy_pred <- predict(model_hwy, test_data)

cty_pred
hwy_pred

# Dodajemy kolumny
test_data$cty_pred <- cty_pred
test_data$hwy_pred <- hwy_pred

# CTY
nrow(test_data)
ggplot(test_data) +
  geom_point(mapping = aes(x=c(1:59), y=cty),color="blue") +
  geom_point(mapping = aes(x=c(1:59),y=cty_pred),color="red")

# HWY
nrow(test_data)
ggplot(test_data) +
  geom_point(mapping = aes(x=c(1:59), y=hwy),color="blue") +
  geom_point(mapping = aes(x=c(1:59),y=hwy_pred),color="red")
