library(tidyverse)


# przygotowanie danych ----------------------------------------------------

dane <- read_delim(delim = ";", 
  file = "http://theta.edu.pl/wp-content/uploads/2012/02/DanePakietyStatystyczne2.csv")


# recznie regresja --------------------------------------------------------

# input for function 
zmienne_niezalezne <- c("Wiek", "Weight", "Height")
zmienna_zalezna <- c("BMI")


regresja <- function(ind_variables, dep_variables)
{
  # Funkcja regresja przyjmuje argumenty ind_variables i dep_variables, 
  # które reprezentują nazwy zmiennych niezależnych i zależnych,
  # w postaci wektorow lancuchow znakow
  
  # Wybieramy z danych tylko te kolumny, które odpowiadają 
  # zmiennym niezależnym i przypisujemy je do zmiennej X.
  X <- select(dane, all_of(ind_variables))
  # Wybieramy z danych tylko tę kolumnę, 
  # która odpowiada zmiennej zależnej i przypisujemy ją do zmiennej y.
  y <- select(dane, all_of(dep_variables))
  
  ones <- tibble(ones = rep(1, nrow(X)))

  X <- ones %>% bind_cols(X)
  # Przekształcamy ramki danych X i y na macierze, aby umożliwić obliczenia macierzowe.
  X <- as.matrix(X)
  y <- as.matrix(y)
  
  # Obliczamy estymatory współczynników regresji
  wspolczynnik <- solve(t(X) %*% X) %*% t(X) %*% y
  
  
  # Tworzymy ramkę danych "predicted" zawierającą kolumnę z samymi jedynkami.
  # Liczba wierszy tej ramki danych odpowiada liczbie obserwacji w X.
  # ji. Początkowe przewidywane wartości są ustawione na 1, ponieważ 
  # biorą pod uwagę wpływ wyrazu wolnego w modelu regresji
  predicted <- tibble(predicted = rep(1, nrow(X)))
  
  # Dla każdej zmiennej niezależnej (oraz dla wyrazu wolnego) obliczamy wartości
  # przewidywane, sumując odpowiednie współczynniki z pomnożonymi przez wartości zmiennych niezależnych.
  for (i in 1:(length(ind_variables) + 1))
  {
    predicted <- predicted %>% 
      mutate(predicted = predicted + wspolczynnik[i] * X[, i])
  }
  
  
  # Dostosowujemy wartości przewidywane przez odjęcie 1, ponieważ wcześniej dodaliśmy kolumnę jedynkową do X.
  predicted <- predicted %>% 
    mutate(predicted = predicted - 1)
  
  
  # Łączymy ramki danych "predicted" i "y" kolumnowo, tworząc ramkę danych "comparison" 
  # zawierającą wartości przewidywane i wartości rzeczywiste. Następnie obliczamy reszty modelu 
  # jako różnicę między wartościami przewidywanymi a wartościami rzeczywistymi.
  comparasion <- predicted %>% bind_cols(tibble(real = y))  %>%
    mutate(residual = predicted - real)
  
  # Obliczamy stopnie swobody jako różnicę między liczbą obserwacji a liczbą zmiennych niezależnych i wyrazem wolnym
  stopnie_swobody <- nrow(X) - length(ind_variables) - 1
  
  #   # Przypisujemy kolumnę "residual" z ramki danych "comparison" do zmiennej
  # "res", która zawiera reszty modelu.
  res <- comparasion$residual[, 1]
  
  
  # Obliczamy błąd standardowy jako pierwiastek z sumy kwadratów reszt podzielonych przez 
  # (liczbę obserwacji - liczba zmiennych niezależnych).
  S2e <- t(res) %*% res / (nrow(y) - ncol(X))
  Se <- sqrt(S2e)
  
  # Obliczamy błędy standardowe współczynników regresji i wartości t
  seBeta <- sqrt(diag(c(S2e) * solve(t(X) %*% X)))
  t <- wspolczynnik / seBeta
  
  
  # Obliczamy wartości p, które są dwukrotne większe od wartości t.
  p.value.t <- 2 * pt(abs(t), nrow(y) - ncol(X), lower.tail = FALSE)
  ind_variables <- append(ind_variables, "Intercept", after = 0)
  
  # Tworzymy ramkę danych "Coefficients" zawierającą nazwy zmiennych, estymaty współczynników i wartości p.
  Coefficients <- tibble(Coefficients = ind_variables,
                         estimate = wspolczynnik[,1],
                         "Pr(>|t|)" = p.value.t[,1])
  
  # Obliczamy statystyki modelu: R-kwadrat, skorygowany R-kwadrat, statystykę F i wartość p dla modelu.
  
  R2 <- 1 - t(res) %*% res / sum((y - mean(y))^2)
  # Obliczamy współczynnik determinacji (R-kwadrat) modelu. Jest to miara, która informuje o tym, 
  # jak dobrze model wyjaśnia zmienność w danych. Wykorzystujemy sumę kwadratów reszt i sumę 
  # kwadratów różnic między wartościami zależnymi a ich średnią.
  
  adjusted_R2 <- 1 - (1 - R2) * (nrow(y) - 1) / (nrow(y) - ncol(X) - 1)
  # Obliczamy skorygowany współczynnik determinacji (skorygowany R-kwadrat), który uwzględnia
  # liczbę zmiennych niezależnych i liczbę obserwacji w modelu.
  
  F_statistic <-  R2 / (1-R2) * (nrow(y) - ncol(X)) / (ncol(X) - 1)
  # Obliczamy statystykę F, która jest stosowana do testowania istotności modelu regresji. 
  
  p.value.model <- 1 - pf(F_statistic, length(ind_variables), nrow(y) - ncol(X),
                          lower.tail = TRUE)
  # Obliczamy wartość p dla testu statystycznego modelu, który informuje o istotności
  # statystycznej modelu. Wartość p jest prawdopodobieństwem uzyskania statystyki F większej
  # lub równej obliczonej statystyce F
  
  # Tworzymy ramkę danych "statistics_table" zawierającą statystyki modelu.
  statistics_table <- tibble(statistic = c("adj.R2", "p.value.model", "F-statistic"),
                             value = c(adjusted_R2, p.value.model, F_statistic))
  
  # Wyświetlamy tabelę ze statystykami modelu oraz tabelę z estymatami współczynników.
  print(statistics_table)
  print(Coefficients)
  
}

regresja(zmienne_niezalezne, zmienna_zalezna)



# wzor --------------------------------------------------------

# regresja z wieloma zmiennymi niezaleznymi

model_bmi <- lm(formula = BMI~Wiek+Weight+Height, data = dane)
summary(model_bmi) # F statyistic, adj.R oraz p.value modelu, 2 tabelka
tidy(model_bmi) # coefficients, p.value zmiennych = moja 1 tabelka


