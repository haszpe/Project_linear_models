#----NUMERIC CHECK--------------------------------------------------------------

if_num <- function(proba){
  stopifnot(is.numeric(proba))
}

#----NORMALNOSC ROZKLADU--------------------------------------------------------

if_norm <- function(proba){
  x = shapiro.test(proba)
  stopifnot(x[2] < 0.05)
}

#----JEDNORODNOSC WARIANCJI-----------------------------------------------------

homo_var <- function(proba_1, proba_2) {
  x <- var.test(proba_1, proba_2)
  stopifnot(x$p.value > 0.05)
}

#----TESTY T-STUDENTA-----------------------------------------------------------

t_test_jedna_niezal <- function(proba, mu0, alternatywa) {
  mean_x = mean(proba)
  sd_x = sd(proba)
  n_x = length(proba)
  df_x = n_x - 1
  
  stat_t = sqrt(n_x)*(mean_x-mu0)/sd_x

  if (alternatywa == 'greater') {
    p_wartosc = pt(stat_t, df_x, lower.tail = FALSE) 
  } else if (alternatywa == 'lesser') {
    p_wartosc = 2*pt(abs(stat_t), df_x, lower.tail = FALSE)     
  } else {
    p_wartosc = pt(stat_t, df_x) 
  }
  list(statystyka = stat_t, p = p_wartosc)
}

t_test_dwie_niezal <- function(proba, proba2, alternatywa) {    
  mean_x = mean(proba)
  var_x = var(proba)
  n_x = length(proba)
  mean_y = mean(proba2)
  var_y = var(proba2)
  n_y = length(proba2)
  df = n_x + n_y - 2
  stat_t = (mean_x-mean_y)/sqrt((n_x-1)*var_x+(n_y-1)*var_y)*sqrt((n_x*n_y*df) / (n_x+n_y))
  
  if (alternatywa == 'greater') {
    p_wartosc = pt(stat_t, df, lower.tail = FALSE)
  } else if (alternatywa == 'lesser') {
    p_wartosc = 2*pt(abs(stat_t), df, lower.tail = FALSE)         
  } else {
    p_wartosc = pt(stat_t, df)
  }
  list(statystyka = stat_t, p = p_wartosc)
}

t_test_dwie_zal <- function(proba, proba2, alternatywa) {
  d = proba - proba2
  mean_d = mean(d)
  sd_d = sd(d)
  n_d = length(d)
  df_d = n_d - 1
  stat_t = sqrt(n_d)*mean_d/sd_d
  
  if (alternatywa == 'greater') {
    p_wartosc = pt(stat_t, df_d, lower.tail = FALSE)
  } else if (alternatywa == 'lesser') {
    p_wartosc = 2*pt(abs(stat_t), df_d, lower.tail = FALSE)         
  } else {
    p_wartosc = pt(stat_t, df_d)
  }
  list(statystyka = stat_t, p = p_wartosc)
}

#----ANALIZA WARIANCJI----------------------------------------------------------

'WORK IN PROGRESS'

#----REGRESJA-------------------------------------------------------------------

regresja <- function(ind_variables, dep_variables)
{
  # Funkcja regresja przyjmuje argumenty ind_variables i dep_variables, 
  # ktore reprezentuja nazwy zmiennych niezaleznych i zaleznych,
  # w postaci wektorow lancuchow znakow.
  
  # Wybieramy z danych tylko te kolumny, ktore odpowiadaja 
  # zmiennym niezaleznym i przypisujemy je do zmiennej X.
  X <- select(dane, all_of(ind_variables))
  # Wybieramy z danych tylko ta kolumne, 
  # ktora odpowiada zmiennej zaleznej i przypisujemy ja do zmiennej y.
  y <- select(dane, all_of(dep_variables))
  
  ones <- tibble(ones = rep(1, nrow(X)))
  
  X <- ones %>% bind_cols(X)
  # Przeksztalcamy ramki danych X i y na macierze, aby umozliwic obliczenia macierzowe.
  X <- as.matrix(X)
  y <- as.matrix(y)
  
  # Obliczamy estymatory wspoczynnikow regresji.
  wspolczynnik <- solve(t(X) %*% X) %*% t(X) %*% y
  
  
  # Tworzymy ramke danych "predicted" zawierajaca kolumne z samymi jedynkami.
  # Liczba wierszy tej ramki danych odpowiada liczbie obserwacji w X.
  # ji. Poczatkowe przewidywane wartosci sa ustawione na 1, poniewaz 
  # biora pod uwage wplyw wyrazu wolnego w modelu regresji.
  predicted <- tibble(predicted = rep(1, nrow(X)))
  
  # Dla kazdej zmiennej niezaleznej (oraz dla wyrazu wolnego) obliczamy wartosci
  # przewidywane, sumujac odpowiednie wspolczynniki z pomnozonymi przez wartosci zmiennych niezaleznych.
  for (i in 1:(length(ind_variables) + 1))
  {
    predicted <- predicted %>% 
      mutate(predicted = predicted + wspolczynnik[i] * X[, i])
  }
  
  # Dostosowujemy wartosci przewidywane przez odjecie 1, poniewaz wczesniej dodalismy kolumne jedynkowa do X.
  predicted <- predicted %>% 
    mutate(predicted = predicted - 1)
  
  
  # Laczymy ramki danych "predicted" i "y" kolumnowo, tworzac ramke danych "comparison" 
  # zawierajaca wartosci przewidywane i wartosci rzeczywiste. Nastepnie obliczamy reszty modelu 
  # jako roznice miedzy wartosciami przewidywanymi a wartosciami rzeczywistymi.
  comparasion <- predicted %>% bind_cols(tibble(real = y))  %>%
    mutate(residual = predicted - real)
  
  # Obliczamy stopnie swobody jako roznice miedzy liczba obserwacji a liczba zmiennych niezaleznych i wyrazem wolnym.
  stopnie_swobody <- nrow(X) - length(ind_variables) - 1
  
  #  Przypisujemy kolumne "residual" z ramki danych "comparison" do zmiennej
  # "res", ktora zawiera reszty modelu.
  res <- comparasion$residual[, 1]
  
  
  # Obliczamy blad standardowy jako pierwiastek z sumy kwadratow reszt podzielonych przez 
  # (liczbe obserwacji - liczba zmiennych niezaleznych).
  S2e <- t(res) %*% res / (nrow(y) - ncol(X))
  Se <- sqrt(S2e)
  
  # Obliczamy bledy standardowe wspolczynnikow regresji i wartosci t.
  seBeta <- sqrt(diag(c(S2e) * solve(t(X) %*% X)))
  t <- wspolczynnik / seBeta
  
  
  # Obliczamy wartosci p, ktore sa dwukrotne wieksze od wartosci t.
  p.value.t <- 2 * pt(abs(t), nrow(y) - ncol(X), lower.tail = FALSE)
  ind_variables <- append(ind_variables, "Intercept", after = 0)
  
  # Tworzymy ramke danych "Coefficients" zawierajaca nazwy zmiennych, estymaty wspolczynnikow i wartosci p.
  Coefficients <- tibble(Coefficients = ind_variables,
                         estimate = wspolczynnik[,1],
                         "Pr(>|t|)" = p.value.t[,1])
  
  # Obliczamy statystyki modelu: R-kwadrat, skorygowany R-kwadrat, statystyke F i wartosc p dla modelu.
  
  R2 <- 1 - t(res) %*% res / sum((y - mean(y))^2)
  # Obliczamy wspolczynnik determinacji (R-kwadrat) modelu. Jest to miara, ktora informuje o tym, 
  # jak dobrze model wyjasnia zmiennosc w danych. Wykorzystujemy sume kwadratow reszt i sume 
  # kwadratow roznic miedzy wartosciami zaleznymi a ich srednia.
  
  adjusted_R2 <- 1 - (1 - R2) * (nrow(y) - 1) / (nrow(y) - ncol(X) - 1)
  # Obliczamy skorygowany wspolczynnik determinacji (skorygowany R-kwadrat), ktory uwzglednia
  # liczbe zmiennych niezaleznych i liczbe obserwacji w modelu.
  
  F_statistic <-  R2 / (1-R2) * (nrow(y) - ncol(X)) / (ncol(X) - 1)
  # Obliczamy statystyke F, ktora jest stosowana do testowania istotnosci modelu regresji. 
  
  p.value.model <- 1 - pf(F_statistic, length(ind_variables), nrow(y) - ncol(X),
                          lower.tail = TRUE)
  # Obliczamy wartosc p dla testu statystycznego modelu, ktory informuje o istotnosci
  # statystycznej modelu. WartoÅsc p jest prawdopodobienstwem uzyskania statystyki F wiekszej
  # lub rownej obliczonej statystyce F.
  
  # Tworzymy ramke danych "statistics_table" zawierajaca statystyki modelu.
  statistics_table <- tibble(statistic = c("adj.R2", "p.value.model", "F-statistic"),
                             value = c(adjusted_R2, p.value.model, F_statistic))
  
  # Wyswietlamy tabele ze statystykami modelu oraz tabele z estymatami wspolczynnikow.
  print(statistics_table)
  print(Coefficients)
  
}
#-------------------------------------------------------------------------------