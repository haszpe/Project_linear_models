#----POPRAWNOSC NAZW ZMIENNYCH--------------------------------------------------
check_var_name <- function(vars, columns){
  czy_wszystkie_obecne <- FALSE
  brakujaca_kolumna <- ""
  zmienne <- c()
  while (!czy_wszystkie_obecne) {
    czy_wszystkie_obecne <- TRUE
    for (var in vars) {
      zmienne <- vars
      if (!(var %in% columns)) {
        czy_wszystkie_obecne <- FALSE
        brakujaca_kolumna <- var
        break
      }
    }
    if (!czy_wszystkie_obecne) {
      print(paste("Brak kolumny ", var, "."))
      vars <- readline(prompt = "Podaj ponownie nazwy kolumn: ")
      vars <- unlist(strsplit(vars, ", "))
    }
  }
  print("Kazda zmienna znajduje sie w danych.")
  return(vars)
}

#----NUMERIC CHECK--------------------------------------------------------------

if_num <- function(proba){
  if(is.numeric(proba))
  {
    print("zmienna jest liczbowa")
  }
  else{
    print("zmienna NIE jest liczbowa")
    
  }
  
}

#----NORMALNOSC ROZKLADU--------------------------------------------------------

if_norm <- function(proba, alfa){
  x = shapiro.test(proba)
  if(x[2] < alfa)
  {
    print("brak normalnosci ")
  }
  else {
    print("normalnosc spelniona ")
  }
}

#----JEDNORODNOSC WARIANCJI-----------------------------------------------------

homo_var <- function(proba_1, proba_2) {
  x <- var.test(proba_1, proba_2)
  stopifnot(x$p.value > 0.05)
  #print warning: wariancje nie sa jednorodne
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

ANOVA <- function(proba, alfa) {
  k = sort(unique(alfa))
  
  n_i = numeric()
  mean_i = numeric()
  for (i in k) {
    n_i[i] = length(alfa[alfa == i])
    # srednie i-tej grup:
    mean_i[i] = mean(proba[alfa == i])
  }
  
  # srednia ogolna:
  mean_a = mean(proba)
  
  # zmiennosc pomiedzy grupami:
  SSA = sum(n_i*(mean_i - mean_a)^2)
  MSA = SSA / (length(k) - 1)
  #MSA
  
  # zmimennosc calkowita:
  SST = sum((proba - mean_a)^2)
  MST = SST / (length(proba) - 1)
  #MST
  
  # zmiennosc wewnatrz grup:
  SSE = SST - SSA
  MSE = SSE / (length(proba) - length(k))
  #MSE
  
  # statystyka F i p-value:
  Fish = MSA / MSE
  p_val = pf(Fish, length(k) - 1, length(proba) - length(k), lower.tail = FALSE)
  
  return(list(F_statistcs = Fish, p.value = p_val))
}

#----POST HOC-------------------------------------------------------------------

# testem post hoc uzytym przez nas jest test Tukeya (HSD - Honest Significant Differences)
post_hoc <- function(){
  ###tutaj powinien byc test post hoc
}

#----REGRESJA-------------------------------------------------------------------

regresja <- function(ind_variables, dep_variables)
{
  # Funkcja regresja przyjmuje argumenty ind_variables i dep_variables, 
  # ktore reprezentuja nazwy zmiennych niezaleznych i zaleznych,
  # w postaci wektorow lancuchow znakow.
  
  # Wybieramy z danych tylko te kolumny, ktore odpowiadaja 
  # zmiennym niezaleznym i przypisujemy je do zmiennej X.
  X <- select(data, all_of(ind_variables))
  # Wybieramy z danych tylko ta kolumne, 
  # ktora odpowiada zmiennej zaleznej i przypisujemy ja do zmiennej y.
  y <- select(data, all_of(dep_variables))
  
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
  # statystycznej modelu. WartoĹsc p jest prawdopodobienstwem uzyskania statystyki F wiekszej
  # lub rownej obliczonej statystyce F.
  
  # Tworzymy ramke danych "statistics_table" zawierajaca statystyki modelu.
  statistics_table <- tibble(statistic = c("adj.R2", "p.value.model", "F-statistic"),
                             value = c(adjusted_R2, p.value.model, F_statistic))
  
  # Wyswietlamy tabele ze statystykami modelu oraz tabele z estymatami wspolczynnikow.
  print(statistics_table)
  print(Coefficients)
  
}
#-------------------------------------------------------------------------------
