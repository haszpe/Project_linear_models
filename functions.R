#--------SPRAWDZENIE DANYCH----------------------------------------------------
"Funkcja do sprawdzania danych
    - czu dane zostały poprawnie wczytane 
    - usuwa wiersze z brakującymi danymi
    - zwraca komunikat o liczbie wierszy z brakującymi danymi"

check <- function(data){
  stopifnot(is.list(data))
  del_rows = 0
  
  for (i in 1:ncol(data)){
    for (j in nrow(data[,i])){
      if (is.na(data[j,i]))
        rm(data[j,i])
      del_rows = del_rows + 1
    }}
  
  if ( del_rows == 0) message('Great, there is no missing values!') 
  else message('Number of rows that are deleted due to missing values:', del_rows)
}

#--------NORMALNOŚĆ ROZKŁADU---------------------------------------------------

if_norm <- function() {
  
}



#--------JEDNORODNOŚĆ WARIANCJI------------------------------------------------

homogen_var <- function() {
  
}



#--------TESTY T-STUDENTA------------------------------------------------------
    "- jednej próby
    - dwóch prób niezależnych
    - dwóch prób zależnych"

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


#--------KORELACJA-------------------------------------------------------------
    "- liniowa pearsona
    - rang spearmana"

correlation_spearman <- function(y, dane, ...) {
  zmienne <- (...)
  for (i in 1:length(zmienne_zal)){
    stopifnot(is.numeric(zmienne_zal[i]))
  }
  
  
}
correlation_spearman <- function(y, dane, ...) {
  zmienne <- (...)
  for (i in 1:length(zmienne_zal)){
    stopifnot(is.numeric(zmienne_zal[i]))
  }
  
}


#--------ANALIZA WARIANCJI-----------------------------------------------------

variance <- function() {
  
  
}

#--------REGRESJA-----------------------------------------------------


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