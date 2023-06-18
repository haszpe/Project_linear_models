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
