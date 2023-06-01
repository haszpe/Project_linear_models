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



" Testy T-Studenta
    - jednej próby
    - dwóch prób niezależnych
    - dwóch prób zależnych"

t_test_jedna_niezal <- function(proba, mu0, alternatywa) {
  mean_x = mean(proba)
  sd_x = sd(proba)
  n_x = length(proba)
  df_x = n_x - 1
  t = sqrt(n_x)*(mean_x-mu0)/sd_x
  if (alternatywa == 0) {
    p_wartosc = pt(t, df_x)
  } else if (alternatywa == 1) {
    p_wartosc = pt(t, df_x, lower.tail = FALSE)        
  } else {
    p_wartosc = 2*pt(abs(t), df_x, lower.tail = FALSE) 
  }
  list(statystyka = t, p = p_wartosc)
}

t_test_dwie_niezal <- function(proba, proba2, alternatywa) {    
  mean_x = mean(proba)
  var_x = var(proba)
  n_x = length(proba)
  mean_y = mean(proba2)
  var_y = var(proba2)
  n_y = length(proba2)
  df = n_x + n_y - 2
  t = (mean_x-mean_y)/sqrt((n_x-1)*var_x+(n_y-1)*var_y)*sqrt((n_x*n_y*df) / (n_x+n_y))
  if (alternatywa == 0) {
    p_wartosc = pt(t, df)
  } else if (alternatywa == 1) {
    p_wartosc = pt(t, df, lower.tail = FALSE)        
  } else {
    p_wartosc = 2*pt(abs(t), df, lower.tail = FALSE) 
  }
  list(statystyka = t, p = p_wartosc)
}

t_test_dwie_zal <- function(proba, proba2, alternatywa) {
  d = proba - proba2
  mean_d = mean(d)
  sd_d = sd(d)
  n_d = length(d)
  df_d = n_d - 1
  t = sqrt(n_d)*mean_d/sd_d
  if (alternatywa == 0) {
    p_wartosc = pt(t, df_d)
  } else if (alternatywa == 1) {
    p_wartosc = pt(t, df_d, lower.tail = FALSE)        
  } else {
    p_wartosc = 2*pt(abs(t), df_d, lower.tail = FALSE) 
  }
  list(statystyka = t, p = p_wartosc)
}


