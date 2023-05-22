'AUTORZY:
Krzysztof Jakubowski  121992
Hanna Pêciak          113752 
Zygmunt £atyszewicz   121724'

###próba:
proba = rnorm(100, 23, 10)

data("USArrests")

#funkcje t-test pochodz¹ z wyk³adu Suchockiego

###One Sample t-test============================================================

t_test_jedna_niezal <- function(proba, m0, alternatywa){
  mean_x=mean(proba)
  sd_x=sd(proba)
  n_x = length(proba)
  df_x = n_x - 1
  t = sqrt(n_x)*(mean_x-m0)/sd_x
  if (alternatywa == 0) {
    p_wartosc = pt(t, df_x)
  } else if (alternatywa == 1) {
    p_wartosc = pt(t, df_x, lower.tail = FALSE)        
  } else {
    p_wartosc = 2*pt(abs(t), df_x, lower.tail = FALSE) 
  }
  list(statystyka = t, p = p_wartosc)
}

###Welch Two Sample t-test======================================================

t_test_podwojna_niezal <- function(proba, proba2, alternatywa) {    
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

###Two Paired t-test============================================================

t_test_podwojna_zale <- function(proba, proba2, alternatywa) {
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

###Linear Model=================================================================

model_lm <- function(y, dane, ...){
  #managing variables
  attach(dane)
  #u¿ycie ellipsis???
  zmienne_zal <- (...)
  #checking whether data is in correct format
  for (i in 1:length(zmienne_zal)){
    stopifnot(is.numeric(zmienne_zal[i]))
  }
  #performing linear model - TRZEBA ZAST¥PIÆ TÊ FUNKCJÊ SWOJ¥ W£ASN¥!!!
  model <- lm(y ~ zmienne_zal, data=dane)
  summary(model)
}
#===============================================================================