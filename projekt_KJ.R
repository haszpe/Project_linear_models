'AUTORZY:
Krzysztof Jakubowski  121992
Hanna Pêciak          113752 
Zygmunt £atyszewicz   121724'

###próba:
proba = rnorm(100, 23, 10)

data("USArrests")

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