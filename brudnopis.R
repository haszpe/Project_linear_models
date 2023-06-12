library(tidyverse)
library(broom)
library(purrr)

data("USArrests")
USArrests <- USArrests %>% rownames_to_column(var = "county") %>%
  select(-county) %>% relocate(UrbanPop, .after = "Rape")




funkcja <- function(... , y, zmienne_niezalezne)
{
  
  dane <- ... %>% select(zmienne_niezalezne)
  
  colnames <- colnames(dane)
  
  result <- tibble(variable = character(),
                   term  = character(),
                   estimate = numeric(),
                   p.value = numeric())
  
  
  for (i in seq_along(1:length(dane)))
  {
    x <- unlist(dane[i])
    model <- tidy(lm(y ~ x))  %>% select(-std.error, -statistic)
    result <- result %>% bind_rows(tibble(variable = colnames[i],
                                          term = model$term,
                                          estimate = model$estimate,
                                          p.value = model$p.value))
    
    print(ggplot(mapping = aes(x = x, y = y)) + labs(x =  colnames[i], y = "UrbanPop") 
          + geom_point() + stat_smooth(method = "lm", se = FALSE)) 
  }
  result
}


funkcja(select(USArrests, -UrbanPop), y = USArrests$UrbanPop,
        zmienne_niezalezne = c("Murder", "Rape"))

#-------------------------------------------------------------------------------

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
