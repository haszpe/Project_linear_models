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





