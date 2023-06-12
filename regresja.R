library(tidyverse)
library(broom) # tidy
library(purrr) # map


# przygotowanie danych ----------------------------------------------------

dane <- read_delim(delim = ";", file = "../dane/DanePakietyStatystyczne2.csv")

glimpse(dane)



dane <- dane %>% select(-YOB, -Sex, -VitB12_log, -Creatinine_log)  %>%
  rename(Obesity = Nadwaga, Age = Wiek, Sex = Sex2) %>%
  relocate(Age, Sex, City, .before = Height) %>%
  mutate(City = str_replace_all(City,
                  c("Wroc\xb3aw" = "Wroclaw", "Gda\xf1sk" = "Gdansk")))
  

# recznie regresja --------------------------------------------------------

# NIE MOGE UZYC LM


# input, tutaj podaje nazwy zmiennych niezaleznych ktore beda w modelu
zmienne_niezalezne <- c("Age")
zmienna_zalezna <- c("BMI")

zmienne_niezalezne <- readline(prompt = 
      "Podaj zmienne niezależne oddzielone przecinkiem bez cydzyslowow: ")
zmienne_niezalezne <- unlist(strsplit(zmienne_niezalezne, ", "))
zmienne_niezalezne

zmienna_zalezna <- readline(prompt = "Podaj zmienna zalezna: ")



regresja <- function(ind_variables, dep_variables)
{
  # NIE MOGE UZYC LM
  X <- select(dane, all_of(ind_variables))
  y <- select(dane, all_of(dep_variables))
  
  ones <- tibble(ones = rep(1, nrow(X)))
  
  X <- ones %>% bind_cols(X)
  X <- as.matrix(X)
  y <- as.matrix(y)
  # i tak nie zmienia wyniku wspolczynnikow
  
  wspolczynnik <- solve( t(X) %*% X ) %*% t(X) %*% y
  
  predicted <- tibble(predicted = rep(1, nrow(X)))
  for (i in 1:(length(ind_variables)+1))
  {
    predicted <- predicted %>% 
      mutate(predicted = predicted + wspolczynnik[i] * X[,i])
  }
  
  # potem moze byc problem
  predicted <- predicted %>% 
    mutate(predicted = predicted -1)
  
  comparasion <- predicted %>% bind_cols(tibble(real = y))  %>%
    mutate(residual = predicted - real)
  
  stopnie_swobody <- nrow(X) - length(ind_variables) - 1
  
  res <- comparasion$residual[,1]
  
  S2e = t(res)%*%res / (nrow(y) - ncol(X))
  Se = sqrt(S2e)
  
  seBeta = sqrt(diag(c(S2e) * solve(t(X)%*%X)))
  t = wspolczynnik / seBeta
  
  p.value.t = 2*pt(abs(t), nrow(y) - ncol(X), lower.tail = F)
  
  ind_variables <- append(ind_variables,"interpret", after = 0)
  
  result_table <- tibble(variable = ind_variables,
                         estimate = wspolczynnik,
                         p_value = p.value.t)
  
  # R2, p.value dla modelu, statystyka F
  
  R2 = 1- t(res)%*%res / sum((y-mean(y))^2)
  # adjusted R2
  
  f = R2 / (1-R2) * (nrow(y) - ncol(X)) / (ncol(X) - 1)
  
  p.value.model = pf(F, ncol(X) - 1, nrow(y) - ncol(X), lower.tail = FALSE)
  
  print(p.value.model)
  # wyzsza tym lepiej?
  
  statistics_table <- tibble(statistic = c("adj.R2","p.value.model","F"),
                             value = c(R2, p.value.model, f))
  
  print(statistics_table)
  print(result_table)
  
}

regresja(zmienne_niezalezne, zmienna_zalezna)






# wzor --------------------------------------------------------


# regresja prosta

ggplot(data = dane, aes(x = Age, y = BMI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


model_bmi_age <- lm(formula = BMI~Age, data = dane)
tidy(model_bmi_age) %>% select(-std.error, -statistic) %>% 
  rename(coefficient = estimate, ind_variable = term)

# age = BMI * -0.306 + 85.1
# ze wzrostem Age o 1 BMI maleje o 0.0582


# regresja zlozona
model_bmi <- lm(formula = BMI~Age+Weight+Height, data = dane)
summary(model_bmi)
tidy(model_bmi)
glimpse(model_bmi)
augment(model_bmi)
predict <- tibble(fitted(model_bmi))

