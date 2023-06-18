library(tidyverse)


# przygotowanie danych ----------------------------------------------------

dane <- read_delim(delim = ";", 
  file = "http://theta.edu.pl/wp-content/uploads/2012/02/DanePakietyStatystyczne2.csv")


# recznie regresja --------------------------------------------------------

# input for function 
zmienne_niezalezne <- c("Wiek", "Weight", "Height")
zmienna_zalezna <- c("BMI")


regresja <- function(ind_variables, dep_variables)
{
  X <- select(dane, all_of(ind_variables))
  y <- select(dane, all_of(dep_variables))
  
  ones <- tibble(ones = rep(1, nrow(X)))
  
  X <- ones %>% bind_cols(X)
  X <- as.matrix(X)
  y <- as.matrix(y)
  
  # independent variables estimate
  wspolczynnik <- solve(t(X) %*% X) %*% t(X) %*% y
  
  predicted <- tibble(predicted = rep(1, nrow(X)))
  for (i in 1:(length(ind_variables) + 1))
  {
    predicted <- predicted %>% 
      mutate(predicted = predicted + wspolczynnik[i] * X[, i])
  }
  
  predicted <- predicted %>% 
    mutate(predicted = predicted - 1)
  
  comparasion <- predicted %>% bind_cols(tibble(real = y))  %>%
    mutate(residual = predicted - real)
  
  stopnie_swobody <- nrow(X) - length(ind_variables) - 1
  
  res <- comparasion$residual[, 1]
  
  S2e <- t(res) %*% res / (nrow(y) - ncol(X))
  Se <- sqrt(S2e)
  
  seBeta <- sqrt(diag(c(S2e) * solve(t(X) %*% X)))
  t <- wspolczynnik / seBeta
  
  # independent variables p.values
  p.value.t <- 2 * pt(abs(t), nrow(y) - ncol(X), lower.tail = FALSE)
  ind_variables <- append(ind_variables, "Intercept", after = 0)
  
  
  Coefficients <- tibble(Coefficients = ind_variables,
                         estimate = wspolczynnik[,1],
                         "Pr(>|t|)" = p.value.t[,1])
  
  # model statistics 
  
  R2 <- 1 - t(res) %*% res / sum((y - mean(y))^2)
  adjusted_R2 <- 1 - (1 - R2) * (nrow(y) - 1) / (nrow(y) - ncol(X) - 1)
  F_statistic <-  R2 / (1-R2) * (nrow(y) - ncol(X)) / (ncol(X) - 1)
  p.value.model <- 1 - pf(F_statistic, length(ind_variables), nrow(y) - ncol(X),
                          lower.tail = TRUE)
  
  statistics_table <- tibble(statistic = c("adj.R2", "p.value.model", "F-statistic"),
                             value = c(adjusted_R2, p.value.model, F_statistic))
  
  print(statistics_table)
  print(Coefficients)
  
}

regresja(zmienne_niezalezne, zmienna_zalezna)






# wzor --------------------------------------------------------

# regresja z wieloma zmiennymi niezaleznymi

model_bmi <- lm(formula = BMI~Age+Weight+Height, data = dane)
summary(model_bmi) # F statyistic, adj.R oraz p.value modelu, 2 tabelka
tidy(model_bmi) # coefficients, p.value zmiennych = moja 1 tabelka

