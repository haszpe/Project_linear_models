'AUTORZY:
Krzysztof Jakubowski  121992
Hanna Peciak          113752 
Zygmunt Latyszewicz   121724'

#----WCZYTANIE FUNKCJI----------------------------------------------------------

source("~/Documents/GitHub/Project_linear_models/functions.R")

#----WCZYTANIE DANYCH-----------------------------------------------------------

'Wczytanie pliku z danymi ze sciezki podanej przez uzytkownika  -
     > zdefiniowac w dokumentacji format pliku csv'

library(tidyverse)
library(stats)

data <- read_delim(delim = ";", 
                   file = "http://theta.edu.pl/wp-content/uploads/2012/02/DanePakietyStatystyczne2.csv")
attach(data)

# dane z rozkladu normalnego:
tabelka <- tibble(a = rnorm(30, 16, 4),
                   b = as_factor(c(rep(0, 10), rep(1, 10), rep(2, 10))))
#data <- tabelka

#----WYBOR PRZEPROWADZANEJ ANALIZY----------------------------------------------

#Definiujemy zmienna funk, ktora okresla jaka funkcja zostanie wykorzystana w
#naszym programie do przeprowadzenia danej analizy
run = TRUE
analiza <- as.integer(readline(prompt = "Jaka analize chcesz przeprowadzic?   
 Do wyboru masz:
 (1) T-student,
 (2) regresja,
 (3) ANOVA, \n"))

while (run == TRUE){
  if(analiza == 1) {
    print("Przeprowadzam test T-studenta...")
    
    t_stud <-  as.integer(readline(prompt = "Jaki test T-studenta chcesz przepowadzic?   
  Masz do wyboru:
  (1) one-sample,
  (2) two-sample independent,
  (3) two-sample dependent, \n"))
    
    if(t_stud == 1) {
      print('...dla jednej proby.')
      proba <- as.matrix(data[readline(prompt = 'Jaka zmienna chcesz przetestowac?   ')])
      if_num(proba)
      if_norm(proba)
      
      mo <- as.numeric(readline(prompt = 'Testowana srednia:   '))
      
      alt <-readline(prompt = 'Alternatywa? (greater/lesser/none)   ')
      
      res <- t_test_jedna_niezal(proba, mo, alt)
      run <- FALSE
      
    } else if(t_stud ==2){
      print("...dla dwoch prob niezaleznych.")
      proba_1 <- as.matrix(data[readline(prompt = 'Proba pierwsza:   ')])
      proba_2 <- as.matrix(data[readline(prompt = 'Proba druga:   ')])
      
      if_num(proba_1)
      if_norm(proba_1)
      if_num(proba_2)
      if_norm(proba_2)
      homo_var(proba_1, proba_2)
      
      alt <-readline(prompt = 'Alternatywa? (greater/lesser/none)   ')
      
      res <- t_test_dwie_niezal(proba_1, proba_2, alt)
      run <- FALSE
      
    } else {
      print('...dla dwoch prob zaleznych.')
      proba_1 <- as.matrix(data[readline(prompt = 'Proba pierwsza:   ')])
      proba_2 <- as.matrix(data[readline(prompt = 'Proba druga:   ')])
      
      if_num(proba_1)
      if_norm(proba_1)
      if_num(proba_2)
      if_norm(proba_2)
      homo_var(proba_1, proba_2)
      
      alt <-readline(prompt = 'Alternatywa? (greater/lesser/none)   ')
      
      res <- t_test_dwie_zal(proba_1, proba_2, alt)
      run <- FALSE
    }
  } else if (analiza == 2) {
   print("Przeprowadzam regresje.")
    
    columns <- colnames(data)

    
    niezalezne <- readline(prompt = "Jakie kolumny sa zmiennymi niezaleznymi?
    (wpisz nazwy kolumn bez cudzyslwowu i po przecinkach) ")
    niezalezne <- unlist(strsplit(niezalezne, ", "))
    niezalezne <- check_var_name(niezalezne, columns)
    
    
    zalezna <- readline(prompt = "Jaka kolumna z pliku wejsciowego jest zmienna zalezna?
    (zmienna musi byc numeryczna) ")
    zalezna <- check_var_name(zalezna, columns)
    
    res <- regresja(niezalezne, zalezna)
    print(res$statistics$data)
    print(res$Coefficients$data)
    run <- FALSE
    
  } else if (analiza == 3) {
    print("Przeprowadzam ANOVE.")
    
    columns <- colnames(data)
    
    # ustalenie zmiennych liczbowej i grupujacej:
    zmienna_liczbowa <- readline(prompt = "Podaj nazwe kolumny zawierajaca dane liczbowe:   
    (wpisz nazwe kolumny bez cudzyslowu)   ")
    zmienna_liczbowa <- check_var_name(zmienna_liczbowa, columns)
    zmienna_liczbowa <- as.matrix(zmienna_liczbowa)
    if_num(data[zmienna_liczbowa[1]])
    if_norm(data[zmienna_liczbowa[1]])
    
    zmienna_grupujaca <- readline(prompt = "Podaj kolumne grupujaca:   
    (wpisz nazwe kolumny bez cudzyslowu)  ")
    #zmienna_grupujaca <- unlist(strsplit(zmienna_grupujaca, ", "))
    zmienna_grupujaca <- check_var_name(zmienna_grupujaca, columns)
    zmienna_grupujaca <- as.matrix(zmienna_grupujaca)
    if_num(data[zmienna_grupujaca[1]])
    
    #homo_var(zmienna_liczbowa, zmienna_grupujaca)
    
    # wywolanie ANOVY:
    res <- ANOVA(as.matrix(data[zmienna_liczbowa[1]]),as.matrix(data[zmienna_grupujaca[1]]))
    print(res)
    
    # opcja wykonania testu post-hoc:
    if (res$p.value < 0.05){
       Tuk <- readline(prompt = "Czy chcesz wykonac test Tukeya HSD? (y/n)   ")
       if (Tuk == "y"){
         res <- post_hoc(data[zmienna_liczbowa[1]], data[zmienna_grupujaca[1]])
         print(res)
         run <- FALSE
       }
       else{
         run <- FALSE
       }
    }
    run <- FALSE
    } else{
    print(paste("Bledny numer analizy.", analiza))
    print("Do wyboru masz: (1) T-student, (2) regresja, (3) ANOVA")
    analiza <-  as.integer(readline(prompt = "Jaka analize chcesz przeprowadzic?   "))
    break
  }
}

print("Koniec analizy.")
