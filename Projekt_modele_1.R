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
data <- read_delim(delim = ";", 
                   file = "http://theta.edu.pl/wp-content/uploads/2012/02/DanePakietyStatystyczne2.csv")

attach(data)

#----DANE O ROZKLADZIE NORMALNYM------------------------------------------------
'
a <- rnorm(20, 16, 4)
b <- rnorm(20, 12, 6)
c <- rnorm(20, 25, 3)
'
#----WYBOR PRZEPROWADZANEJ ANALIZY----------------------------------------------

"Definiujemy zmienna funk, ktora okresla jaka funkcja zostanie wykorzystana w
naszym programie do przeprowadzenia danej analizy"

analiza <- as.integer(readline(prompt = "Jaka analize chcesz przeprowadzic? \n
                          Do wyboru masz: (1)T-student, (2)regresja, (3)ANOVA. "))

while (TRUE) {
  if(analiza == 1) {
    print("Przeprowadzam test T-studenta...")
    
    t_stud <-  as.integer(readline(prompt = "Jaki test T-studenta chcesz przepowadzic? \n
                              Masz do wyboru: (1)one-sample, (2)two-sample independent, 
                              (3)two-sample dependent. "))
    
    if(t_stud == 1) {
      print('...dla jednej proby.')
      proba <- readline(prompt = 'Jaka zmienna chcesz przetestowac? ')
      if_num(proba)
      if_norm(proba)
      mo <- readline(prompt = 'Testowana srednia:')
      alt <-readline(prompt = 'Alternatywa? (greater, lesser, none) ')
      t_test_jedna_niezal(proba, mo, alt)
      break
      
    } else if(t_stud ==2){
      print("two-sample independent")
      proba_1 <-readline(prompt = 'Proba pierwsza: ')
      proba_2 <- readline(prompt = 'Proba druga: ')
      
      if_num(proba_1)
      if_norm(proba_1)
      if_num(proba_2)
      if_norm(proba_2)
      
      homo_var(proba_1, proba_2)
      
      alt <-readline(prompt = 'Alternatywa? (greater, lesser, none) ')
      t_test_dwie_niezal(proba_1, proba_2, alt)
      break
      
    } else {
      print('...dla dwoch prob zaleznych.')
      proba_1 <-readline(prompt = 'Proba pierwsza:')
      proba_2 <- readline(prompt = 'Proba druga:')
      
      if_num(proba_1)
      if_norm(proba_1)
      if_num(proba_2)
      if_norm(proba_2)
      homo_var(proba_1, proba_2)
      
      alt <-readline(prompt = 'Alternatywa? (greater, lesser, none) ')
      t_test_dwie_zal(proba_1, proba_2, alt)
      break
      
    }
  } else if (analiza == 2) {
    print("Przeprowadzam regresje.")
    columns <- colnames(data)
    zalezna <- readline(prompt = "Jaka kolumna z pliku wejsciowego jest zmienna zalezna?
                              (zmienna musi byc numeryczna) ")
    
    zalezna <- check_var_name(zalezna, columns)
    
    
    niezalezne <- readline(prompt = "Jakie kolumny sa zmiennymi niezaleznymi?
                          (wpisz nazwy kolumn bez cudzyslwowu i po przecinkach) ")
    niezalezne <- unlist(strsplit(niezalezne, ", "))
    
    niezalezne <- check_var_name(niezalezne, columns)
    
    regresja(niezalezne, zalezna)
    break
    
  } else if (analiza == 3) {
    print("Przeprowadzam ANOVE.")
    
    columns <- colnames(data)
    
    zmienna_liczbowa <- readline(prompt = "podaj nazwe kolumny zawierajaca dane liczbowe : ")
    zmienna_liczbowa <- check_var_name(zmienna_liczbowa, columns)
    
    zmienna_grupujaca <- readline(prompt = "podaj kolumne grupujaca : ")
    zmienna_grupujaca <- unlist(strsplit(zmienna_grupujaca, ", "))
    zmienna_grupujaca <- check_var_name(zmienna_grupujaca, columns)
    
    p_value <- ANOVA(as.matrix(dane[zmienna_liczbowa]),as.matrix(dane[zmienna_grupujaca]))
    p_value <- p_value$p.value
    print(p_value)

       if (p_value < 0.05){
         # post hoc
       }


       
    break
    
  } else{
    print(paste("Bledny numer analizy.", analiza))
    print("Do wyboru masz: (1)T-student, (2)regresja, (3)ANOVA")
    analiza <-  as.integer(readline(prompt = "Jaka analize chcesz przeprowadzic? "))
  }
}
