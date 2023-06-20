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

a <- rnorm(20, 16, 4)
b <- rnorm(20, 12, 6)
c <- rnorm(20, 25, 3)

#----WYBOR PRZEPROWADZANEJ ANALIZY----------------------------------------------

#Definiujemy zmienna funk, ktora okresla jaka funkcja zostanie wykorzystana w
#naszym programie do przeprowadzenia danej analizy

analiza <- as.integer(readline(prompt = "Jaka analize chcesz przeprowadzic?   
 Do wyboru masz:
 (1)T-student,
 (2)regresja,
 (3)ANOVA, \n"))

while (TRUE) {
  if(analiza == 1) {
    print("Przeprowadzam test T-studenta...")
    
    t_stud <-  as.integer(readline(prompt = "Jaki test T-studenta chcesz przepowadzic?   
  Masz do wyboru:
  (1)one-sample,
  (2)two-sample independent,
  (3)two-sample dependent, \n"))
    
    if(t_stud == 1) {
      print('...dla jednej proby.')
      proba <- readline(prompt = 'Jaka zmienna chcesz przetestowac?   ')
      proba <- as.matrix(data[proba])
      
      if_num(proba)
      if_norm(proba)
      mo <- readline(prompt = 'Testowana srednia:   ')
      mo <- as.numeric(mo)
      alt <-readline(prompt = 'Alternatywa? (greater/lesser/none)   ')
      res <- t_test_jedna_niezal(proba, mo, alt)
      print(res)
      break
      
    } else if(t_stud ==2){
      print("...dla dwoch prob niezaleznych.")
      proba_1 <-readline(prompt = 'Proba pierwsza:   ')
      proba_2 <- readline(prompt = 'Proba druga:   ')
      
      if_num(proba_1)
      if_norm(proba_1)
      if_num(proba_2)
      if_norm(proba_2)
      
      homo_var(proba_1, proba_2)
      
      alt <-readline(prompt = 'Alternatywa? (greater/lesser/none)   ')
      t_test_dwie_niezal(proba_1, proba_2, alt)
      break
      
    } else {
      print('...dla dwoch prob zaleznych.')
      proba_1 <-readline(prompt = 'Proba pierwsza:   ')
      proba_2 <- readline(prompt = 'Proba druga:   ')
      
      if_num(proba_1)
      if_norm(proba_1)
      if_num(proba_2)
      if_norm(proba_2)
      homo_var(proba_1, proba_2)
      
      alt <-readline(prompt = 'Alternatywa? (greater/lesser/none)   ')
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
    (wpisz nazwy kolumn bez cudzyslwowu i po przecinkach)")
    niezalezne <- unlist(strsplit(niezalezne, ", "))
    
    niezalezne <- check_var_name(niezalezne, columns)
    
    regresja(niezalezne, zalezna)
    break
    
  } else if (analiza == 3) {
    print("Przeprowadzam ANOVE.")
    
    columns <- colnames(data)
    
    # ustalenie zmiennych liczbowej i grupujacej:
    zmienna_liczbowa <- readline(prompt = "Podaj nazwe kolumny zawierajaca dane liczbowe:   
    (wpisz nazwe kolumny bez cudzyslowu)")
    zmienna_liczbowa <- check_var_name(zmienna_liczbowa, columns)
    #if_num(zmienna_liczbowa)
    #if_norm(zmienna_liczbowa)
    
    zmienna_grupujaca <- readline(prompt = "Podaj kolumne grupujaca:   
    (wpisz nazwe kolumny bez cudzyslowu)")
    zmienna_grupujaca <- unlist(strsplit(zmienna_grupujaca, ", "))
    zmienna_grupujaca <- check_var_name(zmienna_grupujaca, columns)
    #if_num(zmienna_grupujaca)
    #if_norm(zmienna_grupujaca)
    
    #homo_var(zmienna_liczbowa, zmienna_grupujaca)
    
    # wywolanie ANOVY:
    wynik <- ANOVA(as.matrix(data[zmienna_liczbowa]),as.matrix(data[zmienna_grupujaca]))
    print(wynik)
    
    # opcja wykonania testu post-hoc:
    if (p_value < 0.05){
       Tuk <- realine(prompt = "Czy chcesz wykonac test Tukeya HSD? (y/n)   ")
       if (Tuk == y){
         #post_hoc()
       }
       else{
         break
       }
      }
    break
    
  } else{
    print(paste("Bledny numer analizy.", analiza))
    print("Do wyboru masz: (1)T-student, (2)regresja, (3)ANOVA")
    analiza <-  as.integer(readline(prompt = "Jaka analize chcesz przeprowadzic?   "))
  }
}
print("Koniec analizy.")