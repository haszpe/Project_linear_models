'AUTORZY:
Krzysztof Jakubowski  121992
Hanna Peciak          113752 
Zygmunt Latyszewicz   121724'


#----WCZYTANIE FUNKCJI----------------------------------------------------------

source("~/Documents/GitHub/Project_linear_models/functions.R")

#----PROGRAM--------------------------------------------------------------------

#Wczytanie danych:
'Wczytanie pliku z danymi ze sciezki podanej przez uzytkownika  -
     > zdefiniowac w dokumentacji format pliku csv'

library(tidyverse)
data <- read_delim(delim = ";", 
  file = "http://theta.edu.pl/wp-content/uploads/2012/02/DanePakietyStatystyczne2.csv")

attach(data)

#----WYBOR PRZEPROWADZANEJ ANALIZY----------------------------------------------

"Definiujemy zmienna funk, ktora okresla jaka funkcja zostanie wykorzystana w
naszym programie do przeprowadzenia danej analizy"

analiza <- readline(prompt = "Jaka analize chcesz przeprowadzic? \n
                          Do wyboru masz: T-student, regresja, ANOVA")

  
if (analiza == 'T-student') {
  print("Przeprowadzam test T-studenta...")
  
  t_stud <- readline(prompt = "Jaki test T-studenta chcesz przepowadzic? \n
                              Masz do wyboru: one-sample, two-sample independent, 
                              two-sample dependent")
  
  if (t_stud == 'one-sample') {
    print('...dla jednej proby.')
    proba <- readline(prompt = 'Jaka zmienna chcesz przetestowac?')
    if_num(proba)
    if_norm(proba)
    mo <- readline(prompt = 'Testowana srednia:')
    alt <-readline(prompt = 'Alternatywa:')
    t_test_jedna_niezal(proba, mo, alt)
  } else if (t_stud =='...dla dwoch prob niezaleznych.'){
    print("two-sample independent")
    proba_1 <-readline(prompt = 'Proba pierwsza:')
    proba_2 <- readline(prompt = 'Proba druga:')
    
    if_num(proba_1)
    if_norm(proba_1)
    if_num(proba_2)
    if_norm(proba_2)
    
    homo_var(proba_1, proba_2)
    
    alt <-readline(prompt = 'Alternatywa: (greater, lesser, none')
    t_test_dwie_niezal(proba_1, proba_2, alt)
  } else {
    print('...dla dwoch prob zaleznych.')
    proba_1 <-readline(prompt = 'Proba pierwsza:')
    proba_2 <- readline(prompt = 'Proba druga:')
    
    if_num(proba_1)
    if_norm(proba_1)
    if_num(proba_2)
    if_norm(proba_2)
    homo_var(proba_1, proba_2)
    
    alt <-readline(prompt = 'Alternatywa: (greater, lesser, none')
    t_test_dwie_zal(proba_1, proba_2, alt)
  }
} else if (analiza == 'regresja') {
  print("Przeprowadzam regresje.")
  columns <- colnames(data)
  zalezna <- readline(prompt = "Jaka kolumna z pliku wejsciowego jest zmienna zalezna?
                              *Zmienna musi byc numeryczna : ")
  
  zalezna <- check_var_name(zalezna, columns)
  

  niezalezne <- readline(prompt = "Jakie kolumny sa zmiennymi niezaleznymi?
                          Wpisz nazwy kolumn bez cudzyslwowu i po przecinkach : ")
  niezalezne <- unlist(strsplit(niezalezne, ", "))
  
  niezalezne <- check_var_name(niezalezne, columns)
  
  regresja(niezalezne, zalezna)
} else if (analiza == 'ANOVA') {
  print("Przeprowadzam ANOVE.")
} else{
  print('Blad. Nie ma takiej analizy.')
}
