'AUTORZY:
Krzysztof Jakubowski  121992
Hanna P?ciak          113752 
Zygmunt ?atyszewicz   121724'


#----WCZYTANIE FUNKCJI----------------------------------------------------------

source("~/Documents/GitHub/Project_linear_models/functions.R")

#----PROGRAM--------------------------------------------------------------------

#Wczytanie danych:
'Wczytanie pliku z danymi ze Äąâ€şcieÄąÄ˝ki podanej przez uÄąÄ˝ytkownika  -
     > zdefiniowaĂ„â€ˇ w dokumentacji format pliku csv'

library(tidyverse)
data <- read_delim(delim = ";", 
  file = "http://theta.edu.pl/wp-content/uploads/2012/02/DanePakietyStatystyczne2.csv")

attach(data)

#----WYBOR PRZEPROWADZANEJ ANALIZY----------------------------------------------

"Definiujemy zmiennĂ„â€¦ funk, ktÄ‚Ĺ‚ra okreÄąâ€şla jaka funkcja zostanie wykorzystana w
naszym programie do przeprowadzenia danej analizy"

analiza <- readline(prompt = "JakÄ… analizÄ™ chcesz przeprowadziÄ‡? \n
                          Do wyboru masz: T-student, regresja, ANOVA")

  
if (analiza == 'T-student') {
  print("Przeprowadzam test T-studenta...")
  
  t_stud <- readline(prompt = "Jaki test T-studenta chcesz przepowadziÄ‡? \n
                              Masz do wyboru: one-sample, two-sample independent, 
                              two-sample dependent")
  
  if (t_stud == 'one-sample') {
    print('...dla jednej pr?by.')
    proba <- readline(prompt = 'JakĂ„â€¦ zmiennĂ„â€¦ chcesz przetestowaĂ„â€ˇ?')
    if_num(proba)
    if_norm(proba)
    mo <- readline(prompt = 'Testowana Äąâ€şrednia:')
    alt <-readline(prompt = 'Alternatywa:')
    t_test_jedna_niezal(proba, mo, alt)
  } else if (t_stud =='...dla dw?ch pr?b niezale?nych.'){
    print("two-sample independent")
    proba_1 <-readline(prompt = 'PrÄ‚Ĺ‚ba pierwsza:')
    proba_2 <- readline(prompt = 'PrÄ‚Ĺ‚ba druga:')
    
    if_num(proba_1)
    if_norm(proba_1)
    if_num(proba_2)
    if_norm(proba_2)
    
    homo_var(proba_1, proba_2)
    
    alt <-readline(prompt = 'Alternatywa: (greater, lesser, none')
    t_test_dwie_niezal(proba_1, proba_2, alt)
  } else {
    print('...dla dw?ch pr?b zale?nych.')
    proba_1 <-readline(prompt = 'PrÄ‚Ĺ‚ba pierwsza:')
    proba_2 <- readline(prompt = 'PrÄ‚Ĺ‚ba druga:')
    
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
  zalezna <- readline(prompt = "Jaka kolumna z pliku wejÄąâ€şciowego jest zmiennĂ„â€¦ zaleÄąÄ˝nĂ„â€¦?
                              *Zmienna musi byĂ„â€ˇ numeryczna")
  
  niezalezne <- readline(prompt = "Jakie kolumny sa zmiennymi niezaleznymi?
                          Wpisz nazwy kolumn bez cudzysÄąâ€šowiÄ‚Ĺ‚w i po przecinkach")
  niezalezne <- unlist(strsplit(niezalezne, ", "))
  
  regresja(niezalezne, zalezne)
} else if (analiza == 'ANOVA') {
  print("Przeprowadzam ANOVE.")
} else{
  print('Błąd. Nie ma takiej analizy.')
}
