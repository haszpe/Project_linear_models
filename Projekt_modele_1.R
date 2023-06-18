'AUTORZY:
Krzysztof Jakubowski  121992
Hanna P?ciak          113752 
Zygmunt ?atyszewicz   121724'


#----WCZYTANIE FUNKCJI----------------------------------------------------------

source("~/Documents/GitHub/Project_linear_models/functions.R")

#----PROGRAM--------------------------------------------------------------------

#Wczytanie danych:
'Wczytanie pliku z danymi ze Ĺ›cieĹĽki podanej przez uĹĽytkownika  -
     > zdefiniowaÄ‡ w dokumentacji format pliku csv'

data <- readline(prompt = "Podaj Ĺ›cieĹĽkÄ™ do pliku: ")
# przykładowe dane:
# http://theta.edu.pl/wp-content/uploads/2012/02/DanePakietyStatystyczne2.csv
data <- read.csv2(dta)

attach(data)

#----WYBOR PRZEPROWADZANEJ ANALIZY----------------------------------------------

"Definiujemy zmiennÄ… funk, ktĂłra okreĹ›la jaka funkcja zostanie wykorzystana w
naszym programie do przeprowadzenia danej analizy"

analiza <- readline(prompt = "Jaką analizę chcesz przeprowadzić? \n
                          Do wyboru masz: T-student, regresja, ANOVA")

  
if (analiza == 'T-student') {
  print("Przeprowadzam test T-studenta...")
  
  t_stud <- readline(prompt = "Jaki test T-studenta chcesz przepowadzić? \n
                              Masz do wyboru: one-sample, two-sample independent, 
                              two-sample dependent")
  
  if (t_stud == 'one-sample') {
    print('...dla jednej pr?by.')
    proba <- readline(prompt = 'JakÄ… zmiennÄ… chcesz przetestowaÄ‡?')
    if_num(proba)
    if_norm(proba)
    mo <- readline(prompt = 'Testowana Ĺ›rednia:')
    alt <-readline(prompt = 'Alternatywa:')
    t_test_jedna_niezal(proba, mo, alt)
  } else if (t_stud =='...dla dw?ch pr?b niezale?nych.'){
    print("two-sample independent")
    proba_1 <-readline(prompt = 'PrĂłba pierwsza:')
    proba_2 <- readline(prompt = 'PrĂłba druga:')
    
    if_num(proba_1)
    if_norm(proba_1)
    if_num(proba_2)
    if_norm(proba_2)
    
    homo_var(proba_1, proba_2)
    
    alt <-readline(prompt = 'Alternatywa: (greater, lesser, none')
    t_test_dwie_niezal(proba_1, proba_2, alt)
  } else {
    print('...dla dw?ch pr?b zale?nych.')
    proba_1 <-readline(prompt = 'PrĂłba pierwsza:')
    proba_2 <- readline(prompt = 'PrĂłba druga:')
    
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
  zalezna <- readline(prompt = "Jaka kolumna z pliku wejĹ›ciowego jest zmiennÄ… zaleĹĽnÄ…?
                              *Zmienna musi byÄ‡ numeryczna")
  
  niezalezne <- readline(prompt = "Jakie kolumny sa zmiennymi niezaleznymi?
                          Wpisz nazwy kolumn bez cudzysĹ‚owiĂłw i po przecinkach")
  niezalezne <- unlist(strsplit(niezalezne, ", "))
  
  regresja(niezalezne, zalezne)
} else {
  print("Przeprowadzam ANOVE.")
}

