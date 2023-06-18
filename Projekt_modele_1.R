'AUTORZY:
Krzysztof Jakubowski  121992
Hanna P?ciak          113752 
Zygmunt ?atyszewicz   121724'


#----WCZYTANIE FUNKCJI----------------------------------------------------------

source("~/Documents/GitHub/Project_linear_models/functions.R")

#----PROGRAM--------------------------------------------------------------------

#Wczytanie danych:
'Wczytanie pliku z danymi ze ścieżki podanej przez użytkownika  -
     > zdefiniować w dokumentacji format pliku csv'

data <- readline(prompt = "Podaj ścieżkę do pliku: ")
# przyk�adowe dane:
# http://theta.edu.pl/wp-content/uploads/2012/02/DanePakietyStatystyczne2.csv
data <- read.csv2(dta)

#Filtrowanie danych:
check(data)

#----WYBOR PRZEPROWADZANEJ ANALIZY----------------------------------------------

"Definiujemy zmienną funk, która określa jaka funkcja zostanie wykorzystana w
naszym programie do przeprowadzenia danej analizy"

analiza <- readline(prompt = "Jaką analizę chcesz przeprowadzić? \n
                          Do wyboru masz: T-student, regresja, ANOVA")

if (analiza == 'T-student') {
  t_stud <- readline(prompt = "Jaki test T-studenta chcesz przepowadzić?
                              Masz do wyboru: one-sample, two-sample independent, 
                              two-sample dependent")
  
  if (t_stud == 'one-sample') {
    proba <- readline(prompt = 'Jaką zmienną chcesz przetestować?')
    if_norm
    mo <- readline(prompt = 'Testowana średnia:')
    alt <-readline(prompt = 'Alternatywa:')
    t_test_jedna_niezal(proba, mo, alt)
  } 
  if (t_stud =='two-sample independent'){
    proba_1 <-readline(prompt = 'Próba pierwsza:')
    proba_2 <- readline(prompt = 'Próba druga:')
    alt <-readline(prompt = 'Alternatywa: (greater, lesser, none')
    t_test_dwie_niezal(proba_1, proba_2, alt)
  }
  if (t_stud == 'two-sample dependent'){
    proba_1 <-readline(prompt = 'Próba pierwsza:')
    proba_2 <- readline(prompt = 'Próba druga:')
    alt <-readline(prompt = 'Alternatywa: (greater, lesser, none')
    t_test_dwie_zal(proba_1, proba_2, alt)
  }}
  
if (analiza == 'regresja') {
  zalezna <- readline(prompt = "Jaka kolumna z pliku wejściowego jest zmienną zależną?
                              *Zmienna musi być numeryczna")
  
  niezalezne <- readline(prompt = "Jakie kolumny sa zmiennymi niezaleznymi?
                          Wpisz nazwy kolumn bez cudzysłowiów i po przecinkach")
  niezalezne <- unlist(strsplit(niezalezne, ", "))
  
  regresja(niezalezne, zalezne)
}
  
if (analiza == 'ANOVA') {
  funk <- ANOVA}

#----ZAPISANIE WYNIK�W----------------------------------------------------------

#Czy chcesz zapisac wyniki? 
  #if ('y') write(wynik) to csv
  #if ('n') print('end of program')
