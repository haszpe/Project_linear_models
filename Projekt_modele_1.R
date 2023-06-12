'AUTORZY:
Krzysztof Jakubowski  121992
Hanna P?ciak          113752 
Zygmunt ?atyszewicz   121724'


#------------------------------------------------------------------------------
#Wczytanie pliku z danymi ze ścieżki podanej przez użytkownika  -
#     > zdefiniować w dokumentacji format pliku csv


#----WCZYTANIE FUNKCJI---------------------------------------------------------

source("~/Documents/GitHub/Project_linear_models/functions.R")

#-----------PROGRAM------------------------------------------------------------

#Wczytanie danych
dta <- readline(prompt = "Podaj ścieżkę do pliku: ")
# http://theta.edu.pl/wp-content/uploads/2012/02/DanePakietyStatystyczne2.csv
data <- read.csv2(dta)

#Filtrowanie danych
check(data)

#Wybór analizy
"Definiujemy zmienną funk, która określa jaka funkcja zostanie wykorzystana w
naszym programie do przeprowadzenia danej analizy"

analiza <- readline(prompt = "Jaką analizę chcesz przeprowadzić? \n
                          Do wyboru masz: T-student, regresja, ANOVA")

if (analiza == 'T-student') {
  t_stud <- readline(prompt = "Jaki test T-studenta chcesz przepowadzić?
                              Masz do wyboru: one-sample, two-sample independent, 
                              two-sample dependent")
  
  if (t_stud == 'one-sample') {
    funk <- t_test_jedna_niezal
  } 
  if (t_stud =='two-sample independent'){
    funk <- t_test_dwie_niezal
  }
  if (t_stud == 'two-sample dependent'){
    funk <- t_test_dwie_zal
  }}
  
if (analiza == 'regresja') {
  funk <- funkcja_regresji
}
  
if (analiza == 'ANOVA') {
  funk <- funkcja_anovy}


#Wybór zmiennych
niezal <- readline(prompt = "Jaka kolumna z pliku wejściowego jest zmienną niezależną?")

zal <- readline(prompt == "Jakie kolumny są zmienną/ymi zależną/ymi?
                          Wpisz nazwy kolumn bez cudzysłowiów i po przecinkach")
zal <- unlist(strsplit(zal, ", "))


#Działania
"Weź zmienną funk (określa jaka będzie funkcja do użycia i zmienne: zal i niezal"

check(data[proba])

#Normalność próby
shapiro.test(data[proba])
as.numeric(data[proba])

#Homogeniczność wariancji


#Regresja

reg_one <- readline(prompt = "Podaj nazwę kolumny zmiennej objaśnianej")


reg_two <- readline(prompt = "Podaj nazwę/y kolumn/y zmienntch objaśnianych")



#Jaki test chcesz przeprowadzić?
  # test t dla jednej próby niezależnej
        #check czy tam można, jeśli nie, zaproponuj inny test
  #test t dla dwóch prób niezależnych
  #test t dla dwóch prób zależnych

#Czy chcesz zapisac wyniki? 
  #if ('y') write(wynik) to csv
  #if ('n') print('end of program')