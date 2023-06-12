'AUTORZY:
Krzysztof Jakubowski  121992
Hanna P�ciak          113752 
Zygmunt �atyszewicz   121724'


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


#Wybór danych do analizy
proba <- readline(prompt = "Jaką kolumnę z pliku wejściowego chcesz przeanalizować? \n 
                  *Pamiętaj, że musisz podać kolumnę z wartościami numerycznymi")


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