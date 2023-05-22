#Wczytanie pliku z danymi ze ścieżki podanej przez użytkownika  -
#     > zdefiniować w dokumentacji format pliku csv

#----FUNKCJE--------------------------------------------------------------------

check <- function(data){
  stopifnot(is.list(data))
  del_rows = 0
  
  for (i in 1:ncol(data)){
    for (j in nrow(data[,i])){
      if (is.na(data[j,i]))
        rm(data[j,i])
        del_rows = del_rows + 1
    }}
  
  if ( del_rows == 0) message('Great, there is no missing values!') 
  else message('Number of rows that are deleted due to missing values:', del_rows)
  }

t_test_jedna_niezal <- function(proba, mu0, alternatywa) {
  mean_x = mean(proba)
  sd_x = sd(proba)
  n_x = length(proba)
  df_x = n_x - 1
  t = sqrt(n_x)*(mean_x-mu0)/sd_x
  if (alternatywa == 0) {
    p_wartosc = pt(t, df_x)
  } else if (alternatywa == 1) {
    p_wartosc = pt(t, df_x, lower.tail = FALSE)        
  } else {
    p_wartosc = 2*pt(abs(t), df_x, lower.tail = FALSE) 
  }
  list(statystyka = t, p = p_wartosc)
}

t_test_dwie_niezal <- function(proba, proba2, alternatywa) {    
  mean_x = mean(proba)
  var_x = var(proba)
  n_x = length(proba)
  mean_y = mean(proba2)
  var_y = var(proba2)
  n_y = length(proba2)
  df = n_x + n_y - 2
  t = (mean_x-mean_y)/sqrt((n_x-1)*var_x+(n_y-1)*var_y)*sqrt((n_x*n_y*df) / (n_x+n_y))
  if (alternatywa == 0) {
    p_wartosc = pt(t, df)
  } else if (alternatywa == 1) {
    p_wartosc = pt(t, df, lower.tail = FALSE)        
  } else {
    p_wartosc = 2*pt(abs(t), df, lower.tail = FALSE) 
  }
  list(statystyka = t, p = p_wartosc)
}

t_test_dwie_zal <- function(proba, proba2, alternatywa) {
  d = proba - proba2
  mean_d = mean(d)
  sd_d = sd(d)
  n_d = length(d)
  df_d = n_d - 1
  t = sqrt(n_d)*mean_d/sd_d
  if (alternatywa == 0) {
    p_wartosc = pt(t, df_d)
  } else if (alternatywa == 1) {
    p_wartosc = pt(t, df_d, lower.tail = FALSE)        
  } else {
    p_wartosc = 2*pt(abs(t), df_d, lower.tail = FALSE) 
  }
  list(statystyka = t, p = p_wartosc)
}


#-----------PROGRAM------------------------------------------------------------

#Wczytanie danych
dta <- readline(prompt = "Podaj ścieżkę do pliku: ")
# http://theta.edu.pl/wp-content/uploads/2012/02/DanePakietyStatystyczne2.csv
data <- read.csv2(dta)

#Filtrowanie danych
check(data)

'''
Co robimy?
  -> test t_stud 1
  -> test t_stud 2 niezal
  -> test t_stud 2 zal
  -> regresja liniowa 
  -> anova
  -> post hoc
'''

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