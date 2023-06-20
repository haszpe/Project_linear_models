'AUTORZY:
Krzysztof Jakubowski  121992
Hanna Peciak          113752 
Zygmunt Latyszewicz   121724'

'PROGRAM DO MODELU MIESZANEGO:'



print("Do wyboru masz: (1) efekty modelu, (2)parametry wariancji, (3)oba")
wybor <-  as.integer(readline(prompt = ""))


while (TRUE) {
  if(wybor == 1) {
    print("Przeprowadzam efekty modelu ")
    break
      
  } else if (wybor == 2) {
    print("Przeprowadzam regresje ")
    break
    
  } else if (analiza == 3) {
    print("oba elementy ")
    break
    
  } else{
    print(paste("Bledny wybor", wybor))
    print("Do wyboru masz: (1) efekty modelu, (2)parametry wariancji, (3)oba")
    wybor <-  as.integer(readline(prompt = "co chcesz otrzymac? "))
  }
}


