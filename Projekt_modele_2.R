'AUTORZY:
Krzysztof Jakubowski  121992
Hanna Pęciak          113752 
Zygmunt Łatyszewicz   121724'

'PROGRAM DO MODELU MIESZANEGO:'

# parametry wariancji -----------------------------------------------------

library(AGHmatrix)
library(MASS)

# identyfikator osobnika
id = 1:8
# sire i dam wskazują na identyfikatory ojca i matki


# ojciec
sire = c(0, 0, 0, 1, 3, 1, 4, 3)
# matka
dam = c(0, 0, 0, 0, 2, 2, 5, 6)

ped = cbind(id, sire, dam)
# służy do tworzenia macierzy hodowlanych dla populacji zwierząt.
# ploidy = 2 oznacza, że populacja zwierząt jest dwuploidowa
A = Amatrix(ped, ploidy = 2)
(A = as.matrix(A))

# A to macierz jednostkowa
# kolumny i wiersze to identyfikator osobnika
# Wartości w pozostałych komórkach reprezentują stopień pokrewieństwa między osobnikami

y = as.matrix(c(4.5, 2.9, 3.9, 3.5, 5.0))

#  macierz X, która zawiera zmienne niezależne (w tym przypadku jedna zmienna, sex)
#  co oznacza, że pierwsza kolumna macierzy X reprezentuje efekty samców
# (wartości 1 dla samców i 0 dla samic)
#Kolumna druga macierzy X jest wypełniona wartościami odwróconymi wektora sex (1-sex)
sex = c(1, 0, 0, 1, 1)
X = matrix(0, 5, 2)
X[,1] = sex
X[,2] = 1-sex
X

model.matrix(~factor(sex) - 1)
I = diag(5)
Z = matrix(0, 5, 8)
Z[1:5, 4:8] = I
Z



mme = function(y, X, Z, A, sigma_a, sigma_e) {
  alpha = sigma_e / sigma_a
  invA = ginv(A)
  C = rbind(cbind(t(X)%*%X, t(X)%*%Z),
            cbind(t(Z)%*%X, t(Z)%*%Z+invA*c(alpha)))
  rhs = rbind(t(X)%*%y, t(Z)%*%y)
  invC = ginv(C)
  estimators = invC%*%rhs
  list(C = C, est = estimators)
}

mme(y, X, Z, A, 20, 40)

C = as.matrix(mme(y, X, Z, A, 20, 40)$C)
(invC = ginv(C))

invC22 = invC[3:10, 3:10]
(r2 = diag(1 - invC22*2))
(r = sqrt(r2))

sigma_a = 1.01  #starting value for random effect
sigma_e = 10.01 #starting value for error variance

# W przypadku modeli mieszanych, takich jak ten, algorytm EM jest używany
# do estymacji optymalnych wartości parametrów modelu
EM = function(y, X, Z, A, sigma_a, sigma_e) {
  n = nrow(X)
  p = ncol(X) 
  q = nrow(A) 
  
  t = 1 #iteration number 1
  tmp = 0.1 #test for convergance
  
  while (tmp > 0.00001) {
    mme_new = mme(y, X, Z, A, sigma_a, sigma_e)
    C_new = ginv(mme_new$C)
    Ck = C_new[(p+1):(p+q), (p+1):(p+q)]
    mme2 = mme_new$est
    
    a = as.matrix(mme2[(p+1):(p+q)])
    sigma_a_new = (t(a)%*%ginv(A)%*%a + sum(diag(ginv(A)%*%Ck))*c(sigma_e))/q
    
    res = as.matrix(y-X%*%as.matrix(mme2[1:p]) - Z%*%as.matrix(mme2[(p+1):(p+q)]))
    X.tmp1 = cbind(X,Z) %*% C_new
    X.tmp2 = t(cbind(X,Z))
    sigma_e_new = (t(res)%*%res + sum(diag(X.tmp1%*%X.tmp2))*c(sigma_e))/n
    
    tmp = max(abs(sigma_a - sigma_a_new), abs(sigma_e - sigma_e_new))
    sigma_a = sigma_a_new
    sigma_e = sigma_e_new
    
    t = t + 1
  }
  list(t = t, sigma_a = sigma_a, sigma_e = sigma_e)
}

# sigma_a odnosi się do wariancji efektu hodowlanego, który jest efektem losowym
# związanych z rodzicami, podczas gdy sigma_e odnosi się do wariancji błędu,
# który jest składnikiem losowym niezależnym od efektu hodowlanego.
(wyniki = EM(y, X, Z, A, sigma_a, sigma_e))
wyniki$sigma_a+wyniki$sigma_e
var(y)



