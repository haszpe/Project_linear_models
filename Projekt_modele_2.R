'AUTORZY:
Krzysztof Jakubowski  121992
Hanna Peciak          113752 
Zygmunt Latyszewicz   121724'

'PROGRAM DO MODELU MIESZANEGO:'



print("Do wyboru masz: (1) efekty modelu, (2)parametry wariancji, (3)oba")
wybor <-  as.integer(readline(prompt = ""))
run <- TRUE

while (run == TRUE) {
  if(wybor == 1) {
    print("Przeprowadzam efekty modelu ")
    run <-FALSE
      
  } else if (wybor == 2) {
    print("Przeprowadzam regresje ")
    run <-FALSE
    
  } else if (analiza == 3) {
    print("oba elementy ")
    run <-FALSE
    
  } else{
    print(paste("Bledny wybor", wybor))
    print("Do wyboru masz: (1) efekty modelu, (2)parametry wariancji, (3)oba")
    wybor <-  as.integer(readline(prompt = "co chcesz otrzymac? "))
    break
  }
}
print('Koniec programu.')

#----FUNKCJE------------------------------------------------------------------
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

EM = function(y, X, Z, A, sigma_a, sigma_e,tmp_threshold) {
  n = nrow(X)
  p = ncol(X) 
  q = nrow(A) 
  
  t = 1
  tmp = 0.1
  
  while (tmp > tmp_threshold) {
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
    
    # verbose options 
    if (t%%100 == 0){
      cat("Przeprowadzono ",t,"iteracji\n" )
    }
  }
  list(t = t, sigma_a = sigma_a, sigma_e = sigma_e, estimators = mme_new$est)
}
