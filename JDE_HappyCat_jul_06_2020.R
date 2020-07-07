library('DEoptimR')

griewank <- function(x) { 1 + crossprod(x)/4000 - prod( cos(x/sqrt(seq_along(x))) )}

HappyCat = function(x)
  {
   Alpha=1. / 8
   Num=length(x)
   s=0
   for( i in 1:Num){s=s+x[i]*x[i]}
   fun=((s - length(x))**2)**Alpha +   
      (s / 2 + sum(x)) / length(x) + 0.5 
   return(fun )
    
  }



# global mimima at (-1,-1,...,-1) - f(x)=0  - d=[-2,2] (qlq dominio)





JDEoptim(rep(-2, 2), rep(2, 2), HappyCat,
         tol = 1e-20, trace = TRUE, triter = 100)

