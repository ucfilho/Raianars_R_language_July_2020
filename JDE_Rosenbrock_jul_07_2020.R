library('DEoptimR')


Rosenbrock= function(x)
  {
  Num=length(x)
  fun=0
  for (i in 1:Num)
  {
    fun = fun+100*(x[i+1]-x[i]**2)**2 + (1-x[i])**2
   }

  return(fun)
  }
# Global Minimum: 0 , domain=[-30,30] at [1,1,..,1]

dim=10
RUNS=50
ITE=2000
Y=0;X=0
for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-30, dim), rep(30, dim), Rosenbrock ,
         tol = 1e-20,NP=100, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value
  }
MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Rosenbrock JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')
