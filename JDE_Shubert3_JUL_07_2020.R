library('DEoptimR')

Shubert3= function(x)
  {
  Num=length(x)
  fun=0
  for (i in 1:Num)
  {
    fun = fun+ i *sin(((i + 1) * x[i]) + i)
   }
 
  return(fun)
  }
# global minimum −29.6733337 at x (not specified but DE found)=(-1.11409968,-1.11409968) 
# not convex it is usually evaluated  xi =[(-10,10),..,(-10,10)]

dim=10
RUNS=50
ITE=2000
Y=0;X=0
for(i in 1:RUNS)
  {
  JDE_R=JDEoptim(rep(-10, dim), rep(10, dim), Shubert3 ,
         tol = 1e-20,NP=100, trace = FALSE,  maxiter =ITE)
  Y[i]=JDE_R$value
  }
MEAN=mean(Y)
STD=sd(Y)
MAX=max(Y)
MIN=min(Y)
cat('Shubert3 JDE DIM=',dim,'RUNS=',RUNS,'ITE=',ITE,'\n')
cat('MEAN=',MEAN,'\n')
cat('MAX',MAX,'\n')
cat('MIN=',MIN,'\n')
cat('STD',STD,'\n')
