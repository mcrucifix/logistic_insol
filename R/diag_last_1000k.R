# source('support_functions.R')

load(file.path(RDATAPATH, 'params_mcmc_final_downweighted_j_specificity100_last1Myr.RData'))
#load('params_mcmc_final_downweighted_d_specificity90_last2-5Myr.RData')

pdf(file.path(EPSPATH, 'diag_slope_1_Myr.pdf'), width=136/24.5,  heigh=5, pointsize=10)

Data$Side = 1
# Data$Side [ which (Data$Int < 0.5 ) ] = -1

Datalast100k <- Data

Data_to_use = list(Datalast100k)
corr = c(0,0.5,1)

par (mar=c(4,4,2,2))

for (j in seq(1,1))
{
Data = Data_to_use[[j]]


n = nrow(Data)
distmatrix = as.matrix(dist(cbind(Data$Int, Data$CalIns*1)))
# I tried to optimise automatically the lables, but no lucke 
# Taka ended up doing this manually.... 

#for (i in seq(n-1, 1, -1))
#{
#  print(c(i, min(distmatrix[i, seq(i+1, n)]) ))
# #  if (min(distmatrix[i, seq(i+1, n)]) < 0.01) Data$Side[i] = -Data$Side[i]
#}
#
isInt = which(Data$Int == 1 & Data$Open == 0)
isaInt = which(Data$Int == 1 & Data$Open == 1)

isNInt = which(Data$Int == 0 & Data$Open == 0)
isaNInt = which(Data$Int == 0 & Data$Open == 1)

isGreen = which(Data$Int == 2)


with(Data[isInt,], 
  plot(Time, CalIns, 
       col='red',pch=19, xlim=c(0,120), 
       ylim=c(5.80, 6.15), type='n', 
       ylab =  "65 N caloric summer insolation (GJ/m2)", 
       xlab = "Elpased time [kyr]", yaxs='i', xaxs='i'))


types = list ( 
 Int = list ( data=isInt, pch=16, col='red' ), 
 aInt = list ( data=isaInt, pch=1, col='red' ), 
 NInt = list ( data=isNInt, pch=17, col='dodgerblue' ), 
 aNInt = list ( data=isaNInt, pch=2, col='dodgerblue' ), 
 Cont = list ( data=isGreen, pch=18, col='black' ))



fth <- function(Time)
{
  quantile ( pexp[,"threshold"] - Time * pexp[,"slope"] + exp(pexp[,"sd_minus_1"]) * dx, c(0.25, 0.50, 0.75) )
  }




exps = sample(nrow(out.params), 100000, repl=TRUE)
out.params[exps,]
pexp = data.frame(out.params[exps,])


pexp$sd_minus_1 <- - log(pexp$b*1000)
pexp$slope      <-  pexp$a * exp(pexp$sd_minus_1)


y = runif(100000)
dx = log( y / (1-y) )

ages = seq(120)
TT = sapply( ages , fth  ) 

polygon( c(ages, rev(ages)), c(TT[1,], rev(TT[3,])),  col='grey', border='grey')
lines(ages, TT[2,], lwd= 2)

save(file = file.path(RDATAPATH, sprintf('quantiles_part_%i.Rda', j)), TT)

for (type in types)
{
with (type, 
 { 
   if (length(data) > 0 )
   {
   with(Data[data,], points(Time, CalIns,  col=col,pch=pch, ) )
   with(Data[data,], text(Time, CalIns+Side*0.012, Name, col='black', adj = 0.5 ))
   }
 })
}



}
dev.off()
