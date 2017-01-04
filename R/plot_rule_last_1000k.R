# source('support_functions.R')

load('params_mcmc_final_downweighted_j_specificity100_last1Myr.RData')
#load(file.path(RDATAPATH, 'params_mcmc_final_downweighted_d_specificity90_last2-5Myr.RData'))
postscript(file.path(EPSPATH, 'diag_slope_1_Myr.pdf', width=7, height=7, pontsize=14)

Datalast100k <- Data[69:(nrow(Data)), ]
DataMPT      <- Data[53:68, ]
Databeforelast120k <- Data[1:52, ]

Datalast100k <- Data

Data_to_use = list(Datalast100k,DataMPT,  Databeforelast120k)
#Data_to_use = list(Datalast100k )
#Data_to_use = list(DataMPT)
corr = c(0,0.5,1)

par (mar=c(4,4,2,2))
par(mfrow=c(2,3))

for (j in seq(1,3))
{
Data = Data_to_use[[j]]


n = nrow(Data)
distmatrix = as.matrix(dist(cbind(Data$Int, Data$CalIns*1)))
for (i in seq(n-1, 1, -1))
{
  print(c(i, min(distmatrix[i, seq(i+1, n)]) ))
  if (min(distmatrix[i, seq(i+1, n)]) < 0.01) Data$Side[i] = -Data$Side[i]
}

isInt = which(Data$Int == 1 & Data$Open == 0)
isaInt = which(Data$Int == 1 & Data$Open == 1)

isNInt = which(Data$Int == 0 & Data$Open == 0)
isaNInt = which(Data$Int == 0 & Data$Open == 1)

isGreen = which(Data$Int == 2)


par(cex=0.5)
with(Data[isInt,], 
  plot(Time, CalIns, 
       col='red',pch=19, xlim=c(0,100), 
       ylim=c(5.75, 6.2), type='n', 
       ylab = "Caloric Insolation (GJ)", 
       xlab = "Time since last Deglaciation"))


types = list ( 
 Int = list ( data=isInt, pch=16, col='red' ), 
 aInt = list ( data=isaInt, pch=1, col='red' ), 
 NInt = list ( data=isNInt, pch=17, col='blue' ), 
 aNInt = list ( data=isaNInt, pch=2, col='blue' ), 
 Cont = list ( data=isGreen, pch=18, col='darkgreen' ))






fth <- function(Time)
{
  quantile ( pexp[,"threshold"] + corr[j]* pexp[,"deltaThreshold"] - Time * pexp[,"slope"] + exp(pexp[,"sd_minus_1"]) * dx )
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

lines(ages, TT[2,], lwd= 0.5)
lines(ages, TT[3,], lwd = 0.5)
lines(ages, TT[4,], lwd = 0.5)

save(file = sprintf('quantiles_part_%i.Rda', j), TT)

for (type in types)
{
with (type, 
 { 
   if (length(data) > 0 )
   {
   with(Data[data,], points(Time, CalIns,  col=col,pch=pch, ) )
   with(Data[data,], text(Time+Side*1.7, CalIns+Side*0.007, Name, col=col, cex=.4, adj = -Side ))
   }
 })
}



}
dev.off()
