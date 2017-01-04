# source('support_functions.R')

#load('params_mcmc_final_downweighted_j_specificity100_last1Myr.RData')
load(file.path(RDATAPATH, 'params_mcmc_final_downweighted_d_specificity90_last2-5Myr.RData'))
pdf(file.path(EPSPATH, 'diag_slope_2_1_Myr.pdf'), width=136/24.5, height=8, pointsize=9)

Data$Side = 1
Data$Side [ which (Data$Int < 0.5 ) ] = -1
Datalast100k <- Data[75:(nrow(Data)), ]
DataMPT      <- Data[42:62, ]
Databeforelast120k <- Data[1:61, ]

print(Data)
Data_to_use = list(Datalast100k,DataMPT,  Databeforelast120k)
corr = c(0,0.5,1)

par (mar=c(4,4,2,2))
par(mfrow=c(2,1))

for (j in seq(2,3))
{
Data = Data_to_use[[j]]


n = nrow(Data)

isInt = which(Data$Int == 1 & Data$Open == 0)
isaInt = which(Data$Int == 1 & Data$Open == 1)

isNInt = which(Data$Int == 0 & Data$Open == 0)
isaNInt = which(Data$Int == 0 & Data$Open == 1)

isGreen = which(Data$Int == 2)



with(Data[isInt,], 
  plot(Time, CalIns, 
       col='red',pch=19, xlim=c(0,80), 
       ylim=c(5.75, 6.17), type='n', 
       ylab =  "65 N caloric summer insolation (GJ/m2)", 
       xlab = "Elpased time [kyr]", yaxs='i', xaxs='i', axes=FALSE, frame=TRUE))

axis(1, at=seq(0,100,5), tck=-0.01, labels=FALSE)
axis(1, at=seq(0,100,20), tck=-0.02)
axis(2, at=seq(5.75, 6.17, 0.05), tck=-0.02)
axis(2, at=seq(5.75, 6.17, 0.01), tck=-0.01, labels=FALSE)


types = list ( 
 Int = list ( data=isInt, pch=16, col='red' ), 
 aInt = list ( data=isaInt, pch=1, col='red' ), 
 NInt = list ( data=isNInt, pch=17, col='dodgerblue' ), 
 aNInt = list ( data=isaNInt, pch=2, col='dodgerblue' ), 
 Cont = list ( data=isGreen, pch=18, col='black' ))


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


# only plot quantiles on last figure 

if (j == 3)
{
  ages = seq(80)
  TT = sapply( ages , fth  ) 
  
  lines(ages, TT[2,], lwd= 0.5)
  lines(ages, TT[3,], lwd = 0.5)
  lines(ages, TT[4,], lwd = 0.5)
  
  save(file = file.path(RDATAPATH, sprintf('quantiles_25Myr_part_%i.Rda', j)), TT)
}

for (type in types)
{
with (type, 
 { 
   if (length(data) > 0 )
   {
   with(Data[data,], points(Time, CalIns,  col=col,pch=pch, ) )
   with(Data[data,], text(Time+Side*1.7, CalIns+Side*0.007, Name, col='black', adj = .5 ))
   }
 })
}



}
dev.off()
