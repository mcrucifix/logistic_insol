
#source('support_functions.R')
load(file.path(RDATAPATH, 'params_mcmc_final_downweighted_j_specificity100_last1Myr.RData'))
postscript(file.path(EPSPATH, 'frequent_traj.eps'), width=136/24.5, height=5, pointsize=7)

last1000k = simuls[,-1]
Data <- Data[-1,]


Data$Index = seq(nrow(Data))

isInt = which(Data$Int == 1 & Data$Open == 0)
isaInt = which(Data$Int == 1 & Data$Open == 1)

isNInt = which(Data$Int == 0 & Data$Open == 0)
isaNInt = which(Data$Int == 0 & Data$Open == 1)

isGreen = which(Data$Int == 2)

types = list ( 
 Int = list ( data=isInt, pch=16, col='red' ), 
 aInt = list ( data=isaInt, pch=1, col='red' ), 
 NInt = list ( data=isNInt, pch=17, col='dodgerblue' ), 
 aNInt = list ( data=isaNInt, pch=2, col='dodgerblue' ), 
 Coni  = list ( data=isGreen, pch=18, col='black'))


Data$pch = 1
Data$isGreen = 1

for (type in types)
{ 
  Data$pch[type$data] = type$pch
  Data$col[type$data] = type$col
}


ulast1000k = unique(last1000k)

F = aggregate(numdup ~., data=transform(last1000k, numdup=1), length)
frequencies = F$numdup

#plot ( cumsum(sort(frequencies, decreasing=TRUE)) / sum(frequencies))

#row.match(Data[63:104,'Int'], ulast100k)

of <- order(frequencies, decreasing=TRUE) 

nn = length(ulast1000k[1,]) 
x = seq(nn+1)
sf = sum(frequencies)

par(mfrow=c(4,1))
# par(oma=c(0,0,5.0,0))
par(mar=c(4,2,2,2))

labels = c('a','b','c','d')

for (i in seq(4))
{
  plot ( x-0.5, c(F[of[i],seq(nn)] , F[of[i],nn])  , axes=FALSE, type='n', xlab='', ylab='', ylim=c(0,1.1), cex=1.5)

  mtext( sprintf('%.0f %%', F[of[i], nn+1]/sf * 100), side=2, adj=0, line=2.0, cex=1, las=1);
  mtext( sprintf('%s', labels[i]), side=2, adj=0, line=2.0, at=1, cex=1.3, las=1, font=2);
  
 # axis(2, at=c(0,1),labels=c('Non Onset', 'Onset'))
  axis(1, labels=Data$Name, at=seq(length(ulast1000k[1,])), las=2)
  # yellow rectangles
  for (j in seq(nn))  if (abs (F[of[i],j] - Data$Score[j]) > 0.75) rect ( j-0.5, 0, j+0.5, 1, col='yellow', lty=0)
  
  # draw 

  iin = which( F[of[i], seq(nn) ]  == 1  )
  points ( x[iin], F[of[i], iin  ]  , type='h', xlab='', ylab='', main=sprintf('%.0f %%', F[of[i], nn+1]/sf * 100), lwd=5, col='black')

  if ( i == 1 )
  {
  
  for (type in types)
  {
  with (type, 
   { 
     if (length(data) > 0 )
     {
      with(Data[data,],  points(Index, Score, col=col, pch=pch, cex = 2.0))
     }
   })
  }
  }


}

dev.off()
