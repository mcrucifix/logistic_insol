# Greatly inspired from https://github.com/mylesmharrison/5_ways_2D_histograms/blob/master/5_ways_2D_histograms.R
# Color housekeeping

postscript(file.path(EPSPATH, 'Ramp_distribution.eps'), width=134/22.54, height=6, pointsize=7)
library(RColorBrewer)

# we had some debate about the color scheme.... 
#rf <- colorRampPalette((brewer.pal(11,'YlGnBu')))

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

# Create normally distributed data for plotting
load(file.path(RDATAPATH, 'params_mcmc_final_downweighted_d_specificity90_last2-5Myr.RData'))
print(colnames(out.params))
x <- out.params[,'p1'] - out.params[,'deltaTime']/2
y <- out.params[,'p1'] + out.params[,'deltaTime']/2

df <- data.frame(Begin.Age=x,End.Age=y)

library(hexbin)
library(grid)

some.plot = hexbinplot(df$End.Age ~ df$Begin.Age, xbins=30,  colorcut=seq(0,1,length=10), maxcnt=90 , mincnt=0, 
            vp = viewport(x=0.3,y=0.5,
            default.units='npc',angle=0,height=0.9), 
            xlab = 'End of the ramp [kyr]', 
            ylab = 'Start of the ramp [kyr]', colramp=rf, axes=FALSE, cex.lab=0.8, cex.axis=1, 
            scales=list(x=list(at=seq(0,7)*200), y=list(at=1000+seq(0,7)*200))
            )

up.image = update(some.plot,par.settings = list(
   layout.widths=list(left.padding = 0,right.padding = 0
      ,key.ylab.padding = 10,ylab.axis.padding = 10
      ,ylab.right = 0,axis.key.padding = 0)
   ,layout.heights=list(top.padding = 0,bottom.padding = 0
      ,main.key.padding = 0,key.axis.padding = 0
      ,axis.xlab.padding = 0,xlab.key.padding = 0
      ,key.sub.padding = 0))
   )

plot(some.plot)


dev.off()
