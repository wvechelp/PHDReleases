##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir())
###############Section 2: Packages########################################
install.packages('ggplot2') #For plotting
###############Section 3: Libraries#######################################
library(ggplot2)
###############Section 4: Defining functions##############################
###############Section 5: Plot and save###################################
plot<-TRUE; save<-FALSE

##############Part 1: World population####################################
###############Section 1: In- and output##################################
df.dat<-read.csv('D_C1_WorldPopulation.csv',header = TRUE,sep = ',')
###############Section 2: Plotting and saving#############################
if(plot){
  p.Population<-ggplot(df.dat,aes(x = Year,y = Population,group = Series)) + coord_cartesian(xlim = c(1950,2100),ylim = c(0,15*10^9)) + 
    #Data
    geom_ribbon(aes(ymin = Min,ymax = Max),fill = 'grey90') + geom_line(aes(colour = Series,linetype = Series)) + 
    scale_colour_manual(values = c('grey30','black')) + scale_linetype_manual(values = c('dashed','solid')) +
    #Extra lines
    geom_line(aes(x = seq(1940,2057,length.out = nrow(df.dat)),y = 10*10^9),colour = 'grey60',linetype = 'dotted',size = 0.4) + 
    geom_line(aes(x = rep(2057,nrow(df.dat)),y = seq(-10^9,10*10^9,length.out = nrow(df.dat))),colour = 'grey60',linetype = 'dotted',size = 0.4) +
    geom_line(aes(x = seq(1940,2020,length.out = nrow(df.dat)),y = df.dat$Population[df.dat$Year == 2020]),colour = 'black',linetype = 'dotted',size = 0.4) + 
    geom_line(aes(x = rep(2020,nrow(df.dat)),y = seq(-10^9,df.dat$Population[df.dat$Year == 2020],length.out = nrow(df.dat))),colour = 'black',linetype = 'dotted',size = 0.4) + 
    #Layout 
    scale_y_continuous('Population (billion)',breaks = c(0,5*10^9,10*10^9,15*10^9),labels = c(0,5,10,15)) + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black',size = 7),axis.title = element_text(size = 9)) + 
    theme(legend.position = 'none')
  plot(p.Population)
  if(save){
    tiff('F_C1S1_WorldPopulation.tiff',units = 'mm',width = 160,height = 60,res = 300,pointsize = 7)
    plot(p.Population)
    dev.off()
  }
}
###############Section 3: Variable removal################################
rm(df.dat,p.Population)

