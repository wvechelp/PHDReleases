##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
#Remarks: Last successful run: 30/01/2020; Last changes: 30/01/2020
###############Section 2: Packages########################################
# install.packages('reshape2') #For melting dataframes into 'long' mode
# install.packages('ggplot2') # For plotting
###############Section 3: Libraries#######################################
library(reshape2)
library(ggplot2)
###############Section 4: Global variables################################
s.data <- '../Data/'
s.figs <- '../Figures/'
###############Section 5: Plot and save###################################
b.plot <- T; b.save <- T

##############Part 1: Temporal trend biomass##############################
###############Section 1: In- and output##################################
v.out <- v.in <- c('None', 'Low', 'High')
df.out <- df.in <- c()
r.out <- 0.2
r.in <- 0.05
r.grow <- c(0.10, 0.15, 0.20, 0.30)

###############Section 2: Simulation data#################################
################Action dataframes#########################################
for (i in c(1:length(v.out))){
  if(v.out[i] == 'High'){ v.f <- rep(c(0, 1, 0, 1), 3) }
  else if(v.out[i] == 'Low'){ v.f <- rep(c(0, 1, 0, 0), 3) }
  else if(v.out[i] == 'None'){ v.f <- rep(c(0, 0, 0, 0), 3) }
  else { message('Code not known') }
  v.g <- c(0, 0, v.f, 0, 0, 0, 0) # Includes days without any action before and after cycles
  df.out <- rbind(df.out, c(v.in[1], v.out[i], v.g))
  df.out <- rbind(df.out, c(v.in[2], v.out[i], v.g))
  df.out <- rbind(df.out, c(v.in[3], v.out[i], v.g))
}
df.out <- as.data.frame(df.out)
names(df.out) <- c('In', 'Out', seq(0, 34, 2))
df.out[, c(3:ncol(df.out))] <- apply(df.out[, c(3:ncol(df.out))], c(1, 2), as.numeric)

for (i in c(1:length(v.in))){
  if(v.in[i] == 'High'){ v.f <- rep(c(1, 0, 1, 0), 3) }
  else if(v.in[i] == 'Low'){ v.f <- rep(c(1, 0, 0, 0), 3) }
  else if(v.in[i] == 'None'){ v.f <- rep(c(0, 0, 0, 0), 3) }
  else { message('Code not known') }
  v.g <- c(0, 0, v.f, 0, 0, 0, 0) # Includes days without any action before and after cycles
  df.in <- rbind(df.in, c(v.in[i], v.out[1], v.g))
  df.in <- rbind(df.in, c(v.in[i], v.out[2], v.g))
  df.in <- rbind(df.in, c(v.in[i], v.out[3], v.g))
}
df.in <- as.data.frame(df.in)
names(df.in) <- c('In', 'Out', seq(0, 34, 2))
df.in[, c(3:ncol(df.in))] <- apply(df.in[, c(3:ncol(df.in))], c(1, 2), as.numeric)

################Simulations###############################################
lst.sim <- list()
for (i in c(1:length(r.grow))){
  df.info <- cbind.data.frame(expand.grid(v.in, v.out), r.grow[i])
  names(df.info) <- c('In', 'Out', 'RGR')
  
  df.sim <- cbind(df.info, data.frame(matrix(nrow = nrow(df.info), ncol = (ncol(df.in) - 2))))
  names(df.sim) <- c(names(df.info), names(df.in)[3:ncol(df.in)])
  df.sim$`0` <- 0.5 * 0.05
  
  for (j in c(1:nrow(df.sim))){
    for (k in c(which(names(df.sim) == 2):ncol(df.sim))){
      i.in <- which(df.in$In == df.sim$In[j] & df.in$Out == df.sim$Out[j])
      i.out <- which(df.out$In == df.sim$In[j] & df.out$Out == df.sim$Out[j])
      df.sim[j, k] <- exp(r.grow[i] * 2) * df.sim[j, k-1] - 
        df.out[i.out, which(names(df.out) == names(df.sim)[k])] * r.out * exp(r.grow[i] * 2) * df.sim[j, k-1]
    }
  }
  lst.sim[[i]] <- df.sim
}
df.sim <- do.call('rbind', lst.sim)

###############Section 3: Plotting and saving#############################
df.bm <- melt(df.sim, id.vars = names(df.sim)[c(1:3)], 
              measure.vars = names(df.sim)[which(names(df.sim) == 0):(ncol(df.sim))])
df.bm$variable <- as.numeric(as.character(df.bm$variable))
df.bm$In <- factor(df.bm$In, c('None', 'Low', 'High'))
df.bm$Out <- factor(df.bm$Out, c('None', 'Low', 'High'))
df.bm$RGR <- factor(df.bm$RGR)

if(b.plot){
  p.Trends <- ggplot(df.bm, aes(x = variable, y = value, group = RGR)) + 
    coord_cartesian(ylim = c(0, 5)) +
    geom_line(aes(linetype = RGR), size = 0.4) + 
    scale_x_continuous('Time (days)') + 
    scale_y_continuous('Biomass (g)') + 
    labs(linetype = expression(paste('Growth rate (', d^{-1}, ')', sep = ''))) + 
    facet_grid(.~Out) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.text = element_text(size = 8), 
          strip.placement = 'outside', 
          legend.background = element_blank(), 
          legend.position = c(0.08, 0.86), 
          legend.spacing.y = unit(0.4, 'mm'), 
          legend.text = element_text(size = 7), 
          legend.title = element_text(size = 8))
  plot(p.Trends)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C9S3_SimulationsDifferentGrowthRate.tiff'), 
         units = 'mm', width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.Trends)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(i, p.Trends, v.in, v.out, df.out, df.in, r.in, r.out, v.f, v.g, df.sim, j, 
   i.in, i.out, df.bm)
