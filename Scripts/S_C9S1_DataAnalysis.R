##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 05/02/2020; Last changes: 05/02/2020
###############Section 2: Packages########################################
# install.packages('reshape2') # For melting dataframes into 'long' mode
# install.packages('ggplot2') # For plotting
# install.packages('gridExtra') # For flexible plotting
# install.packages('ggpubr') # For merging plots
###############Section 3: Libraries#######################################
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggpubr)
###############Section 4: Global variables################################
s.data <- '../Data/'
s.figs <- '../Figures/'
###############Section 5: Plot and save###################################
b.plot <- T; b.save <- F

##############Part 1: Final biomass#######################################
###############Section 1: In- and output##################################
df.biomass <- read.table(paste0(s.data, 'D_C9_Biomass.csv'), header = T, sep = ',')
df.biomass$Scenario <- factor(as.character(df.biomass$Scenario))
df.biomass$Focus <- factor(as.character(df.biomass$Focus))
df.bm.dw <- df.biomass[df.biomass$Series == 'DW', ]

###############Section 2: Combine replicates##############################
df.info <- unique(df.bm.dw[, c(3:8)])
df.bm.dw.d34 <- c()
for (i in c(1:nrow(df.info))){
  df.temp <- df.bm.dw[which(apply(df.bm.dw[, c(3:8)], 1, function(x) sum(x == df.info[i, ])) == ncol(df.info)),]
  df.bm.dw.d34 <- rbind(df.bm.dw.d34, cbind(df.info[i, ], mean(df.temp$Day34), sd(df.temp$Day34)))
}
names(df.bm.dw.d34) <- c(names(df.info), 'mean', 'sd')

df.tot <- c()
for (i in levels(df.bm.dw$Focus)){
  for (j in levels(df.bm.dw$Scenario)){
    for (k in c(min(df.bm.dw$Replicate):max(df.bm.dw$Replicate))){
      df.temp <- df.bm.dw[df.bm.dw$Focus == i & df.bm.dw$Scenario == j & df.bm.dw$Replicate == k, ]
      df.tot <- rbind.data.frame(df.tot, cbind.data.frame(df.temp[1, c(1:6)], sum(df.temp$Day34)))
    }
  }
}
names(df.tot)[ncol(df.tot)] <- 'Day34'

df.bm.dw.d34.tot <- c()
for (i in levels(df.tot$Focus)){
  for (j in levels(df.tot$Scenario)){
    df.temp <- df.tot[df.tot$Focus == i & df.tot$Scenario == j, ]
    df.bm.dw.d34.tot <- rbind(df.bm.dw.d34.tot, cbind(df.temp[1, ], mean(df.temp$Day34), sd(df.temp$Day34)))
  }
}
names(df.bm.dw.d34.tot) <- c(names(df.temp), 'mean', 'sd')

###############Section 3: Plotting and saving#############################
if(b.plot){
  # Separate species
  df.bm.dw.d34$In <- factor(df.bm.dw.d34$In, c('None', 'Low', 'High'))
  df.bm.dw.d34$Out <- factor(df.bm.dw.d34$Out, c('None', 'Low', 'High'))
  levels(df.bm.dw.d34$In) <- list('No introduction' = 'None', 
                                  'Low introduction' = 'Low', 
                                  'High introduction' = 'High')
  p.FinalBiomass <- ggplot(df.bm.dw.d34, aes(x = Out, y = mean, group = Species)) + 
    geom_col(aes(fill = Species), position = position_dodge(width = 0.9, preserve = 'single')) + 
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                  position = position_dodge(0.9), width = 0.2) + 
    scale_x_discrete('Removal frequency') + 
    scale_y_continuous('Biomass dry weight (g)') + 
    scale_fill_manual(values = c('L. minor' = 'grey30', 'L. minuta' = 'grey50')) + 
    facet_grid(Focus~In, scales = 'free', switch = 'y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8), 
          strip.text.y = element_text(face = 'italic'), 
          legend.title = element_blank(), 
          legend.background = element_blank(), 
          legend.position = c(0.92, 0.95), 
          legend.text = element_text(face = 'italic', size = 7))
  plot(p.FinalBiomass)
  
  # Total biomass
  df.bm.dw.d34.tot$In <- factor(df.bm.dw.d34.tot$In, c('None', 'Low', 'High'))
  df.bm.dw.d34.tot$Out <- factor(df.bm.dw.d34.tot$Out, c('None', 'Low', 'High'))
  levels(df.bm.dw.d34.tot$In) <- list('No introduction' = 'None', 
                                      'Low introduction' = 'Low', 
                                      'High introduction' = 'High')
  p.TotalBiomass <- ggplot(df.bm.dw.d34.tot, aes(x = Out, y = mean)) + 
    geom_col() + 
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) + 
    scale_x_discrete('Removal frequency') + 
    scale_y_continuous('Biomass dry weight (g)') +
    facet_grid(Focus~In, scales = 'free', switch = 'y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8), 
          strip.text.y = element_text(face = 'italic'), 
          legend.title = element_blank(),
          legend.background = element_blank(), 
          legend.position = c(0.92, 0.95), 
          legend.text = element_text(face = 'italic', size = 7))
  plot(p.TotalBiomass)
  # Saving
  if(b.save){
    tiff(paste0(s.figs, 'F_C9S1_FinalBiomass.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.FinalBiomass)
    dev.off()
    
    tiff(paste0(s.figs, 'F_SI_C9S1_TotalBiomass.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.TotalBiomass)
    dev.off()
  }
}

if(b.save){
  write.table(df.bm.dw.d34, file = paste0(s.data, 'FD_C9S1_FinalBiomass.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(df.biomass, df.bm.dw, df.info, df.bm.dw.d34, i, j, k, df.temp, df.tot, 
   p.FinalBiomass, p.TotalBiomass)

##############Part 2: Overall RGR#########################################
###############Section 1: In- and output##################################
df.biomass <- read.table(paste0(s.data, 'D_C9_Biomass.csv'), header = T, sep = ',')
df.biomass$Scenario <- factor(as.character(df.biomass$Scenario))
df.biomass$Focus <- factor(as.character(df.biomass$Focus))
df.bm.dw <- df.biomass[df.biomass$Series == 'DW' & 
                         df.biomass$Species == df.biomass$Focus, ]

###############Section 2: Combine replicates##############################
df.bm.dw.s <- df.bm.dw[which(df.bm.dw$Scenario %in% c(1, 4, 7)), ]
df.rgr <- df.bm.dw.s[, c(1:7)]
df.rgr$Series <- 'RGR_A'
for (i in c(1:nrow(df.rgr))){
  df.rgr$RGR[i] <- log(df.bm.dw.s$Day34[i] / df.bm.dw.s$Day0[i]) / 34
}

df.info <- unique(df.rgr[, c(3:8)])
df.rgr.sum <- c()
for (i in c(1:nrow(df.info))){
  df.temp <- df.rgr[which(apply(df.rgr[,c(3:8)], 1, 
                                function(x) sum(x == df.info[i, ])) == ncol(df.info)), ]
  df.rgr.sum <- rbind(df.rgr.sum, cbind(df.info[i, ], mean(df.temp$RGR), sd(df.temp$RGR)))
}
names(df.rgr.sum) <- c(names(df.info), 'mean', 'sd')

###############Section 3: Plotting and saving#############################
r.minor <- mean(df.rgr.sum$mean[1:3])
sd(df.rgr.sum$mean[1:3])
r.minuta <- mean(df.rgr.sum$mean[4:6])
sd(df.rgr.sum$mean[4:6])

###############Section 4: Variable removal################################
rm(df.biomass, df.bm.dw, df.rgr, df.info, df.rgr.sum, i, df.temp)

##############Part 3: Temporal trend in DW-FW Ratio#######################
###############Section 1: In- and output##################################
df.ratio <- read.table(paste0(s.data, 'D_C9_DWFWRatio.csv'), header = T, sep = ',')
df.ratio$Scenario <- factor(as.character(df.ratio$Scenario))
df.rat <- df.ratio[df.ratio$Series == 'RATIO' & df.ratio$Focus == df.ratio$Species, ]
df.info <- unique(df.rat[, c(3:8)])

###############Section 2: Combining replicates############################
df.rat.avg <- df.rat.sd <- c()
for (i in c(1:nrow(df.info))){
  df.temp <- df.rat[which(apply(df.rat[, c(3:8)], 1, 
                                function(x) sum(x == df.info[i, ])) == ncol(df.info)), ]
  df.rat.avg <- rbind(df.rat.avg, cbind(df.info[i, ], 
                                        t(apply(df.temp[, c(which(names(df.temp) == 'Day6'):ncol(df.temp))], 2, mean))))
  df.rat.sd <- rbind(df.rat.sd, cbind(df.info[i, ], 
                                      t(apply(df.temp[, c(which(names(df.temp) == 'Day6'):ncol(df.temp))], 2, sd))))
}

###############Section 3: Plotting and saving#############################
names(df.rat.avg)[c(which(names(df.rat.avg) == 'Day6'):ncol(df.rat.avg))] <- c(seq(6, 26, 4), 34)
names(df.rat.sd)[c(which(names(df.rat.sd) == 'Day6'):ncol(df.rat.sd))] <- c(seq(6, 26, 4), 34)

df.rat.avg.m <- melt(df.rat.avg, id.vars = names(df.rat.avg)[1:5], 
                     measure.vars = names(df.rat.avg)[which(names(df.rat.avg) == 6):ncol(df.rat.avg)])
df.rat.sd.m <- melt(df.rat.sd, id.vars = names(df.rat.sd)[1:5], 
                    measure.vars = names(df.rat.sd)[which(names(df.rat.sd) == 6):ncol(df.rat.sd)])

names(df.rat.avg.m)[ncol(df.rat.avg.m)] <- 'mean'
names(df.rat.sd.m)[ncol(df.rat.sd.m)] <- 'sd'

df.rat.obs <- cbind.data.frame(df.rat.avg.m, df.rat.sd.m$sd)
names(df.rat.obs)[ncol(df.rat.obs)] <- 'sd'
df.rat.obs$variable <- as.numeric(as.character(df.rat.obs$variable))
df.rat.obs$In <- factor(df.rat.obs$In, c('None', 'Low', 'High'))
df.rat.obs$Out <- factor(df.rat.obs$Out, c('None', 'Low', 'High'))

if(b.plot){
  # Lemna minor
  df.temp <- df.rat.obs[df.rat.obs$Species == 'L. minor', ]
  p.RatioMinor <- ggplot(df.temp) + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean), colour = 'grey70') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) - 
                 sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'longdash', colour = 'grey60') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) + 
                 sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'longdash', colour = 'grey60') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) - 
                 2 * sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'dashed', colour = 'grey60') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) + 
                 2 * sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'dashed', colour = 'grey60') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) - 
                 3 * sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'dotted', colour = 'grey60') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) + 
                 3 * sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'dotted', colour = 'grey60') + 
    geom_pointrange(aes(x = variable, y = mean, ymin = mean - sd, 
                        ymax = mean + sd, shape = Out), size = 0.2) + 
    labs(shape = 'Removal') +
    scale_x_continuous('Time (days)', limits = c(0, 35)) + 
    scale_y_continuous('Dry weight ratio (-)', limits = c(0.04, 0.07)) + 
    facet_grid(In~Focus) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 7), 
          legend.title = element_text(size = 8), 
          legend.text = element_text(size = 7))
  plot(p.RatioMinor)
  
  # Lemna minuta
  df.temp <- df.rat.obs[df.rat.obs$Species == 'L. minuta', ]
  p.RatioMinuta <- ggplot(df.temp) + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean), colour = 'grey70') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) - 
                 sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'longdash', colour = 'grey60') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) + 
                 sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'longdash', colour = 'grey60') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) - 
                 2 * sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'dashed', colour = 'grey60') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) + 
                 2 * sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'dashed', colour = 'grey60') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) - 
                 3 * sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'dotted', colour = 'grey60') + 
    geom_hline(yintercept = mean(df.temp[df.temp$variable == 34, ]$mean) + 
                 3 * sd(df.temp[df.temp$variable == 34, ]$sd), 
               linetype = 'dotted', colour = 'grey60') + 
    geom_pointrange(aes(x = variable, y = mean, ymin = mean - sd, 
                        ymax = mean + sd, shape = Out), size = 0.2) + 
    labs(shape = 'Removal') +
    scale_x_continuous('Time (days)', limits = c(0, 35)) + 
    scale_y_continuous('Dry weight ratio (-)', limits = c(0.04, 0.07)) + 
    facet_grid(In~Focus) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8), 
          legend.direction = 'horizontal', 
          legend.background = element_blank(), 
          legend.position = c(0.28, 0.95), 
          legend.spacing.x = unit(x = 0.5, 'mm'), 
          legend.title = element_blank(), 
          legend.text = element_text(size = 7))
  plot(p.RatioMinuta)
  
  # Combined
  p.RatioAverage <- ggarrange(p.RatioMinor + 
                                theme(legend.position = "none", 
                                      strip.text.y = element_blank()), 
                              p.RatioMinuta + 
                                theme(axis.title.y = element_blank(), 
                                      axis.text.y = element_blank(), 
                                      axis.ticks.y = element_blank()), 
                              nrow = 1, ncol = 2)
  plot(p.RatioAverage)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C9S1_FreshDryRatio.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.RatioAverage)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.ratio, df.rat, df.info, df.rat.avg, df.rat.sd, i, df.temp, df.rat.avg.m, 
   df.rat.sd.m, df.rat.obs, p.RatioMinor, p.RatioMinuta, p.RatioAverage)

##############Part 4: Temporal trend biomass##############################
###############Section 1: In- and output##################################
################Observations
df.biomass <- read.table(paste0(s.data, 'D_C9_Biomass.csv'), header = T, sep = ',')
df.biomass$Scenario <- factor(as.character(df.biomass$Scenario))
df.bm.dw <- df.biomass[df.biomass$Series == 'DW' & 
                         df.biomass$Focus == df.biomass$Species, ]
df.info <- unique(df.bm.dw[, c(3:8)])

################Simulations
v.out <- v.in <- c('None', 'Low', 'High')
df.out <- df.in <- c()
r.out <- 0.2
r.in <- 0.05 
# r.minor <- 0.15; r.minuta <- 0.15

###############Section 2: Combine replicates##############################
df.bm.avg <- df.bm.sd <- c()
for (i in c(1:nrow(df.info))){
  df.temp <- df.bm.dw[which(apply(df.bm.dw[, c(3:8)], 1, 
                                  function(x) sum(x == df.info[i, ])) == ncol(df.info)), ]
  df.bm.avg <- rbind(df.bm.avg, cbind(df.info[i, ], 
                                      t(apply(df.temp[, c(which(names(df.temp) == 'Day0'):ncol(df.temp))], 2, mean))))
  df.bm.sd <- rbind(df.bm.sd, cbind(df.info[i, ], 
                                    t(apply(df.temp[, c(which(names(df.temp) == 'Day0'):ncol(df.temp))], 2, sd))))
}
names(df.bm.avg)[c(which(names(df.bm.avg) == 'Day0'):ncol(df.bm.avg))] <- c(seq(0, 26, 2), 30, 34)
names(df.bm.sd)[c(which(names(df.bm.sd) == 'Day0'):ncol(df.bm.sd))] <- c(seq(0, 26, 2), 30, 34)

###############Section 3: Simulation data#################################
################Action dataframes
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

################Simulations
df.sim <- cbind(df.info, data.frame(matrix(nrow = nrow(df.info), ncol = (ncol(df.in) - 2))))
names(df.sim) <- c(names(df.info), names(df.in)[3:ncol(df.in)])

df.sim$`0` <- 0.5 * 0.05
for (i in c(1:nrow(df.sim))){
  for (j in c(which(names(df.sim) == 2):ncol(df.sim))){
    if(df.sim$Species[i] == 'L. minor'){ r.grow <- r.minor } 
    else if(df.sim$Species[i] == 'L. minuta'){ r.grow <- r.minuta }
    else {message('Macrophyte not known, rate = 0'); r.grow <- 0 }
    i.in <- which(df.in$In == df.sim$In[i] & df.in$Out == df.sim$Out[i])
    i.out <- which(df.out$In == df.sim$In[i] & df.out$Out == df.sim$Out[i])
    df.sim[i, j] <- exp(r.grow * 2) * df.sim[i, j-1] - 
      df.out[i.out, which(names(df.out) == names(df.sim)[j])] * r.out * exp(r.grow * 2) * df.sim[i, j-1]
  }
}

###############Section 4: Error calculation###############################
df.err <- df.bm.avg
for (i in c(1:nrow(df.err))){
  for (j in c((which(names(df.err) == '0')):ncol(df.err))){
    i.sim <- which(apply(df.sim[, c(1:6)], 1, function(x) sum(x == df.err[i, c(1:6)])) == 6)
    df.err[i, j] <- df.sim[i.sim, which(names(df.sim) == names(df.err)[j])] - df.bm.avg[i, j]
  }
}

###############Section 5: Plotting and saving#############################
# If looking for general label: https://stackoverflow.com/questions/36941197/overall-label-for-facets
if(b.plot){
  df.bm.avg.m <- melt(df.bm.avg, id.vars = names(df.bm.avg)[1:5], 
                      measure.vars = names(df.bm.avg)[which(names(df.bm.avg) == 0):ncol(df.bm.avg)])
  df.bm.sd.m <- melt(df.bm.sd, id.vars = names(df.bm.sd)[1:5], 
                     measure.vars = names(df.bm.sd)[which(names(df.bm.sd) == 0):ncol(df.bm.sd)])
  df.err.m <- melt(df.err, id.vars = names(df.err)[1:5], 
                   measure.vars = names(df.err)[which(names(df.err) == 0):ncol(df.err)])
  names(df.bm.avg.m)[ncol(df.bm.avg.m)] <- 'mean'
  names(df.bm.sd.m)[ncol(df.bm.sd.m)] <- 'sd'
  names(df.err.m)[ncol(df.err.m)] <- 'error'
  
  df.bm.obs <- cbind.data.frame(df.bm.avg.m, df.bm.sd.m$sd)
  names(df.bm.obs)[ncol(df.bm.obs)] <- 'sd'
  df.bm.obs$Type <- 'Observation'
  
  df.bm.sim <- melt(df.sim, id.vars = names(df.sim)[1:5], 
                    measure.vars = names(df.sim)[which(names(df.sim) == 0):ncol(df.sim)])
  names(df.bm.sim)[ncol(df.bm.sim)] <- 'mean'
  df.bm.sim$sd <- 0
  df.bm.sim$Type <- 'Simulation'
  
  df.bm <- rbind(df.bm.obs, df.bm.sim)
  df.bm$variable <- as.numeric(as.character(df.bm$variable))
  df.bm$In <- factor(df.bm$In, c('None', 'Low', 'High'))
  df.bm$Out <- factor(df.bm$Out, c('None', 'Low', 'High'))
  
  p.TrendSimulation <- ggplot(df.bm, aes(x = variable, y = mean, group = Out)) + 
    geom_line(data = df.bm[df.bm$Type == 'Simulation', ], aes(linetype = Out), size = 0.4) + 
    geom_point(data = df.bm[df.bm$Type == 'Observation', ], aes(shape = Out), size = 1) + 
    geom_linerange(data = df.bm[df.bm$Type == 'Observation', ], 
                   aes(ymin = mean - sd, ymax = mean + sd)) + 
    # Include removal and introduction events
    geom_point(aes(x = sort(rep(c(6, 14, 22), nrow(df.bm) / 3)), y = -0.05), 
               shape = 15, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(10, 18, 26), nrow(df.bm) / 3)), y = -0.05), 
               shape = 0, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(4, 12, 20), nrow(df.bm) / 3)), y = -0.05), 
               shape = 16, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(8, 16, 24), nrow(df.bm) / 3)), y = -0.05), 
               shape = 1, colour = 'grey60', size = 1) +
    # Further layout
    scale_x_continuous('Time (days)') + 
    scale_y_continuous('Biomass dry weight (g)') + 
    labs(linetype = 'Removal frequency', shape = 'Removal frequency') + 
    facet_grid(In~Focus) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.text.x = element_text(face = 'italic', size = 8), 
          strip.placement = 'outside', 
          legend.background = element_blank(), 
          legend.position = c(0.1, 0.88), 
          legend.spacing.y = unit(0.5, 'mm'), 
          legend.text = element_text(size = 7), 
          legend.title = element_text(size = 8))
  plot(p.TrendSimulation)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C9S1_TemporalBiomassIncrease.tiff'), units = 'mm', 
         width = 160, height = 105, res = 300, pointsize = 7)
    plot(p.TrendSimulation)
    dev.off()
  }
}

if(b.plot){
  df.err.m$variable <- as.numeric(as.character(df.err.m$variable))
  df.err.m$In <- factor(df.err.m$In, c('None', 'Low', 'High'))
  df.err.m$Out <- factor(df.err.m$Out, c('None', 'Low', 'High'))
  
  p.Error <- ggplot(df.err.m, aes(x = variable, y = error, group = Out)) + 
    geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey70', size = 0.4) +
    geom_point(aes(shape = Out), size = 1) + 
    # Include removal and introduction events
    geom_point(aes(x = sort(rep(c(6, 14, 22), nrow(df.err.m) / 3)), y = -0.8),
               shape = 15, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(10, 18, 26), nrow(df.err.m) / 3)), y = -0.8), 
               shape = 0, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(4, 12, 20), nrow(df.err.m) / 3)), y = -0.8), 
               shape = 16, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(8, 16, 24), nrow(df.err.m) / 3)), y = -0.8), 
               shape = 1, colour = 'grey60', size = 1) +
    # Further layout
    scale_x_continuous('Time (days)') + 
    scale_y_continuous(expression(Delta[Pred - Obs]~(g)), limits = c(-0.8, 0.2)) + 
    facet_grid(In~Focus) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8), 
          legend.title = element_blank(), 
          legend.background = element_blank(), 
          legend.position = c(0.05, 0.8), 
          legend.text = element_text(size = 7))
  plot(p.Error)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C9S1_ErrorSimulationObservation.tiff'), 
         units = 'mm', width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.Error)
    dev.off()
  }
}

###############Section 6: Variable removal################################
rm(df.biomass, df.bm.dw, df.info, df.bm.avg, df.bm.sd, i, df.temp, df.bm.avg.m, 
   df.bm.sd.m, df.bm.obs, df.err, df.err.m, p.TrendSimulation, p.Error, v.in,
   v.out, df.out, df.in, r.in, r.out, v.f, v.g, df.sim, i.sim, j, r.grow, 
   i.in, i.out, df.bm.sim, df.bm)

##############Part 5: Temporal trends growth rate#########################
###############Section 1: In- and output##################################
df.biomass <- read.table(paste0(s.data, 'D_C9_Biomass.csv'), header = T, sep = ',')
df.biomass$Scenario <- factor(as.character(df.biomass$Scenario))
df.rgr <- df.biomass[df.biomass$Series == 'RGR' & 
                       df.biomass$Focus == df.biomass$Species, ]
df.info <- unique(df.rgr[, c(3:8)])

###############Section 2: Combine replicates##############################
df.rgr.avg <- df.rgr.sd <- c()
for (i in c(1:nrow(df.info))){
  df.temp <- df.rgr[which(apply(df.rgr[, c(3:8)], 1, 
                                function(x) sum(x == df.info[i, ])) == ncol(df.info)), ]
  df.rgr.avg <- rbind(df.rgr.avg, cbind(df.info[i, ], 
                                        t(apply(df.temp[, c(which(names(df.temp) == 'Day0'):ncol(df.temp))], 2, mean))))
  df.rgr.sd <- rbind(df.rgr.sd, cbind(df.info[i, ], 
                                      t(apply(df.temp[, c(which(names(df.temp) == 'Day0'):ncol(df.temp))], 2, sd))))
}

###############Section 3: Plotting and saving#############################
names(df.rgr.avg)[c(which(names(df.rgr.avg) == 'Day0'):ncol(df.rgr.avg))] <- c(seq(0, 26, 2), 30, 34)
names(df.rgr.sd)[c(which(names(df.rgr.sd) == 'Day0'):ncol(df.rgr.sd))] <- c(seq(0, 26, 2), 30, 34)
df.rgr.avg.m <- melt(df.rgr.avg, id.vars = names(df.rgr.avg)[1:5], 
                     measure.vars = names(df.rgr.avg)[which(names(df.rgr.avg) == 0):ncol(df.rgr.avg)])
df.rgr.sd.m <- melt(df.rgr.sd, id.vars = names(df.rgr.sd)[1:5], 
                    measure.vars = names(df.rgr.sd)[which(names(df.rgr.sd) == 0):ncol(df.rgr.sd)])
names(df.rgr.avg.m)[ncol(df.rgr.avg.m)] <- 'mean'
names(df.rgr.sd.m)[ncol(df.rgr.sd.m)] <- 'sd'

df.rgr.obs <- cbind.data.frame(df.rgr.avg.m,df.rgr.sd.m$sd)
names(df.rgr.obs)[ncol(df.rgr.obs)] <- 'sd'
df.rgr.obs$variable <- as.numeric(as.character(df.rgr.obs$variable))
df.rgr.obs$In <- factor(df.rgr.obs$In, c('None', 'Low', 'High'))
df.rgr.obs$Out <- factor(df.rgr.obs$Out, c('None', 'Low', 'High'))

if(b.plot){
  p.GrowthRateTrend <- ggplot(df.rgr.obs, aes(x = variable, y = mean, group = Out)) + 
    geom_hline(data = data.frame(Focus = as.factor(c('L. minor', 'L. minuta')), 
                                 Z = c(r.minor, r.minuta)), 
               aes(yintercept = Z), colour = 'grey70', linetype = 'dashed') + 
    geom_point(aes(shape = Out), size = 1) + 
    geom_line(aes(linetype = Out), size = 0.4) + 
    geom_linerange(aes(ymin = mean - sd, ymax = mean + sd)) + 
    # Include removal and introduction events
    geom_point(aes(x = sort(rep(c(6, 14, 22), nrow(df.rgr.obs) / 3)), y = -0.05), 
               shape = 15, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(10, 18, 26), nrow(df.rgr.obs) / 3)), y = -0.05), 
               shape = 0, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(4, 12, 20), nrow(df.rgr.obs) / 3)), y = -0.05),
               shape = 16, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(8, 16, 24), nrow(df.rgr.obs) / 3)), y = -0.05), 
               shape = 1, colour = 'grey60', size = 1) +
    # Further layout
    scale_x_continuous('Time (days)') + 
    scale_y_continuous(expression(paste('Relative Growth Rate (', d^{-1}, ')', sep = '')), 
                       limits = c(-0.05, 0.6)) + 
    labs(shape = 'Removal', linetype = 'Removal') + 
    facet_grid(In~Focus) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.text.x = element_text(face = 'italic', size = 8), 
          strip.placement = 'outside', 
          legend.background = element_blank(), 
          legend.position = c(0.93, 0.88), 
          legend.text = element_text(size = 7), 
          legend.title = element_text(size = 8))
  plot(p.GrowthRateTrend)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C9S1_GrowthRateTrend.tiff'), units = 'mm', 
         width = 160, height = 105, res = 300, pointsize = 7)
    plot(p.GrowthRateTrend)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.biomass, df.rgr, df.info, df.rgr.avg, df.rgr.sd, i, df.temp, df.rgr.avg.m, 
   df.rgr.sd.m, df.rgr.obs, p.GrowthRateTrend)

##############Part 6: Temporal trends biomass ratio#######################
###############Section 1: In- and output##################################
df.biomass <- read.table(paste0(s.data, 'D_C9_Biomass.csv'), header = T, sep = ',')
df.biomass$Scenario <- factor(as.character(df.biomass$Scenario))
df.rel <- df.biomass[df.biomass$Series == 'REL' & 
                       df.biomass$Focus == df.biomass$Species, ]
df.info <- unique(df.rel[, c(3:8)])

###############Section 2: Combine replicates##############################
df.rel.avg <- df.rel.sd <- c()
for (i in c(1:nrow(df.info))){
  df.temp <- df.rel[which(apply(df.rel[, c(3:8)], 1, 
                                function(x) sum(x == df.info[i, ])) == ncol(df.info)), ]
  df.rel.avg <- rbind(df.rel.avg, cbind(df.info[i, ], 
                                        t(apply(df.temp[, c(which(names(df.temp) == 'Day0'):ncol(df.temp))], 2, mean))))
  df.rel.sd <- rbind(df.rel.sd, cbind(df.info[i, ], 
                                      t(apply(df.temp[, c(which(names(df.temp) == 'Day0'):ncol(df.temp))], 2, sd))))
}

###############Section 3: Plotting and saving#############################
names(df.rel.avg)[c(which(names(df.rel.avg) == 'Day0'):ncol(df.rel.avg))] <- c(seq(0, 26, 2), 30, 34)
names(df.rel.sd)[c(which(names(df.rel.sd) == 'Day0'):ncol(df.rel.sd))] <- c(seq(0, 26, 2), 30, 34)

df.rel.avg.m <- melt(df.rel.avg, id.vars = names(df.rel.avg)[1:5], 
                     measure.vars = names(df.rel.avg)[which(names(df.rel.avg) == 0):ncol(df.rel.avg)])
df.rel.sd.m <- melt(df.rel.sd, id.vars = names(df.rel.sd)[1:5], 
                    measure.vars = names(df.rel.sd)[which(names(df.rel.sd) == 0):ncol(df.rel.sd)])
names(df.rel.avg.m)[ncol(df.rel.avg.m)] <- 'mean'
names(df.rel.sd.m)[ncol(df.rel.sd.m)] <- 'sd'

df.rel.obs <- cbind.data.frame(df.rel.avg.m, df.rel.sd.m$sd)
names(df.rel.obs)[ncol(df.rel.obs)] <- 'sd'
df.rel.obs$variable <- as.numeric(as.character(df.rel.obs$variable))
df.rel.obs$In <- factor(df.rel.obs$In, c('None', 'Low', 'High'))
df.rel.obs$Out <- factor(df.rel.obs$Out, c('None', 'Low', 'High'))

if(b.plot){
  df.sub <- df.rel.obs[df.rel.obs$In != 'None', ]
  p.RelativeDominance <- ggplot(df.sub, aes(x = variable, y = mean, group = Out)) + 
    geom_hline(yintercept = 1, linetype = 'dashed', colour = 'grey70', size = 0.4) + 
    geom_point(aes(shape = Out), size = 1) + 
    geom_line(aes(linetype = Out), size = 0.4) + 
    geom_linerange(aes(ymin = mean - sd, ymax = mean + sd)) + 
    # Include removal and introduction events
    geom_point(aes(x = sort(rep(c(6, 14, 22), nrow(df.sub) / 3)), y = -1), 
               shape = 15, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(10, 18, 26), nrow(df.sub) / 3)), y = -1), 
               shape = 0, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(4, 12, 20), nrow(df.sub) / 3)), y = -1), 
               shape = 16, colour = 'grey60', size = 1) +
    geom_point(aes(x = sort(rep(c(8, 16, 24), nrow(df.sub) / 3)), y = -1), 
               shape = 1, colour = 'grey60', size = 1) +
    # Further layout
    scale_x_continuous('Time (days)') + 
    scale_y_continuous('Relative dominance (-)') + 
    labs(shape = 'Removal', linetype = 'Removal') + 
    facet_grid(In~Focus) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.text.x = element_text(face = 'italic', size = 8), 
          strip.placement = 'outside', 
          legend.background = element_blank(), 
          legend.position = c(0.93, 0.88), 
          legend.text = element_text(size = 7), 
          legend.title = element_text(size = 8))
  plot(p.RelativeDominance)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C9S1_BiomassRelativeDominance.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.RelativeDominance)
    dev.off()
  }
}

if(b.save){
  write.table(df.rel.obs, file = paste0(s.data, 'FD_C9S1_BiomassDominance.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(df.biomass, df.rel, df.info, df.rel.avg, df.rel.sd, i, df.temp, df.rel.avg.m, 
   df.rel.sd.m, df.rel.obs, p.RelativeDominance)
