##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run:05/02/2020; Last changes: 05/02/2020
###############Section 2: Packages########################################
# install.packages('reshape2') #For melting dataframes into 'long' mode
# install.packages('ggplot2') # For plotting
# install.packages('gridExtra') # For flexible plotting
# install.packages('ggpubr') # For merging plots
# install.packages('dunn.test') # For comparison among groups
# install.packages('car') # For variance homogeneity testing
# install.packages('nlme') # For LMEM
###############Section 3: Libraries#######################################
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(dunn.test)
library(nlme)
library(car)
###############Section 4: Defining functions##############################
f.Wilcox <- function(data, x, y, paired = T){
  return(c(x, y, wilcox.test(df.temp[which(df.temp$Scenario == x), ]$Day34, 
                             df.temp[which(df.temp$Scenario == y), ]$Day34, 
                             paired = paired)$p.value))
}
f.Ttest <- function(data, x, y, paired = F){
  return(c(x, y, t.test(df.temp[which(df.temp$Scenario == x), ]$Day34, 
                        df.temp[which(df.temp$Scenario == y), ]$Day34, 
                        paired = paired)$p.value))
}
###############Section 5: Global variables################################
s.data <- '../Data/'
s.figs <- '../Figures/'
###############Section 6: Plot and save###################################
b.plot <- T; b.save <- T

##############Part 1: Final biomass#######################################
###############Section 1: In- and output##################################
df.biomass <- read.table(paste0(s.data, 'D_C9_Biomass.csv'), header = T, sep = ',')
df.biomass$Scenario <- factor(as.character(df.biomass$Scenario))
df.bm.dw <- df.biomass[df.biomass$Series == 'DW', ]
v.mf <- c('L. minor', 'L. minuta')

###############Section 2: Statistical tests###############################
################Normality in groups? -> Apparently yes####################
df.stat.shap <- c()
for (i in c(1:length(v.mf))){
  for (j in c(1:length(v.mf))){
    df.temp <- df.bm.dw[df.bm.dw$Focus == v.mf[i] & 
                          df.bm.dw$Species == v.mf[j] & 
                          df.bm.dw$Day34 != 0, ]
    df.temp$Scenario <- factor(df.temp$Scenario)
    for (k in c(levels(df.temp$Scenario))){
      s.test <- shapiro.test(df.temp[df.temp$Scenario == k, ]$Day34)
      df.stat.shap <- rbind(df.stat.shap, c(v.mf[i], v.mf[j], k, 
                                            s.test$statistic, s.test$p.value))
    }
  }
}
df.stat.shap <- as.data.frame(df.stat.shap)
names(df.stat.shap) <- c('Focus', 'Species', 'Scenario', 'Statistic', 'Pvalue')

################Homoscedastic? -> Apparently yes##########################
df.stat.bart <- c()
for (i in c(1:length(v.mf))){
  for (j in c(1:length(v.mf))){
    df.temp <- df.bm.dw[df.bm.dw$Focus == v.mf[i] & 
                          df.bm.dw$Species == v.mf[j] & 
                          df.bm.dw$Day34 != 0, ]
    s.test <- bartlett.test(Day34~Scenario, data = df.temp)
    df.stat.bart <- rbind(df.stat.bart, c(v.mf[i], v.mf[j], s.test$statistic, 
                                          s.test$parameter, s.test$p.value))
  }
}
df.stat.bart <- as.data.frame(df.stat.bart)
names(df.stat.bart) <- c('Focus', 'Species', 'Statistic', 'Freedom', 'Pvalue')

################ANOVA?####################################################
for (i in c(1:length(v.mf))){
  for (j in c(1:length(v.mf))){
    df.temp <- df.bm.dw[df.bm.dw$Focus == v.mf[i] & 
                          df.bm.dw$Species == v.mf[j] & 
                          df.bm.dw$Day34 != 0, ]
    message(paste0('ANOVA test for ', v.mf[i], ' and ', v.mf[j]))
    print(summary(aov(Day34~Scenario, data = df.temp)))
  }
}
mod.aov <- aov(Day34~In*Out, data = df.bm.dw)
summary(mod.aov)
plot(mod.aov, 1)

leveneTest(Day34~In*Out, data = df.bm.dw)
TukeyHSD(mod.aov, which = 'Out')
TukeyHSD(mod.aov, which = 'In')
pairwise.t.test(df.bm.dw$Day34, df.bm.dw$Out, p.adjust.method = 'BH')
pairwise.t.test(df.bm.dw$Day34, df.bm.dw$In, p.adjust.method = 'BH')

################Specific differences among groups? - pairwise#############
lst.pTtest <- list()
w <- 1
for (i in c(1:length(v.mf))){
  for (j in c(1:length(v.mf))){
    df.temp <- df.bm.dw[df.bm.dw$Focus == v.mf[i] & 
                          df.bm.dw$Species == v.mf[j] & 
                          df.bm.dw$Day34 != 0, ]
    message(paste0('T-test for ', v.mf[i], ' (Primary) and ', v.mf[j]))
    df.pTtest <- c()
    # Removal
    if(v.mf[i] == v.mf[j]){
      df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 1, 2))
      df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 1, 3))
      df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 2, 3))
    }
    df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 4, 5))
    df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 4, 6))
    df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 5, 6))
    df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 7, 8))
    df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 7, 9))
    df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 8, 9))
    
    # Input
    if(v.mf[i] == v.mf[j]){
      df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 1, 4))
      df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 1, 7))
      df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 2, 5))
      df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 2, 8))
      df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 3, 6))
      df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 3, 9))
    }
    df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 4, 7))
    df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 5, 8))
    df.pTtest <- rbind(df.pTtest, f.Ttest(df.temp, 6, 9))
    
    # Adjust
    df.pTtest <- cbind(df.pTtest, p.adjust(df.pTtest[, 3], method = 'BH'))
    colnames(df.pTtest) <- c('Scenario1', 'Scenario2', 'pValue', 'pValue_adj')
    lst.pTtest[[w]] <- df.pTtest
    w <- w + 1
  }
}

################Overall group difference?#################################
df.stat.kw <- c()
for (i in c(1:length(v.mf))){
  for (j in c(1:length(v.mf))){
    df.temp <- df.bm.dw[df.bm.dw$Focus == v.mf[i] & 
                          df.bm.dw$Species == v.mf[j] & 
                          df.bm.dw$Day34 != 0, ]
    s.test <- kruskal.test(Day34~Scenario, data = df.temp)
    df.stat.kw <- rbind(df.stat.kw, c(v.mf[i], v.mf[j], s.test$statistic, 
                                      s.test$parameter, s.test$p.value))
  }
}
df.stat.kw <- as.data.frame(df.stat.kw)
names(df.stat.kw) <- c('Focus', 'Species', 'Statistic', 'Freedom', 'Pvalue')

################Specific differences among groups? - Dunn#################
# Remark: Dunn-test can be over-conservative (correcting for non-interesting comparisons)
for (i in c(1:length(v.mf))){
  for (j in c(1:length(v.mf))){
    df.temp <- df.bm.dw[df.bm.dw$Focus == v.mf[i] & 
                          df.bm.dw$Species == v.mf[j] & 
                          df.bm.dw$Day34 != 0, ]
    message(paste0('Dunn test for ', v.mf[i], ' and ', v.mf[j]))
    print(dunn.test(x = df.temp$Day34, g = df.temp$Scenario, method = 'bh', kw = T))
  }
}

###############Section 3: Variable removal################################
rm(df.biomass, df.bm.dw)

##############Part 2: Relative growth rate################################
###############Section 1: In- and output##################################
df.biomass <- read.table(paste0(s.data, 'D_C9_Biomass.csv'), header = T, sep = ',')
df.biomass$Scenario <- factor(as.character(df.biomass$Scenario))
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
  df.temp <- df.rgr[which(apply(df.rgr[, c(3:8)], 1, 
                                function(x) sum(x == df.info[i, ])) == ncol(df.info)), ]
  df.rgr.sum <- rbind(df.rgr.sum, cbind(df.info[i, ], mean(df.temp$RGR), sd(df.temp$RGR)))
}
names(df.rgr.sum) <- c(names(df.info), 'mean', 'sd')

###############Section 3: Statistics#############################
shapiro.test(df.rgr.sum$mean[1:3])
shapiro.test(df.rgr.sum$mean[4:6])
bartlett.test(mean~Species, data = df.rgr.sum)
wilcox.test(mean~Species, data = df.rgr.sum)
t.test(mean~Species, data = df.rgr.sum)

###############Section 4: Variable removal################################
rm(df.biomass,df.bm.dw,df.rgr,df.info,df.rgr.sum,i,df.temp)

##############Part 3: Temporal trends#####################################
###############Section 1: In- and output##################################
df.biomass <- read.table(paste0(s.data, 'D_C9_Biomass.csv'), header = T, sep = ',')
df.biomass$Scenario <- factor(as.character(df.biomass$Scenario))
df.bm.dw <- df.biomass[df.biomass$Series == 'DW', ]

###############Section 2: General information#############################
################Subsection: Species-specific data#########################
# Lemna minor
df.minor <- df.bm.dw[df.bm.dw$Focus == 'L. minor' & 
                       df.bm.dw$Species == 'L. minor', ]
df.minor$n <- c(1:nrow(df.minor))
df.minor.m <- melt(df.minor, id.vars = c('In', 'Out', 'n'), 
                   measure.vars = names(df.minor)[c(9:24)])
df.minor.m$In <- as.factor(df.minor.m$In)
df.minor.m$Out <- as.factor(df.minor.m$Out)
df.minor.m$T1 <- c(rep(0, 27), rep(2, 27), rep(4, 14 * 27))
df.minor.m$T2 <- c(rep(0, 3 * 27), sort(rep(seq(2, 22, by = 2), 27)), rep(22, 2 * 27))
df.minor.m$T3 <- c(rep(0, 14 * 27), rep(4, 27), rep(8, 27))
df.minor.m$Time <- c(sort(rep(seq(0, 26, 2), 27)), rep(30, 27), rep(34, 27))

# Lemna minuta
df.minuta <- df.bm.dw[df.bm.dw$Focus == 'L. minuta' & 
                        df.bm.dw$Species == 'L. minuta', ]
df.minuta$n <- c(1:nrow(df.minuta))
df.minuta.m <- melt(df.minuta, id.vars = c('In', 'Out', 'n'), 
                    measure.vars = names(df.minuta)[c(9:24)])
df.minuta.m$In <- as.factor(df.minuta.m$In)
df.minuta.m$Out <- as.factor(df.minuta.m$Out)
df.minuta.m$T1 <- c(rep(0, 27), rep(2, 27), rep(4, 14 * 27))
df.minuta.m$T2 <- c(rep(0, 3 * 27), sort(rep(seq(2, 22, by = 2), 27)), rep(22, 2 * 27))
df.minuta.m$T3 <- c(rep(0, 14 * 27), rep(4, 27), rep(8, 27))
df.minuta.m$Time <- c(sort(rep(seq(0, 26, 2), 27)), rep(30, 27), rep(34, 27))

################Subsection: Exploration of data (patterns)################
# Lemna minor
df.minor.var <- c()
for (i in c(1:length(levels(df.minor.m$In)))){
  for (j in c(1:length(levels(df.minor.m$Out)))){
    for (k in c(1:length(levels(df.minor.m$variable)))){
      df.temp <- df.minor.m[df.minor.m$In == levels(df.minor.m$In)[i] & 
                              df.minor.m$Out == levels(df.minor.m$Out)[j] & 
                              df.minor.m$variable == levels(df.minor.m$variable)[k], ]
      df.minor.var <- rbind.data.frame(df.minor.var, 
                                       cbind.data.frame(levels(df.minor.m$In)[i], 
                                                        levels(df.minor.m$Out)[j], 
                                                        levels(df.minor.m$variable)[k], 
                                                        mean(df.temp$Time), 
                                                        mean(df.temp$value), 
                                                        var(df.temp$value)))
    }
  }
}
names(df.minor.var) <- c('In', 'Out', 'Day', 'Time', 'Mean', 'Var')
df.minor.var$In <- factor(df.minor.var$In, c('None', 'Low', 'High'))
df.minor.var$Out <- factor(df.minor.var$Out, c('None', 'Low', 'High'))

if(b.plot){
  p.Histo <- ggplot(df.minor.m) + 
    geom_histogram(aes(x = value), bins = 10, fill = 'white', colour = 'black') + 
    scale_x_continuous(name = expression(Dry~biomass~italic(L.~minor)~(g))) + 
    scale_y_continuous('Occurrence count (-)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.Histo)
  
  p.MeanTrend <- ggplot(df.minor.var, aes(Time, Mean)) + 
    geom_point(size = 1) + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous('Mean(Dry biomass) (g)') + 
    facet_grid(In~Out) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.MeanTrend)
  
  p.VarianceTrend <- ggplot(df.minor.var, aes(Time, Var)) + 
    geom_point(size = 1) + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous(expression(Variance(Dry~biomass)~(g^{2}))) + 
    facet_grid(In~Out) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.VarianceTrend)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C9S2_VarianceTemporalMinor.tiff'), units = 'mm', 
         width = 160, height = 120, res = 300, pointsize = 7)
    plot(p.VarianceTrend)
    dev.off()
  }
}

# L. minuta
df.minuta.var <- c()
for (i in c(1:length(levels(df.minuta.m$In)))){
  for (j in c(1:length(levels(df.minuta.m$Out)))){
    for (k in c(1:length(levels(df.minuta.m$variable)))){
      df.temp <- df.minuta.m[df.minuta.m$In == levels(df.minuta.m$In)[i] & 
                               df.minuta.m$Out == levels(df.minuta.m$Out)[j] & 
                               df.minuta.m$variable == levels(df.minuta.m$variable)[k], ]
      df.minuta.var <- rbind.data.frame(df.minuta.var, 
                                        cbind.data.frame(levels(df.minuta.m$In)[i], 
                                                         levels(df.minuta.m$Out)[j], 
                                                         levels(df.minuta.m$variable)[k], 
                                                         mean(df.temp$Time), 
                                                         mean(df.temp$value), 
                                                         var(df.temp$value)))
    }
  }
}
names(df.minuta.var) <- c('In', 'Out', 'Day', 'Time', 'Mean', 'Var')
df.minuta.var$In <- factor(df.minuta.var$In, c('None', 'Low', 'High'))
df.minuta.var$Out <- factor(df.minuta.var$Out, c('None', 'Low', 'High'))

if(b.plot){
  p.Histo <- ggplot(df.minuta.m) + 
    geom_histogram(aes(x = value), bins = 10, fill = 'white', colour = 'black') + 
    scale_x_continuous(name = expression(Dry~biomass~italic(L.~minuta)~(g))) + 
    scale_y_continuous('Occurrence count (-)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.Histo)
  
  p.MeanTrend <- ggplot(df.minuta.var, aes(Time, Mean)) + 
    geom_point(size = 1) + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous('Mean(Dry biomass) (g)') + 
    facet_grid(In~Out) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.MeanTrend)
  
  p.VarianceTrend <- ggplot(df.minuta.var, aes(Time, Var)) + 
    geom_point(size = 1) + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous(expression(Variance(Dry~biomass)~(g^{2}))) + 
    facet_grid(In~Out) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.VarianceTrend)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C9S2_VarianceTemporalMinuta.tiff'), units = 'mm', 
         width = 160, height = 120, res = 300, pointsize = 7)
    plot(p.VarianceTrend)
    dev.off()
  }
}

################Subsection: Data transformation + exploration#############
# L. minor
df.minor.m$valueT <- df.minor.m$value^(1/3)
df.minorT.var <- c()
for (i in c(1:length(levels(df.minor.m$In)))){
  for (j in c(1:length(levels(df.minor.m$Out)))){
    for (k in c(1:length(levels(df.minor.m$variable)))){
      df.temp <- df.minor.m[df.minor.m$In == levels(df.minor.m$In)[i] & 
                              df.minor.m$Out == levels(df.minor.m$Out)[j] & 
                              df.minor.m$variable == levels(df.minor.m$variable)[k], ]
      df.minorT.var <- rbind.data.frame(df.minorT.var, 
                                        cbind.data.frame(levels(df.minor.m$In)[i], 
                                                         levels(df.minor.m$Out)[j], 
                                                         levels(df.minor.m$variable)[k], 
                                                         mean(df.temp$Time), 
                                                         mean(df.temp$valueT), 
                                                         var(df.temp$valueT)))
    }
  }
}
names(df.minorT.var) <- c('In', 'Out', 'Day', 'Time', 'Mean', 'Var')
df.minorT.var$In <- factor(df.minor.var$In, c('None', 'Low', 'High'))
df.minorT.var$Out <- factor(df.minor.var$Out, c('None', 'Low', 'High'))

if(b.plot){
  p.Histo <- ggplot(df.minor.m) + 
    geom_histogram(aes(x = valueT), bins = 10, fill = 'white', colour = 'black') + 
    scale_x_continuous(name = expression(Dry~biomass~italic(L.~minor)~(g))) + 
    scale_y_continuous('Occurrence count (-)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.Histo)
  
  p.MeanTrend <- ggplot(df.minorT.var, aes(Time, Mean)) + 
    geom_point(size = 1) + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous('Mean(Dry biomass) (g)') + 
    facet_grid(In~Out) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.MeanTrend)
  
  p.VarianceTrend <- ggplot(df.minorT.var, aes(Time, Var)) + 
    geom_point(size = 1) + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous(expression(Variance(Dry~biomass)~(g^{2}))) + 
    facet_grid(In~Out) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.VarianceTrend)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C9S2_VarianceTransformedTemporalMinor.tiff'), 
         units = 'mm', width = 160, height = 120, res = 300, pointsize = 7)
    plot(p.VarianceTrend)
    dev.off()
  }
}

# L. minuta
df.minuta.m$valueT <- df.minuta.m$value^(1/3)
df.minutaT.var <- c()
for (i in c(1:length(levels(df.minuta.m$In)))){
  for (j in c(1:length(levels(df.minuta.m$Out)))){
    for (k in c(1:length(levels(df.minuta.m$variable)))){
      df.temp <- df.minuta.m[df.minuta.m$In == levels(df.minuta.m$In)[i] & 
                               df.minuta.m$Out == levels(df.minuta.m$Out)[j] & 
                               df.minuta.m$variable == levels(df.minuta.m$variable)[k], ]
      df.minutaT.var <- rbind.data.frame(df.minutaT.var, 
                                         cbind.data.frame(levels(df.minuta.m$In)[i], 
                                                          levels(df.minuta.m$Out)[j], 
                                                          levels(df.minuta.m$variable)[k], 
                                                          mean(df.temp$Time), 
                                                          mean(df.temp$valueT), 
                                                          var(df.temp$valueT)))
    }
  }
}
names(df.minutaT.var) <- c('In', 'Out', 'Day', 'Time', 'Mean', 'Var')
df.minutaT.var$In <- factor(df.minutaT.var$In, c('None', 'Low', 'High'))
df.minutaT.var$Out <- factor(df.minutaT.var$Out, c('None', 'Low', 'High'))

if(b.plot){
  p.Histo <- ggplot(df.minuta.m) + 
    geom_histogram(aes(x = valueT), bins = 10, fill = 'white', colour = 'black') + 
    scale_x_continuous(name = expression(Dry~biomass~italic(L.~minuta)~(g))) + 
    scale_y_continuous('Occurrence count (-)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.Histo)
  
  p.MeanTrend <- ggplot(df.minutaT.var, aes(Time, Mean)) + 
    geom_point(size = 1) + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous('Mean(Dry biomass) (g)') + 
    facet_grid(In~Out) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.MeanTrend)
  
  p.VarianceTrend <- ggplot(df.minutaT.var, aes(Time, Var)) + 
    geom_point(size = 1) + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous(expression(Variance(Dry~biomass)~(g^{2}))) + 
    facet_grid(In~Out) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.VarianceTrend)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C9S2_VarianceTransformedTemporalMinuta.tiff'), 
         units = 'mm', width = 160, height = 120, res = 300, pointsize = 7)
    plot(p.VarianceTrend)
    dev.off()
  }
}

# Combined plot
if(b.plot){
  p.HistoMinor <- ggplot(df.minor.m) + 
    geom_histogram(aes(x = value), bins = 10, fill = 'white', colour = 'black') + 
    scale_x_continuous(expression(Dry~biomass~italic(L.~Minor)~(g))) + 
    scale_y_continuous('Occurrence count (-)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  
  p.HistoMinuta <- ggplot(df.minuta.m) + 
    geom_histogram(aes(x = value), bins = 10, fill = 'white', colour = 'black') + 
    scale_x_continuous(expression(Dry~biomass~italic(L.~Minuta)~(g))) + 
    scale_y_continuous('Occurrence count (-)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  
  p.HistoMinorTrans <- ggplot(df.minor.m) + 
    geom_histogram(aes(x = valueT), bins = 10, fill = 'white', colour = 'black') + 
    scale_x_continuous(expression(sqrt(Dry~biomass~italic(L.~Minor)~(g), 3))) + 
    scale_y_continuous('Occurrence count (-)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))

  p.HistoMinutaTrans <- ggplot(df.minuta.m) + 
    geom_histogram(aes(x = valueT), bins = 10, fill = 'white', colour = 'black') + 
    scale_x_continuous(expression(sqrt(Dry~biomass~italic(L.~Minuta)~(g), 3))) + 
    scale_y_continuous('Occurrence count (-)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  
  p.Histograms <- ggarrange(p.HistoMinor, p.HistoMinuta, p.HistoMinorTrans, 
                            p.HistoMinutaTrans, nrow = 2, ncol = 2, 
                            labels = 'AUTO', label.x = 0.15, label.y = 0.95, 
                            font.label = list(size = 10, face = 'bold'), 
                            align = 'hv')
  plot(p.Histograms)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C9S2_TransformationEffectBiomass.tiff'), 
         units = 'mm', width = 160, height = 120, res = 300, pointsize = 7)
    plot(p.Histograms)
    dev.off()
  }
}

###############Section 3: Modelling, Plotting and saving##################
################Subsection: L. minor######################################
#####Step 1: Ordinary vs GLM######
lm0 <- gls(valueT~(T1+T2+T3)*Out*In, data = df.minor.m)
lme0 <- lme(valueT~(T1+T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
            method = 'REML', data = df.minor.m)
anova(lm0, lme0) # LME is significantly better

#####Step 2: Random structure#######
lme0 <- lme(valueT~(T1+T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
            method = 'REML', data = df.minor.m)
lme1a <- lme(valueT~(T1+T2+T3)*Out*In, random = ~0+T1|n, correlation = corAR1(), 
             method = 'REML', data = df.minor.m)
lme1b <- lme(valueT~(T1+T2+T3)*Out*In, random = ~0+T2|n, correlation = corAR1(), 
             method = 'REML', data = df.minor.m)
lme1c <- lme(valueT~(T1+T2+T3)*Out*In, random = ~0+T1+T2|n, correlation = corAR1(), 
             method = 'REML', data = df.minor.m)

anova(lme0, lme1a)
anova(lme0, lme1b)
anova(lme0, lme1c) # LME with random intercepts (lme0) is least complex

#####Step 3: Fixed structure#######
# First round: no interaction between T1 or T3 and treatments
lme1 <- lme(valueT~(T1+T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
            method = 'ML', data = df.minor.m)
lme2a <- lme(valueT~T1+(T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
             method = 'ML', data = df.minor.m)
lme2b <- lme(valueT~(T1+T2)*Out*In+T3, random = ~1|n, correlation = corAR1(), 
             method = 'ML', data = df.minor.m)

anova(lme1, lme2a)
anova(lme1, lme2b) # No significant difference without interactions for T1 (lme2a) has better AIC

# Second round: no 3-way interactions whatsoever
lme2 <- lme(valueT~T1+(T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
            method = 'ML', data = df.minor.m)
lme3a <- lme(valueT~T1+(T2+T3)*Out*In-T2:Out:In, random = ~1|n, 
             correlation = corAR1(), method = 'ML', data = df.minor.m)
lme3b <- lme(valueT~T1+(T2+T3)*Out*In-T3:Out:In, random = ~1|n, 
             correlation = corAR1(), method = 'ML', data = df.minor.m)

anova(lme2, lme3a)
anova(lme2, lme3b) # Significant difference with 3-way interactions, original (lme2) better AIC

# Third round: final check importance T1
lme3 <- lme(valueT~T1+(T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
            method = 'ML', data = df.minor.m)
lme4a <- lme(valueT~(T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
             method = 'ML', data = df.minor.m)

anova(lme3, lme4a) # Significant difference, with inclusion of T1 (lme3) better AIC

# Final model
lme4 <- lme(valueT~T1+(T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
            method = 'REML', data = df.minor.m)

summary(lme4)
AIC(lme4)

df.minor.coef <- cbind.data.frame(coef(summary(lme4)), intervals(lme4)$fixed[, c(1, 3)])

if(b.save) { 
  write.table(df.minor.coef, file = paste0(s.data, 'D_SI_C9S2_CoefficientsLMEMinor.txt'), 
              row.names = T, sep = ',')
}

#####Step 4: Plotting#####
df.minor.mod <- df.minor.m
df.minor.mod$Pred <- predict(lme4)
df.minor.mod$Resid <- resid(lme4)

# For CI on predictions, use Monte Carlo: https://stats.stackexchange.com/questions/231074/confidence-intervals-on-predictions-for-a-non-linear-mixed-model-nlme
df.minor.mod$In <- factor(df.minor.mod$In, c('None', 'Low', 'High'))
df.minor.mod$Out <- factor(df.minor.mod$Out, c('None', 'Low', 'High'))

if(b.plot){
  # Model description
  p.ResponseFitted <- ggplot(df.minor.mod, aes(Pred, Resid)) + 
    geom_hline(yintercept = 0, colour = 'grey60', linetype = 'dashed', size = 0.4) + 
    geom_point(size = 1) + 
    scale_x_continuous('Fitted values') + 
    scale_y_continuous('Residuals', limits = c(-0.075, 0.075)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.ResponseFitted)
  
  p.QQplot <- ggplot(df.minor.mod, aes(sample = Resid)) + 
    stat_qq(size = 1) + 
    scale_x_continuous('Theoretical') + 
    scale_y_continuous('Residuals', limits = c(-0.075, 0.075)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.QQplot)
  
  p.Model <- ggarrange(p.ResponseFitted, p.QQplot, nrow = 1, ncol = 2, 
                       labels = 'AUTO', label.x = 0.17, label.y = 0.975, 
                       font.label = list(size = 10, face = 'bold'), align = 'hv')
  plot(p.Model)
  
  # Residuals versus predictors
  p.PredictorIn <- ggplot(df.minor.mod, aes(In, Resid)) + 
    geom_hline(yintercept = 0, colour = 'grey60', linetype = 'dashed', size = 0.4) + 
    geom_boxplot() + 
    scale_x_discrete('Introduction frequency') + 
    scale_y_continuous('Residuals', limits = c(-0.075, 0.075)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.PredictorIn)
  
  p.PredictorOut <- ggplot(df.minor.mod, aes(Out, Resid)) + 
    geom_hline(yintercept = 0, colour = 'grey60', linetype = 'dashed', size = 0.4) + 
    geom_boxplot() + 
    scale_x_discrete('Removal frequency') + 
    scale_y_continuous('Residuals', limits = c(-0.075, 0.075)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.PredictorOut)
  
  p.PredictorTime <- ggplot(df.minor.mod, aes(Time, Resid)) + 
    geom_hline(yintercept = 0, colour = 'grey60', linetype = 'dashed', size = 0.4) + 
    geom_point(size = 1) + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous('Residuals', limits = c(-0.075, 0.075)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.PredictorTime)
  
  p.Predictors <- ggarrange(p.PredictorIn, p.PredictorOut, p.PredictorTime, 
                            nrow = 1, ncol = 3, labels = 'AUTO', label.x = 0.25,
                            label.y = 0.975, font.label = list(size = 10, face = 'bold'), 
                            align = 'hv')
  plot(p.Predictors)
  
  # Predictions
  df.pred <- c()
  for (i in c(1:length(levels(df.minor.mod$In)))){
    for (j in c(1:length(levels(df.minor.mod$Out)))){
      for (k in c(1:length(levels(df.minor.mod$variable)))){
        df.temp <- df.minor.mod[df.minor.mod$In == levels(df.minor.mod$In)[i] & 
                                  df.minor.mod$Out == levels(df.minor.mod$Out)[j] & 
                                  df.minor.mod$variable == levels(df.minor.mod$variable)[k], ]
        df.pred <- rbind.data.frame(df.pred, 
                                    cbind.data.frame(levels(df.minor.mod$In)[i], 
                                                     levels(df.minor.mod$Out)[j], 
                                                     mean(df.temp$Time), 
                                                     mean(df.temp$value), 
                                                     sd(df.temp$value), 
                                                     mean(df.temp$Pred^3), 
                                                     sd(df.temp$Pred^3)))
      }
    }
  }
  names(df.pred) <- c('In', 'Out', 'Time', 'Mean', 'sd_Mean', 'Pred', 'sd_Pred')
  
  p.LME <- ggplot(df.pred, aes(x = Time)) + 
    geom_ribbon(aes(ymin = Pred - sd_Pred, ymax = Pred + sd_Pred), fill = 'grey90') + 
    geom_errorbar(aes(ymin = Mean - sd_Mean, ymax = Mean + sd_Mean), 
                  width = 0.3, colour = 'grey60') + 
    geom_point(aes(y = Mean), size = 1, colour = 'grey30') + 
    geom_line(aes(y = Pred), size = 0.4, colour = 'black') + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous('Dry biomass (g)', limits = c(0, 1.7), 
                       breaks = c(0, 0.5, 1, 1.5)) + 
    facet_grid(In~Out) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.LME)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C9S2_LMEMinorModel.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.Model)
    dev.off()
    
    tiff(paste0(s.figs, 'F_SI_C9S2_LMEMinorPredictors.tiff'), units = 'mm',
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.Predictors)
    dev.off()
    
    tiff(paste0(s.figs, 'F_SI_C9S2_LMEMinorPredictions.tiff'), units = 'mm',
         width = 160, height = 120, res = 300, pointsize = 7)
    plot(p.LME)
    dev.off()
  }
}

################Subsection: L. minuta#####################################
#####Step 1: Ordinary vs GLM######
lm0 <- gls(valueT~(T1+T2+T3)*Out*In, data = df.minuta.m)
lme0 <- lme(valueT~(T1+T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
            method = 'REML', data = df.minuta.m)

anova(lm0, lme0) # LME is significantly better

#####Step 2: Random structure#######
lme0 <- lme(valueT~(T1+T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
            method = 'REML', data = df.minuta.m)
lme1a <- lme(valueT~(T1+T2+T3)*Out*In, random = ~0+T1|n, correlation = corAR1(), 
             method = 'REML', data = df.minuta.m)
lme1b <- lme(valueT~(T1+T2+T3)*Out*In, random = ~0+T2|n, correlation = corAR1(), 
             method = 'REML', data = df.minuta.m)
lme1c <- lme(valueT~(T1+T2+T3)*Out*In, random = ~0+T3|n, correlation = corAR1(), 
             method = 'REML', data = df.minuta.m)

anova(lme0, lme1a)
anova(lme0, lme1b)
anova(lme0, lme1c) # LME with random intercepts (lme0) is least complex

#####Step 3: Fixed structure#######
# First round: no interaction between T1 and treatments
lme1 <- lme(valueT~(T1+T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
            method = 'ML', data = df.minuta.m)
lme2a <- lme(valueT~T1+(T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
             method = 'ML', data = df.minuta.m)
lme2b <- lme(valueT~(T1+T2)*Out*In+T3, random = ~1|n, correlation = corAR1(), 
             method = 'ML', data = df.minuta.m)

anova(lme1, lme2a)
anova(lme1, lme2b) # No significant difference without interactions for T1 (lme2a) has better AIC

# Second round: no 3-way interaction whatsoever
lme2 <- lme(valueT~T1+(T2+T3)*Out*In, random = ~1|n, correlation = corAR1(), 
            method = 'ML', data = df.minuta.m)
lme3a <- lme(valueT~T1+(T2+T3)*Out*In-T2:Out:In, random = ~1|n, 
             correlation = corAR1(), method = 'ML', data = df.minuta.m)
lme3b <- lme(valueT~T1+(T2+T3)*Out*In-T3:Out:In, random = ~1|n, 
             correlation = corAR1(), method = 'ML', data = df.minuta.m)

anova(lme2, lme3a)
anova(lme2, lme3b) # No significant differences, best improvement for T3 (lme3b) better AIC

# Third round: further reduction T3
lme3 <- lme(valueT~T1+(T2+T3)*Out*In-T3:Out:In, random = ~1|n, 
            correlation = corAR1(), method = 'ML', data = df.minuta.m)
lme4a <- lme(valueT~T1+(T2+T3)*Out*In-T3:Out:In-T3:Out, random = ~1|n, 
             correlation = corAR1(), method = 'ML', data = df.minuta.m)
lme4b <- lme(valueT~T1+(T2+T3)*Out*In-T3:Out:In-T3:In, random = ~1|n, 
             correlation = corAR1(), method = 'ML', data = df.minuta.m)

anova(lme3, lme4a)
anova(lme3, lme4b) # Significant difference by excluding interaction with In (lme4b) gives better AIC

# Fourth round: further reductions
lme4 <- lme(valueT~T1+T2*Out*In+T3*Out, random = ~1|n, correlation = corAR1(), 
            method = 'ML', data = df.minuta.m)
lme5a <- lme(valueT~T1+T2*Out*In+T3*Out-T3:Out, random = ~1|n, 
             correlation = corAR1(), method = 'ML', data = df.minuta.m)
lme5b <- lme(valueT~T1+T2*Out*In+T3*Out-T2:Out:In, random = ~1|n, 
             correlation = corAR1(), method = 'ML', data = df.minuta.m)
lme5c <- lme(valueT~T1+T2*Out*In+T3*Out-T1, random = ~1|n, 
             correlation = corAR1(), method = 'ML', data = df.minuta.m)

anova(lme4, lme5a)
anova(lme4, lme5b)
anova(lme4, lme5c) # All significant decreases in AIC

# Final model
lme5 <- lme(valueT~T1+T2*Out*In+T3*Out, random = ~1|n, correlation = corAR1(), 
            method = 'REML', data = df.minuta.m)

summary(lme5)
AIC(lme5)

df.minuta.coef <- cbind.data.frame(coef(summary(lme5)), intervals(lme5)$fixed[, c(1, 3)])

if(b.save) { 
  write.table(df.minuta.coef, file = paste0(s.data, 'D_SI_C9S2_CoefficientsLMEMinuta.txt'), 
              row.names = T, sep = ',')
}

#####Step 4: Plotting#####
df.minuta.mod <- df.minuta.m
df.minuta.mod$Pred <- predict(lme5)
df.minuta.mod$Resid <- resid(lme5)

# For CI on predictions, use Monte Carlo: https://stats.stackexchange.com/questions/231074/confidence-intervals-on-predictions-for-a-non-linear-mixed-model-nlme
df.minuta.mod$In <- factor(df.minuta.mod$In, c('None', 'Low', 'High'))
df.minuta.mod$Out <- factor(df.minuta.mod$Out, c('None', 'Low', 'High'))

if(b.plot){
  # Model description
  p.ResponseFitted <- ggplot(df.minuta.mod, aes(Pred, Resid)) + 
    geom_hline(yintercept = 0, colour = 'grey60', linetype = 'dashed', size = 0.4) + 
    geom_point(size = 1) + 
    scale_x_continuous('Fitted values') + 
    scale_y_continuous('Residuals', limits = c(-0.1, 0.1)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.ResponseFitted)
  
  p.QQplot <- ggplot(df.minuta.mod, aes(sample = Resid)) + 
    stat_qq(size = 1) + 
    scale_x_continuous('Theoretical') + 
    scale_y_continuous('Residuals', limits = c(-0.1, 0.1)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.QQplot)
  
  p.Model <- ggarrange(p.ResponseFitted, p.QQplot, nrow = 1, ncol = 2, 
                       labels = 'AUTO', label.x = 0.17, label.y = 0.975, 
                       font.label = list(size = 10, face = 'bold'), align = 'hv')
  plot(p.Model)
  
  # Residuals versus predictors
  p.PredictorIn <- ggplot(df.minuta.mod, aes(In, Resid)) + 
    geom_hline(yintercept = 0, colour = 'grey60', linetype = 'dashed', size = 0.4) + 
    geom_boxplot() + 
    scale_x_discrete('Introduction frequency') + 
    scale_y_continuous('Residuals', limits = c(-0.1, 0.1)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.PredictorIn)
  
  p.PredictorOut <- ggplot(df.minuta.mod, aes(Out, Resid)) + 
    geom_hline(yintercept = 0, colour = 'grey60', linetype = 'dashed', size = 0.4) + 
    geom_boxplot() + 
    scale_x_discrete('Removal frequency') + 
    scale_y_continuous('Residuals', limits = c(-0.1, 0.1)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.PredictorOut)
  
  p.PredictorTime <- ggplot(df.minuta.mod, aes(Time, Resid)) + 
    geom_hline(yintercept = 0, colour = 'grey60', linetype = 'dashed', size = 0.4) + 
    geom_point(size = 1) + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous('Residuals', limits = c(-0.1, 0.1)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.PredictorTime)
  
  p.Predictors <- ggarrange(p.PredictorIn, p.PredictorOut, p.PredictorTime, 
                            nrow = 1, ncol = 3, labels = 'AUTO', label.x = 0.25, 
                            label.y = 0.975, font.label = list(size = 10, face = 'bold'), 
                            align = 'hv')
  plot(p.Predictors)
  
  # Predictions
  df.pred <- c()
  for (i in c(1:length(levels(df.minuta.mod$In)))){
    for (j in c(1:length(levels(df.minuta.mod$Out)))){
      for (k in c(1:length(levels(df.minuta.mod$variable)))){
        df.temp <- df.minuta.mod[df.minuta.mod$In == levels(df.minuta.mod$In)[i] & 
                                   df.minuta.mod$Out == levels(df.minuta.mod$Out)[j] & 
                                   df.minuta.mod$variable == levels(df.minuta.mod$variable)[k], ]
        df.pred <- rbind.data.frame(df.pred, 
                                    cbind.data.frame(levels(df.minuta.mod$In)[i], 
                                                     levels(df.minuta.mod$Out)[j], 
                                                     mean(df.temp$Time), 
                                                     mean(df.temp$value), 
                                                     sd(df.temp$value), 
                                                     mean(df.temp$Pred^3), 
                                                     sd(df.temp$Pred^3)))
      }
    }
  }
  names(df.pred) <- c('In', 'Out', 'Time', 'Mean', 'sd_Mean', 'Pred', 'sd_Pred')
  
  p.LME <- ggplot(df.pred, aes(x = Time)) + 
    geom_ribbon(aes(ymin = Pred - sd_Pred, ymax = Pred + sd_Pred), fill = 'grey90') + 
    geom_errorbar(aes(ymin = Mean - sd_Mean, ymax = Mean + sd_Mean), 
                  width = 0.3, colour = 'grey60') + 
    geom_point(aes(y = Mean), size = 1, colour = 'grey30') + 
    geom_line(aes(y = Pred), size = 0.4, colour = 'black') + 
    scale_x_continuous('Time (day)') + 
    scale_y_continuous('Dry biomass (g)', limits = c(0, 1.7), 
                       breaks = c(0, 0.5, 1, 1.5)) + 
    facet_grid(In~Out) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.LME)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C9S2_LMEMinutaModel.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.Model)
    dev.off()
    
    tiff(paste0(s.figs, 'F_SI_C9S2_LMEMinutaPredictors.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.Predictors)
    dev.off()
    
    tiff(paste0(s.figs, 'F_SI_C9S2_LMEMinutaPredictions.tiff'), units = 'mm', 
         width = 160, height = 120, res = 300, pointsize = 7)
    plot(p.LME)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.biomass, df.bm.dw, df.minor, df.minor.m, df.minuta, df.minuta.m, 
   df.minor.var, df.minuta.var, df.minorT.var, df.minutaT.var, df.temp, i, j, k, 
   p.Histo, p.MeanTrend, p.VarianceTrend, p.HistoMinor, p.HistoMinuta, 
   p.HistoMinorTrans, p.HistoMinutaTrans, p.Histograms, lm0, lme0, lme1a, lme1b, 
   lme1c, lme1, lme2a, lme2b, lme2, lme3a, lme3b, lme3, lme4a, lme4b, lme4, 
   df.minor.mod, p.ResponseFitted, p.QQplot, p.Model, p.PredictorIn, 
   p.PredictorOut, p.PredictorTime, p.Predictors, df.pred, p.LME, lme5a, lme5b, 
   lme5c, lme5, df.minuta.mod)
