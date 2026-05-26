##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 20/12/2019; Last changes: 05/08/2020
###############Section 2: Packages########################################
# install.packages('missForest') # To create artificially missing data
# install.packages('Hmisc') # For mean imputation
# install.packages('VIM') # For kNN imputation
# install.packages('reshape2') # For melting tables, a.o.
# install.packages('lmerTest') # For developing (and analysing) lmer-models
# install.packages('multcomp') # For multiple comparison in mixed models
# install.packages('ggplot2') # For plotting
###############Section 3: Libraries#######################################
library(missForest) 
library(Hmisc)
library(VIM)
library(reshape2)
library(lmerTest)
library(multcomp)
library(ggplot2)
###############Section 4: Global variables################################
s.data <- '../Data/'
s.figs <- '../Figures/'
v.fVar <- c(1.50, 1.00, 0.50) # To limit potential correlations within the data, affecting imputation
v.fObs <- c(1.0, 0.75, 0.50, 0.25) # Decreases number of observations, which affects power and techniques
v.fNA <- c(0.01, 0.05, 0.10, 0.20, 0.50, 0.75) # Fraction of missing data to be created
n.Nrep <- 10 # Repetitions for creating missing data
v.met <- c('mean', 'ls', 'kNN', 'mF') # Vector with methods
###############Section 5: Plot and save###################################
dir.create('./Temp'); dir.create('./Graphs') # Create directories for temporary data and graphs
b.plot <- T; b.save <- T

##############Part 1: Baseline data (30 secs)#############################
###############Section 1: In- and output##################################
lst.base <- list()
for (i in c(1:length(v.fVar))){ 
  lst.base[[i]] <- read.table(paste0(s.data, 'D_C5S1_BaselineData_', round(100 * v.fVar[i], 0), '.txt'),
                              header = T, sep = ',') 
}
df.info <- read.table(paste0(s.data, 'D_C5S1_BaselineDataInfo.txt'), 
                      header = T, sep = ',')

###############Section 2: Selecting number of observations################
lst.base.ext <- list()
w <- 1
for (i in c(1:length(v.fVar))){
  for (j in c(1:length(v.fObs))){
    set.seed(621) # Seed to make results reproducible
    lst.base.ext[[w]] <- lst.base[[i]][sample(c(1:nrow(lst.base[[i]])), 
                                              floor(v.fObs[j] * nrow(lst.base[[i]])),
                                              replace = F), ]
    lst.base.ext[[w]] <- lst.base.ext[[w]][order(row.names(lst.base.ext[[w]])), ]
    w <- w + 1
  }
}

if(length(v.fVar) * length(v.fObs) != length(lst.base.ext)){
  message('Incorrect number of data sets - Check codes!')
}

###############Section 3: Creating artificially missing data##############
lst.data <- list()
w <- 1
for (i in c(1:length(lst.base.ext))){
  for (j in c(1:length(v.fNA))){
    for (k in c(1:n.Nrep)){
      set.seed(k) # Seed to make results reproducible, semi-fixed (otherwise exact replicates)
      lst.data[[w]] <- cbind(lst.base.ext[[i]][, c(1, 2)], 
                             prodNA(lst.base.ext[[i]][, c(3:ncol(lst.base.ext[[i]]))], 
                                    noNA = v.fNA[j]))
      w <- w + 1
    }
  }
}

###############Section 4: Variable removal################################
rm(lst.base, w, i, j, k)

##############Part 2: Comparison of imputation methods (7 days)###########
###############Section 1: In- and output##################################
v.sel <- c(1:length(lst.data))
v.nrmse.avg <- v.nrmse.ls <- v.nrmse.kNN <- v.nrmse.mF <- c()
v.time.avg <- v.time.ls <- v.time.kNN <- v.time.mF <- c()

###############Section 2: Imputation######################################
################Imputation via mean (10 secs/run)#########################
w <- 1
for (i in v.sel){
  print(paste0('Imputation ', w, ' of ', length(v.sel)))
  df.imp.avg <- df.data <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  ##############Subsection: Imputation
  n.time0 <- Sys.time()
  for (j in c(1:ncol(df.data))){
    df.imp.avg[, j] <- impute(df.data[, j], mean)
  }
  n.time1 <- Sys.time()
  ##############Subsection: NRMSE
  df.orig <- lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  v.nrmse.avg[w] <- missForest::nrmse(df.imp.avg, lst.data[[i]][, c(3:ncol(lst.data[[i]]))], df.orig)
  v.time.avg[w] <- round(difftime(n.time1, n.time0, units = 'secs'), 3)
  w <- w + 1
}

df.avg <- cbind.data.frame(v.sel, df.info[v.sel, ], 'mean', v.nrmse.avg, v.time.avg)
names(df.avg) <- c('ID', names(df.info), 'Method', 'NRMSE', 'Time')

if(b.save){
  write.table(df.avg, file = paste0(s.data, 'D_SI_C5S4_ImputationPerformance_avg.txt'),
              row.names = F, sep = ',')
}

################Imputation via ls (LSImpute) (6 secs/run)#################
# Remark: check Bo (2004) for more information
w <- 1
for (i in v.sel){ # Adapt to (i in v.sel)
  print(paste0('Imputation ', w, ' of ', length(v.sel)))
  df.data <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  ##############Subsection: Start with mean
  df.temp <- df.data
  for (j in c(1:ncol(df.data))){
    df.temp[, j] <- impute(df.temp[, j], mean)
  }
  ##############Subsection: Iterative imputation
  v <- 2; max.e <- c(1, 0); n.time0 <- Sys.time()
  while(max.e[v] / max.e[(v - 1)] < 0.99 & v < 11){ # Thresholds are subjective...
    if(max.e[2] == 0){ v <- 1 }
    print(paste0('Iteration ', v))
    v.avg <- round(apply(df.temp, 2, mean), 2)
    v.sd <- round(apply(df.temp, 2, sd), 2)
    m.cov <- cov(df.temp)
    df.imp.ls <- df.temp
    for (j in c(1:ncol(df.imp.ls))){
      # Determine matrix product of covariances for each variable/ 'solve' is for inverse
      cov.scale <- (m.cov[j, -j]) %*% solve(m.cov[-j, -j]) 
      for (k in c(1:nrow(df.imp.ls))){
        if(is.na(df.data[k, j])){
          df.imp.ls[k, j] <- v.avg[j] + cov.scale %*% t(df.temp[k, -j] - v.avg[-j])
        }
      }
    }
    max.e[v + 1] <- max(abs(cov(scale(df.imp.ls, center = T, scale = T)) - 
                              cov(scale(df.temp, center = T, scale = T))))
    df.temp <- df.imp.ls
    v <- v + 1
  }
  n.time1 <- Sys.time()
  ##############Subsection: NRMSE
  df.orig <- lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  v.nrmse.ls[w] <- missForest::nrmse(df.imp.ls, lst.data[[i]][, c(3:ncol(lst.data[[i]]))], df.orig)
  v.time.ls[w] <- round(difftime(n.time1, n.time0, units = 'secs'), 3)
  w <- w + 1
}

df.ls <- cbind.data.frame(v.sel, df.info[v.sel, ], 'ls', v.nrmse.ls, v.time.ls)
names(df.ls) <- c('ID', names(df.info), 'Method', 'NRMSE', 'Time')

if(b.save){
  write.table(df.ls, file = paste0(s.data, 'D_SI_C5S4_ImputationPerformance_ls.txt'),
              row.names = F, sep = ',')
}

################Imputation via kNN (20 secs/run)##########################
w <- 1
for (i in v.sel){
  print(paste0('Imputation for dataset ', i))
  df.chem <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  ##############Subsection: Scaling of continuous, dummy scores for categorical
  v.avg <- round(apply(df.chem, 2, function(x) mean(x, na.rm = T)), 2)
  v.sd <- round(apply(df.chem, 2, function(x) sd(x, na.rm = T)), 2)
  df.scale <- as.data.frame(scale(df.chem, center = T, scale = T))
  ##############Subsection: Imputation
  n.time0 <- Sys.time()
  df.imp.temp <- as.data.frame(kNN(df.scale, variable = colnames(df.scale), 
                                   numFun = mean, imp_var = F))
  row.names(df.imp.temp) <- row.names(df.scale)
  n.time1 <- Sys.time()
  ##############Subsection: Re-scaling
  df.imp.kNN <- df.chem
  for (j in c(1:ncol(df.chem))){
    for (k in c(1:nrow(df.chem))){
      if(is.na(df.imp.kNN[k, j])){
        df.imp.kNN[k, j] <- (df.imp.temp[k, j] * v.sd[j]) + v.avg[j]        
      }
    }
  }
  ##############Subsection: NRMSE
  df.orig <- lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  v.nrmse.kNN[w] <- missForest::nrmse(df.imp.kNN, df.chem, df.orig)
  v.time.kNN[w] <- round(difftime(n.time1, n.time0, units = 'secs'), 3)
  w <- w + 1
}

df.kNN <- cbind.data.frame(v.sel, df.info[v.sel, ], 'kNN', v.nrmse.kNN, v.time.kNN)
names(df.kNN) <- c('ID', names(df.info), 'Method', 'NRMSE', 'Time')

if(b.save){
  write.table(df.kNN, file = paste0(s.data, 'D_SI_C5S4_ImputationPerformance_kNN.txt'),
              row.names = F, sep = ',')
}

################Imputation via mF (90 secs/run)###########################
w <- 1
for (i in v.sel){
  print(paste0('Imputation for dataset ', i))
  df.chem <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  ##############Subsection: Imputation
  n.time0 <- Sys.time()
  set.seed(621) # Seed to make results reproducible
  df.imp.mF <- missForest(df.chem, replace = F)$ximp #missForest creates more output, only imputed part is extracted
  n.time1 <- Sys.time()
  ##############Subsection: NRMSE
  df.orig <- lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  v.nrmse.mF[w] <- missForest::nrmse(df.imp.mF, df.chem, df.orig)
  v.time.mF[w] <- round(difftime(n.time1, n.time0, units = 'secs'), 3)
  w <- w + 1
}

df.mF <- cbind.data.frame(v.sel, df.info[v.sel, ], 'mF', v.nrmse.mF, v.time.mF)
names(df.mF) <- c('ID', names(df.info), 'Method', 'NRMSE', 'Time')

if(b.save){
  write.table(df.mF, file = paste0(s.data, 'D_SI_C5S4_ImputationPerformance_mF.txt'),
              row.names = F, sep = ',')
}

###############Section 3: Saving##########################################
df.all <- rbind(df.avg, df.ls, df.kNN, df.mF)
if(b.save){
  write.table(df.all, paste0(s.data, 'D_C5S4_ImputationPerformance.txt'),
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(v.sel, v.nrmse.avg, v.nrmse.ls, v.nrmse.kNN, v.nrmse.mF, v.time.avg, v.time.ls,
   v.time.kNN, v.time.mF, w, n.time0, n.time1, df.imp.avg, df.data, i, j, df.orig, 
   df.avg, df.temp, v, max.e, v.avg, v.sd, m.cov, cov.scale, k, df.ls, df.chem, 
   df.scale, df.kNN, df.mF)

##############Part 3a: Analysis - All data################################
###############Section 1: In- and output##################################
df.perf <- read.table(paste0(s.data, 'D_C5S4_ImputationPerformance.txt'), 
                      header = T, sep = ',')
df.perf$Method <- factor(df.perf$Method, v.met)
df.stat.nrmse <- data.frame(matrix(nrow = 4, ncol = 6))
names(df.stat.nrmse) <- c('Method', 'Min', 'Max', 'Avg', 'sd', 'se')
df.stat.time <- data.frame(matrix(nrow = 4, ncol = 6))
names(df.stat.time) <- c('Method', 'Min', 'Max', 'Avg', 'sd', 'se')

###############Section 2: Statistics######################################
################Subsection: Unconditional characteristic values
for (i in c(1:length(v.met))){
  df.temp <- df.perf[df.perf$Method == v.met[i], ]
  print(paste0('Proportion with NRMSE < 1 for ', v.met[i], ': ', round(sum(df.temp$NRMSE < 1) / 7.2, 2)))
  df.stat.nrmse$Method[i] <- df.stat.time$Method[i] <- v.met[i]
  df.stat.nrmse$Min[i] <- min(df.temp$NRMSE)
  df.stat.nrmse$Max[i] <- max(df.temp$NRMSE)
  df.stat.nrmse$Avg[i] <- mean(df.temp$NRMSE)
  df.stat.nrmse$sd[i] <- sd(df.temp$NRMSE)
  df.stat.nrmse$se[i] <- sqrt(sd(df.temp$NRMSE)) / nrow(df.temp)
  df.stat.time$Min[i] <- min(df.temp$Time)
  df.stat.time$Max[i] <- max(df.temp$Time)
  df.stat.time$Avg[i] <- mean(df.temp$Time)
  df.stat.time$sd[i] <- sd(df.temp$Time)
  df.stat.time$se[i] <- sqrt(sd(df.temp$Time)) / nrow(df.temp)
}

################Subsection: Dataset-specific performance and time
df.perf.all <- df.time.all <- data.frame(matrix(nrow = 72, ncol = 11)) 
names(df.perf.all) <- names(df.time.all) <- c('MD', 'Var', 'Obs', 'Mean', 'ls', 
                                              'kNN', 'mF', 'Mean_sd', 'ls_sd', 
                                              'kNN_sd', 'mF_sd')
w <- 1
for (i in v.fNA){
  for (j in v.fVar){
    for (k in v.fObs){
      df.temp <- df.perf[df.perf$MD == i & df.perf$Var == j & df.perf$Obs == k, ]
      df.perf.all$MD[w] <- i
      df.perf.all$Var[w] <- j
      df.perf.all$Obs[w] <- k
      df.time.all$MD[w] <- i
      df.time.all$Var[w] <- j
      df.time.all$Obs[w] <- k
      for (m in c(1:length(v.met))){
        df.perf.all[w, (3 + m)] <- mean(df.temp$NRMSE[df.temp$Method == v.met[m]])
        df.perf.all[w, (7 + m)] <- sd(df.temp$NRMSE[df.temp$Method == v.met[m]])
        df.time.all[w, (3 + m)] <- mean(df.temp$Time[df.temp$Method == v.met[m]])
        df.time.all[w, (7 + m)] <- sd(df.temp$Time[df.temp$Method == v.met[m]])
      }
      w <- w + 1
    }
  }
}

if(b.save){
  write.table(df.perf.all, paste0(s.data, 'D_C5S4_MethodSpecificPerformance.txt'), 
              row.names = F, sep = ',')
}

################Subsection: Method comparison per MD
df.stat.meth <- as.data.frame(matrix(nrow = length(v.fNA) * length(v.fVar) * length(v.fObs), ncol = 9))
names(df.stat.meth)[1:3] <- c('MD', 'Var', 'Obs')
w <- 1
for (i in v.fNA){
  for (j in v.fVar){
    for (k in v.fObs){
      df.temp <- df.perf[df.perf$MD == i & df.perf$Var == j & df.perf$Obs == k, ]
      lme.temp <- lmer(NRMSE~Method + (1|ID), data = df.temp)
      tuk.temp <- summary(glht(lme.temp, linfct = mcp(Method = 'Tukey'), test = adjusted('hochberg')))
      df.stat.meth$MD[w] <- i
      df.stat.meth$Var[w] <- j
      df.stat.meth$Obs[w] <- k
      df.stat.meth[w, 4:9] <- tuk.temp$test$pvalues[1:6]
      w <- w + 1
    }
  }
  if(w == 1){ names(df.stat.meth)[4:9] <- names(tuk.temp$test$coefficients) }
}

################Subsection: Mixed model for performance
lme.all.ext <- lmer(NRMSE~Method * MD * Obs * Var + (1|ID), data = df.perf)
lme.all.red <- lmer(NRMSE~Method * MD * Obs * Var - Method:MD:Obs:Var + (1|ID), data = df.perf)
anova(lme.all.red, lme.all.ext) # Significant difference, recommended to keep extended version
summary(lme.all.ext)
confint(lme.all.ext)
qqnorm(scale(resid(lme.all.ext)))
summary(glht(lme.all.ext, linfct = mcp(Method = 'Tukey'), test = adjusted('hochberg')))
###############Section 3: Plotting and saving#############################
if(b.plot){
  df.all.m <- c()
  for (i in c(1:4)){
    df.temp <- cbind.data.frame(df.perf$Method, df.perf$NRMSE, 
                                df.perf[df.perf$Method == v.met[i], ]$Method, 
                                df.perf[df.perf$Method == v.met[i], ]$NRMSE)
    names(df.temp) <- c('xvar', 'xvalue', 'yvar', 'yvalue')
    df.all.m <- rbind(df.all.m, df.temp)
  }
  df.all.m$xvar <- factor(df.all.m$xvar, v.met)
  df.all.m$yvar <- factor(df.all.m$yvar, v.met)
  # Plotting
  p.OverallPerf <- ggplot(df.all.m, aes(x = xvalue, y = yvalue)) + 
    geom_abline(intercept = 0, slope = 1, size = 0.4) + 
    geom_point(size = 0.4) + 
    scale_x_continuous('NRMSE (-)') + 
    scale_y_continuous('NRMSE (-)') + 
    facet_grid(yvar~xvar) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(face = 'bold', size = 8))
  plot(p.OverallPerf)
  if(b.save){
    tiff(paste0(s.figs, 'F_C5S4_OverallPerformance.tiff'), units = 'mm', 
         width = 160, height = 105, res = 300, pointsize = 7)
    plot(p.OverallPerf)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.stat.nrmse, df.stat.time, i, j, k, m, w, df.perf.all, df.time.all, 
   df.stat.meth, tuk.temp, lme.temp, df.temp, lme.all.ext, lme.all.red, 
   df.all.m, p.OverallPerf)

##############Part 3b: Analysis - Baseline performance####################
###############Section 1: In- and output##################################
# df.perf <- read.table(paste0(s.data, 'D_C5S4_ImputationPerformance.txt'), header = T, sep = ',')
# df.perf$Method <- factor(df.perf$Method, v.met)
df.stat.nrmse <- data.frame(matrix(nrow = 4, ncol = 5))
names(df.stat.nrmse) <- c('Method', 'Min', 'Max', 'Avg', 'sd')
df.stat.time <- data.frame(matrix(nrow = 4, ncol = 5))
names(df.stat.time) <- c('Method', 'Min', 'Max', 'Avg', 'sd')
df.sub <- df.perf[df.perf$Var == 1 & df.perf$Obs == 1, ]

###############Section 2: Statistics######################################
################Subsection: Characteristic values
for (i in c(1:length(v.met))){
  df.temp <- df.sub[df.sub$Method == v.met[i], ]
  print(paste0('Proportion with NRMSE < 1 for ', v.met[i], ': ', 
               round(sum(df.temp$NRMSE < 1) / 0.6, 2)))
  df.stat.nrmse$Method[i] <- df.stat.time$Method[i] <- v.met[i]
  df.stat.nrmse$Min[i] <- min(df.temp$NRMSE)
  df.stat.nrmse$Max[i] <- max(df.temp$NRMSE)
  df.stat.nrmse$Avg[i] <- mean(df.temp$NRMSE)
  df.stat.nrmse$sd[i] <- sd(df.temp$NRMSE)
  df.stat.time$Min[i] <- min(df.temp$Time)
  df.stat.time$Max[i] <- max(df.temp$Time)
  df.stat.time$Avg[i] <- mean(df.temp$Time)
  df.stat.time$sd[i] <- sd(df.temp$Time)
}

################Subsection: Dataset-specific performance and time
df.perf.bl <- df.time.bl <- data.frame(matrix(nrow = 6, ncol = 11))
names(df.perf.bl) <- names(df.time.bl) <- c('MD', 'Var', 'Obs', 'Mean', 'ls', 
                                            'kNN', 'mF', 'Mean_sd', 'ls_sd', 
                                            'kNN_sd', 'mF_sd')
w <- 1
for (i in v.fNA){
  df.temp <- df.sub[df.sub$MD == i, ]
  df.perf.bl$MD[w] <- i
  df.perf.bl$Var[w] <- 1
  df.perf.bl$Obs[w] <- 1
  df.time.bl$MD[w] <- i
  df.time.bl$Var[w] <- 1
  df.time.bl$Obs[w] <- 1
  for (j in c(1:length(v.met))){
    df.perf.bl[w, (3 + j)] <- mean(df.temp$NRMSE[df.temp$Method == v.met[j]])
    df.perf.bl[w, (7 + j)] <- sd(df.temp$NRMSE[df.temp$Method == v.met[j]])
    df.time.bl[w, (3 + j)] <- mean(df.temp$Time[df.temp$Method == v.met[j]])
    df.time.bl[w, (7 + j)] <- sd(df.temp$Time[df.temp$Method == v.met[j]])
  }
  w <- w + 1
}

################Subsection: Method comparison per MD
df.stat.meth <- as.data.frame(matrix(nrow = length(v.fNA), ncol = 7))
names(df.stat.meth)[1] <- 'MD'
w <- 1
for (i in v.fNA){
  df.temp <- df.sub[df.sub$MD == i, ]
  lme.temp <- lmer(NRMSE~Method + (1|ID), data = df.temp)
  tuk.temp <- summary(glht(lme.temp, linfct = mcp(Method = 'Tukey'), test = adjusted('hochberg')))
  df.stat.meth$MD[w] <- i
  df.stat.meth[w, 2:7] <- tuk.temp$test$pvalues[1:6]
  if(w == 1){ names(df.stat.meth)[2:7] <- names(tuk.temp$test$coefficients) }
  w <- w + 1
}

################Subsection: Mixed model
lme.bl.ext <- lmer(NRMSE~Method * MD + (1|ID), data = df.sub)
lme.bl.red1 <- lmer(NRMSE~Method + MD + (1|ID), data = df.sub)
lme.bl.red2 <- lmer(NRMSE~Method + (1|ID), data = df.sub)
anova(lme.bl.ext, lme.bl.red1, lme.bl.red2) # Significant differences, suggested to keep extended model
summary(lme.bl.ext)
confint(lme.bl.ext)
qqnorm(scale(resid(lme.bl.ext)))
summary(glht(lme.bl.ext, linfct = mcp(Method = 'Tukey'), test = adjusted('hochberg')))

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.sub.m <- melt(df.sub, id.vars = c('MD', 'Method'), measure.vars = c('NRMSE', 'Time'))
  names(df.sub.m) <- c('MD', 'Method', 'Series', 'Value')
  levels(df.sub.m$Series) <- list('NRMSE (-)' = 'NRMSE', 'Time (s)' = 'Time')
  
  # Plotting
  p.BaselineBoxplot <- ggplot(df.sub.m) + 
    geom_boxplot(aes(x = Method, y = Value), size = 0.3, outlier.size = 1) + 
    facet_grid(Series~as.factor(MD), scales = 'free_y', switch = 'y') + 
    scale_x_discrete('Method') + 
    ylab('') + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          strip.text = element_text(size = 8, face = 'bold'), 
          strip.background.x = element_blank(), 
          strip.background.y = element_blank(), 
          strip.placement = 'outside', 
          axis.title = element_text(size = 9), 
          axis.text.x = element_text(size = 7, colour = "black", angle = 90, vjust = 0.5), 
          axis.text.y = element_text(size = 7, colour = "black"))
  plot(p.BaselineBoxplot)
  
  # Saving
  if(b.save){
    tiff(paste0(s.figs, 'F_C5S4_BaselineBoxplots.tiff'), units = 'mm', 
         height = 90, width = 160, res = 300, pointsize = 7)
    plot(p.BaselineBoxplot)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.stat.nrmse, df.stat.time, df.sub, i, j, w, df.perf.bl, df.time.bl, 
   df.stat.meth, tuk.temp, lme.temp, df.temp, lme.bl.ext, lme.bl.red1, 
   lme.bl.red2, df.sub.m, p.BaselineBoxplot)

##############Part 3c: Analysis - Sample size variability#################
###############Section 1: In- and output##################################
# df.perf <- read.table(paste0(s.data, 'D_C5S4_ImputationPerformance.txt'), header = T, sep = ',')
# df.perf$Method <- factor(df.perf$Method, v.met)
df.stat.nrmse <- data.frame(matrix(nrow = 4, ncol = 5))
names(df.stat.nrmse) <- c('Method', 'Min', 'Max', 'Avg', 'sd')
df.stat.time <- data.frame(matrix(nrow = 4, ncol = 5))
names(df.stat.time) <- c('Method', 'Min', 'Max', 'Avg', 'sd')
df.sub <- df.perf[df.perf$Var == 1, ]

###############Section 2: Statistics######################################
################Subsection: Characteristic values
for (i in c(1:length(v.met))){
  df.temp <- df.sub[df.sub$Method == v.met[i], ]
  print(paste0('Proportion with NRMSE < 1 for ', v.met[i], ': ', 
               round(sum(df.temp$NRMSE < 1) / 2.4, 2)))
  df.stat.nrmse$Method[i] <- df.stat.time$Method[i] <- v.met[i]
  df.stat.nrmse$Min[i] <- min(df.temp$NRMSE)
  df.stat.nrmse$Max[i] <- max(df.temp$NRMSE)
  df.stat.nrmse$Avg[i] <- mean(df.temp$NRMSE)
  df.stat.nrmse$sd[i] <- sd(df.temp$NRMSE)
  df.stat.time$Min[i] <- min(df.temp$Time)
  df.stat.time$Max[i] <- max(df.temp$Time)
  df.stat.time$Avg[i] <- mean(df.temp$Time)
  df.stat.time$sd[i] <- sd(df.temp$Time)
}

################Subsection: Dataset-specific performance and time
df.perf.ss <- df.time.ss <- data.frame(matrix(nrow = 24, ncol = 11))
names(df.perf.ss) <- names(df.time.ss) <- c('MD', 'Var', 'Obs', 'Mean', 'ls', 
                                            'kNN', 'mF', 'Mean_sd', 'ls_sd', 
                                            'kNN_sd', 'mF_sd')
w <- 1
for (i in v.fNA){
  for (j in v.fObs){
    df.temp <- df.sub[df.sub$MD == i & df.sub$Obs == j, ]
    df.perf.ss$MD[w] <- i
    df.perf.ss$Var[w] <- 1
    df.perf.ss$Obs[w] <- j
    df.time.ss$MD[w] <- i
    df.time.ss$Var[w] <- 1
    df.time.ss$Obs[w] <- j
    for (k in c(1:length(v.met))){
      df.perf.ss[w, (3 + k)] <- mean(df.temp$NRMSE[df.temp$Method == v.met[k]])
      df.perf.ss[w, (7 + k)] <- sd(df.temp$NRMSE[df.temp$Method == v.met[k]])
      df.time.ss[w, (3 + k)] <- mean(df.temp$Time[df.temp$Method == v.met[k]])
      df.time.ss[w, (7 + k)] <- sd(df.temp$Time[df.temp$Method == v.met[k]])
    }
    w <- w + 1
  }
}

################Subsection: Method comparison per MD
df.stat.meth <- as.data.frame(matrix(nrow = length(v.fNA) * length(v.fObs), ncol = 8))
names(df.stat.meth)[1:2] <- c('MD', 'Obs')
w <- 1
for (i in v.fNA){
  for (j in v.fObs){
    df.temp <- df.sub[df.sub$MD == i & df.sub$Obs == j, ]
    lme.temp <- lmer(NRMSE~Method + (1|ID), data = df.temp)
    tuk.temp <- summary(glht(lme.temp, linfct = mcp(Method = 'Tukey'), test = adjusted('hochberg')))
    df.stat.meth$MD[w] <- i
    df.stat.meth$Obs[w] <- j
    df.stat.meth[w, 3:8] <- tuk.temp$test$pvalues[1:6]
    if(w == 1){ names(df.stat.meth)[3:8] <- names(tuk.temp$test$coefficients) }
    w <- w + 1
  }
}

################Subsection: Mixed model
lme.ss.ext <- lmer(NRMSE~Method * MD * Obs + (1|ID), data = df.sub)
lme.ss.red <- lmer(NRMSE~Method * MD * Obs - Method:MD:Obs + (1|ID), data = df.sub)
anova(lme.ss.ext, lme.ss.red) # No significant difference, suggested to keep extended model
summary(lme.ss.ext)
confint(lme.ss.ext)
qqnorm(scale(resid(lme.ss.ext)))
summary(glht(lme.ss.ext, linfct = mcp(Method = 'Tukey'), test = adjusted('hochberg')))

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.sub.m <- c()
  df.temp <- data.frame(matrix(nrow = 2, ncol = 6))
  names(df.temp) <- c('MD', 'Obs', 'Method', 'Series', 'Mean', 'sd')
  for (i in v.fNA){
    for (j in v.fObs){
      for (k in v.met){
        df.sel <- df.sub[df.sub$MD == i & df.sub$Obs == j & df.sub$Method == k, ]
        df.temp$MD <- i
        df.temp$Obs <- j
        df.temp$Method <- k
        df.temp$Series[1] <- 'NRMSE (-)'
        df.temp$Mean[1] <- round(mean(df.sel$NRMSE), 4)
        df.temp$sd[1] <- round(sd(df.sel$NRMSE), 4)
        df.temp$Series[2] <- 'Time (s)'
        df.temp$Mean[2] <- round(mean(df.sel$Time), 4)
        df.temp$sd[2] <- round(sd(df.sel$Time), 4)
        df.sub.m <- rbind.data.frame(df.sub.m, df.temp)
      }
    }
  }
  df.sub.m$Method <- factor(df.sub.m$Method, v.met)
  
  # Plotting
  p.SampleSize <- ggplot(df.sub.m) + 
    geom_pointrange(aes(x = as.factor(Obs), y = Mean, ymax = Mean + sd, 
                        ymin = Mean - sd, shape = Method), size = 0.2) +
    geom_line(linetype = 2, aes(x = as.factor(Obs), y = Mean, group = Method), size = 0.4) + 
    scale_shape_manual(values = c(20, 17, 15, 18)) +
    facet_grid(Series~MD, scales = 'free_y', switch = 'y') + 
    xlab('Fraction of instances (%)') + 
    scale_x_discrete(breaks = c(0.25, 0.50, 0.75, 1.00), 
                     labels = c('25','50','75','100')) + 
    ylab('') + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          strip.text = element_text(size = 8, face = 'bold'), 
          strip.background.x = element_blank(), 
          strip.background.y = element_blank(), 
          strip.placement = 'outside', 
          axis.title = element_text(size = 9), 
          axis.text.x = element_text(size = 7, colour = "black", vjust = 0.5), 
          axis.text.y = element_text(size = 7, colour = "black"), 
          legend.text = element_text(size = 8), 
          legend.title = element_blank())
  plot(p.SampleSize)
  
  # Saving
  if(b.save){
    tiff(paste0(s.figs, 'F_C5S4_EffectSampleSize.tiff'), units = 'mm', 
         height = 90, width = 160, res = 300, pointsize = 7)
    plot(p.SampleSize)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.stat.nrmse, df.stat.time, df.sub, i, df.perf.ss, df.time.ss, df.stat.meth, 
   tuk.temp, lme.temp, df.temp, lme.ss.ext, lme.ss.red, j, k, w, df.sel, 
   df.sub.m, p.SampleSize)

##############Part 3d: Analysis - Dimensionality##########################
###############Section 1: In- and output##################################
# df.perf <- read.table(paste0(s.data, 'D_C5S4_ImputationPerformance.txt'), header = T, sep = ',')
# df.perf$Method <- factor(df.perf$Method, v.met)
df.stat.nrmse <- data.frame(matrix(nrow = 4, ncol = 5))
names(df.stat.nrmse) <- c('Method', 'Min', 'Max', 'Avg', 'sd')
df.stat.time <- data.frame(matrix(nrow = 4, ncol = 5))
names(df.stat.time) <- c('Method', 'Min', 'Max', 'Avg', 'sd')
df.sub <- df.perf[df.perf$Obs == 1, ]

###############Section 2: Statistics######################################
################Subsection: Unconditional characteristic values
for (i in c(1:length(v.met))){
  df.temp <- df.sub[df.sub$Method == v.met[i], ]
  print(paste0('Proportion with NRMSE < 1 for ', v.met[i], ': ', 
               round(sum(df.temp$NRMSE < 1) / 2.4, 2)))
  df.stat.nrmse$Method[i] <- df.stat.time$Method[i] <- v.met[i]
  df.stat.nrmse$Min[i] <- min(df.temp$NRMSE)
  df.stat.nrmse$Max[i] <- max(df.temp$NRMSE)
  df.stat.nrmse$Avg[i] <- mean(df.temp$NRMSE)
  df.stat.nrmse$sd[i] <- sd(df.temp$NRMSE)
  df.stat.time$Min[i] <- min(df.temp$Time)
  df.stat.time$Max[i] <- max(df.temp$Time)
  df.stat.time$Avg[i] <- mean(df.temp$Time)
  df.stat.time$sd[i] <- sd(df.temp$Time)
}

################Subsection: Dataset-specific performance and time
df.perf.dim <- df.time.dim <- data.frame(matrix(nrow = length(v.fNA) * length(v.fVar), ncol = 11))
names(df.perf.dim) <- names(df.time.dim) <- c('MD', 'Var', 'Obs', 'Mean', 'ls', 
                                              'kNN', 'mF', 'Mean_sd', 'ls_sd', 
                                              'kNN_sd', 'mF_sd')
w <- 1
for (i in v.fNA){
  for (j in v.fVar){
    df.temp <- df.sub[df.sub$MD == i & df.sub$Var == j, ]
    df.perf.dim$MD[w] <- i
    df.perf.dim$Var[w] <- j
    df.perf.dim$Obs[w] <- 1
    df.time.dim$MD[w] <- i
    df.time.dim$Var[w] <- j
    df.time.dim$Obs[w] <- 1
    for (k in c(1:length(v.met))){
      df.perf.dim[w, (3 + k)] <- mean(df.temp$NRMSE[df.temp$Method == v.met[k]])
      df.perf.dim[w, (7 + k)] <- sd(df.temp$NRMSE[df.temp$Method == v.met[k]])
      df.time.dim[w, (3 + k)] <- mean(df.temp$Time[df.temp$Method == v.met[k]])
      df.time.dim[w, (7 + k)] <- sd(df.temp$Time[df.temp$Method == v.met[k]])
    }
    w <- w + 1
  }
}

################Subsection: Method comparison per MD
df.stat.meth <- as.data.frame(matrix(nrow = length(v.fNA) * length(v.fVar), ncol = 8))
names(df.stat.meth)[1:2] <- c('MD', 'Var')
w <- 1
for (i in v.fNA){
  for (j in v.fVar){
    df.temp <- df.sub[df.sub$MD == i & df.sub$Var == j, ]
    lme.temp <- lmer(NRMSE~Method + (1|ID), data = df.temp)
    tuk.temp <- summary(glht(lme.temp, linfct = mcp(Method = 'Tukey'), test = adjusted('hochberg')))
    df.stat.meth$MD[w] <- i
    df.stat.meth$Var[w] <- j
    df.stat.meth[w, 3:8] <- tuk.temp$test$pvalues[1:6]
    if(w == 1){ names(df.stat.meth)[3:8] <- names(tuk.temp$test$coefficients) }
    w <- w + 1
  }
}

################Subsection: Mixed model
lme.dim.ext <- lmer(NRMSE~Method * MD * Var + (1|ID), data = df.sub)
lme.dim.red <- lmer(NRMSE~Method * MD * Var - Method:MD:Var + (1|ID), data = df.sub)
anova(lme.dim.ext, lme.dim.red) # No significant difference, suggested to keep extended model
summary(lme.dim.ext)
confint(lme.dim.ext)
qqnorm(scale(resid(lme.dim.ext)))
summary(glht(lme.dim.ext, linfct = mcp(Method = 'Tukey'), test = adjusted('hochberg')))

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.sub.m <- c()
  df.temp <- data.frame(matrix(nrow = 2, ncol = 6))
  names(df.temp) <- c('MD', 'Var', 'Method', 'Series', 'Mean', 'sd')
  for (i in v.fNA){
    for (j in v.fVar){
      for (k in v.met){
        df.sel <- df.sub[df.sub$MD == i & df.sub$Var == j & df.sub$Method == k, ]
        df.temp$MD <- i
        df.temp$Var <- j
        df.temp$Method <- k
        df.temp$Series[1] <- 'NRMSE (-)'
        df.temp$Mean[1] <- round(mean(df.sel$NRMSE), 4)
        df.temp$sd[1] <- round(sd(df.sel$NRMSE), 4)
        df.temp$Series[2] <- 'Time (s)'
        df.temp$Mean[2] <- round(mean(df.sel$Time), 4)
        df.temp$sd[2] <- round(sd(df.sel$Time), 4)
        df.sub.m <- rbind.data.frame(df.sub.m, df.temp)
      }
    }
  }
  df.sub.m$Method <- factor(df.sub.m$Method, v.met)
  
  # Plotting
  p.Dimension <- ggplot(df.sub.m) + 
    geom_pointrange(aes(x = as.factor(Var), y = Mean, ymax = Mean + sd, 
                        ymin = Mean - sd, shape = Method), size = 0.2) +
    scale_shape_manual(values = c(20, 17, 15, 18)) +
    facet_grid(Series~MD, scales = 'free_y', switch = 'y') + 
    xlab('Fraction of variables (%)') + 
    scale_x_discrete(breaks = c(0.5, 1.0, 1.5), labels = c('50', '100', '150')) + 
    ylab('') + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(), 
          strip.text = element_text(size = 8, face = 'bold'), 
          strip.background.x = element_blank(), 
          strip.background.y = element_blank(), 
          strip.placement = 'outside', 
          axis.title = element_text(size = 9), 
          axis.text.x = element_text(size = 7, colour = "black", vjust = 0.5), 
          axis.text.y = element_text(size = 7, colour = "black"), 
          legend.text = element_text(size = 8), 
          legend.title = element_blank())
  plot(p.Dimension)
  
  # Saving
  if(b.save){
    tiff(paste0(s.figs, 'F_C5S4_EffectDimensions.tiff'), units = 'mm', 
         height = 90, width = 160, res = 300, pointsize = 7)
    plot(p.Dimension)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.stat.nrmse, df.stat.time, df.sub, i, df.perf.dim, df.time.dim, 
   df.stat.meth, tuk.temp, lme.temp, df.temp, lme.dim.ext, lme.dim.red, j, k, 
   w, df.sel, df.sub.m, p.Dimension)
