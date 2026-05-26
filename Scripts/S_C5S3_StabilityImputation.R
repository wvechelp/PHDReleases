##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 12/12/2019; Last changes: 05/08/2020
###############Section 2: Packages########################################
# install.packages('missForest') # To create artificially missing data
# install.packages('Hmisc') # For mean imputation
# install.packages('VIM') # For kNN imputation
# install.packages('ggplot2') # For plotting
###############Section 3: Libraries#######################################
library(missForest)
library(Hmisc)
library(VIM)
library(ggplot2)
###############Section 4: Global variables################################
s.data <- '../Data/'
s.figs <- '../Figures/'
v.fVar <- c(1.50, 1.00, 0.50) # To limit potential correlations within the data, affecting imputation
v.fObs <- c(1.0, 0.75, 0.50, 0.25) # Decreases number of observations, which affects power and techniques
v.fNA <- c(0.01, 0.05, 0.10, 0.20, 0.50, 0.75) # Fraction of missing data to be created
n.Nrep <- 10 # Repetitions for creating missing data
###############Section 5: Plot and save###################################
b.plot <- T; b.save <- T

##############Part 1: Baseline data (30 secs)#############################
###############Section 1: In- and output##################################
lst.base <- list()
for (i in c(1:length(v.fVar))){ 
  lst.base[[i]] <- read.table(paste0(s.data, 'D_C5S1_BaselineData_', 
                                     round(100 * v.fVar[i], 0), '.txt'), 
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
                                              replace = FALSE), ]
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

##############Part 2: Stability of imputation methods (3 hours)###########
###############Section 1: In- and output##################################
v.sel <- c(123, 123, 123, 357, 357, 357, 485, 485, 485)
v.nrmse.avg <- v.nrmse.ls <- v.nrmse.kNN <- v.nrmse.mF <- c()

###############Section 2: Imputation######################################
################Imputation via mean (10 secs/run)#########################
w <- 1
for (i in v.sel){
  print(paste0('Imputation ', w, ' of ', length(v.sel)))
  df.imp.avg <- df.data <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  ##############Subsection: Imputation
  for (j in c(1:ncol(df.data))){
    df.imp.avg[, j] <- impute(df.data[, j], mean)
  }
  ##############Subsection: NRMSE
  df.orig <- lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  v.nrmse.avg[w] <- missForest::nrmse(df.imp.avg, lst.data[[i]][, c(3:ncol(lst.data[[i]]))], df.orig)
  w <- w + 1
}
df.avg <- cbind.data.frame('mean', v.sel, v.nrmse.avg)
names(df.avg) <- c('Method', 'Selection', 'NRMSE')

if(b.save){
  write.table(df.avg, file = paste0(s.data, 'D_SI_C5S3_ImputationStability_avg.txt'), 
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
  v <- 2; max.e <- c(1, 0)
  while(max.e[v] / max.e[(v-1)] < 0.99 & v < 11){ # Thresholds are subjective...
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
  ##############Subsection: NRMSE
  df.orig <- lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  v.nrmse.ls[w] <- missForest::nrmse(df.imp.ls, lst.data[[i]][, c(3:ncol(lst.data[[i]]))], df.orig)
  w <- w + 1
}
df.ls <- cbind.data.frame('ls', v.sel, v.nrmse.ls)
names(df.ls) <- c('Method', 'Selection', 'NRMSE')

if(b.save){
  write.table(df.ls, file = paste0(s.data, 'D_SI_C5S3_ImputationStability_ls.txt'), 
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
  df.imp.temp <- as.data.frame(kNN(df.scale, variable = colnames(df.scale), 
                                   numFun = mean, imp_var = F))
  row.names(df.imp.temp) <- row.names(df.scale)
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
  w <- w + 1
}
df.kNN <- cbind.data.frame('kNN', v.sel, v.nrmse.kNN)
names(df.kNN) <- c('Method', 'Selection', 'NRMSE')

if(b.save){
  write.table(df.kNN, file = paste0(s.data, 'D_SI_C5S3_ImputationStability_kNN.txt'), 
              row.names = F, sep = ',')
}

################Imputation via mF (90 secs/run)###########################
w <- 1
for (i in v.sel){
  print(paste0('Imputation for dataset ', i))
  df.chem <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  ##############Subsection: Imputation
  set.seed(621) # Seed to make results reproducible
  df.imp.mF <- missForest(df.chem, replace = F)$ximp # missForest creates more output, only imputed part is extracted
  ##############Subsection: NRMSE
  df.orig <- lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  v.nrmse.mF[w] <- missForest::nrmse(df.imp.mF, df.chem, df.orig)
  w <- w + 1
}
df.mF <- cbind.data.frame('mF', v.sel, v.nrmse.mF)
names(df.mF) <- c('Method', 'Selection', 'NRMSE')

if(b.save){
  write.table(df.mF, file = paste0(s.data, 'D_SI_C5S3_ImputationStability_mF.txt'), 
              row.names = F, sep = ',')
}

###############Section 3: Plotting and saving#############################
df.all <- rbind(df.avg, df.ls, df.kNN, df.mF)
if(b.plot){
  df.all$Method <- factor(df.all$Method, c('mean', 'ls', 'kNN', 'mF'))
  p.StabilityImputation <- ggplot(df.all, aes(x = as.character(Selection), y = NRMSE)) + 
    geom_point() + 
    scale_x_discrete('Dataset') + 
    facet_grid(.~Method) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(face = 'bold', size = 8))
  plot(p.StabilityImputation)
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C5S3_StabilityImputation.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.StabilityImputation)
    dev.off()
  }
}

if(b.save){
  write.table(df.all, paste0(s.figs, 'FD_SI_C5S3_StabilityImputation.txt'),
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(v.sel, v.nrmse.avg, v.nrmse.ls, v.nrmse.kNN, v.nrmse.mF, w, i, j, df.data, 
   df.orig, df.avg, df.imp.avg, df.temp, df.imp.ls, v, max.e, v.avg, v.sd, 
   df.imp.temp, cov.scale, m.cov, k, df.ls, df.scale, df.imp.kNN, df.kNN, 
   df.imp.mF)