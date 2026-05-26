##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
#Remarks: Last successful run: 20/12/2019; Last changes: 23/11/2019
###############Section 2: Packages########################################
# install.packages('missForest') # To create artificially missing data
# install.packages('Hmisc') # For mean imputation
# install.packages('VIM') # For kNN imputation
# install.packages('ggplot2'); install.packages('ggpubr') # For plotting
###############Section 3: Libraries#######################################
library(missForest)
library(Hmisc)
library(VIM)
library(ggplot2)
library(ggpubr)
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

##############Part 2: Value-specific comparison, example##################
###############Section 1: In- and output##################################
v.sel <- 281 # 661 = Smallest dataset with 1% missing data; 281 = maximum data points (10 variables, 100% instances, 50% missing)
df.data <- lst.data[[v.sel]][, c(3:ncol(lst.data[[v.sel]]))]
lst.imp <- list()
df.orig <- lst.base.ext[[ceiling(v.sel / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(v.sel / (n.Nrep * length(v.fNA)))]]))]
df.sum.avg <- df.sum.ls <- df.sum.kNN <- df.sum.mF <- data.frame(matrix(ncol = 5))
names(df.sum.avg) <- names(df.sum.ls) <- names(df.sum.kNN) <- names(df.sum.mF) <- 
  c('Selection', 'Variable', 'Method', 'Original', 'Imputed')

################Imputation via mean (10 secs/run)#########################
print(paste0('Imputation for dataset ', v.sel))
df.imp.avg <- df.data
##############Subsection: Imputation
for (j in c(1:ncol(df.data))){
  df.imp.avg[,j] <- impute(df.data[, j], mean)
}
lst.imp[[1]] <- df.imp.avg
##############Subsection: Individual accuracy
v <- 1
for (j in c(1:ncol(df.data))){
  for (k in c(1:nrow(df.data))){
    if(is.na(df.data[k, j])){
      df.sum.avg[v, ] <- c(v.sel, names(df.data)[j], 'mean', 
                           df.orig[k, j], round(df.imp.avg[k, j], 3))
      v <- v + 1
    }
  }
}

if(b.save){
  write.table(df.imp.avg, paste0(s.data, 'D_SI_C5S5_ExampleCompleteData', v.sel, '_avg.txt'), 
              row.names = F, sep = ',')
}

################Imputation via ls (LSImpute) (6 secs/run)#################
# Remark: check Bo (2004) for more information
print(paste0('Imputation for dataset ', v.sel))
##############Subsection: Start with mean
df.temp <- df.data
for (j in c(1:ncol(df.data))){
  df.temp[, j] <- impute(df.temp[, j], mean)
}
##############Subsection: Iterative imputation
v <- 2; max.e <- c(1, 0)
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
lst.imp[[2]] <- df.imp.ls
##############Subsection: Individual accuracy
v <- 1
for (j in c(1:ncol(df.data))){
  for (k in c(1:nrow(df.data))){
    if(is.na(df.data[k,j])){
      df.sum.ls[v, ] <- c(v.sel, names(df.data)[j], 'ls', df.orig[k, j], round(df.imp.ls[k, j], 3))
      v <- v + 1
    }
  }
}

if(b.save){
  write.table(df.imp.ls, paste0(s.data, 'D_SI_C5S5_ExampleCompleteData', v.sel, '_ls.txt'), 
              row.names = F, sep = ',')
}
################Imputation via kNN (20 secs/run)##########################
print(paste0('Imputation for dataset ', v.sel))
df.chem <- df.data
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
lst.imp[[3]] <- df.imp.kNN
#############Subsection: Individual accuracy
v <- 1
for (j in c(1:ncol(df.data))){
  for (k in c(1:nrow(df.data))){
    if(is.na(df.data[k, j])){
      df.sum.kNN[v, ] <- c(v.sel, names(df.data)[j], 'kNN', df.orig[k, j], round(df.imp.kNN[k, j], 3))
      v <- v + 1
    }
  }
}

if(b.save){
  write.table(df.imp.kNN, paste0(s.data, 'D_SI_C5S5_ExampleCompleteData', v.sel, '_kNN.txt'), 
              row.names = F, sep = ',')
}
################Imputation via mF (90 secs/run)###########################
print(paste0('Imputation for dataset ', v.sel))
df.chem <- df.data
##############Subsection: Imputation
set.seed(621) # Seed to make results reproducible
df.imp.mF <- missForest(df.chem, replace = F)$ximp # missForest creates more output, only imputed part is extracted
lst.imp[[4]] <- df.imp.mF
v <- 1
for (j in c(1:ncol(df.data))){
  for (k in c(1:nrow(df.data))){
    if(is.na(df.data[k, j])){
      df.sum.mF[v, ] <- c(v.sel, names(df.data)[j], 'mF', df.orig[k, j], round(df.imp.mF[k, j], 3))
      v <- v + 1
    }
  }
}

if(b.save){
  write.table(df.imp.mF, paste0(s.data, 'D_SI_C5S5_ExampleCompleteData', v.sel, '_mF.txt'),
              row.names = F, sep = ',')
}

###############Section 3: Plotting and saving#############################
df.all <- rbind(df.sum.avg, df.sum.ls, df.sum.kNN, df.sum.mF)
if(b.plot){
  # Plot 1: Method-specific assessments of distortions
  df.orig.m <- melt(df.orig)
  df.orig.m$Series <- 'Original'
  for (i in c(1:length(lst.imp))){
    df.dat.m <- melt(lst.imp[[i]])
    df.dat.m$Series <- 'Imputed'
    df.comb <- rbind.data.frame(df.orig.m, df.dat.m)
    p.Boxplot <- ggplot(df.comb, aes(x = Series, y = value)) + 
      geom_boxplot() + 
      scale_x_discrete('') + 
      scale_y_continuous('') + 
      coord_flip() + 
      facet_wrap(~variable, ncol = 2, scales = 'free') + 
      theme_bw() + 
      theme(panel.grid = element_blank(), 
            axis.text = element_text(colour = 'black', size = 7), 
            strip.background = element_blank())
    plot(p.Boxplot)
    
    if(b.save){
      tiff(paste0(s.figs, 'F_SI_C5S5_ExampleDistortion', v.sel, '_', 
           c('avg', 'ls', 'kNN', 'mF')[i], '.tiff'), 
      units = 'mm', width = 160, height = 180, res = 300, pointsize = 7)
      plot(p.Boxplot)
      dev.off()
    }
  }
  
  # Plot 2: Pairwise comparison of imputed values
  df.all$Method <- factor(df.all$Method, c('mean', 'ls', 'kNN', 'mF'))
  df.all$Variable <- as.factor(df.all$Variable)
  df.all$Original <- as.numeric(df.all$Original)
  df.all$Imputed <- as.numeric(df.all$Imputed)
  for (i in c(1:length(levels(df.all$Variable)))){
    df.temp <- df.all[df.all$Variable == levels(df.all$Variable)[i], ]
    n.min <- min(df.temp$Original, df.temp$Imputed)
    n.max <- max(df.temp$Original, df.temp$Imputed)
    p.Imputed <- ggplot(df.temp, aes(x = Original, y = Imputed)) + 
      geom_abline(slope = 1, intercept = 0, size = 0.4, 
                  linetype = 'dotted', colour = 'grey40') + 
      geom_point(size = 1) + 
      scale_x_continuous(paste0('Original value for ', levels(df.all$Variable)[i]),
                         limits = c(n.min, n.max)) + 
      scale_y_continuous('Imputed value', limits = c(n.min, n.max)) + 
      facet_wrap(~Method, ncol = 2) + 
      theme_bw() + 
      theme(panel.grid = element_blank(), 
            axis.text = element_text(colour = 'black', size = 7), 
            axis.title = element_text(size = 9), 
            strip.background = element_blank())
    plot(p.Imputed)
    
    if(b.save){
      tiff(paste0(s.figs, 'F_SI_C5S5_ExampleImputed', v.sel, '_', 
                  levels(df.all$Variable)[i], '.tiff'), 
           units = 'mm', width = 160, height = 90, res = 300, pointsize = 7)
      plot(p.Imputed)
      dev.off()
    }
  }
}

if(b.save){
  write.table(df.all, paste0(s.data, 'D_SI_C5S5_ExampleImputatedValues', v.sel, '.txt'),
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(v.sel, df.sum.avg, df.sum.ls, df.sum.kNN, df.sum.mF, i, df.imp.avg, df.data, 
   j, k, df.orig, v, df.temp, max.e, v.avg, v.sd, df.imp.ls, cov.scale, m.cov, 
   df.chem, df.scale, df.imp.temp, df.imp.kNN, df.imp.mF, df.all, df.orig.m, 
   df.dat.m, df.comb, lst.imp, n.min, n.max, p.Boxplot, p.Imputed)
