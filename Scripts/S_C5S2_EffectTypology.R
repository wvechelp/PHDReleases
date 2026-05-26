##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 23/11/2019; Last changes: 04/08/2020
###############Section 2: Packages########################################
# install.packages('missForest') # To create artificially missing data
# install.packages('VIM') # For kNN imputation
# install.packages('ggplot2') # For plotting
###############Section 3: Libraries#######################################
library(missForest)
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
                                     round(100 * v.fVar[i], 0), '.txt'), header = T, sep = ',') 
}
df.info <- read.table(paste0(s.data, 'D_C5S1_BaselineDataInfo.txt'), 
                      header = T, sep = ',')
df.loc <- read.table(paste0(s.data, 'D_C5_AllLocationsType.txt'), 
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

##############Part 2: Typology inclusion for kNN (9 hours)################
###############Section 1: In- and output##################################
v.sel <- seq(1, length(lst.data), 10)
v.nrmse.kNN <- v.nrmse.kNN.typ <- c()

###############Section 2: Imputation by kNN, without typology#############
w <- 1
for (i in v.sel){
  print(paste0('Imputation for dataset ', i))
  df.chem <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  ##############Subsection: Scaling of continuous, dummy scores for categorical
  v.avg <- round(apply(df.chem, 2, function(x) mean(x, na.rm = T)), 2)
  v.sd <- round(apply(df.chem, 2, function(x) sd(x, na.rm = T)), 2)
  df.scale <- as.data.frame(scale(df.chem, center = T, scale = T))
  ##############Subsection: Imputation
  df.imp.knn <- as.data.frame(kNN(df.scale, variable = colnames(df.scale), 
                                  numFun = mean, imp_var = FALSE))
  row.names(df.imp.knn) <- row.names(df.scale)
  ##############Subsection: Re-scaling
  df.imp.tmp <- df.chem
  for (j in c(1:ncol(df.chem))){
    for (k in c(1:nrow(df.chem))){
      if(is.na(df.imp.tmp[k,j])){
        df.imp.tmp[k, j] <- (df.imp.knn[k, j] * v.sd[j]) + v.avg[j]        
      }
    }
  }
  ##############Subsection: NRMSE
  df.orig <- lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  v.nrmse.kNN[w] <- missForest::nrmse(df.imp.tmp, df.chem,df.orig)
  print(v.nrmse.kNN[w])
  w <- w + 1
}

###############Section 3: Imputation by kNN, with typology################
w <- 1
for (i in v.sel){
  print(paste0('Imputation for dataset ', i))
  # Merging needed, because for some locations there is no description
  df.data <- merge(lst.data[[i]], unique(df.loc[, c(1, 4:5)])) 
  df.chem <- df.data[, c(3:ncol(lst.data[[i]]))]
  df.loc.sub <- df.data[, c(ncol(df.data) - 1, ncol(df.data))]
  ##############Subsection: Scaling of continuous, dummy scores for categorical
  v.avg <- round(apply(df.chem, 2, function(x) mean(x, na.rm = T)), 2)
  v.sd <- round(apply(df.chem, 2, function(x) sd(x, na.rm = T)), 2)
  df.temp <- scale(df.chem, center = T, scale = T)
  for (level in unique(df.loc.sub$Gebied)){
    df.loc.sub[paste('dummy', level, sep = '_')] <- ifelse(df.loc.sub$Gebied == level, 1, 0)
  }
  for (level in unique(df.loc.sub$Type.mp)){
    df.loc.sub[paste('dummy', level, sep = '_')] <- ifelse(df.loc.sub$Type.mp == level, 1, 0)
  }
  ##############Subsection: Imputation
  df.scale <- cbind(df.temp, df.loc.sub[, c(3:ncol(df.loc.sub))])
  df.imp.knn <- kNN(df.scale, variable = colnames(df.scale), numFun = mean, imp_var = F)[, c(1:ncol(df.chem))]
  ##############Subsection: Re-scaling
  df.imp.tmp <- df.chem
  for (j in c(1:ncol(df.chem))){
    for (k in c(1:nrow(df.chem))){
      if(is.na(df.imp.tmp[k, j])){
        df.imp.tmp[k, j] <- (df.imp.knn[k, j] * v.sd[j]) + v.avg[j]        
      }
    }
  }
  ##############Subsection: NRMSE
  df.orig <- merge(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]], 
                   unique(df.loc[, c(1, 4:5)]))[, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  v.nrmse.kNN.typ[w] <- missForest::nrmse(df.imp.tmp, df.chem, df.orig)
  print(v.nrmse.kNN.typ[w])
  w <- w + 1
}

###############Section 4: Plotting and saving#############################
df.kNN <- cbind.data.frame('kNN', v.nrmse.kNN, v.nrmse.kNN.typ)
names(df.kNN) <- c('Method', 'Without', 'With')
if(b.plot){
  p.kNNTypology <- ggplot(df.kNN, aes(x = With, y = Without)) + 
    geom_abline(intercept = 0, slope = 1) + 
    geom_point() + 
    scale_x_continuous(limits = c(0, 1)) + 
    scale_y_continuous(limits = c(0, 1)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.text = element_text(colour = 'black'))
  plot(p.kNNTypology)
}
if(b.save){
  write.table(df.kNN, file = paste0(s.data, 'D_SI_C5S2_IncludingTypology_kNN.txt'), 
              row.names = F, sep = ',')
}

###############Section 5: Variable removal################################
rm(v.sel, df.imp.tmp, v.nrmse.kNN, v.nrmse.kNN.typ, w, i, df.chem, df.loc.sub, 
   v.avg, v.sd, level, df.scale, df.imp.knn, j, k, df.orig, df.kNN, p.kNNTypology)

##############Part 3: Typology inclusion for mF (22 hours)################
###############Section 1: In- and output##################################
v.sel <- seq(1, length(lst.data), 10)
v.nrmse.mF <- v.nrmse.mF.typ <- c()

###############Section 2: imputation by mF, without typology##############
w <- 1
for (i in v.sel){
  print(paste0('Imputation for dataset ', i))
  df.chem <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  set.seed(621) # Seed to make results reproducible
  df.imp.tmp <- missForest(df.chem, replace = FALSE)$ximp # missForest creates more output, only imputed part is extracted
  ##############Subsection: NRMSE
  df.orig <- lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  v.nrmse.mF[w] <- missForest::nrmse(df.imp.tmp, df.chem, df.orig)
  print(v.nrmse.mF[w])
  w <- w + 1
}

###############Section 3: Imputation by mF, with typology#################
w <- 1
for (i in v.sel){
  print(paste0('Imputation for dataset ', i))
  ##############Subsection: Include typology
  # Merging needed, because for some locations there is no description
  df.temp <- merge(lst.data[[i]], unique(df.loc[, c(1, 4:5)])) 
  df.data <- df.temp[, c(3:ncol(df.temp))] # Excluding Spatiotemporal information
  df.data$Gebied <- as.factor(df.data$Gebied)
  df.data$Type.mp <- as.factor(df.data$Type.mp) # Consider typology as classes
  ##############Subsection: Imputation
  set.seed(621) # Seed to make results reproducible
  df.imp.tmp <- missForest(df.data[, c(1:ncol(df.data))], replace = FALSE)$ximp # missForest creates more output, only imputed part is extracted
  ##############Subsection: NRMSE
  df.orig <- merge(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]], 
                   unique(df.loc[, c(1, 4:5)]))[, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  v.nrmse.mF.typ[w] <- missForest::nrmse(df.imp.tmp[, c(1:ncol(df.orig))], 
                                         df.data[, c(1:ncol(df.orig))], df.orig)
  print(v.nrmse.mF.typ[w])
  w <- w + 1
}

###############Section 4: Plotting and saving#############################
df.mF <- cbind.data.frame('mF', v.nrmse.mF, v.nrmse.mF.typ)
names(df.mF) <- c('Method', 'Without', 'With')
if(b.plot){
  p.mFTypology <- ggplot(df.mF, aes(x = With, y = Without)) + 
    geom_abline(intercept = 0, slope = 1) + 
    geom_point() + 
    scale_x_continuous(limits = c(0, 1)) + 
    scale_y_continuous(limits = c(0, 1)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.text = element_text(colour = 'black'))
  plot(p.mFTypology)
}
if(b.save){
  write.table(df.mF, file = paste0(s.data,'D_SI_C5S2_IncludingTypology_mF.txt'), 
              row.names = F, sep = ',')
}

###############Section 5: Variable removal################################
rm(v.sel, df.imp.tmp, v.nrmse.mF, v.nrmse.mF.typ, w, i, df.temp, df.orig, df.mF, 
   p.mFTypology)

##############Part 4: Combining information###############################
###############Section 1: In- and output##################################
df.kNN <- read.table(paste0(s.data, 'D_SI_C5S2_IncludingTypology_kNN.txt'), 
                     header = T, sep = ',')
df.mF <- read.table(paste0(s.data, 'D_SI_C5S2_IncludingTypology_mF.txt'), 
                    header = T, sep = ',')

###############Section 2: Plotting and saving#############################
df.typ <- rbind(df.kNN, df.mF)
if(b.plot){
  p.EffectTypology <- ggplot(df.typ, aes(x = With, y = Without)) + 
    geom_abline(intercept = 0, slope = 1, linewidth = 0.4) + 
    geom_point(size = 0.8) + 
    scale_x_continuous(limits = c(0, 1)) + 
    scale_y_continuous(limits = c(0, 1)) + 
    facet_grid(.~Method) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.EffectTypology)
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C5S2_EffectTypology.tiff'), units = 'mm', 
         width = 160, height = 75, res = 300, pointsize = 7)
    plot(p.EffectTypology)
    dev.off()
  }
}
if(b.save){
  write.table(df.typ, paste0(s.figs, 'FD_SI_C5S2_EffectTypology.txt'), 
              row.names = F, sep = ',')
}

###############Section 3: Variable removal################################
rm(df.kNN, df.mF, df.typ, p.EffectTypology)
