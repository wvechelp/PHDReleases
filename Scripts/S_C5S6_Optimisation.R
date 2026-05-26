##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 20/12/2019; Last changes: 05/08/2020
###############Section 2: Packages########################################
# install.packages('missForest') # To create artificially missing data
# install.packages('VIM') # For kNN imputation
# install.packages('reshape2') # For melting tables, a.o.
# install.packages('parallel'); install.packages('foreach'); install.packages('doParallel') # For parallel computing
# install.packages('ggplot2') # For plotting
###############Section 3: Libraries#######################################
library(missForest)
library(VIM)
library(reshape2)
library(parallel)
library(foreach)
library(doParallel)
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

##############Part 2: Optimisation hyperparameters########################
###############Section 1: In- and output##################################
v.sel <- seq(1, length(lst.data), 5)
df.opt.kNN <- df.opt.mF <- c()

###############Section 2: Optimisation####################################
################Imputation via kNN########################################
for (i in v.sel){
  print(paste0('Optimise for dataset ', i))
  df.chem <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  df.set <- data.frame(matrix(nrow = 1, ncol = 2))
  names(df.set) <- c('k', 'NRMSE')
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
  df.set$k[1] <- 5
  df.set$NRMSE[1] <- missForest::nrmse(df.imp.kNN, df.chem, df.orig)
  ##############Subsection: Start iteration
  x <- 1; y <- 1
  while(x < 11 & y < 4){
    # Define new range of settings
    z <- df.set$k[which.min(df.set$NRMSE)]
    v.kNew <- c(z - 3 * y, z - 3 * y + 1, z - 3 * y + 2, z + 3 * y - 2, 
                z + 3 * y - 1, z + 3 * y)
    v.kNew <- v.kNew[v.kNew > 0]
    # Start iteration
    cl <- makeCluster(detectCores() - 1); registerDoParallel(cl)
    df.set.temp <- foreach(n = 1:length(v.kNew), .combine = rbind, .packages = c('VIM', 'missForest')) %dopar% {
      df.imp.temp <- as.data.frame(kNN(df.scale, k = v.kNew[n], 
                                       variable = colnames(df.scale), 
                                       numFun = mean, imp_var = F))
      row.names(df.imp.temp) <- row.names(df.scale)
      df.imp.kNN <- df.chem
      for (j in c(1:ncol(df.chem))){
        for (k in c(1:nrow(df.chem))){
          if(is.na(df.imp.kNN[k, j])){
            df.imp.kNN[k, j] <- (df.imp.temp[k, j] * v.sd[j]) + v.avg[j]        
          }
        }
      }
      c(v.kNew[n], missForest::nrmse(df.imp.kNN, df.chem, df.orig))
    }
    stopCluster(cl); registerDoSEQ()
    df.set.temp <- as.data.frame(df.set.temp)
    names(df.set.temp) <- c('k', 'NRMSE')
    row.names(df.set.temp) <- c()
    # Determine original setting and settings after new range
    min.nrmse0 <- min(df.set$NRMSE)
    df.set <- rbind(df.set, df.set.temp)
    min.nrmse1 <- min(df.set$NRMSE)
    e <- min.nrmse0 - min.nrmse1
    if(e == 0){ # If no change in NRMSE, increase counter
      y <- y + 1
    }
    x <- x + 1
  }
  df.opt.kNN <- rbind.data.frame(df.opt.kNN, 
                                 cbind.data.frame(i, x, df.set[which.min(df.set$NRMSE), ]))
}
names(df.opt.kNN) <- c('ID', 'Iterations', 'k', 'NRMSE')
################Imputation via missForest#################################
v.ntree0 <- c(25, 50, 100)
for (i in v.sel){
  print(paste0(strftime(Sys.time(), format = "%H:%M"), ' - Optimise for dataset ', i))
  df.chem <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  df.orig <- lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))]
  # Starting point
  cl <- makeCluster(detectCores() - 1); registerDoParallel(cl)
  df.set <- foreach (n = 1:length(v.ntree0), .combine = rbind, .packages = c('missForest')) %dopar% {
    set.seed(621) # Seed to make results reproducible
    df.imp.mF <- missForest(df.chem, maxiter = 10, ntree = v.ntree0[n], replace = F)$ximp 
    c(v.ntree0[n], floor(sqrt(ncol(df.chem))), 1, 
      missForest::nrmse(df.imp.mF, df.chem, df.orig))
  }
  stopCluster(cl); registerDoSEQ()
  df.set <- as.data.frame(df.set)
  names(df.set) <- c('ntree', 'mtry', 'nodesize', 'NRMSE')
  row.names(df.set) <- c()
  # Iterations
  y <- 1; x <- 1
  while(x < 6){ # x represents total number of iterations
    # Determine new setting combinations
    z <- which.min(df.set$NRMSE)
    v.ntree.new <- c(ceiling((1 - 0.5 / y) * df.set$ntree[z]),
                     df.set$ntree[z], round((1 + 0.5 / y) * df.set$ntree[z]))
    v.mtry.new <- c(ceiling((1 - 0.5 / y) * df.set$mtry[z]),
                    df.set$mtry[z], round((1 + 0.5 / y) * df.set$mtry[z]))
    v.node.new <- c(ceiling((1 - 0.5 / y) * df.set$nodesize[z]),
                    df.set$nodesize[z], round((1 + 0.5 / y) * df.set$nodesize[z]))
    df.set.new <- unique(expand.grid(v.ntree.new, v.mtry.new, v.node.new, 
                                     stringsAsFactors = T))
    names(df.set.new) <- c('ntree', 'mtry', 'nodesize')
    df.set.new <- df.set.new[order(df.set.new$ntree, decreasing = T), ]
    # Parallel setting assessment
    cl <- makeCluster(detectCores() - 1); registerDoParallel(cl)
    df.set.temp <- foreach (n = 1:nrow(df.set.new), .combine = rbind, .packages = c('missForest')) %dopar% {
      set.seed(621)
      df.imp.mF <- missForest(df.chem, maxiter = 10, ntree = df.set.new$ntree[n], 
                              mtry = df.set.new$mtry[n], 
                              nodesize = c(df.set.new$nodesize[n], 5), 
                              replace = F)$ximp # Adapt to maxiter = 10
      cbind(df.set.new[n, ], missForest::nrmse(df.imp.mF, df.chem, df.orig))
    }
    stopCluster(cl); registerDoSEQ()
    df.set.temp <- as.data.frame(df.set.temp)
    names(df.set.temp) <- names(df.set)
    row.names(df.set.temp) <- c()
    # Determine original setting and settings after new range
    min.nrmse0 <- min(df.set$NRMSE)
    df.set <- rbind.data.frame(df.set, df.set.temp)
    min.nrmse1 <- min(df.set$NRMSE)
    e <- min.nrmse0 - min.nrmse1
    if(e == 0){ # If no change in NRMSE, increase counter
      y <- y + 1
    }
    x <- x + 1
  }
  df.opt.mF <- rbind.data.frame(df.opt.mF, cbind(i, x, df.set[which.min(df.set$NRMSE), ]))
}
names(df.opt.mF) <- c('ID', 'Iterations', 'ntree', 'mtry', 'nodesize', 'NRMSE')

###############Section 3: Plotting and saving#############################
if(b.save){
  write.table(df.opt.kNN, paste0(s.data, 'D_SI_C5S6_OptimisedSettingskNN.txt'), 
              row.names = F, sep = ',')
  write.table(df.opt.mF, paste0(s.data, 'D_SI_C5S6_OptimisedSettingsmF.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(v.sel, df.opt.kNN, df.opt.mF, i, df.chem, df.set, v.avg, v.sd, df.scale, 
   df.imp.temp, df.imp.kNN, j, k, df.orig, x, y, z, v.kNew, cl, df.set.temp, 
   min.nrmse0, min.nrmse1, e, v.ntree0, v.ntree.new, v.mtry.new, v.node.new, 
   df.set.new)

##############Part 3: Statistics and plotting#############################
###############Section 1: In- and output##################################
df.perf.bl <- read.table(paste0(s.data, 'D_C5S4_ImputationPerformance.txt'), 
                         header = T, sep = ',')
df.opt.kNN <- read.table(paste0(s.data, 'D_SI_C5S6_OptimisedSettingskNN.txt'), 
                         header = T, sep = ',')
df.opt.mF <- read.table(paste0(s.data, 'D_SI_C5S6_OptimisedSettingsmF.txt'), 
                        header = T, sep = ',')

# kNN dataset
df.sub <- df.perf.bl[df.perf.bl$Method == 'kNN', 
                     which(names(df.perf.bl) %in% c('ID', 'MD', 'Var', 'Obs', 'NRMSE'))] # Method-specific subset with limited columns
df.perf.kNN <- cbind.data.frame(df.sub[df.opt.kNN$ID, ], 
                                df.opt.kNN$k, df.opt.kNN$NRMSE)
names(df.perf.kNN)[6:7] <- c('k', 'NRMSE_opt')

# mF dataset
df.sub <- df.perf.bl[df.perf.bl$Method == 'mF', 
                     which(names(df.perf.bl) %in% c('ID', 'MD', 'Var', 'Obs', 'NRMSE'))] # Method-specific subset with limited columns
df.perf.mF <- cbind.data.frame(df.sub[df.opt.mF$ID, ], df.opt.mF$ntree, 
                               df.opt.mF$mtry, df.opt.mF$nodesize, df.opt.mF$NRMSE)
names(df.perf.mF)[6:9] <- c('ntree', 'mtry', 'nodesize', 'NRMSE_opt')

###############Section 2: Statistics######################################
# kNN
summary(df.perf.kNN$k)
sort(df.perf.kNN$k)[c(48, 96)] # Thresholds to split range in 3 parts (represent 33% and 66%)
summary(df.perf.kNN$NRMSE_opt - df.perf.kNN$NRMSE)
sd(df.perf.kNN$NRMSE_opt - df.perf.kNN$NRMSE)

#mF
summary(df.perf.mF$ntree)
summary(df.perf.mF$nodesize)
summary(df.perf.mF$mtry[df.perf.mF$Var == 0.5])
summary(df.perf.mF$mtry[df.perf.mF$Var == 1.0])
summary(df.perf.mF$mtry[df.perf.mF$Var == 1.5])
sum(df.perf.mF$ntree < 100) / nrow(df.perf.mF) # Fraction data sets requiring less than 100 trees
sort(df.perf.mF$ntree)[c(36, 72, 108)] # Thresholds to split range in 4 parts (represent 25%, 50% and 75%)
summary(df.perf.mF$NRMSE_opt - df.perf.mF$NRMSE)
sd(df.perf.mF$NRMSE_opt - df.perf.mF$NRMSE)

###############Section 3: Plotting and saving#############################
################Subsection: Overview improvement##########################
if(b.plot){
  # Preparation
  df.perf.kNN$Method <- 'kNN'; df.perf.mF$Method <- 'mF'
  df.perf.both <- rbind.data.frame(
    df.perf.kNN[, which(names(df.perf.kNN) %in% c('MD', 'NRMSE', 'NRMSE_opt', 'Method'))], 
    df.perf.mF[, which(names(df.perf.mF) %in% c('MD', 'NRMSE', 'NRMSE_opt', 'Method'))])
  
  # Plotting
  p.OptimisedImputation <- ggplot(df.perf.both,aes(x = NRMSE, y = NRMSE_opt - NRMSE)) + 
    geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey', size = 0.4) + 
    geom_point(size = 1) + 
    facet_grid(Method~MD) + 
    scale_x_continuous('Baseline NRMSE (-)', breaks = c(0, 0.5, 1.0), 
                       limits = c(-0.1,1.2)) + 
    scale_y_continuous(expression(paste(Delta, 'NRMSE (-)', sep = '')), 
                       limits = c(-0.5, 0.1)) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(colour = 'black', face = 'bold', size = 8))
  p.OptimisedImputation
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C5S6_OptimisedImputationPerformance.tiff'), 
         units = 'mm', width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.OptimisedImputation)
    dev.off()
  }
}

################Subsection: Parameter-specific graphs#####################
if(b.plot){
  # Optimal k for kNN
  p.OptimisedKValue <- ggplot(df.perf.kNN, aes(as.factor(MD), k)) + 
    geom_boxplot(fill = 'lightgrey', colour = 'grey') + 
    geom_hline(yintercept = 5, linetype = 'dashed') + 
    geom_point(size = 1) +
    scale_x_discrete('Degree of missing data') + 
    scale_y_continuous(expression(Number~of~neighbours~(italic(k[nn])))) + 
    scale_shape_discrete('') +
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          legend.text = element_text(size = 8))
  p.OptimisedKValue
  
  # Optimal ntree for mF
  p.OptimisedNtree <- ggplot(df.perf.mF, aes(as.factor(MD), ntree)) + 
    geom_boxplot(fill = 'lightgrey', colour = 'grey') + 
    geom_hline(yintercept = 100, linetype = 'dashed') + 
    geom_point(size = 1) +
    scale_x_discrete('Degree of missing data') + 
    scale_y_continuous('Number of individual trees (ntree)') + 
    scale_shape_discrete('') +
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7),
          axis.title = element_text(size = 9),
          legend.text = element_text(size = 8))
  p.OptimisedNtree
  
  # Optimal mtry for mF
  p.OptimisedMtry <- ggplot(df.perf.mF) + 
    geom_boxplot(aes(x = as.factor(MD), y = mtry), fill = 'lightgrey', colour = 'grey') + 
    geom_line(aes(x = 9 * MD, y = floor(sqrt(10 * Var))), linetype = 'dashed') + # Multiplying MD needed to cross complete graph (discrete steps for boxplots)
    geom_point(aes(x = as.factor(MD), y = mtry), size = 1) +
    scale_x_discrete('Degree of missing data') + 
    scale_y_continuous('Number of variables (mtry)') + 
    scale_shape_discrete('') +
    facet_grid(as.factor(Var)~., scales = 'free_y') +
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7),
          axis.title = element_text(size = 9),
          legend.text = element_text(size = 8),
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text = element_text(size = 8))
  p.OptimisedMtry
  
  # Optimal nodesize for mF
  p.OptimisedNodesize <- ggplot(df.perf.mF, aes(as.factor(MD), nodesize)) + 
    geom_boxplot(fill = 'lightgrey', colour = 'grey') + 
    geom_hline(yintercept = 1, linetype = 'dashed') + 
    geom_point(size = 1) +
    scale_x_discrete('Degree of missing data') + 
    scale_y_continuous('Number of elements in node (nodesize)') + 
    scale_shape_discrete('') +
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7),
          axis.title = element_text(size = 9),
          legend.text = element_text(size = 8))
  p.OptimisedNodesize
  
  # Saving
  if(b.save){
    tiff(paste0(s.figs, 'F_C5S6_OptimisedKNN.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.OptimisedKValue)
    dev.off()
    tiff(paste0(s.figs, 'F_SI_C5S6_OptimisedMF_ntree.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.OptimisedNtree)
    dev.off()
    tiff(paste0(s.figs, 'F_SI_C5S6_OptimisedMF_mtry.tiff'), units = 'mm', 
         width = 160, height = 135, res = 300, pointsize = 7)
    plot(p.OptimisedMtry)
    dev.off()
    tiff(paste0(s.figs, 'F_SI_C5S6_OptimisedMF_nodesize.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.OptimisedNodesize)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.perf.bl, df.opt.kNN, df.opt.mF, df.sub, df.perf.kNN, df.perf.mF, 
   df.perf.both, p.OptimisedImputation, p.OptimisedKValue, p.OptimisedNtree, 
   p.OptimisedNodesize, p.OptimisedMtry)
