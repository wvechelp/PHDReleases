##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
#Remarks: Last successful run: 20/12/2019; Last changes: 05/08/2020
###############Section 2: Packages########################################
# install.packages('missForest') # To create artificially missing data
# install.packages('VIM') # For kNN imputation
# install.packages('reshape2') # For melting tables, a.o.
# install.packages('parallel'); install.packages('foreach'); install.packages('doParallel') # For parallel computing
# install.packages('ggplot2')# For plotting
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
                             prodNA(lst.base.ext[[i]][, c(3:ncol(lst.base.ext[[i]]))], noNA = v.fNA[j]))
      w <- w + 1
    }
  }
}

###############Section 4: Variable removal################################
rm(lst.base, w, i, j, k)

##############Part 2: Optimisation hyperparameters########################
###############Section 1: In- and output##################################
v.sel <- which(df.info$MD %in% c(0.05, 0.20, 0.75) & df.info$Obs %in% c(0.25, 1))
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
    v.kNew <- c(z - 3 * y, z - 3 * y + 1, z - 3 * y + 2, 
                z + 3 * y - 2, z + 3 * y - 1, z + 3 * y)
    v.kNew <- v.kNew[v.kNew > 0]
    # Start iteration
    cl <- makeCluster(detectCores() - 1); registerDoParallel(cl)
    df.set.temp <- foreach(n = 1:length(v.kNew), .combine = rbind, .packages = c('VIM','missForest')) %dopar% {
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
    c(v.ntree0[n], floor(sqrt(ncol(df.chem))), 1, missForest::nrmse(df.imp.mF, df.chem, df.orig))
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
    v.ntree.new <- c(ceiling((1 - 0.5 / y) * df.set$ntree[z]), df.set$ntree[z], 
                     round((1 + 0.5 / y) * df.set$ntree[z]))
    v.mtry.new <- c(ceiling((1 - 0.5 / y) * df.set$mtry[z]), df.set$mtry[z], 
                    round((1 + 0.5 / y) * df.set$mtry[z]))
    v.node.new <- c(ceiling((1 - 0.5 / y) * df.set$nodesize[z]), 
                    df.set$nodesize[z], round((1 + 0.5 / y) * df.set$nodesize[z]))
    df.set.new <- unique(expand.grid(v.ntree.new, v.mtry.new, v.node.new, stringsAsFactors = TRUE))
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
  write.table(df.opt.kNN, paste0(s.data, 'D_SI_C5S7_OptimisationVariabilitykNN.txt'),
              row.names = F, sep = ',')
  write.table(df.opt.mF, paste0(s.data, 'D_SI_C5S7_OptimisationVariabilitymF.txt'),
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(v.sel, df.opt.kNN, df.opt.mF, i, df.chem, df.set, v.avg, v.sd, df.scale, 
   df.imp.temp, df.imp.kNN, j, k, df.orig, x, y, z, v.kNew, cl, df.set.temp, 
   min.nrmse0, min.nrmse1, e, v.ntree0, v.ntree.new, v.mtry.new, 
   v.node.new, df.set.new)

##############Part 3: Statistics and plotting#############################
###############Section 1: In- and output##################################
df.perf.bl <- read.table(paste0(s.data, 'D_C5S4_ImputationPerformance.txt'), 
                         header = T, sep = ',')
df.opt.kNN <- read.table(paste0(s.data, 'D_SI_C5S7_OptimisationVariabilitykNN.txt'),
                         header = T, sep = ',')
df.opt.mF <- read.table(paste0(s.data, 'D_SI_C5S7_OptimisationVariabilitymF.txt'),
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

###############Section 3: Plotting and saving#############################
if(b.plot){
  ##############Subsection: Preparation
  # For kNN
  df.perf.kNN.m <- melt(df.perf.kNN, id.vars = c('MD', 'Var', 'Obs'), 
                        measure.vars = c('k', 'NRMSE'))
  df.perf.kNN.m$Var <- as.factor(df.perf.kNN.m$Var)
  df.perf.kNN.m$Obs <- as.factor(df.perf.kNN.m$Obs)
  levels(df.perf.kNN.m$Var) <- list("Nvar = 5" = "0.5", "Nvar = 10" = "1", "Nvar = 15" = "1.5")
  levels(df.perf.kNN.m$Obs) <- list("Ninst = 25%" = "0.25", "Ninst = 100%" = "1")
  
  # For mF
  df.perf.mF.m <- melt(df.perf.mF, id.vars = c('MD', 'Var', 'Obs'), 
                       measure.vars = c('ntree', 'mtry', 'nodesize', 'NRMSE'))
  df.perf.mF.m$Var <- as.factor(df.perf.mF.m$Var)
  df.perf.mF.m$Obs <- as.factor(df.perf.mF.m$Obs)
  levels(df.perf.mF.m$Var) <- list("Nvar = 5" = "0.5", "Nvar = 10" = "1", "Nvar = 15" = "1.5")
  levels(df.perf.mF.m$Obs) <- list("Ninst = 25%" = "0.25", "Ninst = 100%" = "1")

  ##############Subsection: Plotting
  # For kNN
  levels(df.perf.kNN.m$variable) <- list('Neighbours (-)' = 'k', 'NRMSE (-)' = 'NRMSE')
  p.VariabilityKNN <- ggplot(df.perf.kNN.m, aes(x = as.factor(100 * MD), y = value)) + 
    geom_boxplot() + 
    scale_x_discrete('Fraction missing data (%)') + 
    scale_y_continuous('') + 
    facet_grid(variable~Var + Obs, scales = 'free_y', switch = 'y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text = element_text(size = 8))
  p.VariabilityKNN
  
  # For mF
  levels(df.perf.mF.m$variable) <- list('ntree (-)' = 'ntree', 
                                        'mtry (-)' = 'mtry', 
                                        'nodesize (-)' = 'nodesize',
                                        'NRMSE (-)' = 'NRMSE')
  p.VariabilityMF <- ggplot(df.perf.mF.m, aes(x = as.factor(100 * MD), y = value)) + 
    geom_boxplot() + 
    scale_x_discrete('Fraction missing data (%)') + 
    scale_y_continuous('') + 
    facet_grid(variable~Var + Obs, scales = 'free_y', switch = 'y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.text = element_text(colour = 'black', size = 7),
          axis.title = element_text(size = 9), 
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text = element_text(size = 8))
  p.VariabilityMF
  
  ##############Subsection: Saving
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C5S7_OptimisationVariabilitykNN.tiff'), units = 'mm',
         width = 160, height = 105, res = 300, pointsize = 7)
    plot(p.VariabilityKNN)
    dev.off()
    tiff(paste0(s.figs, 'F_SI_C5S7_OptimisationVariabilitymF.tiff'), units = 'mm',
         width = 160, height = 150, res = 300, pointsize = 7)
    plot(p.VariabilityMF)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.perf.bl, df.opt.kNN, df.opt.mF, df.sub, df.perf.kNN, df.perf.mF, 
   df.perf.kNN.m, df.perf.mF.m, p.VariabilityMF, p.VariabilityKNN)
